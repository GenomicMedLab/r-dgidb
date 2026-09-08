.fdaOutputFields <- c(
    "drug_name", "drug_concept_id", "fda_application_number",
    "fda_application_type", "fda_sponsor_name", "fda_product_number",
    "fda_brand_name", "fda_marketing_status", "fda_dosage_form",
    "fda_route", "fda_reference_drug", "fda_reference_standard",
    "fda_te_code", "fda_active_ingredients", "fda_data_last_updated",
    "fda_lookup_status", "fda_lookup_message"
)

.emptyFdaIngredients <- function() {
    data.frame(
        name = character(),
        strength = character(),
        stringsAsFactors = FALSE
    )
}

.fdaValue <- function(value) {
    if (is.null(value) || !length(value)) {
        return(NA_character_)
    }
    as.character(value[[1]])
}

.fdaIngredients <- function(ingredients) {
    if (is.null(ingredients) || !length(ingredients)) {
        return(.emptyFdaIngredients())
    }
    data.frame(
        name = vapply(ingredients, function(x) .fdaValue(x$name), character(1)),
        strength = vapply(
            ingredients, function(x) .fdaValue(x$strength), character(1)
        ),
        stringsAsFactors = FALSE
    )
}

.parseFdaApplication <- function(appNo) {
    if (!is.character(appNo) || length(appNo) != 1L || is.na(appNo)) {
        return(list(valid = FALSE, raw = appNo))
    }
    match <- regexec(
        "^drugsatfda[.](nda|anda|bla):([0-9]+)$",
        tolower(appNo)
    )
    parts <- regmatches(tolower(appNo), match)[[1]]
    if (!length(parts)) {
        return(list(valid = FALSE, raw = appNo))
    }
    type <- toupper(parts[[2]])
    list(
        valid = TRUE,
        raw = appNo,
        type = type,
        number = paste0(type, parts[[3]])
    )
}

.fdaStatusRow <- function(
  drug,
  application = NA_character_,
  applicationType = NA_character_,
  status,
  message = NA_character_,
  lastUpdated = NA_character_,
  sponsor = NA_character_
) {
    list(
        drug_name = drug$name,
        drug_concept_id = drug$conceptId,
        fda_application_number = application,
        fda_application_type = applicationType,
        fda_sponsor_name = sponsor,
        fda_product_number = NA_character_,
        fda_brand_name = NA_character_,
        fda_marketing_status = NA_character_,
        fda_dosage_form = NA_character_,
        fda_route = NA_character_,
        fda_reference_drug = NA_character_,
        fda_reference_standard = NA_character_,
        fda_te_code = NA_character_,
        fda_active_ingredients = .emptyFdaIngredients(),
        fda_data_last_updated = lastUpdated,
        fda_lookup_status = status,
        fda_lookup_message = message
    )
}

.fdaProductRow <- function(drug, parsed, application, product, lastUpdated) {
    list(
        drug_name = drug$name,
        drug_concept_id = drug$conceptId,
        fda_application_number = parsed$number,
        fda_application_type = parsed$type,
        fda_sponsor_name = .fdaValue(application$sponsor_name),
        fda_product_number = .fdaValue(product$product_number),
        fda_brand_name = .fdaValue(product$brand_name),
        fda_marketing_status = .fdaValue(product$marketing_status),
        fda_dosage_form = .fdaValue(product$dosage_form),
        fda_route = .fdaValue(product$route),
        fda_reference_drug = .fdaValue(product$reference_drug),
        fda_reference_standard = .fdaValue(product$reference_standard),
        fda_te_code = .fdaValue(product$te_code),
        fda_active_ingredients = .fdaIngredients(product$active_ingredients),
        fda_data_last_updated = lastUpdated,
        fda_lookup_status = "success",
        fda_lookup_message = NA_character_
    )
}

.fdaRowsToDataFrame <- function(rows) {
    columns <- stats::setNames(lapply(.fdaOutputFields, function(field) {
        if (identical(field, "fda_active_ingredients")) {
            lapply(rows, `[[`, field)
        } else {
            vapply(rows, `[[`, character(1), field)
        }
    }), .fdaOutputFields)
    .columnsToDataFrame(columns, "fda_active_ingredients")
}

.fdaBatches <- function(applications) {
    if (!length(applications)) {
        return(list())
    }
    split(
        applications,
        ceiling(seq_along(applications) / .fdaBatchSize)
    )
}

.fdaLookups <- function(applications, fdaApiKey) {
    lookups <- list()
    failures <- 0L
    for (batch in .fdaBatches(applications)) {
        result <- tryCatch(
            .fetchFdaBatch(batch, fdaApiKey),
            error = function(error) error
        )
        if (inherits(result, "error")) {
            failures <- failures + 1L
            for (application in batch) {
                lookups[[application]] <- list(
                    status = "request_error",
                    message = conditionMessage(result),
                    record = NULL,
                    lastUpdated = NA_character_
                )
            }
            next
        }

        for (application in batch) {
            lookups[[application]] <- list(
                status = "not_found",
                message = "No openFDA Drugs@FDA record was found.",
                record = NULL,
                lastUpdated = result$lastUpdated
            )
        }
        for (record in result$applications) {
            application <- .fdaValue(record$application_number)
            if (!is.na(application) && application %in% batch) {
                lookups[[application]] <- list(
                    status = "success",
                    message = NA_character_,
                    record = record,
                    lastUpdated = result$lastUpdated
                )
            }
        }
    }
    if (failures) {
        failureMessage <- paste(
            "%d openFDA request batch%s failed; inspect",
            "`fda_lookup_status` and `fda_lookup_message`."
        )
        warning(
            sprintf(
                failureMessage,
                failures,
                if (failures == 1L) "" else "es"
            ),
            call. = FALSE
        )
    }
    lookups
}

.fdaRowsForApplication <- function(drug, parsed, lookup) {
    if (!identical(lookup$status, "success")) {
        return(list(.fdaStatusRow(
            drug,
            application = parsed$number,
            applicationType = parsed$type,
            status = lookup$status,
            message = lookup$message,
            lastUpdated = lookup$lastUpdated
        )))
    }

    products <- lookup$record$products
    if (is.null(products) || !length(products)) {
        return(list(.fdaStatusRow(
            drug,
            application = parsed$number,
            applicationType = parsed$type,
            status = "no_products",
            message = "The FDA application contains no products.",
            lastUpdated = lookup$lastUpdated,
            sponsor = .fdaValue(lookup$record$sponsor_name)
        )))
    }
    lapply(products, function(product) {
        .fdaProductRow(
            drug,
            parsed,
            lookup$record,
            product,
            lookup$lastUpdated
        )
    })
}

#' Get Drugs@FDA Product Information
#'
#' Enriches DGIdb drug application identifiers with product information from
#' the openFDA Drugs@FDA API. The returned data are intended for research and
#' should not be used as the sole basis for medical decisions.
#'
#' @param terms Character vector of drug names.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#' @param fdaApiKey Optional openFDA API key. When `NULL`, the function uses
#' `OPENFDA_API_KEY` when set. Use `""` to make an unauthenticated request.
#'
#' @return A product-level data frame. `fda_active_ingredients` is a list-column
#' of data frames containing ingredient names and strengths. Lookup outcomes are
#' reported in `fda_lookup_status` and `fda_lookup_message`.
#'
#' @examplesIf interactive()
#' getDrugApplications("Imatinib")
#' @export
getDrugApplications <- function(terms, apiUrl = NULL, fdaApiKey = NULL) {
    results <- .postQuery(
        apiUrl,
        "queries/get_drug_applications.graphql",
        list(names = terms)
    )
    drugs <- results$drugs$nodes
    if (!length(drugs)) {
        return(.fdaRowsToDataFrame(list()))
    }

    entries <- list()
    for (drug in drugs) {
        applications <- drug$drugApplications
        if (is.null(applications) || !length(applications)) {
            message <- "DGIdb has no Drugs@FDA application for this drug."
            entries[[length(entries) + 1L]] <- list(
                row = .fdaStatusRow(
                    drug,
                    status = "no_applications",
                    message = message
                )
            )
            next
        }
        for (application in applications) {
            parsed <- .parseFdaApplication(application$appNo)
            if (!isTRUE(parsed$valid)) {
                entries[[length(entries) + 1L]] <- list(
                    row = .fdaStatusRow(
                        drug,
                        application = .fdaValue(parsed$raw),
                        status = "invalid_application",
                        message = sprintf(
                            "Unrecognized DGIdb application identifier: %s",
                            .fdaValue(parsed$raw)
                        )
                    )
                )
                next
            }
            entries[[length(entries) + 1L]] <- list(
                drug = drug,
                parsed = parsed
            )
        }
    }

    associations <- Filter(function(x) is.null(x$row), entries)
    applicationNumbers <- unique(vapply(
        associations,
        function(x) x$parsed$number,
        character(1)
    ))
    fdaApiKey <- .resolveFdaApiKey(fdaApiKey)
    lookups <- .fdaLookups(applicationNumbers, fdaApiKey)
    rows <- unlist(lapply(entries, function(entry) {
        if (!is.null(entry$row)) {
            return(list(entry$row))
        }
        .fdaRowsForApplication(
            entry$drug,
            entry$parsed,
            lookups[[entry$parsed$number]]
        )
    }), recursive = FALSE)
    .fdaRowsToDataFrame(rows)
}
