.groupAttributes <- function(row) {
    values <- lapply(row, function(x) x$value)
    keep <- !vapply(values, is.null, logical(1))
    names <- vapply(row[keep], function(x) x$name, character(1))
    keys <- unique(names)
    stats::setNames(
        lapply(keys, function(key) values[keep][names == key]),
        keys
    )
}

.backfillAttributes <- function(col) {
    keys <- unique(unlist(lapply(col, names), use.names = FALSE))
    lapply(col, function(x) {
        stats::setNames(lapply(keys, function(key) x[[key]]), keys)
    })
}

.columnsToDataFrame <- function(columns, listColumns = character()) {
    columnOrder <- names(columns)
    listValues <- columns[listColumns]
    scalarValues <- columns[setdiff(columnOrder, listColumns)]

    output <- as.data.frame(
        scalarValues,
        optional = TRUE,
        stringsAsFactors = FALSE
    )

    for (columnName in listColumns) {
        output[[columnName]] <- listValues[[columnName]]
    }

    output[columnOrder]
}

#' Get Drugs
#'
#' Performs a record lookup in DGIdb for drugs of interest.
#'
#' @param terms Character vector of drug names.
#' @param immunotherapy Optionally retain only immunotherapies.
#' @param antineoplastic Optionally retain drugs by antineoplastic use.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame of drug records. Multi-valued fields are list-columns.
#'
#' @examplesIf interactive()
#' getDrugs("Imatinib")
#' @export
getDrugs <- function(
  terms,
  immunotherapy = NULL,
  antineoplastic = NULL,
  apiUrl = NULL
) {
    params <- list(names = terms)
    if (!is.null(immunotherapy)) params$immunotherapy <- immunotherapy
    if (!is.null(antineoplastic)) params$antiNeoplastic <- antineoplastic
    results <- .postQuery(apiUrl, "queries/get_drugs.graphql", params)

    nodes <- results$drugs$nodes
    output <- list(
        drug_name = vapply(nodes, function(x) x$name, character(1)),
        drug_concept_id = vapply(nodes, function(x) x$conceptId, character(1)),
        drug_aliases = lapply(nodes, function(x) {
            vapply(x$drugAliases, function(a) a$alias, character(1))
        }),
        drug_attributes = lapply(nodes, function(x) {
            .groupAttributes(x$drugAttributes)
        }),
        drug_is_antineoplastic = vapply(
            nodes, function(x) x$antiNeoplastic, logical(1)
        ),
        drug_is_immunotherapy = vapply(
            nodes, function(x) x$immunotherapy, logical(1)
        ),
        drug_is_approved = vapply(nodes, function(x) x$approved, logical(1)),
        drug_approval_ratings = lapply(nodes, function(x) {
            lapply(x$drugApprovalRatings, function(r) {
                list(rating = r$rating, source = r$source$sourceDbName)
            })
        }),
        drug_fda_applications = lapply(nodes, function(x) {
            vapply(x$drugApplications, function(a) a$appNo, character(1))
        })
    )
    output$drug_attributes <- .backfillAttributes(output$drug_attributes)
    .columnsToDataFrame(
        output,
        c(
            "drug_aliases", "drug_attributes", "drug_approval_ratings",
            "drug_fda_applications"
        )
    )
}

#' Get Genes
#'
#' Performs a record lookup in DGIdb for genes of interest.
#'
#' @param terms Character vector of gene names.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame of gene records. Multi-valued fields are list-columns.
#'
#' @examplesIf interactive()
#' getGenes(c("BRAF", "PDGFRA"))
#' @export
getGenes <- function(terms, apiUrl = NULL) {
    params <- list(names = terms)
    results <- .postQuery(apiUrl, "queries/get_genes.graphql", params)

    nodes <- results$genes$nodes
    output <- list(
        gene_name = vapply(nodes, function(x) x$name, character(1)),
        gene_concept_id = vapply(nodes, function(x) x$conceptId, character(1)),
        gene_aliases = lapply(nodes, function(x) {
            vapply(x$geneAliases, function(a) a$alias, character(1))
        }),
        gene_attributes = lapply(nodes, function(x) {
            .groupAttributes(x$geneAttributes)
        })
    )
    output$gene_attributes <- .backfillAttributes(output$gene_attributes)
    .columnsToDataFrame(output, c("gene_aliases", "gene_attributes"))
}

.interactionOutput <- function(results) {
    nodes <- unlist(
        lapply(results, function(x) x$interactions),
        recursive = FALSE
    )
    output <- list(
        gene_name = vapply(nodes, function(x) x$gene$name, character(1)),
        gene_concept_id = vapply(
            nodes, function(x) x$gene$conceptId, character(1)
        ),
        gene_long_name = vapply(
            nodes, function(x) x$gene$longName, character(1)
        ),
        drug_name = vapply(nodes, function(x) x$drug$name, character(1)),
        drug_concept_id = vapply(
            nodes, function(x) x$drug$conceptId, character(1)
        ),
        drug_approved = vapply(nodes, function(x) x$drug$approved, logical(1)),
        interaction_score = vapply(
            nodes, function(x) x$interactionScore, numeric(1)
        ),
        interaction_attributes = lapply(nodes, function(x) {
            .groupAttributes(x$interactionAttributes)
        }),
        interaction_pmids = lapply(nodes, function(x) {
            unlist(lapply(x$interactionClaims, function(y) {
                vapply(y$publications, function(z) z$pmid, numeric(1))
            }))
        }),
        interaction_sources = lapply(nodes, function(x) {
            vapply(
                x$interactionClaims,
                function(y) y$source$sourceDbName,
                character(1)
            )
        })
    )
    output$interaction_attributes <- .backfillAttributes(
        output$interaction_attributes
    )
    .columnsToDataFrame(
        output,
        c(
            "interaction_attributes", "interaction_pmids",
            "interaction_sources"
        )
    )
}

#' Get Interactions
#'
#' Performs an interaction lookup for drugs or genes of interest.
#'
#' @param terms Character vector of drug or gene names.
#' @param search Either `"genes"` or `"drugs"`.
#' @param immunotherapy Optionally filter drug searches by immunotherapy use.
#' @param antineoplastic Optionally filter drug searches by antineoplastic use.
#' @param source Optionally filter by source database name.
#' @param pmid Optionally filter by PubMed identifier.
#' @param interactionType Optionally filter by interaction type.
#' @param approved Optionally filter drug searches by approval status.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame of interaction records. Multi-valued fields are
#' list-columns.
#'
#' @examplesIf interactive()
#' getInteractions(c("BRAF", "PDGFRA"))
#' @usage
#' getInteractions(
#'     terms,
#'     search = "genes",
#'     immunotherapy = NULL,
#'     antineoplastic = NULL,
#'     source = NULL,
#'     pmid = NULL,
#'     interactionType = NULL,
#'     approved = NULL,
#'     apiUrl = NULL
#' )
#' @export
getInteractions <- function(
  terms,
  search = "genes",
  immunotherapy = NULL,
  antineoplastic = NULL,
  source = NULL,
  pmid = NULL,
  interactionType = NULL,
  approved = NULL,
  apiUrl = NULL
) {
    search <- match.arg(search, c("genes", "drugs"))
    params <- list(names = terms)
    if (!is.null(source)) params$sourceDbName <- source
    if (!is.null(pmid)) params$pmid <- pmid
    if (!is.null(interactionType)) params$interactionType <- interactionType
    if (search == "drugs") {
        if (!is.null(immunotherapy)) params$immunotherapy <- immunotherapy
        if (!is.null(antineoplastic)) params$antineoplastic <- antineoplastic
        if (!is.null(approved)) params$approved <- approved
    }
    queryFile <- if (search == "genes") {
        "queries/get_interactions_by_gene.graphql"
    } else {
        "queries/get_interactions_by_drug.graphql"
    }
    results <- .postQuery(apiUrl, queryFile, params)[[search]]$nodes
    .interactionOutput(results)
}

#' Get Gene Categories
#'
#' Performs a category annotation lookup for genes of interest.
#'
#' @param terms Character vector of gene names.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame of gene-category records. Source names are stored in a
#' list-column.
#'
#' @examplesIf interactive()
#' getCategories(c("BRAF", "PDGFRA"))
#' @export
getCategories <- function(terms, apiUrl = NULL) {
    params <- list(names = terms)
    results <- .postQuery(apiUrl, "queries/get_gene_categories.graphql", params)

    nodes <- results$genes$nodes
    rows <- unlist(lapply(nodes, function(x) {
        lapply(x$geneCategoriesWithSources, function(category) {
            list(
                gene_name = x$name,
                gene_concept_id = x$conceptId,
                gene_full_name = x$longName,
                gene_category = category$name,
                gene_category_sources = category$sourceNames
            )
        })
    }), recursive = FALSE)
    output <- list(
        gene_name = vapply(rows, `[[`, character(1), "gene_name"),
        gene_concept_id = vapply(
            rows, `[[`, character(1), "gene_concept_id"
        ),
        gene_full_name = vapply(rows, `[[`, character(1), "gene_full_name"),
        gene_category = vapply(rows, `[[`, character(1), "gene_category"),
        gene_category_sources = lapply(rows, `[[`, "gene_category_sources")
    )
    .columnsToDataFrame(output, "gene_category_sources")
}

#' DGIdb Source Types
#'
#' Supported values for the `sourceType` argument to `getSources()`.
#'
#' @format A named list of four character values.
#'
#' @return A named list of supported DGIdb source-type character values
#'
#' @examples
#' sourceTypes
#' sourceTypes$GENE
#'
#' @export
sourceTypes <- list(
    DRUG = "drug",
    GENE = "gene",
    INTERACTION = "interaction",
    POTENTIALLY_DRUGGABLE = "potentially_druggable"
)

#' Get Sources
#'
#' Performs a lookup for DGIdb aggregate sources.
#'
#' @param sourceType Optional source type from `sourceTypes`.
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame of DGIdb source records.
#'
#' @examplesIf interactive()
#' getSources(sourceTypes$POTENTIALLY_DRUGGABLE)
#' @export
getSources <- function(sourceType = NULL, apiUrl = NULL) {
    params <- if (!is.null(sourceType)) {
        list(sourceType = toupper(sourceType))
    } else {
        NULL
    }
    results <- .postQuery(apiUrl, "queries/get_sources.graphql", params)

    nodes <- results$sources$nodes
    output <- list(
        source_name = vapply(nodes, function(x) x$fullName, character(1)),
        source_short_name = vapply(
            nodes, function(x) x$sourceDbName, character(1)
        ),
        source_version = vapply(
            nodes, function(x) x$sourceDbVersion, character(1)
        ),
        source_drug_claims = vapply(
            nodes, function(x) x$drugClaimsCount, numeric(1)
        ),
        source_gene_claims = vapply(
            nodes, function(x) x$geneClaimsCount, numeric(1)
        ),
        source_interaction_claims = vapply(
            nodes, function(x) x$interactionClaimsCount, numeric(1)
        ),
        source_license = vapply(nodes, function(x) x$license, character(1)),
        source_license_url = vapply(
            nodes, function(x) x$licenseLink, character(1)
        )
    )
    .columnsToDataFrame(output)
}

#' Get All Genes
#'
#' Gets all gene names and identifiers present in DGIdb.
#'
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame containing gene names and concept identifiers.
#'
#' @examplesIf interactive()
#' getAllGenes()
#' @export
getAllGenes <- function(apiUrl = NULL) {
    results <- .postQuery(apiUrl, "queries/get_all_genes.graphql", NULL)

    nodes <- results$genes$nodes
    output <- list(
        gene_name = vapply(nodes, function(x) x$name, character(1)),
        gene_concept_id = vapply(nodes, function(x) x$conceptId, character(1))
    )
    .columnsToDataFrame(output)
}

#' Get All Drugs
#'
#' Gets all drug names and identifiers present in DGIdb.
#'
#' @param apiUrl DGIdb GraphQL endpoint; defaults to `DGIDB_API_URL`.
#'
#' @return A data frame containing drug names and concept identifiers.
#'
#' @examplesIf interactive()
#' getAllDrugs()
#' @export
getAllDrugs <- function(apiUrl = NULL) {
    results <- .postQuery(apiUrl, "queries/get_all_drugs.graphql", NULL)

    nodes <- results$drugs$nodes
    output <- list(
        drug_name = vapply(nodes, function(x) x$name, character(1)),
        drug_concept_id = vapply(nodes, function(x) x$conceptId, character(1))
    )
    .columnsToDataFrame(output)
}
