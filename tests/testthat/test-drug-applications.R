fdaDrug <- function(name = "Example drug", applications = list()) {
    list(
        name = name,
        conceptId = paste0("concept:", tolower(gsub(" ", "-", name))),
        drugApplications = lapply(applications, function(x) list(appNo = x))
    )
}

mockDrugApplicationQuery <- function(drugs) {
    function(apiUrl, queryFile, variables) {
        expect_equal(queryFile, "queries/get_drug_applications.graphql")
        list(drugs = list(nodes = drugs))
    }
}

test_that("FDA application identifiers are parsed and validated", {
    expect_equal(
        .parseFdaApplication("drugsatfda.nda:021588")$number,
        "NDA021588"
    )
    expect_equal(
        .parseFdaApplication("DRUGSATFDA.AnDa:078340")$number,
        "ANDA078340"
    )
    expect_equal(
        .parseFdaApplication("drugsatfda.bla:125514")$type,
        "BLA"
    )
    expect_false(.parseFdaApplication("NDA021588")$valid)
    expect_false(.parseFdaApplication(NA_character_)$valid)
})

test_that("FDA ingredient names remain paired with their strengths", {
    ingredients <- .fdaIngredients(list(
        list(name = "INGREDIENT ONE", strength = "10 mg"),
        list(name = "INGREDIENT TWO", strength = "20 mg")
    ))

    expect_equal(
        ingredients,
        data.frame(
            name = c("INGREDIENT ONE", "INGREDIENT TWO"),
            strength = c("10 mg", "20 mg"),
            stringsAsFactors = FALSE
        )
    )
})

test_that("getDrugApplications() reports local lookup statuses", {
    drugs <- list(
        fdaDrug("No applications"),
        fdaDrug("Valid application", "drugsatfda.nda:000001"),
        fdaDrug("Invalid application", "unexpected:123")
    )
    local_mocked_bindings(
        .postQuery = mockDrugApplicationQuery(drugs),
        .fetchFdaBatch = function(applications, fdaApiKey) {
            list(
                status = "success",
                lastUpdated = "2026-08-20",
                applications = list(list(
                    application_number = "NDA000001",
                    products = list(list(brand_name = "Valid product"))
                ))
            )
        }
    )

    results <- getDrugApplications("unused")

    expect_equal(
        results$fda_lookup_status,
        c("no_applications", "success", "invalid_application")
    )
    expect_equal(results$fda_application_number[[3]], "unexpected:123")
    expect_true(all(vapply(
        results$fda_active_ingredients,
        is.data.frame,
        logical(1)
    )))
})

test_that("FDA applications without products retain application metadata", {
    local_mocked_bindings(
        .postQuery = mockDrugApplicationQuery(list(
            fdaDrug("Example drug", "drugsatfda.nda:000001")
        )),
        .fetchFdaBatch = function(applications, fdaApiKey) {
            list(
                status = "success",
                lastUpdated = "2026-08-20",
                applications = list(list(
                    application_number = "NDA000001",
                    sponsor_name = "Example sponsor",
                    products = list()
                ))
            )
        }
    )

    results <- getDrugApplications("Example drug", fdaApiKey = "")

    expect_equal(results$fda_lookup_status, "no_products")
    expect_equal(results$fda_sponsor_name, "Example sponsor")
    expect_equal(results$fda_data_last_updated, "2026-08-20")
})

test_that("batched FDA lookups are deduplicated and joined to each drug", {
    calls <- list()
    drugs <- list(
        fdaDrug(
            "First drug",
            c("drugsatfda.nda:000001", "drugsatfda.anda:000002")
        ),
        fdaDrug(
            "Second drug",
            c("drugsatfda.nda:000001", "drugsatfda.bla:000003")
        )
    )
    local_mocked_bindings(
        .fdaBatchSize = 2L,
        .postQuery = mockDrugApplicationQuery(drugs),
        .fetchFdaBatch = function(applications, fdaApiKey) {
            calls[[length(calls) + 1L]] <<- applications
            list(
                status = "success",
                lastUpdated = "2026-08-20",
                applications = lapply(applications, function(application) {
                    list(
                        application_number = application,
                        products = list(list(brand_name = application))
                    )
                })
            )
        }
    )

    results <- getDrugApplications("unused", fdaApiKey = "")

    expect_equal(calls, list(c("NDA000001", "ANDA000002"), "BLA000003"))
    expect_equal(sum(results$fda_application_number == "NDA000001"), 2L)
    expect_equal(nrow(results), 4L)
})

test_that("missing FDA records are reported without dropping DGIdb rows", {
    local_mocked_bindings(
        .postQuery = mockDrugApplicationQuery(list(
            fdaDrug(
                "Example drug",
                c("drugsatfda.nda:000001", "drugsatfda.nda:000002")
            )
        )),
        .fetchFdaBatch = function(applications, fdaApiKey) {
            list(
                status = "success",
                lastUpdated = "2026-08-20",
                applications = list(list(
                    application_number = "NDA000001",
                    products = list(list(brand_name = "Found"))
                ))
            )
        }
    )

    results <- getDrugApplications("Example drug", fdaApiKey = "")

    expect_equal(results$fda_lookup_status, c("success", "not_found"))
    expect_equal(results$fda_brand_name[[1]], "Found")
    expect_true(is.na(results$fda_brand_name[[2]]))
    expect_match(results$fda_lookup_message[[2]], "No openFDA")
})

test_that("FDA request errors produce rows and one summarized warning", {
    local_mocked_bindings(
        .postQuery = mockDrugApplicationQuery(list(
            fdaDrug(
                "Example drug",
                c("drugsatfda.nda:000001", "drugsatfda.nda:000002")
            )
        )),
        .fetchFdaBatch = function(...) stop("service unavailable")
    )

    expect_warning(
        results <- getDrugApplications("Example drug", fdaApiKey = ""),
        "1 openFDA request batch failed"
    )
    expect_equal(
        results$fda_lookup_status,
        c("request_error", "request_error")
    )
    expect_equal(
        results$fda_lookup_message,
        c("service unavailable", "service unavailable")
    )
})

test_that("FDA API keys use argument, environment, and anonymous precedence", {
    oldKey <- Sys.getenv("OPENFDA_API_KEY", unset = NA_character_)
    on.exit({
        if (is.na(oldKey)) {
            Sys.unsetenv("OPENFDA_API_KEY")
        } else {
            Sys.setenv(OPENFDA_API_KEY = oldKey)
        }
    })
    Sys.setenv(OPENFDA_API_KEY = "environment-key")
    expect_equal(.resolveFdaApiKey(NULL), "environment-key")
    expect_equal(.resolveFdaApiKey("argument-key"), "argument-key")
    expect_null(.resolveFdaApiKey(""))
    expect_error(.resolveFdaApiKey(c("one", "two")), "single string")
})

test_that("FDA searches use a grouped application-number expression", {
    expect_equal(
        .fdaSearchExpression(c("NDA021588", "ANDA078340")),
        "application_number:(NDA021588 OR ANDA078340)"
    )
})

test_that("FDA requests can be configured without performing them", {
    request <- .fdaRequest(c("NDA021588", "ANDA078340"), "test-key")

    expect_s3_class(request, "httr2_request")
    expect_match(request$url, "application_number%3A%28NDA021588")
    expect_match(request$url, "ANDA078340%29")
    expect_match(request$url, "limit=99")
    expect_false(grepl("test-key", request$url, fixed = TRUE))
    expect_equal(request$options$timeout, 30000)
    expect_equal(request$policies$retry_max_tries, 3)
    expect_true(request$policies$retry_on_failure)
    expect_true("Authorization" %in% names(request$headers))

    anonymousRequest <- .fdaRequest("NDA021588", NULL)
    expect_false("Authorization" %in% names(anonymousRequest$headers))
})
