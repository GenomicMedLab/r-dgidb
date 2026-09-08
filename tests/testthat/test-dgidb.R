httptest2::with_mock_api({
    test_that("getDrugs() returns matching drugs", {
        results <- getDrugs("Imatinib")
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)
        expect_type(results$drug_aliases, "list")
        expect_type(results$drug_attributes, "list")
        expect_type(results$drug_approval_ratings, "list")
        expect_type(results$drug_fda_applications, "list")

        results_with_added_fake <- getDrugs(c("Imatinib", "not-real"))
        expect_equal(nrow(results_with_added_fake), nrow(results))

        empty_results <- getDrugs("not-real")
        expect_s3_class(empty_results, "data.frame")
        expect_equal(nrow(empty_results), 0)
        expect_identical(names(empty_results), names(results))
    })
})

test_that("getDrugs() applies drug filters", {
    params <- NULL
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            params <<- variables
            list(drugs = list(nodes = list()))
        }
    )

    getDrugs("imatinib", immunotherapy = TRUE, antineoplastic = TRUE)
    expect_true(params$immunotherapy)
    expect_true(params$antiNeoplastic)
})

httptest2::with_mock_api({
    test_that("getGenes() returns matching genes", {
        results <- getGenes("ereg")
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)
        expect_type(results$gene_aliases, "list")
        expect_type(results$gene_attributes, "list")

        results_with_added_fake <- getGenes(c("ereg", "not-real"))
        expect_equal(nrow(results_with_added_fake), nrow(results))

        empty_results <- getGenes("not-real")
        expect_s3_class(empty_results, "data.frame")
        expect_equal(nrow(empty_results), 0)
        expect_identical(names(empty_results), names(results))
    })
})

httptest2::with_mock_api({
    test_that("getInteractions() searches by gene", {
        results <- getInteractions("ereg")
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)
        expect_type(results$interaction_attributes, "list")
        expect_type(results$interaction_pmids, "list")
        expect_type(results$interaction_sources, "list")

        results_with_added_fake <- getInteractions(c("ereg", "not-real"))
        expect_equal(nrow(results_with_added_fake), nrow(results))

        multiple_results <- getInteractions(c("ereg", "braf"))
        expect_gt(
            nrow(multiple_results),
            nrow(results)
        )

        empty_results <- getInteractions("not-real")
        expect_s3_class(empty_results, "data.frame")
        expect_equal(nrow(empty_results), 0)
        expect_identical(names(empty_results), names(results))
    })
})

httptest2::with_mock_api({
    test_that("getInteractions() searches by drug", {
        results <- getInteractions("sunitinib", search = "drugs")
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)

        results_with_added_fake <- getInteractions(
            c("sunitinib", "not-real"),
            search = "drugs"
        )
        expect_equal(nrow(results_with_added_fake), nrow(results))

        multiple_results <- getInteractions(
            c("sunitinib", "clonazepam"),
            search = "drugs"
        )
        expect_gt(
            nrow(multiple_results),
            nrow(results)
        )

        empty_results <- getInteractions("not-real", search = "drugs")
        expect_s3_class(empty_results, "data.frame")
        expect_equal(nrow(empty_results), 0)
        expect_identical(names(empty_results), names(results))
    })
})

test_that("getInteractions() applies filters by search type", {
    params <- NULL
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            params <<- variables
            key <- if (grepl("gene", queryFile)) "genes" else "drugs"
            setNames(list(list(nodes = list())), key)
        }
    )

    getInteractions("braf", search = "genes", immunotherapy = TRUE)
    expect_null(params$immunotherapy)

    getInteractions(
        "imatinib",
        search = "drugs",
        immunotherapy = TRUE,
        antineoplastic = TRUE,
        approved = TRUE
    )
    expect_true(params$immunotherapy)
    expect_true(params$antineoplastic)
    expect_true(params$approved)
})

httptest2::with_mock_api({
    test_that("getCategories() returns gene categories", {
        results <- getCategories("BRAF")
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)
        expect_type(results$gene_category_sources, "list")
        expect_true(all(c(
            "DRUG RESISTANCE",
            "DRUGGABLE GENOME",
            "CLINICALLY ACTIONABLE"
        ) %in% results$gene_category))
    })
})

httptest2::with_mock_api({
    test_that("getSources() returns source data", {
        results <- getSources()
        expect_s3_class(results, "data.frame")
        expect_equal(nrow(results), 45)

        sources <- getSources(sourceTypes$GENE)$source_name
        expect_length(sources, 3)
        expect_setequal(
            sources,
            c("NCBI Gene", "HUGO Gene Nomenclature Committee", "Ensembl")
        )
    })
})

httptest2::with_mock_api({
    test_that("getAllGenes() returns all genes", {
        results <- getAllGenes()
        expect_s3_class(results, "data.frame")
        expect_gt(nrow(results), 0)
        expect_length(results$gene_concept_id, length(results$gene_name))
    })
})

test_that("getAllDrugs() returns all drugs", {
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            expect_equal(queryFile, "queries/get_all_drugs.graphql")
            expect_null(variables)
            list(drugs = list(nodes = list(list(
                name = "Imatinib",
                conceptId = "chembl:941"
            ))))
        }
    )

    results <- getAllDrugs()
    expect_s3_class(results, "data.frame")
    expect_equal(results$drug_name, "Imatinib")
    expect_equal(results$drug_concept_id, "chembl:941")
})

test_that("getDrugApplications() returns FDA product data", {
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            expect_equal(queryFile, "queries/get_drug_applications.graphql")
            expect_equal(variables, list(names = "Imatinib"))
            list(drugs = list(nodes = list(list(
                name = "Imatinib",
                conceptId = "chembl:941",
                drugApplications = list(list(appNo = "drugsatfda.nda:021588"))
            ))))
        },
        .fetchFdaBatch = function(applications, fdaApiKey) {
            expect_equal(applications, "NDA021588")
            expect_null(fdaApiKey)
            list(
                status = "success",
                lastUpdated = "2026-08-20",
                applications = list(list(
                    application_number = "NDA021588",
                    sponsor_name = "Novartis Pharmaceuticals Corp",
                    products = list(list(
                        product_number = "001",
                        brand_name = "GLEEVEC",
                        marketing_status = "Prescription",
                        dosage_form = "TABLET",
                        route = "ORAL",
                        reference_drug = "Yes",
                        reference_standard = "Yes",
                        te_code = "AB",
                        active_ingredients = list(
                            list(name = "IMATINIB MESYLATE", strength = "100MG")
                        )
                    ))
                ))
            )
        }
    )

    results <- getDrugApplications("Imatinib", fdaApiKey = "")
    expect_s3_class(results, "data.frame")
    expect_equal(results$drug_name, "Imatinib")
    expect_equal(results$drug_concept_id, "chembl:941")
    expect_equal(results$fda_application_number, "NDA021588")
    expect_equal(results$fda_application_type, "NDA")
    expect_equal(results$fda_brand_name, "GLEEVEC")
    expect_equal(results$fda_marketing_status, "Prescription")
    expect_equal(results$fda_dosage_form, "TABLET")
    expect_equal(results$fda_data_last_updated, "2026-08-20")
    expect_equal(results$fda_lookup_status, "success")
    expect_equal(
        results$fda_active_ingredients[[1]],
        data.frame(
            name = "IMATINIB MESYLATE",
            strength = "100MG",
            stringsAsFactors = FALSE
        )
    )
})

test_that("query functions preserve their schemas for empty results", {
    local_mocked_bindings(
        .postQuery = function(apiUrl, queryFile, variables) {
            connection <- if (grepl("sources", queryFile)) {
                "sources"
            } else if (grepl("gene|categories", queryFile)) {
                "genes"
            } else {
                "drugs"
            }
            stats::setNames(list(list(nodes = list())), connection)
        }
    )

    outputs <- list(
        drugs = getDrugs("not-real"),
        genes = getGenes("not-real"),
        interactions = getInteractions("not-real"),
        categories = getCategories("not-real"),
        sources = getSources(),
        all_genes = getAllGenes(),
        all_drugs = getAllDrugs(),
        applications = getDrugApplications("not-real")
    )
    expectedNames <- list(
        drugs = c(
            "drug_name", "drug_concept_id", "drug_aliases",
            "drug_attributes", "drug_is_antineoplastic",
            "drug_is_immunotherapy", "drug_is_approved",
            "drug_approval_ratings", "drug_fda_applications"
        ),
        genes = c(
            "gene_name", "gene_concept_id", "gene_aliases",
            "gene_attributes"
        ),
        interactions = c(
            "gene_name", "gene_concept_id", "gene_long_name", "drug_name",
            "drug_concept_id", "drug_approved", "interaction_score",
            "interaction_attributes", "interaction_pmids",
            "interaction_sources"
        ),
        categories = c(
            "gene_name", "gene_concept_id", "gene_full_name",
            "gene_category", "gene_category_sources"
        ),
        sources = c(
            "source_name", "source_short_name", "source_version",
            "source_drug_claims", "source_gene_claims",
            "source_interaction_claims", "source_license",
            "source_license_url"
        ),
        all_genes = c("gene_name", "gene_concept_id"),
        all_drugs = c("drug_name", "drug_concept_id"),
        applications = c(
            "drug_name", "drug_concept_id", "fda_application_number",
            "fda_application_type", "fda_sponsor_name",
            "fda_product_number", "fda_brand_name",
            "fda_marketing_status", "fda_dosage_form", "fda_route",
            "fda_reference_drug", "fda_reference_standard", "fda_te_code",
            "fda_active_ingredients", "fda_data_last_updated",
            "fda_lookup_status", "fda_lookup_message"
        )
    )

    for (name in names(outputs)) {
        expect_s3_class(outputs[[name]], "data.frame")
        expect_equal(nrow(outputs[[name]]), 0)
        expect_identical(names(outputs[[name]]), expectedNames[[name]])
    }
})
