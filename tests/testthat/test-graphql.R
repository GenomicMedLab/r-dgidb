test_that(".postQuery() fetches and merges every connection page", {
    requests <- list()
    responses <- list(
        list(data = list(genes = list(
            nodes = list(list(name = "A")),
            pageInfo = list(hasNextPage = TRUE, endCursor = "cursor-1")
        ))),
        list(data = list(genes = list(
            nodes = list(list(name = "B")),
            pageInfo = list(hasNextPage = TRUE, endCursor = "cursor-2")
        ))),
        list(data = list(genes = list(
            nodes = list(list(name = "C")),
            pageInfo = list(hasNextPage = FALSE, endCursor = "cursor-3")
        )))
    )
    local_mocked_bindings(
        .fetchQueryPage = function(apiUrl, query, variables) {
            requests[[length(requests) + 1L]] <<- list(
                apiUrl = apiUrl,
                query = query,
                variables = variables
            )
            responses[[length(requests)]]
        }
    )

    result <- .postQuery(
        "https://example.test/graphql",
        "queries/get_all_genes.graphql",
        list(names = "BRAF")
    )

    expect_equal(
        vapply(result$genes$nodes, `[[`, character(1), "name"),
        c("A", "B", "C")
    )
    expect_length(requests, 3L)
    expect_equal(requests[[1]]$variables, list(names = "BRAF"))
    expect_equal(
        requests[[2]]$variables,
        list(names = "BRAF", after = "cursor-1")
    )
    expect_equal(
        requests[[3]]$variables,
        list(names = "BRAF", after = "cursor-2")
    )
    expect_false(result$genes$pageInfo$hasNextPage)
})

test_that(".postQuery() supports queries without initial variables", {
    requests <- list()
    responses <- list(
        list(data = list(drugs = list(
            nodes = list(),
            pageInfo = list(hasNextPage = TRUE, endCursor = "next")
        ))),
        list(data = list(drugs = list(
            nodes = list(),
            pageInfo = list(hasNextPage = FALSE, endCursor = "done")
        )))
    )
    local_mocked_bindings(
        .fetchQueryPage = function(apiUrl, query, variables) {
            requests[[length(requests) + 1L]] <<- list(variables)
            responses[[length(requests)]]
        }
    )

    .postQuery(
        "https://example.test/graphql",
        "queries/get_all_drugs.graphql",
        NULL
    )

    expect_null(requests[[1]][[1]])
    expect_equal(requests[[2]][[1]], list(after = "next"))
})

test_that(".postQuery() rejects pagination responses that cannot advance", {
    local_mocked_bindings(
        .fetchQueryPage = function(apiUrl, query, variables) {
            list(data = list(genes = list(
                nodes = list(),
                pageInfo = list(hasNextPage = TRUE, endCursor = NULL)
            )))
        }
    )

    expect_error(
        .postQuery(
            "https://example.test/graphql",
            "queries/get_all_genes.graphql",
            NULL
        ),
        "without a valid new cursor"
    )
})

test_that("all DGIdb connection queries request pagination metadata", {
    queryFiles <- list.files(
        system.file("queries", package = "dgiR"),
        pattern = "[.]graphql$",
        full.names = TRUE
    )

    expect_gt(length(queryFiles), 0L)
    for (queryFile in queryFiles) {
        query <- paste(readLines(queryFile, warn = FALSE), collapse = "\n")
        expect_match(query, "[$]first: Int = 1000")
        expect_match(query, "first: [$]first")
        expect_match(query, "[$]after: String")
        expect_match(query, "after: [$]after")
        expect_match(query, "pageInfo")
        expect_match(query, "hasNextPage")
        expect_match(query, "endCursor")
    }
})
