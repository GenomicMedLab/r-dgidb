.apiEndpointUrl <- Sys.getenv(
    "DGIDB_API_URL",
    unset = "https://dgidb.org/api/graphql"
)

#' Fetch a Page from the DGIdb GraphQL API
#'
#' @param apiUrl DGIdb GraphQL endpoint.
#' @param query GraphQL query text.
#' @param variables Named list of GraphQL variables.
#'
#' @return The decoded GraphQL response.
#' @noRd
.fetchQueryPage <- function(apiUrl, query, variables) {
    httr2::request(apiUrl) |>
        httr2::req_headers("dgidb-client-name" = "dgiR") |>
        httr2::req_body_json(list(query = query, variables = variables)) |>
        httr2::req_timeout(30) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
}

.queryConnection <- function(data) {
    connections <- names(Filter(
        function(value) {
            is.list(value) && !is.null(value$nodes) &&
                !is.null(value$pageInfo)
        },
        data
    ))
    if (length(connections) != 1L) {
        stop(
            "Expected exactly one paginated connection in the GraphQL response.",
            call. = FALSE
        )
    }
    connections[[1]]
}

.graphqlErrors <- function(response) {
    if (is.null(response$errors)) {
        return(invisible(NULL))
    }
    messages <- vapply(response$errors, `[[`, character(1), "message")
    stop(paste(messages, collapse = "\n"), call. = FALSE)
}

.hasNextPage <- function(connection) {
    hasNextPage <- connection$pageInfo$hasNextPage
    if (
        length(hasNextPage) != 1L || !is.logical(hasNextPage) ||
            is.na(hasNextPage)
    ) {
        stop(
            "GraphQL pageInfo did not include a valid hasNextPage value.",
            call. = FALSE
        )
    }
    hasNextPage
}

#' Send a DGIdb Query
#'
#' Sends a GraphQL query to DGIdb and follows the connection cursor until every
#' page has been fetched.
#'
#' @param apiUrl DGIdb GraphQL endpoint.
#' @param queryFile Path to an installed GraphQL query file.
#' @param variables Named list of GraphQL variables.
#'
#' @return The merged `data` fields from every GraphQL response page.
#' @noRd
.postQuery <- function(apiUrl, queryFile, variables) {
    apiUrl <- if (!is.null(apiUrl)) apiUrl else .apiEndpointUrl
    queryFilePath <- system.file(queryFile, package = "dgiR", mustWork = TRUE)
    query <- readChar(
        queryFilePath,
        file.info(queryFilePath)$size,
        useBytes = TRUE
    )

    response <- .fetchQueryPage(apiUrl, query, variables)
    .graphqlErrors(response)
    data <- response$data
    connectionName <- .queryConnection(data)
    cursor <- NULL

    while (.hasNextPage(data[[connectionName]])) {
        nextCursor <- data[[connectionName]]$pageInfo$endCursor
        if (
            !is.character(nextCursor) || length(nextCursor) != 1L ||
                is.na(nextCursor) || !nzchar(nextCursor) ||
                identical(nextCursor, cursor)
        ) {
            stop(
                "DGIdb reported another page without a valid new cursor.",
                call. = FALSE
            )
        }
        cursor <- nextCursor
        pageVariables <- variables
        if (is.null(pageVariables)) pageVariables <- list()
        pageVariables$after <- cursor

        response <- .fetchQueryPage(apiUrl, query, pageVariables)
        .graphqlErrors(response)
        page <- response$data
        pageConnectionName <- .queryConnection(page)
        if (!identical(pageConnectionName, connectionName)) {
            stop(
                "GraphQL connection changed while fetching pages.",
                call. = FALSE
            )
        }
        data[[connectionName]]$nodes <- c(
            data[[connectionName]]$nodes,
            page[[connectionName]]$nodes
        )
        data[[connectionName]]$pageInfo <- page[[connectionName]]$pageInfo
    }

    data
}
