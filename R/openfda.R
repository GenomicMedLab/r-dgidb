.fdaEndpointUrl <- "https://api.fda.gov/drug/drugsfda.json"
.fdaBatchSize <- 50L

.resolveFdaApiKey <- function(fdaApiKey) {
    if (is.null(fdaApiKey)) {
        fdaApiKey <- Sys.getenv("OPENFDA_API_KEY", unset = "")
    }
    if (
        !is.character(fdaApiKey) || length(fdaApiKey) != 1L ||
            is.na(fdaApiKey)
    ) {
        stop("`fdaApiKey` must be NULL or a single string.", call. = FALSE)
    }
    if (!nzchar(fdaApiKey)) NULL else fdaApiKey
}

.fdaSearchExpression <- function(applications) {
    paste0(
        "application_number:(",
        paste(applications, collapse = " OR "),
        ")"
    )
}

.fdaRequest <- function(applications, fdaApiKey) {
    request <- httr2::request(.fdaEndpointUrl) |>
        httr2::req_url_query(
            search = .fdaSearchExpression(applications),
            limit = 99L
        ) |>
        httr2::req_timeout(30) |>
        httr2::req_throttle(
            capacity = 240,
            fill_time_s = 60,
            realm = .fdaEndpointUrl
        ) |>
        httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) |>
        httr2::req_error(is_error = function(resp) {
            !httr2::resp_status(resp) %in% c(200L, 404L)
        })
    if (!is.null(fdaApiKey)) {
        request <- httr2::req_auth_basic(request, fdaApiKey, "")
    }
    request
}

#' Fetch a Batch from the openFDA Drugs@FDA API
#'
#' @param applications Character vector of canonical FDA application numbers.
#' @param fdaApiKey Optional resolved openFDA API key.
#'
#' @return A list containing lookup status, application records, and the FDA
#' data update date.
#' @noRd
.fetchFdaBatch <- function(applications, fdaApiKey) {
    response <- .fdaRequest(applications, fdaApiKey) |>
        httr2::req_perform()
    if (httr2::resp_status(response) == 404L) {
        return(list(
            status = "not_found",
            applications = list(),
            lastUpdated = NA_character_
        ))
    }

    body <- httr2::resp_body_json(response)
    records <- body$results
    if (is.null(records)) records <- list()
    lastUpdated <- body$meta$last_updated
    if (is.null(lastUpdated)) lastUpdated <- NA_character_
    list(
        status = "success",
        applications = records,
        lastUpdated = lastUpdated
    )
}
