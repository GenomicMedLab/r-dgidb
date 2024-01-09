library(httr)
library(jsonlite)

api_url <- function(env = "local") {
  url <- "http://localhost:3000/api/graphql"
  if (env == "local") {
    url <- "http://localhost:3000/api/graphql"
  }
  if (env == "staging") {
    url <- "https://staging.dgidb.org/api/graphql"
  }
  return(url)
}

base_url <- api_url('staging')

get_interactions <- function(terms,search='genes',immunotherapy=NULL,antineoplastic=NULL,sourcedbname=NULL,pmid=NULL,interactiontype=NULL,approved=NULL) {
  if (is.list(terms)) {
    terms <- paste0("\"", paste(terms, collapse = "\",\""), "\"")
  }
  if (search == "genes") {
    immunotherapy <- NULL
    antineoplastic <- NULL
  }

  # TODO: Implement Filters
  filters = ""

  if (search == 'genes') {
      query <- paste0("{\ngenes(names: [\"", toupper(terms), "\"]", filters, ") {\nnodes{\nname\nlongName\ngeneCategories{\nname\n}\ninteractions {\ninteractionAttributes {\nname\nvalue\n}\ndrug {\nname\napproved\n}\ninteractionScore\ninteractionClaims {\npublications {\npmid\ncitation\n}\nsource {\nfullName\nid\n}\n}\n}\n}\n}\n}")
  } else if (search == 'drugs') {
      query <- paste0("{\ndrugs(names: [\"", toupper(terms), "\"]", filters, ") {\nnodes{\nname\napproved\ninteractions {\ngene {\nname\n}\ninteractionAttributes {\nname\nvalue\n}\ninteractionScore\ninteractionClaims {\npublications {\npmid\ncitation\n}\nsource {\nfullName\nid\n}\n}\n}\n}\n}\n}")
  } else {
      stop("Search type must be specified using: search='drugs' or search='genes'")
  }

  r <- POST(base_url, body = list(query = query), encode = "json")

  data <- content(r)$data$genes$nodes
  return(data)
} # data <- get_interactions(c("BRAF"))

get_gene_list <- function() {
  query <- "{\ngenes {\nnodes {\nname\n}\n}\n}"
  r <- POST(base_url, body = list(query = query), encode = "json")
  gene_list <- c()
  raw_nodes <- content(r)$data$genes$nodes
  for(i in 1:length(raw_nodes)) {
    gene_name <- raw_nodes[[i]]$name
    gene_list <- c(gene_list, gene_name)
  }
  gene_list <- sort(gene_list)
  return(gene_list)
}