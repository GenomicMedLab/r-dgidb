library(httr)
library(tidyverse)
library(data.table)

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

get_interactions <- function(terms,use_processing=TRUE,search='genes',immunotherapy=NULL,antineoplastic=NULL,sourcedbname=NULL,pmid=NULL,interactiontype=NULL,approved=NULL) {
  terms <- paste0("[\"", paste(toupper(terms), collapse = "\",\""), "\"]")
  if (search == "genes") {
    immunotherapy <- NULL
    antineoplastic <- NULL
  }

  # TODO: Implement Filters
  filters = ""
  if (search == 'genes') {
      query <- paste0("{\ngenes(names: ", terms, filters, ") {\nnodes{\nname\nlongName\ngeneCategories{\nname\n}\ninteractions {\ninteractionAttributes {\nname\nvalue\n}\ndrug {\nname\napproved\n}\ninteractionScore\ninteractionClaims {\npublications {\npmid\ncitation\n}\nsource {\nfullName\nid\n}\n}\n}\n}\n}\n}")
  } else if (search == 'drugs') {
      query <- paste0("{\ndrugs(names: ", terms, filters, ") {\nnodes{\nname\napproved\ninteractions {\ngene {\nname\n}\ninteractionAttributes {\nname\nvalue\n}\ninteractionScore\ninteractionClaims {\npublications {\npmid\ncitation\n}\nsource {\nfullName\nid\n}\n}\n}\n}\n}\n}")
  } else {
      stop("Search type must be specified using: search='drugs' or search='genes'")
  }

  r <- POST(base_url, body = list(query = query), encode = "json")
  data <- content(r)$data

  if(use_processing == TRUE) {
    if (search == 'genes') {
      data <- process_gene(data)
    } else if (search == 'drugs') {
      data <- process_drug(data)
    } else {
      stop("Search type must be specified using: search='drugs' or search='genes'")
    }
  }
  return(data)
}

process_gene <- function(data) {
  data <- data$genes$nodes
  dt <- rbindlist(lapply(data, as.data.table))
  dt <- unnest_wider(dt, col = "interactions")
  dt <- unnest_wider(dt, col = "drug",names_sep="_")
  
  dt$interactionAttributes <- lapply(dt$interactionAttributes, function(x) {
    attributes = list()
    for(i in 1:length(x)) {
      elem <- paste(x[[i]]$name, x[[i]]$value, sep = ": ")
      attributes <- append(attributes, elem)
    }
    intAttributes <- paste(attributes, collapse = " | ")
    return(intAttributes)
  })
  
  dt$pmid <- lapply(dt$interactionClaims, function(x) {
    pmids = list()
    
    for(i in 1:length(x)) {
        curr_publication <- x[[i]]$publications
        if(length(curr_publication) == 0) next
        for(j in 1:length(curr_publication)) {
          current_pmid <- curr_publication[[j]]$pmid
          pmids <- append(pmids, current_pmid)
        }
    }
    pmids_str <- paste(pmids, collapse = " | ")
    
    return(pmids_str)
  })

  dt$source <- lapply(dt$interactionClaims, function(x) {
    sources = list()
    
    for(i in 1:length(x)) {
        current_source_name <- x[[i]]$source$fullName
        sources <- append(sources, current_source_name)
    }
    sources_str <- paste(sources, collapse = " | ")
    
    return(sources_str)
  })
  
  dt$geneCategories <- NULL
  dt$interactionClaims <- NULL
  setnames(dt, 
  old = c("name","interactionAttributes","drug_name","drug_approved","interactionScore"), 
  new = c("gene","interaction_attributes","drug","approval","score"))

  return(dt)
}

process_drug <- function(data) {
  data <- data$drugs$nodes
  dt <- rbindlist(lapply(data, as.data.table))
  dt <- unnest_wider(dt, col = "interactions")
  dt <- unnest_wider(dt, col = "gene",names_sep="_")

  dt$interactionAttributes <- lapply(dt$interactionAttributes, function(x) {
    attributes = list()
    for(i in 1:length(x)) {
      elem <- paste(x[[i]]$name, x[[i]]$value, sep = ": ")
      attributes <- append(attributes, elem)
    }
    intAttributes <- paste(attributes, collapse = " | ")
    return(intAttributes)
  })
  
  dt$pmid <- lapply(dt$interactionClaims, function(x) {
    pmids = list()
    
    for(i in 1:length(x)) {
        curr_publication <- x[[i]]$publications
        if(length(curr_publication) == 0) next
        for(j in 1:length(curr_publication)) {
          current_pmid <- curr_publication[[j]]$pmid
          pmids <- append(pmids, current_pmid)
        }
    }
    pmids_str <- paste(pmids, collapse = " | ")
    
    return(pmids_str)
  })

  dt$source <- lapply(dt$interactionClaims, function(x) {
    sources = list()
    
    for(i in 1:length(x)) {
        current_source_name <- x[[i]]$source$fullName
        sources <- append(sources, current_source_name)
    }
    sources_str <- paste(sources, collapse = " | ")
    
    return(sources_str)
  })

  dt$interactionClaims <- NULL
  setnames(dt, 
  old = c("name","approved","gene_name","interactionAttributes","interactionScore"), 
  new = c("drug","approval","gene","interaction_attributes","score"))
  
  return(dt)
}

get_gene_list <- function() {
  query <- "{\ngenes {\nnodes {\nname\n}\n}\n}"
  r <- POST(base_url, body = list(query = query), encode = "json")
  gene_list <- list()
  raw_nodes <- content(r)$data$genes$nodes
  for(i in 1:length(raw_nodes)) {
    gene_name <- raw_nodes[[i]]$name
    gene_list <- append(gene_list, gene_name)
  }
  gene_list <- sort(unlist(gene_list))
  return(gene_list)
}

get_drug_applications <- function(terms,use_processing=TRUE) {
  terms <- paste0("[\"", paste(toupper(terms), collapse = "\",\""), "\"]")
  query <- paste0("{\ndrugs(names: ", terms, ") {\nnodes{\nname \ndrugApplications {\nappNo\n}\n}\n}\n}\n")
  
  r <- POST(base_url, body = list(query = query), encode = "json")
  data <- content(r)

  if (use_processing == TRUE) {
    data <- process_drug_applications(data)
    data <- openfda_data(data)
  }
  return(data)
}

process_drug_applications <- function(data) {0
  drug_list <- c()
  application_list <- c()
  
  for(node in data$data$drugs$nodes) {
    current_drug <- node$name
    
    for(application in node$drugApplications) {
      drug_list <- c(drug_list, current_drug)
      application <- toupper(gsub(":", "", strsplit(application$appNo, "\\.")[[1]][2]))
      application_list <- c(application_list, application)
    }
  }
  
  dataframe <- data.frame(drug = drug_list, application = application_list)
  return(dataframe)
}

openfda_data <- function(dataframe) {
  openfda_base_url <- 'https://api.fda.gov/drug/drugsfda.json?search=openfda.application_number:'
  terms <- as.list(dataframe$application)
  descriptions <- vector("list", length(terms))
  
  for(i in seq_along(terms)) {
    term <- terms[[i]]
    r <- GET(paste0(openfda_base_url, "\"", term, "\""), add_headers('User-Agent' = 'Custom'))
    
    tryCatch({
      results <- content(r, "parsed")$results
      if (length(results) > 0 && !is.null(results[[1]]$products)) {
        products <- results[[1]]$products
        f <- vector("character", length(products))
        
        for(j in seq_along(products)) {
          product <- products[[j]]
          brand_name <- product$brand_name
          marketing_status <- product$marketing_status
          dosage_form <- product$dosage_form
          active_ingredient <- product$active_ingredients[[1]]$name
          dosage_strength <- product$active_ingredients[[1]]$strength
          f[j] <- paste0(brand_name, ": ", dosage_strength, " ", marketing_status, " ", dosage_form)
        }
        
        descriptions[[i]] <- paste(f, collapse = " | ")
      } else {
        descriptions[[i]] <- "none"
      }
    }, error = function(e) {
      descriptions[[i]] <- "none"
    })
  }
  
  dataframe$description <- descriptions
  return(dataframe)
}

# data <- get_drug_applications(c("SUNITINIB","EXENATIDE"),use_processing=FALSE)
# data <- get_drug_applications(c("SUNITINIB","EXENATIDE"),use_processing=TRUE)