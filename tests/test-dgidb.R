library(testthat)
library(microbenchmark)
source("dgidb.R")

test_that("Test: Get Gene Interactions", {
    gene_data <- get_interactions(c("BRAF","PDGFRA"))
    expect_no_error(gene_data)
    print("Performing Benchmark on: Get Gene Interactions...")
    benchmark_results <- microbenchmark(get_interactions(c("BRAF","PDGFRA")),times=10)
    print(benchmark_results)
})

test_that("Test: Get Drug Interactions", {
    drug_data <- get_interactions(c("SUNITINIB","ZALCITABINE"),search='drugs')
    expect_no_error(drug_data)
    print("Performing Benchmark on: Get Drug Interactions...")
    benchmark_results <- microbenchmark(get_interactions(c("SUNITINIB","ZALCITABINE"),search='drugs'),times=10)
    print(benchmark_results)
})

test_that("Test: Get Drug Applications", {
    drug_app_data <- get_drug_applications(c("SUNITINIB","EXENATIDE"),use_processing=TRUE)
    expect_no_error(drug_app_data)
    print("Performing Benchmark on: Get Drug Applications...")
    benchmark_results <- microbenchmark(get_drug_applications(c("SUNITINIB","EXENATIDE"),use_processing=TRUE),times=10)
    print(benchmark_results)
})