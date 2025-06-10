#!/usr/bin/env Rscript

library(ragnar)
library(fs)
library(cli)
library(glue)
library(dplyr)
library(stringr)
library(purrr)

# ------------------------------------------------------------------------------
# Don't stop everything when there is an issue

ragnar_find_links_clean <- function(x) {
  res <- tryCatch(
    error = function(cnd) cnd$message,
    ragnar_find_links(x)
  )

  if (!inherits(res, "character")) {
    cli::cli_alert_danger("{.fn ragnar_find_links} failed for {x}: {res}")
    res <- character(0)
  }
  res
}

process_page <- function(x) {
  res <- try(ragnar_read(x, frame_by_tags = c("h1", "h2", "h3")), silent = TRUE)
  if (inherits(res, "try-error")) {
    msg <- as.character(res)
    cli::cli_alert_danger("reading failed for {x}: {msg}")
    return(NULL)
  }
  res <- try(
    ragnar_chunk(res, boundaries = c("paragraph", "sentence")),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    msg <- as.character(res)
    cli::cli_alert_danger("chunking failed for {x}: {msg}")
    return(NULL)
  }
  res |> dplyr::filter(!is.na(text))
}

# ------------------------------------------------------------------------------
# Make Store

store_location <- "tidymodels.ragnar.duckdb"

store <- ragnar_store_create(
  store_location,
  embed = \(x) ragnar::embed_openai(x, model = "text-embedding-3-small"),
  overwrite = TRUE
)

# ------------------------------------------------------------------------------
# pkgdown sites

# fmt: skip
tm_pkg_names <-
  c("agua", "applicable", "baguette", "bonsai", "broom", "brulee",
    "butcher", "censored", "dials", "discrim", "embed", "finetune",
    "multilevelmod", "orbital", "parsnip", "plsmod", "poissonreg",
    "probably", "recipes", "rsample", "rules", "spatialsample", "stacks",
    "textrecipes", "tidyclust", "tidyposterior", "tidypredict", "tune",
    "workflowsets", "yardstick")

tm_pkg_urls <-
  c(
    glue("https://{tm_pkg_names}.tidymodels.org/reference/"),
    glue("https://{tm_pkg_names}.tidymodels.org/articles/")
  )

# Include parsnip details not found by ragnar
details <-
  c("https://parsnip.tidymodels.org/reference/details_auto_ml_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_bag_mars_earth.html",
    "https://parsnip.tidymodels.org/reference/details_bag_mlp_nnet.html",
    "https://parsnip.tidymodels.org/reference/details_bag_tree_C5.0.html",
    "https://parsnip.tidymodels.org/reference/details_bag_tree_rpart.html",
    "https://parsnip.tidymodels.org/reference/details_bart_dbarts.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_C5.0.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_lightgbm.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_mboost.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_spark.html",
    "https://parsnip.tidymodels.org/reference/details_boost_tree_xgboost.html",
    "https://parsnip.tidymodels.org/reference/details_C5_rules_C5.0.html",
    "https://parsnip.tidymodels.org/reference/details_cubist_rules_Cubist.html",
    "https://parsnip.tidymodels.org/reference/details_decision_tree_C5.0.html",
    "https://parsnip.tidymodels.org/reference/details_decision_tree_partykit.html",
    "https://parsnip.tidymodels.org/reference/details_decision_tree_rpart.html",
    "https://parsnip.tidymodels.org/reference/details_decision_tree_spark.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_flexible_earth.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_linear_MASS.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_linear_mda.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_linear_sda.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_linear_sparsediscrim.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_quad_MASS.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_quad_sparsediscrim.html",
    "https://parsnip.tidymodels.org/reference/details_discrim_regularized_klaR.html",
    "https://parsnip.tidymodels.org/reference/details_gen_additive_mod_mgcv.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_brulee.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_gee.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_glm.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_glmnet.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_gls.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_keras.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_lm.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_lme.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_lmer.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_quantreg.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_spark.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_stan_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_linear_reg_stan.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_brulee.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_gee.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_glm.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_glmnet.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_keras.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_LiblineaR.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_spark.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_stan_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_logistic_reg_stan.html",
    "https://parsnip.tidymodels.org/reference/details_mars_earth.html",
    "https://parsnip.tidymodels.org/reference/details_mlp_brulee_two_layer.html",
    "https://parsnip.tidymodels.org/reference/details_mlp_brulee.html",
    "https://parsnip.tidymodels.org/reference/details_mlp_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_mlp_keras.html",
    "https://parsnip.tidymodels.org/reference/details_mlp_nnet.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_brulee.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_glmnet.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_keras.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_nnet.html",
    "https://parsnip.tidymodels.org/reference/details_multinom_reg_spark.html",
    "https://parsnip.tidymodels.org/reference/details_naive_Bayes_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_naive_Bayes_klaR.html",
    "https://parsnip.tidymodels.org/reference/details_naive_Bayes_naivebayes.html",
    "https://parsnip.tidymodels.org/reference/details_nearest_neighbor_kknn.html",
    "https://parsnip.tidymodels.org/reference/details_pls_mixOmics.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_gee.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_glm.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_glmnet.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_hurdle.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_stan_glmer.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_stan.html",
    "https://parsnip.tidymodels.org/reference/details_poisson_reg_zeroinfl.html",
    "https://parsnip.tidymodels.org/reference/details_proportional_hazards_glmnet.html",
    "https://parsnip.tidymodels.org/reference/details_proportional_hazards_survival.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_aorsf.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_partykit.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_randomForest.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html",
    "https://parsnip.tidymodels.org/reference/details_rand_forest_spark.html",
    "https://parsnip.tidymodels.org/reference/details_rule_fit_h2o.html",
    "https://parsnip.tidymodels.org/reference/details_rule_fit_xrf.html",
    "https://parsnip.tidymodels.org/reference/details_survival_reg_flexsurv.html",
    "https://parsnip.tidymodels.org/reference/details_survival_reg_flexsurvspline.html",
    "https://parsnip.tidymodels.org/reference/details_survival_reg_survival.html",
    "https://parsnip.tidymodels.org/reference/details_svm_linear_kernlab.html",
    "https://parsnip.tidymodels.org/reference/details_svm_linear_LiblineaR.html",
    "https://parsnip.tidymodels.org/reference/details_svm_poly_kernlab.html",
    "https://parsnip.tidymodels.org/reference/details_svm_rbf_kernlab.html",
    "https://parsnip.tidymodels.org/reference/glmnet-details.html"
  )

tm_pkg_urls <- c(tm_pkg_urls, details)

# ------------------------------------------------------------------------------

mt_pkg_names <- c(
  "modeltime",
  "modeltime.ensemble",
  "timetk",
  "modeltime.gluonts",
  "modeltime.h2o",
  "modeltime.resample",
  "themis"
)

mt_pkg_urls <-
  c(
    glue("https://business-science.github.io/{mt_pkg_names}/reference"),
    glue("https://business-science.github.io/{mt_pkg_names}/articles")
  )

# ------------------------------------------------------------------------------


rs_pkg_names <- c(
  "vetiver-r",
  "bundle"
)

rs_pkg_urls <-
  c(
    glue("https://rstudio.github.io/{rs_pkg_names}/reference"),
    glue("hhttps://rstudio.github.io/{rs_pkg_names}/articles")
  )

# ------------------------------------------------------------------------------

pkg_urls <-
  c(tm_pkg_urls, mt_pkg_urls)

pkg_targets <-
  c(tm_pkg_urls, mt_pkg_urls, rs_pkg_urls) %>%
  map(ragnar_find_links_clean) %>%
  unlist()

print(length(pkg_targets))

# ------------------------------------------------------------------------------
# Books / websites

site_urls <- c(
  "https://www.tmwr.org",
  "https://www.tidymodels.org/learn/",
  "https://tidymodels.aml4td.org",
  "https://workshops.tidymodels.org",
  "https://hfrick.github.io/tidymodels-survival-workshop",
  "https://emilhvitfeldt.github.io/ISLR-tidymodels-labs"
)

site_targets <-
  map(site_urls, ragnar_find_links) %>%
  unlist()

# # fmt: skip
# tmwr_pages <-
#   c("ames.html", "base-r.html", "categorical.html", "compare.html",
#     "dimensionality.html", "ensembles.html", "explain.html", "grid-search.html",
#     "inferential.html", "iterative-search.html", "models.html", "performance.html",
#     "pre-proc-table.html", "recipes.html", "references.html", "resampling.html",
#     "software-modeling.html", "splitting.html", "tidyverse.html",
#     "trust.html", "tuning.html", "workflow-sets.html", "workflows.html"
#   )
# tmwr_urls <- glue("https://www.tmwr.org/{tmwr_pages}")
#
# site_targets <- c(site_urls, tmwr_urls)

length(site_targets)

# ------------------------------------------------------------------------------

all_targets <-
  c(site_targets, pkg_targets) %>%
  str_subset("step_select", negate = TRUE) %>%
  str_subset("step_(ns|bs)", negate = TRUE) %>%
  str_subset("contributing\\.html", negate = TRUE) %>%
  str_subset("news\\.html", negate = TRUE) %>%
  str_subset("/archive", negate = TRUE) %>%
  str_subset("exports", negate = TRUE) |>
  unique()

length(all_targets)

# ------------------------------------------------------------------------------
# parse pages and add to store

cli_rule("Parsing links")

success <- rep(FALSE, length(all_targets))

for (iter in seq_along(all_targets)) {
  page <- all_targets[iter]
  smol_page <- gsub("https://", "", page, fixed = TRUE)

  chunks <- process_page(page)
  if (!is.null(chunks)) {
    store_res <-
      tryCatch(
        error = function(cnd) cnd$message,
        ragnar_store_insert(store, chunks)
      )
    if (!inherits(store_res, "ragnar::DuckDBRagnarStore")) {
      cli::cli_alert_danger("{smol_page}: {store_res}")
    } else {
      cli::cli_alert_success(smol_page)
      success[iter] <- TRUE
    }
  }
}

# ------------------------------------------------------------------------------

mean(success * 100)
missing_pages <- all_targets[!success]
cli_bullets(missing_pages)

# ------------------------------------------------------------------------------

ragnar_store_build_index(store)

DBI::dbDisconnect(store@.con)

# ------------------------------------------------------------------------------

if (!interactive()) {
  q("no")
}


