tm_ragnar_store <- function() {
  # path <- tm_store_path()
  # if (!file.exists(path)) {
  #   update_store()
  # }
  path <- "~/tmp/tidymodels.ragnar.duckdb"
  ragnar::ragnar_store_connect(
    path,
    read_only = TRUE
  )
}

#' Updates the tidymodels knowledge store
#'
#' It always downloads the latest version of the store from the
#' tmhelp GitHub repository.
#'
#' The download location can be configured with a few environment variables:
#' - `TMHELP_STORE_URL`: a custom URL to download the store from. The default is to downlaod the latest store
#' from the `topepo/tmhelp` repository in the `store` release.
#' - `TMHELP_STORE_RELEASE`: the release tag to download the store from. Defaults to `store`.
#' - `TMHELP_STORE_REPOSITORY`: the repository to download the store from. Defaults to `topepo/tmhelp`.
#'
#' @return `NULL` invisibly.
#' @export
update_store <- function() {
  path <- tm_store_path()
  if (!dir.exists(dirname(path))) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  }

  tmp <- tempfile()
  on.exit(unlink(tmp), add = TRUE)

  download.file(tm_store_url(), destfile = tmp)
  file.rename(tmp, path)
  invisible(NULL)
}

tm_store_url <- function() {
  url <- Sys.getenv("TMHELP_STORE_URL", "")
  if (nzchar(url)) {
    return(url)
  }
  release <- Sys.getenv("TMHELP_STORE_RELEASE", "store")
  repository <- Sys.getenv("TMHELP_STORE_REPOSITORY", "topepo/tmhelp")
  commit_hash <- readLines(
    sprintf("https://github.com/%s/releases/download/%s/LATEST", repository, release),
    n = 1L
  )
  sprintf(
    "https://github.com/topepo/tmhelp/releases/download/store/tm.ragnar.store-%s",
    commit_hash
  )
}

tm_store_path <- function() {
  cache_dir <- tools::R_user_dir("tmhelp", which = "cache")
  store_path <- file.path(cache_dir, "tm.ragnar.store")
  store_path
}
