% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/store.r
\name{update_store}
\alias{update_store}
\title{Updates the tidymodels knowledge store}
\usage{
update_store()
}
\value{
\code{NULL} invisibly.
}
\description{
It always downloads the latest version of the store from the
tmhelp GitHub repository.
}
\details{
The download location can be configured with a few environment variables:
\itemize{
\item \code{TMHELP_STORE_URL}: a custom URL to download the store from. The default is to downlaod the latest store
from the \code{topepo/tmhelp} repository in the \code{store} release.
\item \code{TMHELP_STORE_RELEASE}: the release tag to download the store from. Defaults to \code{store}.
\item \code{TMHELP_STORE_REPOSITORY}: the repository to download the store from. Defaults to \code{topepo/tmhelp}.
}
}
