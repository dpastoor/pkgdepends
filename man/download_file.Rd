% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/async-http.R
\name{download_file}
\alias{download_file}
\title{Download a file, asynchronously}
\usage{
download_file(url, destfile, etag_file = NULL,
  tmp_destfile = paste0(destfile, ".tmp"), ...)
}
\arguments{
\item{url}{URL to download.}

\item{destfile}{Destination file.}

\item{etag_file}{If not \code{NULL}, and the reponse is successful and
includes an \code{Etag} header, then this header is stored in this file.
It can be used to cache the file, with the \code{\link[=download_if_newer]{download_if_newer()}} or
the \code{\link[=download_one_of]{download_one_of()}} functions.}

\item{tmp_destfile}{Where to store the temporary destination file.}

\item{...}{Additional arguments are passed to \code{\link[async:http_get]{async::http_get()}}.}
}
\value{
An \link[async:deferred]{async::deferred} object. It resolves to a list with entries:
\itemize{
\item \code{url}: The URL in the request.
\item \code{destfile}: The destination file.
\item \code{response}: The response object from the curl package.
\item \code{etag}: The Etag of the response, of \code{NULL} if missing.
\item \code{etag_file}: The file the Etag was written to, or \code{NULL} otherwise
}
}
\description{
This is the asynchronous version of \code{\link[utils:download.file]{utils::download.file()}}.
}
\details{
\code{download_file} also has some nice improvements:
\itemize{
\item It uses a temporary file, so never leaves a partial file at \code{destfile}.
\item It can write the HTTP Etag from the response to a file, which can
be used in \code{\link[=download_if_newer]{download_if_newer()}}, etc.
\item It returns the HTTP response as part of the error message if the
response status code indicates a client or server error.
\item Well, it is asynchronous.
}
}
\section{Examples}{
\preformatted{dest1 <- tempfile(fileext = ".jpeg")
dest2 <- tempfile(fileext = ".png")
dl <- function() {
  when_all(
    download_file("https://httpbin.org/image/jpeg", dest1),
    download_file("https://httpbin.org/image/png", dest2)
  )
}
resps <- synchronise(dl())
lapply(resps, function(x) x$response$status_code)
resps[[1]]$url
resps[[1]]$destfile
resps[[1]]$response$times
file.exists(dest1)
file.exists(dest2)

## HTTP errors contain the response
dest <- tempfile()
err <- tryCatch(
  synchronise(download_file("https://httpbin.org/status/418", dest)),
  error = function(e) e
)
err
names(err)
cat(rawToChar(err$response$content))
}

[[1]: R:[1
[[1]: R:[1
[[1]: R:[1
}

\seealso{
Other async HTTP tools: \code{\link{download_if_newer}},
  \code{\link{download_one_of}}
}
\keyword{internal}
