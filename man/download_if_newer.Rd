% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/async-http.R
\name{download_if_newer}
\alias{download_if_newer}
\title{Download a file, if it is newer than a local file}
\usage{
download_if_newer(url, destfile, etag_file = NULL, headers = character(),
  tmp_destfile = paste0(destfile, ".tmp"), ...)
}
\arguments{
\item{url}{URL to download.}

\item{destfile}{Destination file.}

\item{etag_file}{If not \code{NULL} then the path to a file that may contain
the Etag of a previous request to this URL. If \code{destfile} exists, and
\code{etag_file} exists and it is not empty, then the \code{If-None-Match} HTTP
header is used with this Etag to avoid downloading the file if it has
not changed. If the file at \code{url} has changed, then it is downloaded,
and the the new Etag is stored in \code{etag_file}.}

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
A version of \code{\link[=download_file]{download_file()}} that only downloads if the file at the
specified URL is different from the local one.
}
\section{Examples}{
\preformatted{dest <- tempfile(fileext = ".jpeg")
etag <- tempfile()
dl <- function() {
  ## This URL will repond with an Etag
  download_if_newer("https://httpbin.org/etag/test", dest,
                    etag_file = etag)
}
file.exists(dest)
file.exists(etag)

res1 <- synchronise(dl())

## Downloaded the file, and also created the etag file
file.exists(dest)
file.exists(etag)
readLines(etag)
res1$response$status_code

## This will not download the file again, as the Etag matches
## The status code is 304 Not Modified
res2 <- synchronise(dl())
res2$response$status_code

## HTTP errors contain the response
dest <- tempfile()
etag <- tempfile()
err <- tryCatch(
  synchronise(download_if_newer("https://httpbin.org/status/418",
                                dest, etag)),
  error = function(e) e
)
err
names(err)
cat(rawToChar(err$response$content))
}
}

\seealso{
Other async HTTP tools: \code{\link{download_file}},
  \code{\link{download_one_of}}
}
\keyword{internal}
