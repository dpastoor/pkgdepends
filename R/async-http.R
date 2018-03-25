
#' @importFrom async http_get http_head http_stop_for_status

http_manager <- R6Class(
  "http_manager",
  cloneable = FALSE,
  public = list(
    initialize = function(headers = character())
      hm_init(self, private, headers),
    head       = function(url, headers = character(), ..., .tags = NULL)
      hm_head(self, private, url, headers, ..., .tags = .tags),
    get        = function(url, headers = character(), ..., .tags = NULL)
      hm_get(self, private, url, headers, ..., .tags = .tags),
    cancel     = function(.tags = NULL) hm_cancel(self, private, .tags),
    list       = function(.tags = NULL) hm_list(self, private, .tags),
    summary    = function(.tags = NULL) hm_summary(self, private, .tags),
    finish     = function() hm_finish(self, private)
  ),
  private = list(
    entries = list(),
    headers = NULL,
    start_at = NULL,
    finish_at = NULL,
    
    get_headers = function(headers) hm__get_headers(self, private, headers),
    add = function(deferred, type, .tags, url)
      hm__add(self, private, deferred, type, .tags, url),
    select = function(.tags) hm__select(self, private, .tags),
    on_progress = function(id, data) hm__on_progress(self, private, id, data)
  )
)

hm_init <- function(self, private, headers) {
  private$headers <- headers
  invisible(self)
}

#' @importFrom utils modifyList

hm_head <- function(self, private, url, headers, ..., .tags) {
  self; private; url; headers; list(...); .tags
  headers <- private$get_headers(headers)
  def <- http_head(
    url = url, headers = headers, ...,
    on_progress = function(data) private$on_progress(id, data))
  id <- private$add(def, type = c("http", "head"), .tags = .tags, url = url)
  invisible(def)
}

hm_get <- function(self, private, url, headers, ..., .tags) {
  self; private; url; headers; list(...); .tags
  headers <- private$get_headers(headers)
  def <- http_get(
    url = url, headers = headers, ...,
    on_progress = function(data) private$on_progress(id, data))
  id <- private$add(def, type = c("http", "get"), .tags = .tags, url = url)
  invisible(def)
}

hm_cancel <- function(self, private, .tags) {
  sel <- which(private$select(.tags))
  for (s in sel) {
    private$entries[[s]]$deferred$cancel()
    private$entries[[s]]$state <- "cancelled"
  }
  invisible(self)
}

hm_list <- function(self, private, .tags) {
  sel <- which(private$select(.tags))
  private$entries[sel]
}

hm_summary <- function(self, private, .tags) {
  entries  <- self$list(.tags = .tags)
  state <- vcapply(entries, "[[", "state")
  list(
    done = sum(state == "done"),
    active = sum(state == "active"),
    failed = sum(state == "failed"),
    cancelled = sum(state == "cancelled"),
    total = sum(viapply(entries, "[[", "total")[state != "cancelled"],
                na.rm = TRUE),
    no_total = sum(is.na(viapply(entries, "[[", "total")) &
                   ! state %in% c("failed", "cancelled")),
    bytes = sum(viapply(entries, "[[", "bytes")[state != "cancelled"],
                na.rm = TRUE)
  )
}

hm_finish <- function(self, private) {
  private$finish_at <- Sys.time()
  self$cancel()
  invisible(self)
}

hm__get_headers <- function(self, private, headers) {
  h <- private$headers
  h[names(headers)] <- headers
  h[!is.na(h)]
}

hm__add <- function(self, private, deferred, type, .tags, url) {
  self; private; deferred; type; .tags; url
  if (!is.null(private$finish_at)) stop("http manager already finished")
  if (is.null(private$start_at)) private$start_at <- Sys.time()

  deferred$
    then(function(resp) {
      private$entries[[id]]$status_code <- resp$status_code
      resp
    })$
    then(http_stop_for_status)$
    then(function() private$entries[[id]]$response <- NULL)$
    catch(function(e) {
      private$entries[[id]]$error <- e
      private$entries[[id]]$state <- "failed"
    })

  private$entries <- c(private$entries, list(list(
    deferred = deferred,
    type = type,
    tags = .tags,
    url = url,
    status_code = NA_integer_,
    total = NA_integer_,
    bytes = 0L,
    state = "active",
    error = NULL,
    response = NULL
  )))

  id <- length(private$entries)
  id 
}

hm__select <- function(self, private, .tags) {
  if (is.null(.tags)) {
    rep(TRUE, length(private$entries))
  } else {
    vlapply(private$entries, function(x) any(x$tags %in% .tags))
  }
}

hm__on_progress <- function(self, private, id, data) {
  if (length(data$amount)) {
    private$entries[[id]]$bytes <- private$entries[[id]]$bytes + data$amount
  }
  if (length(data$total) && is.na(private$entries[[id]]$total)) {
    private$entries[[id]]$total <- data$total
  }

  if (data$event == "done") {
    private$entries[[id]]$status_code <- data$status_code
    private$entries[[id]]$state <- "done"
  }
}
