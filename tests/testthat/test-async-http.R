
context("http_manager")

test_that("create downloads", {

  skip_if_offline()

  hm <- http_manager$new()
  content <- NULL
  headers <- NULL
  do <- function() {
    r1 <- hm$get("https://httpbin.org/get", .tags = "mytag")
    r1$then(function(resp) {
      content <<- rawToChar(resp$content)
      headers <<- curl::parse_headers_list(resp$headers)
    })
  }

  synchronise(do())
  expect_match(content, "\"Host\": \"httpbin.org\"")
  expect_equal(headers$`content-type`, "application/json")

  ent <- hm$list()[[1]]
  expect_equal(ent$type, c("http", "get"))
  expect_equal(ent$tags, "mytag")
  expect_equal(ent$url, "https://httpbin.org/get")
  expect_equal(ent$state, "done")
  expect_equal(ent$status_code, 200)
})

test_that("create head requests", {

  skip_if_offline()

  hm <- http_manager$new()
  content <- "foobar"
  headers <- NULL
  do <- function() {
    r1 <- hm$head("https://httpbin.org/headers", .tags = "mytag")
    r1$then(function(resp) {
      content <<- resp$content
      headers <<- curl::parse_headers_list(resp$headers)
    })
  }

  synchronise(do())
  expect_null(content)
  expect_true("date" %in% names(headers))

  ent <- hm$list()[[1]]
  expect_equal(ent$type, c("http", "head"))
  expect_equal(ent$tags, "mytag")
  expect_equal(ent$url, "https://httpbin.org/headers")
  expect_equal(ent$state, "done")
  expect_equal(ent$status_code, 200)
})

test_that("http errors", {

  skip_if_offline()

  hm <- http_manager$new()
  response <- NULL
  do <- function() {
    r1 <- hm$get("https://httpbin.org/status/418")
    r1$then(function(resp) response <<- resp)
  }

  async::synchronise(do())
  headers <- curl::parse_headers_list(response$headers)
  content <- rawToChar(response$content)
  expect_match(content, "teapot")
  expect_true("date" %in% names(headers))
  expect_equal(response$status_code, 418)

  ent <- hm$list()[[1]]
  expect_equal(ent$state, "failed")
  expect_identical(ent$status_code, 418L)
  expect_true(ent$total > 0)
  expect_true(ent$bytes > 0)
  expect_s3_class(ent$error, "async_rejected")
})

test_that("connection errors", {

  skip_if_offline()

  hm <- http_manager$new()
  called <- FALSE
  do <- function() {
    hm$get("http://0.42.42.42")$
      then(function(x) called <<- TRUE)
  }

  ret <- tryCatch(async::synchronise(do()), error = identity)
  expect_false(called)

  ent <- hm$list()[[1]]
  expect_equal(ent$state, "failed")
  expect_identical(ent$status_code, NA_integer_)
  expect_identical(ent$total, NA_integer_)
  expect_identical(ent$bytes, 0L)
  expect_s3_class(ent$error, "async_rejected")
  expect_null(ent$response)
})

test_that("we can get a summary", {

  skip_if_offline()

  hm <- http_manager$new()
  do <- function() {
    r1 <- hm$get("https://httpbin.org/get", .tags = "mytag")
    r2 <- hm$get("https://httpbin.org/status/404", .tags = "mytag2")
    r3 <- hm$get("http://0.42.42.42", .tags = "mytag2")$
      catch(function() { })
    when_all(r1, r2, r3)
  }
  synchronise(do())
  s1 <- hm$summary()
  expect_equal(s1$done, 1)
  expect_equal(s1$active, 0)
  expect_equal(s1$failed, 2)
  expect_equal(s1$cancelled, 0)
  expect_equal(s1$total, s1$bytes)
  expect_equal(s1$no_total, 0)
  
  s2 <- hm$summary(.tags = "mytag2")
  expect_equal(s2$done, 0)
  expect_equal(s2$active, 0)
  expect_equal(s2$failed, 2)
  expect_equal(s1$cancelled, 0)
  expect_equal(s2$total, 0)
  expect_equal(s2$no_total, 0)
  expect_equal(s2$bytes, 0)
})

test_that("cancelling requests using tags", {
  hm <- http_manager$new()
  do <- function() {
    r1 <- hm$get("https://httpbin.org/get", .tags = "mytag")
    r2 <- hm$get("https://httpbin.org/get", .tags = "mytag2")
    r3 <- hm$get("https://httpbin.org/get", .tags = "mytag2")$
      catch(function() { })
    hm$cancel(.tags = "mytag2")
    s1 <- hm$summary()
    expect_equal(s1$done, 0)
    expect_equal(s1$active, 1)
    expect_equal(s1$failed,  0)
    expect_equal(s1$cancelled, 2)
    when_all(r1, r2, r3)
  }
  synchronise(do())  
})

test_that("list", {

})

test_that("progress is reported properly in summary and list", {

})

test_that("finish", {

})

test_that("finalizer", {

})
