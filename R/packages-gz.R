
#' @importFrom tools file_ext

read_packages_file <- function(path, mirror, repodir, platform, ...,
                               .list = list()) {
  pkgs <- as.tibble(read.dcf.gz(path))
  extra <- c(
    list(repodir = repodir, platform = platform),
    list(...), .list)
  assert_that(all_named(extra))
  pkgs[names(extra)] <- extra
  names(pkgs) <- tolower(names(pkgs))

  if (! "needscompilation" %in% names(pkgs)) {
    pkgs$needscompilation <- if (! "built" %in% names(pkgs)) {
      NA_character_
    } else {
      ifelse(is.na(pkgs$built), NA_character_, "no")
    }
  }

  pkgs$ref <- pkgs$package
  pkgs$type <- "standard"
  pkgs$direct <- FALSE
  pkgs$status <- "OK"
  pkgs$target <- packages_make_target(
    platform, repodir, pkgs$package, pkgs$version, pkgs[["file"]], pkgs[["path"]])
  pkgs$sources <- packages_make_sources(
    mirror, platform, pkgs$target, repodir, pkgs$package, pkgs$version)

  deps <- packages_parse_deps(pkgs)
  list(pkgs = pkgs, deps = deps)
}

packages_parse_deps <- function(pkgs) {
  no_pkgs <- nrow(pkgs)
  cols <- intersect(colnames(pkgs), tolower(deptypes()))
  ## as.character is for empty tibbles, e.g. from empty BioC repos
  deps <- as.character(unlist(pkgs[, cols], use.names = FALSE))
  nna <- which(!is.na(deps))
  if (length(nna)) {
    not_na_deps <- deps[nna]
    sp <- strsplit(not_na_deps, ",", fixed = TRUE)
    ll <- sapply(sp, length, USE.NAMES = FALSE)
    sp <- unlist(sp, use.names = FALSE)
    parsed <- re_match(sp,
      paste0("^\\s*(?<package>[^(\\s]+)\\s*",
             "(?:\\((?<op>[^0-9\\s]+)\\s*(?<version>[^)\\s]+)\\))?\\s*$"))
    parsed$idx <- rep(rep(seq_len(no_pkgs), length(cols))[nna], ll)
    parsed$type <- rep(rep(cols, each = no_pkgs)[nna], ll)
    parsed$ref <- parsed$package
    parsed$upstream <- pkgs$package[parsed$idx]
    parsed <- parsed[, c("upstream", "idx", "ref", "type", "package",
                         "op", "version")]
    parsed <- parsed[order(parsed$idx), ]

  } else {
    parsed <- tibble(upstream = character(),
                     idx = integer(),
                     ref = character(),
                     type = character(),
                     package = character(),
                     version = character(),
                     op = character())
  }

  parsed
}

packages_make_target <- function(platform, repodir, package, version,
                                 file, path) {

  assert_that(
    is_string(platform),
    is_string(repodir),
    is_character(package),
    is_character(version), length(version) == length(package),
    is.null(file) || (is.character(file) && length(file) == length(package)),
    is.null(path) || (is.character(path) && length(path) == length(package))
  )

  res <- rep(NA_character_, length(package))
  ext <- get_cran_extension(platform)

  ## 'File' field, if present
  if (!is.null(file)) {
    wh <- !is.na(file)
    res[wh] <- paste0(repodir, "/", file[wh])
  }

  ## 'Path' field, if present
  if (!is.null(path)) {
    wh <- is.na(res) & !is.na(path)
    res[wh] <- paste0(repodir, "/", path[wh], "/", package[wh], "_",
                      version[wh], ext)
  }

  ## Otherwise default
  wh <- is.na(res)
  res[wh] <- paste0(repodir, "/", package[wh], "_", version[wh], ext)

  res
}

packages_make_sources <- function(mirror, platform, target, repodir,
                                  package, version) {

  assert_that(
    is_string(mirror),
    is_string(platform),
    is_character(target),
    is_string(repodir),
    is_character(package),
    is_character(version), length(version) == length(package))

  url <- paste0(mirror, "/", target)

  if (platform != "source") {
    as.list(url)

  } else {
    url2 <- paste0(mirror, "/", repodir, "/Archive/", package, "_",
                   version, ".tar.gz")
    zip_vecs(url, url2)
  }
}

merge_packages_data <- function(..., .list = list()) {
  pkgslist <- c(list(...), .list)

  pkgs <- rbind_expand(.list = lapply(pkgslist, "[[", "pkgs"))

  ## Need to shift deps indices first to merge deps
  num_pkgs <- viapply(pkgslist, function(x) nrow(x$pkgs), USE.NAMES = FALSE)
  shifts <- c(0L, cumsum(num_pkgs))
  for (i in seq_along(pkgslist)) {
    pkgslist[[i]]$deps$idx <- pkgslist[[i]]$deps$idx + shifts[i]
  }
  deps <- rbind_expand(.list = lapply(pkgslist, "[[", "deps"))

  list(pkgs = pkgs, deps = deps)
}

rbind_expand <- function(..., .list = list()) {
  data <- c(list(...), .list)
  cols <- unique(unlist(lapply(data, function(x) colnames(x))))
  for (i in seq_along(data)) {
    miss_cols <- setdiff(cols, colnames(data[[i]]))
    if (length(miss_cols)) {
      na_df <- as.tibble(structure(
        replicate(length(miss_cols), NA, simplify = FALSE),
        names = miss_cols))
      data[[i]] <- as.tibble(cbind(data[[i]], na_df))
    }
  }

  do.call(rbind, data)
}