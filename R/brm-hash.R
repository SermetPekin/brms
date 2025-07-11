#' Internal helper: Digest wrapper used by all hash methods
#' @noRd
.brms_digest <- function(object, algo = "xxhash64") {
  .require_package("digest")
  digest::digest(object, algo = algo, serialize = TRUE)
}

#' Internal helper: Recursively remove attached environments from an object
#' @noRd
.remove_env_attrs <- function(obj) {
  if (!is.null(attr(obj, ".Environment"))) {
    attr(obj, ".Environment") <- NULL
  }
  if (inherits(obj, "formula")) {
    environment(obj) <- emptyenv()
  }
  if (is.list(obj) || is.pairlist(obj)) {
    obj <- lapply(obj, .remove_env_attrs)
  }
  obj
}

#' Class-aware hashing for individual \code{brm()} arguments
#' Dispatches to methods that normalize and hash each argument according to
#' its class (e.g., formula, family, data.frame). All methods ultimately call
#' \code{digest::digest()}, but strip environments and reorder components so
#' that equivalent inputs produce identical hashes.
#'
#' @param x A single argument from a \code{brm()} call.
#' @param ... Passed to class-specific methods (e.g., \code{algo}, \code{threshold}).
#'
#' @return A character scalar hash.
#'
#' @export
hash_brm_arg <- function(x, ...) {
  UseMethod("hash_brm_arg")
}


#' Hashing method for formula objects
#' Strips the environment from the formula and hashes its character
#' representation. Used internally by \code{hash_brm_arg()}.
#'
#' @inheritParams hash_brm_arg
#'
#' @export
hash_brm_arg.formula <- function(x, ...) {
  environment(x) <- emptyenv()
  .brms_digest(as.character(x), ...)
}


#' @export
hash_brm_arg.brmsformula <- function(x, ...) {
  x$formula <- hash_brm_arg(x$formula, ...)
  if (length(x$pforms))
    x$pforms <- lapply(x$pforms, hash_brm_arg, ...)
  if (length(x$nlpars))
    x$nlpars <- sort(x$nlpars)
  .brms_digest(x, ...)
}

#' @export
hash_brm_arg.mvbrmsformula <- function(x, ...) {
  x$forms <- lapply(x$forms[order(names(x$forms))], hash_brm_arg, ...)
  .brms_digest(x, ...)
}

#' @export
hash_brm_arg.family <- function(x, ...) {
  .brms_digest(list(family = x$family, link = x$link), ...)
}

#' @export
hash_brm_arg.character <- function(x, ...) {
  .brms_digest(x)
}

#' @export
hash_brm_arg.data.frame <- function(x,
                                    threshold = 1e7,
                                    algo = "xxhash64",
                                    ...) {
  cells <- nrow(x) * ncol(x)
  if (cells > threshold) {
    .brms_digest(dim(x), algo = algo)
  } else {
    .brms_digest(remove_env_attrs(x), algo = algo)
  }
}

#' @export
hash_brm_arg.function <- function(x, ...) {
  .brms_digest(deparse(body(x), width.cutoff = 500L), ...)
}

#' @export
hash_brm_arg.language <- function(x, ...) {
  .brms_digest(deparse(x, width.cutoff = 500L), ...)
}

#' @export
hash_brm_arg.call <- function(x, ...) hash_brm_arg.language(x, ...)

#' @export
hash_brm_arg.expression <- function(x, ...) hash_brm_arg.language(x, ...)

#' @export
hash_brm_arg.list <- function(x, ...) {
  ## data.frames have their own method
  if (inherits(x, "data.frame")) {
    return(NextMethod())
  }
  ## ── Empty list: nothing to hash ──────────────────────────────────
  if (length(x) == 0L) {
    # Return an explicit digest of the empty list so the caller still gets
    return(.brms_digest(list(), ...))
  }
  ## ── Validate names ───────────────────────────────────────────────
  nm <- names(x)
  if (is.null(nm) || anyNA(nm) || any(nm == "")) {
    stop("hash_brm_arg.list() expects a fully *named* list.", call. = FALSE)
  }
  ## ── Stable order + recursive hashing ─────────────────────────────
  x <- x[order(nm)]
  x <- lapply(x, hash_brm_arg, ...)   # S3 dispatch handles each element
  .brms_digest(x, ...)
}

#' @export
hash_brm_arg.default <- function(x, ...) {
  .brms_digest(remove_env_attrs(x), ...)
}

#' Stable hash for a set of \code{brm()} arguments
#'
#' Hashes the relevant elements of a \code{brm_call} object in a consistent and
#' order-independent way, producing a stable identifier for the model definition.
#'
#' @param call A \code{brm_call} object defining a model (e.g., formula, data,
#'   family, prior, etc.).
#' @param algo Digest algorithm passed to \code{digest::digest()}.
#'
#' @return The same \code{brm_call} object, augmented with a \code{$hash} field.
#'
#' @export
hash_brm_call_master <- function(call, algo = "xxhash64") {
  if (!is.brm_call(call)) {
    stop2("call must be a *brm_call* object")
  }

  args_list <- call[order(names(call))]
  args_list$mcall <- NULL
  args_list$fit <- NULL

  hashed_parts <- lapply(args_list, hash_brm_arg, algo = algo)

  brms_version <- packageVersion("brms")
  backend_version <- get_backend_version(call$backend)

  call$hash <- .brms_digest(
    nlist(
      hashed_parts,
      brms_version,
      backend_version
    )
  )

  call
}

#' Internal helper: Get version of backend
#'
#' Returns the version of the specified backend used in fitting the model.
#'
#' @param backend A character string; either \code{"rstan"}, \code{"cmdstanr"}, or \code{"mock"}.
#'
#' @return A version object or character string, depending on backend.
#'
#' @noRd
get_backend_version <- function(backend) {
  if (backend == "rstan") {
    v <- utils::packageVersion("rstan")
  } else if (backend == "cmdstanr") {
    v <- cmdstanr::cmdstan_version()
  } else if (backend == "mock") {
    v <- "0.0.1"
  } else {
    stop2("Unknown backend: ", backend)
  }
  v
}


#' Internal helper: Create `file` argument when `file_auto = TRUE`
#'
#' Adds a cache filename and sets `file_refit = "on_change"` based on the hash
#' of the model call. This is used to enable automatic reuse of cached fits.
#'
#' @param call A \code{brm_call} object.
#'
#' @return The modified \code{brm_call} object with auto-generated file settings.
#'
#' @noRd
create_filename_auto <- function(call) {
  if (!call$file_auto) {
    return(call)
  }

  # Inform the user that file/file_refit are overwritten if needed
  # if (!is.null(call$file) || call$file_refit != "on_change") {
  #   message("Since file_auto = TRUE, the file and file_refit arguments were overwritten.")
  # }

  call <- hash_brm_call_master(call)
  call$file <- paste0("cache-brm-result_", call$hash, ".Rds")
  call$file_refit <- "on_change"
  call
}

