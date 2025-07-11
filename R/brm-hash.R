## ------------------------------------------------------------------
##  Central digest helper  (internal)
## ------------------------------------------------------------------
#' Internal helper: digest wrapper used by all hash methods
#' @noRd
.brms_digest <- function(object, algo = "xxhash64") {
  require_package("digest")
  digest::digest(object, algo = algo, serialize = TRUE)
}

#' Recursively remove attached environments from an object
#' @keywords internal
remove_env_attrs <- function(obj) {
  if (!is.null(attr(obj, ".Environment")))
    attr(obj, ".Environment") <- NULL
  if (inherits(obj, "formula"))
    environment(obj) <- emptyenv()
  if (is.list(obj) || is.pairlist(obj))
    obj <- lapply(obj, remove_env_attrs)
  obj
}

## ------------------------------------------------------------------
##  S3 generic
## ------------------------------------------------------------------
#' Class-aware hashing for individual brm() arguments
#'
#' Dispatches to methods that normalise and hash each argument according to
#' its class (formula, family, data.frame, …).  All methods ultimately call
#' \code{digest::digest()}, but strip environments and reorder components so
#' that equivalent inputs produce identical hashes.
#'
#' @param x   A single argument from a brm() call.
#' @param ... Passed on to class-specific methods (e.g. \code{algo},
#'            \code{threshold}).
#' @return    A character scalar hash.
#' @export
hash_brm_arg <- function(x, ...){
  UseMethod("hash_brm_arg")
}
## ------------------------------------------------------------------
##  Methods for major classes
## ------------------------------------------------------------------
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



#' Stable hash for a set of brm() arguments
#'
#' @param args_list A **named** list containing the arguments that uniquely
#'   define a model (e.g., formula, data, family, prior, …).
#' @param algo      Digest algorithm passed to \code{digest}.
#' @return          A character hash key.
#' @export
hash_brm_call_master <- function(call, algo = "xxhash64") {
  if (!is.brm_call(call)){
    stop2("args_list must be a *named* list" )
  }
  # order
  args_list <- call[order(names(call))]
  args_list$mcall <- NULL
  args_list$fit <- NULL
  #  hash elements
  hashed_parts <- lapply(args_list, hash_brm_arg, algo = algo)
  # for (prop in names(args_list)) {
  #   e = args_list[[prop]]
  #
  #   print(prop)
  #   print(e)
  #   print(class(e))
  #
  #   hash_brm_arg(e)
  #
  # }
  # packages' versions
  brms_version <- packageVersion("brms")
  backend_version <- get_backend_version(call$backend)
  # collapse the per-argument hashes into one final key
  call$hash <-   .brms_digest(nlist(hashed_parts,
                                    brms_version,
                                    backend_version ) )
  call
}
get_backend_version<- function(backend){
  if(backend == "rstan" ){
    v = utils::packageVersion("rstan")
  }
  if(backend == "cmdstanr" ){
    v = cmdstanr::cmdstan_version()
  }
  if(backend == "mock" ){
    v = "0.0.1"
  }
  v
}
#' Internal helper: create file argument if file_auto is TRUE
#' @noRd
create_filename_auto <- function(call) {
  if (!call$file_auto) {
    return(call)
  }
  # We inform user that we override file or file_refit arguments in case necessary
  # if (!is.null(call$file) | call$file_refit != 'on_change') {
  #   message("Since file_auto = TRUE, the file and file_refit arguments were overwritten.")
  # }
  call <- hash_brm_call_master(call)
  call$file <- paste0('cache-brm-result_', call$hash, '.Rds')
  call$file_refit <- "on_change"
  call
}

