#' @rdname model_package
#' @export
model_package <- function(model) {
    UseMethod("model_package")
}

#' @rdname model_package
#' @export
model_package.default <- function(model) {
    model
}

#' @rdname model_unpackage
#' @export
model_unpackage <- function(model) {
    UseMethod("model_unpackage")
}

#' @rdname model_unpackage
#' @export
model_unpackage.default <- function(model) {
    model
}
