# a list of other H2O model classes we may want to add:
# H2OAutoEncoderModel
# H2OBinomialModel
# H2OBinomialUpliftModel
# H2OClusteringModel
# H2ODimReductionModel
# H2OOrdinalModel
# H2OWordEmbeddingModel

#' @rdname vetiver_create_description
#' @export
vetiver_create_description.h2o <- function(model) {
    paste('A H2O', class(model), 'model')
}

#' @rdname vetiver_create_meta
#' @export
vetiver_create_meta.h2odefault <- function(model, metadata) {
    metadata$h2o_model_class = class(model)
    vetiver_meta(metadata, required_pkgs = c("h2o"))
}

#' @rdname vetiver_ptype
#' @export
vetiver_ptype.h2o <- function(model, ...) {
    rlang::check_dots_used()
    dots <- list(...)
    check_ptype_data(dots)
    dots$ptype_data |>
        dplyr::select(h2o::getParms(model)$x) |>
        vctrs::vec_ptype() |>
        tibble::as_tibble()
}

#' @rdname model_package
#' @export
model_package.h2o <- function(model) {
    f <- tempfile()
    h2o::h2o.saveModel(model, path = dirname(f), filename = basename(f))
    file_size <- file.info(f)$size
    fd <- file(f, 'rb', raw = T)
    raw_model <- readBin(fd, what = 'raw', n = file_size, endian = 'little')
    close(fd)
    file.remove(f)
    structure(raw_model, class = class(model))
}

#' @rdname model_unpackage
#' @export
model_unpackage.h2o <- function(model) {
    model <- structure(model, class = NULL) # get the original object
    f <- tempfile()
    fd <- file(f, 'wb')
    writeBin(model, con = fd, endian = 'little')
    close(fd)
    loaded_model <- h2o::h2o.loadModel(f)
    file.remove(f)
    loaded_model
}

#' @rdname handler_startup
#' @export
handler_startup.h2o <- function(vetiver_model) {
    attach_pkgs(vetiver_model$metadata$required_pkgs)
    h2o::h2o.init()
}

#' @rdname handler_predict
#' @export
handler_predict.h2o <- function(vetiver_model, ...) {
    function(req) {
        new_data <- req$body
        new_data <- vetiver_type_convert(new_data, vetiver_model$ptype)
        h2o::h2o.predict(vetiver_model$model, h2o::as.h2o(new_data)) |>
            h2o::as.dataframe() |>
            dplyr::select(h2o::getParms()$y) |>
            as.character()
    }
}
