# H2OMultinomialModel

#' @rdname vetiver_create_description
#' @export
vetiver_create_description.H2OMultinomialModel <- function(model) {
    # FIXME: implement
    "h2o desc here"
}

#' @rdname vetiver_create_meta
#' @export
vetiver_create_meta.H2OMultinomialModel <- function(model, metadata) {
    metadata$model_class = class(model)
    vetiver_meta(metadata, required_pkgs = c("h2o"))
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.H2OMultinomialModel <- function(model, ...) {
    # FIXME: implement
}

#' @rdname model_package
#' @export
model_package.H2OMultinomialModel <- function(model) {
    #FIXME: 
}

#' @rdname model_unpackage
#' @export
model_unpackage.H2OMultinomialModel <- function(model) {
    #FIXME: 
}

#' @rdname handler_startup
#' @export
handler_startup.H2OMultinomialModel <- function(vetiver_model) {
    attach_pkgs(vetiver_model$metadata$required_pkgs)
}

#' @rdname handler_startup
#' @export
handler_predict.H2OMultinomialModel <- function(vetiver_model, ...) {
    # FIXME: check if we need to convert input types (e.g. "1" to 1, "true" to T, etc.)
    # use h2o predict function woohoo :)
}

# H2ORegressionModel

#' @rdname vetiver_create_description
#' @export
vetiver_create_description.H2ORegressionModel <- function(model) {
    # FIXME: implement
    "h2o desc here"
}

#' @rdname vetiver_create_meta
#' @export
vetiver_create_meta.H2ORegressionModel <- function(model, metadata) {
    metadata$model_class = class(model)
    vetiver_meta(metadata, required_pkgs = c("h2o"))
}

#' @rdname vetiver_create_ptype
#' @export
vetiver_ptype.H2ORegressionModel <- function(model, ...) {
    # FIXME: implement
}

#' @rdname model_package
#' @export
model_package.H2ORegressionModel <- function(model) {
    f <- tempfile()
    h2o::h2o.saveModel(model, path = dirname(f), filename = basename(f))
    file_size <- file.info(f)$size
    fd <- file(f, 'rb', raw = T)
    raw_model <- readBin(fd, what = 'raw', n = file_size, endian = 'little')
    close(fd)
    file.remove(f)
    raw_model
}

#' @rdname model_unpackage
#' @export
model_unpackage.H2ORegressionModel <- function(model) {
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
handler_startup.H2ORegressionModel <- function(vetiver_model) {
    attach_pkgs(vetiver_model$metadata$required_pkgs)
}

#' @rdname handler_startup
#' @export
handler_predict.H2ORegressionModel <- function(vetiver_model, ...) {
    # FIXME: check if we need to convert input types (e.g. "1" to 1, "true" to T, etc.)
    # use h2o predict function woohoo :)
}

# a list of other H2O model classes we may want to add:
# H2OAutoEncoderModel
# H2OBinomialModel
# H2OBinomialUpliftModel
# H2OClusteringModel
# H2ODimReductionModel
# H2OOrdinalModel
# H2OWordEmbeddingModel
