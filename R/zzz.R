#' @useDynLib betapoisson
NULL

.onUnload <- function(libpath) {
    library.dynam.unload("betapoisson", libpath)
}
