.pkgenv <- new.env(parent=emptyenv())

.onLoad  <- function(libname, pkgname) {
    hasPkg <- requireNamespace("gunsales", quietly = TRUE)
    .pkgenv[["hasPkg"]] <- hasPkg
}

.onAttach <- function(libname, pkgname) {
    if (!.pkgenv$hasPkg) {
        msg <- paste("To fully use this package, you must install the",
                     "gunsales package from CRAN. To install that ",
                     "package, run `install.packages('gunsales').")
        msg <- paste(strwrap(msg), collapse="\n")
        packageStartupMessage(msg)
    }
}

.hasData <- function(has_data = .pkgenv$has_data) {
    if (!.pkgenv$hasPkg) {
        msg <- paste("To fully use this function, you must have the",
                     "`gunsales` package installed.")
        msg <- paste(strwrap(msg), collapse="\n")
        stop(msg)
    }
}
