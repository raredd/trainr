.onAttach <- function(libname, pkgname) {
  ## work around since these are github packages only
  ok <- tryCatch(find.package('rawr'),
                 error = function(e) invisible(TRUE))
  if (isTRUE(ok))
    packageStartupMessage("rawr package is needed for some functionality:\n",
                          "Run: devtools::install_github(\"raredd/rawr\")")
  invisible()
}
