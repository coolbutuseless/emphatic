

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Idea borrowed from https://github.com/rstudio/htmltools/pull/108/files
# (as mentioned in the) knitr::knit_print vignette)
# This is how to have a knit_print method for 'emphatic' objects
# without having knitr as a dependency for this package
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg)    , length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class)  , length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get a logical value from the env variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_env_lgl <- function(nm, unset) {
  res <- Sys.getenv(nm, unset = '')
  if (res == '') {
    unset
  } else {
    isTRUE(as.logical(res))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get a mumeric value from the env variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_env_dbl <- function(nm, unset) {
  res <- Sys.getenv(nm, unset = '')
  if (res == '') {
    res <- unset
  } else {
    res <- as.numeric(res)
    if (is.na(res)) {
      res <- unset
    }
  }
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the default options, but avoid overwriting any option that the
# user might have already set e.g. in their .Rprofile
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
.onLoad <- function(libname, pkgname) {

  options(HL_NA            = Sys.getenv ("HL_NA"           , unset =             'NA'))
  options(HL_TEXT_MODE     = Sys.getenv ("HL_TEXT_MODE"    , unset =       'contrast'))
  options(HL_TEXT_CONTRAST = get_env_dbl("HL_TEXT_CONTRAST", unset =                1))
  options(HL_GREP_COL      = Sys.getenv("HL_GREP_COL"      , unset =         "yellow"))
  options(HL_SUB_COL       = Sys.getenv("HL_SUB_COL"       , unset = "darkslategray1"))
  options(HL_INS_COL       = Sys.getenv("HL_INS_COL"       , unset =     "palegreen1"))
  options(HL_DEL_COL       = Sys.getenv("HL_DEL_COL"       , unset =          "coral"))

  # Is this the Rstudio 256 colour console?
  rstudio_256 <- Sys.getenv("RSTUDIO_CONSOLE_COLOR", 0) == 256

  # Assume anything not Rstudio is 24-bit colour
  options(HL_FULL_COLOUR   = get_env_lgl("HL_FULL_COLOUR"  , unset = !rstudio_256))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Enable knit_print compatibility without requiring 'knitr' in 'Imports'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  register_s3_method("knitr", "knit_print", "emphatic")

  invisible()
}
