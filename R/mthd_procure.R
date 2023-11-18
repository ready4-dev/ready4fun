#' Procure items from a dataset
#' @description procure.ready4fun_abbreviations() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 submodule class for tibble object lookup table of abbreviations. The function returns Object (an output object of multiple potential types).
#' @param x An instance of `ready4fun_abbreviations`, a ready4 submodule class for tibble object lookup table of abbreviations.
#' @param match_1L_chr Match (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("abbreviation", "extension")
#' @param what_1L_chr What (a character vector of length one), Default: c("table", "string")
#' @return Object (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom dplyr pull
#' @importFrom rlang sym
#' @importFrom ready4 procure
procure.ready4fun_abbreviations <- function (x, match_1L_chr = character(0), type_1L_chr = c("abbreviation", 
    "extension"), what_1L_chr = c("table", "string")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    object_xx <- NULL
    if (identical(match_1L_chr, character(0))) {
        warning("No value supplied to `match_1L_chr`. NULL value returned")
    }
    else {
        object_xx <- get_abbrs(match_1L_chr, type_1L_chr = type_1L_chr, 
            abbreviations_lup = x)
        if (what_1L_chr == "string") {
            target_1L_chr <- ifelse(type_1L_chr == "abbreviation", 
                "long_name_chr", "short_name_chr")
            object_xx <- object_xx %>% dplyr::pull(!!rlang::sym(target_1L_chr))
        }
    }
    return(object_xx)
}
#' @rdname procure-methods
#' @aliases procure,ready4fun_abbreviations-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("ready4fun_abbreviations", package = "ready4fun"), procure.ready4fun_abbreviations)
#' Procure items from a dataset
#' @description procure.ready4fun_manifest() is a procure method that procures data by executing a search and retrieval algorithm using data contained in an instance of a class. This method is implemented for the ready4 submodule class for encapsulating the metadata required for package set-up. The function returns Value (an output object of multiple potential types).
#' @param x An instance of `ready4fun_manifest`, a ready4 submodule class for encapsulating the metadata required for package set-up.
#' @param type_1L_chr Type (a character vector of length one)
#' @return Value (an output object of multiple potential types)
#' @rdname procure-methods
#' @export 
#' @importFrom ready4 procure
procure.ready4fun_manifest <- function (x, type_1L_chr) 
{
    if (type_1L_chr == "problems") {
        value_xx <- x$problems_ls
    }
    return(value_xx)
}
#' @rdname procure-methods
#' @aliases procure,ready4fun_manifest-method
#' @importFrom ready4 procure
methods::setMethod("procure", methods::className("ready4fun_manifest", package = "ready4fun"), procure.ready4fun_manifest)
