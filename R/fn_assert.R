#' Assert does not match terms
#' @description assert_does_not_match_terms() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided. Specifically, this function implements an algorithm to assert does not match terms. Function argument input_chr_vec specifies the object on which assert validation checks are to be performed. Argument exclude_if_match_chr_vec provides the object containing values used for validation tests.NA
#' @param input_chr_vec Input (a character vector)
#' @param exclude_if_match_chr_vec Exclude if match (a character vector)
#' @return NULL
#' @rdname assert_does_not_match_terms
#' @export 
#' @importFrom testit assert
#' @importFrom purrr walk
#' @keywords internal
assert_does_not_match_terms <- function (input_chr_vec, exclude_if_match_chr_vec) 
{
    testit::assert("Not a character vector.", is.character(input_chr_vec))
    purrr::walk(input_chr_vec, ~testit::assert(paste0("One or more values of character vector match exlcuded terms '", 
        exclude_if_match_chr_vec, "'"), !.x %in% exclude_if_match_chr_vec))
}
