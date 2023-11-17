#' Assert input does not match terms
#' @description assert_inp_does_not_match_terms() is an Assert function that validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided Specifically, this function implements an algorithm to assert input does not match terms. The function is called for its side effects and does not return a value.
#' @param input_chr Input (a character vector)
#' @param exclude_if_match_chr Exclude if match (a character vector)
#' @return NULL
#' @rdname assert_inp_does_not_match_terms
#' @export 
#' @importFrom testit assert
#' @importFrom purrr walk
#' @keywords internal
assert_inp_does_not_match_terms <- function (input_chr, exclude_if_match_chr) 
{
    testit::assert("Not a character vector.", is.character(input_chr))
    purrr::walk(input_chr, ~testit::assert(paste0("One or more values of character vector match exlcuded terms '", 
        exclude_if_match_chr, "'"), !.x %in% exclude_if_match_chr))
}
