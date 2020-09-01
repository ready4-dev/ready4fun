assert_does_not_match_terms <- function(input_chr,
                                exclude_if_match_chr){
  testit::assert("Not a character vector.", is.character(input_chr))
  purrr::walk(input_chr,
              ~ testit::assert(paste0("One or more values of character vector match exlcuded terms \'",
                                      exclude_if_match_chr,
                                      "\'"),
                               !.x %in% exclude_if_match_chr))
}
