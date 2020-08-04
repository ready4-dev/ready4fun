assert_does_not_match_terms <- function(input_chr_vec,
                                exclude_if_match_chr_vec){
  testit::assert("Not a character vector.", is.character(input_chr_vec))
  purrr::walk(input_chr_vec,
              ~ testit::assert(paste0("One or more values of character vector match exlcuded terms \'",
                                      exclude_if_match_chr_vec,
                                      "\'"),
                               !.x %in% exclude_if_match_chr_vec))
}
