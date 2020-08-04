replace_abbr_chr <- function(title_chr,
                             abbreviations_lup = NULL,
                             collapse_lgl = T){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  title_chr <- title_chr %>%
    strsplit(" ") %>%
    purrr::flatten_chr() %>%
    purrr::map_chr(~{
      match_lgl_vec <- .x==abbreviations_lup$short_name_chr
      ifelse(match_lgl_vec %>%
               any(),
             ifelse(.x %in% abbreviations_lup$short_name_chr[match_lgl_vec],
                    get_from_lup_obj(abbreviations_lup,
                                     match_value_xx = ifelse(.x == abbreviations_lup$short_name_chr[match_lgl_vec],
                                                             .x,
                                                             abbreviations_lup$short_name_chr[match_lgl_vec]),
                                     match_var_nm_chr = "short_name_chr",
                                     target_var_nm_chr = "long_name_chr",
                                     evaluate_lgl = F),
                    .x),
             .x)
    })
  if(collapse_lgl)
    title_chr <- title_chr %>% paste0(collapse = " ")
  return(title_chr)
}
