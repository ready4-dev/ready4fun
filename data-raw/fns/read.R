read_fns <- function(fns_dir_1L_chr = "data-raw/fns/"){
  fns_path_chr <- list.files(fns_dir_1L_chr,
                                 pattern="*.R$",
                                 full.names=TRUE,
                                 ignore.case=TRUE) %>%
    purrr::walk(~source(.x))
  return(fns_path_chr)
}
