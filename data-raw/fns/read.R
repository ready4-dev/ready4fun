read_fns <- function(fns_dir_chr = "data-raw/fns/"){
  fns_path_chr_vec <- list.files(fns_dir_chr,
                                 pattern="*.R$",
                                 full.names=TRUE,
                                 ignore.case=TRUE) %>%
    purrr::walk(~source(.x))
  return(fns_path_chr_vec)
}
