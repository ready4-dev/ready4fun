unload_packages <- function(package_chr_vec){
  if(!package_chr_vec %>% purrr::discard(is.na) %>% identical(character(0))){
    loaded_pck_chr_vec <- search()
    unload_chr_vec <- package_chr_vec[purrr::map_lgl(package_chr_vec,
                                                     ~ paste0("package:",.x) %in% loaded_pck_chr_vec)]
    if(!unload_chr_vec %>% purrr::discard(is.na) %>% identical(character(0)))
      purrr::walk(unload_chr_vec, ~ unloadNamespace(.x))
  }
}
