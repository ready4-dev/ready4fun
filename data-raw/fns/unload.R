unload_packages <- function(package_chr) {
  if (!package_chr %>%
    purrr::discard(is.na) %>%
    identical(character(0))) {
    loaded_pck_chr <- search()
    unload_chr <- package_chr[purrr::map_lgl(
      package_chr,
      ~ paste0("package:", .x) %in% loaded_pck_chr
    )]
    if (!unload_chr %>%
      purrr::discard(is.na) %>%
      identical(character(0))) {
      purrr::walk(unload_chr, ~ unloadNamespace(.x))
    }
  }
}
