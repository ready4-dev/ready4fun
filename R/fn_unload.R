#' Unload packages
#' @description unload_packages() is an Unload function that performs a custom detaching of a package from the search path. Specifically, this function implements an algorithm to unload packages. Function argument package_chr_vec specifies the package(s) to be detached from the search path. Argument NA provides the package(s) to be detached from the search path. The function is called for its side effects and does not return a value.
#' @param package_chr_vec Package (a character vector)
#' @return NULL
#' @rdname unload_packages
#' @export 
#' @importFrom purrr discard map_lgl walk
unload_packages <- function (package_chr_vec) 
{
    if (!package_chr_vec %>% purrr::discard(is.na) %>% identical(character(0))) {
        loaded_pck_chr_vec <- search()
        unload_chr_vec <- package_chr_vec[purrr::map_lgl(package_chr_vec, 
            ~paste0("package:", .x) %in% loaded_pck_chr_vec)]
        if (!unload_chr_vec %>% purrr::discard(is.na) %>% identical(character(0))) 
            purrr::walk(unload_chr_vec, ~unloadNamespace(.x))
    }
}
