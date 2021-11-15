#' unload packages
#' @description unload_packages() is an Unload function that performs a custom detaching of a package from the search path. Specifically, this function implements an algorithm to unload packages. Function argument package_chr specifies the package(s) to be detached from the search path. The function is called for its side effects and does not return a value.
#' @param package_chr Package (a character vector)
#' @return NULL
#' @rdname unload_packages
#' @export 
#' @importFrom purrr discard map_lgl walk
#' @keywords internal
unload_packages <- function (package_chr) 
{
    if (!package_chr %>% purrr::discard(is.na) %>% identical(character(0))) {
        loaded_pck_chr <- search()
        unload_chr <- package_chr[purrr::map_lgl(package_chr, 
            ~paste0("package:", .x) %in% loaded_pck_chr)]
        if (!unload_chr %>% purrr::discard(is.na) %>% identical(character(0))) 
            purrr::walk(unload_chr, ~unloadNamespace(.x))
    }
}
