#' Read functions
#' @description read_fns() is a Read function that reads an R script into memory. Specifically, this function implements an algorithm to read functions. Function argument fns_dir_1L_chr specifies the path to object. The function returns Functions (a list of environments).
#' @param fns_dir_1L_chr Functions directory (a character vector of length one), Default: 'data-raw/fns/'
#' @param fns_env Functions (an environment), Default: NULL
#' @param use_env_1L_lgl Use environment (a logical vector of length one), Default: T
#' @return Functions (a list of environments)
#' @rdname read_fns
#' @export 
#' @importFrom purrr walk
#' @keywords internal
read_fns <- function (fns_dir_1L_chr = "data-raw/fns/", fns_env = NULL, use_env_1L_lgl = T) 
{
    fns_path_chr <- list.files(fns_dir_1L_chr, pattern = "*.R$", 
        full.names = TRUE, ignore.case = TRUE)
    if (use_env_1L_lgl) {
        if (is.null(fns_env)) 
            fns_env <- new.env(parent = globalenv())
        fns_path_chr %>% purrr::walk(~sys.source(file = .x, envir = fns_env, 
            toplevel.env = fns_env))
        fns_env_ls <- list(fns_env = fns_env, fns_path_chr = fns_path_chr)
    }
    else {
        fns_path_chr %>% purrr::walk(~source(.x))
        fns_env_ls <- NULL
    }
    return(fns_env_ls)
}
