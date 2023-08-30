read_fns <- function(fns_dir_1L_chr = "data-raw/fns/",
                     fns_env = NULL,
                     use_env_1L_lgl = T) {
  fns_path_chr <- list.files(fns_dir_1L_chr,
    pattern = "\\.R$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (use_env_1L_lgl) {
    if (is.null(fns_env)) {
      fns_env <- new.env(parent = globalenv())
    }
    fns_path_chr %>%
      purrr::walk(~ sys.source(file = .x, envir = fns_env, toplevel.env = fns_env)) # source(.x)
    fns_env_ls <- list(
      fns_env = fns_env,
      fns_path_chr = fns_path_chr
    )
  } else {
    fns_path_chr %>%
      purrr::walk(~ source(.x))
    fns_env_ls <- NULL
  }
  return(fns_env_ls) # fns_path_chr
}
