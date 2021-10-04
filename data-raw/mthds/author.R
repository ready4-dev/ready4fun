author.ready4fun_manifest <- function(x,
                                      key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                      list_generics_1L_lgl = F,
                                      self_serve_1L_lgl = F,
                                      self_serve_fn_ls = NULL){
  x <- ratify(x)
  if(!is.null(x$problems_ls)){
    message("Execution halted - fix issues with manifest before making a new call to author.")
  }else{
    message("Manifest has been validated. Proceeding to package set-up.")
    author(x$initial_ls)
    authorData(x)
    authorClasses(x,
                  key_1L_chr = key_1L_chr,
                  self_serve_1L_lgl = self_serve_1L_lgl,
                  self_serve_fn_ls = self_serve_fn_ls)
    x <- renew(x,
               type_1L_chr = "fns_dmt",
               key_1L_chr = key_1L_chr)
    authorFunctions(x,
                    list_generics_1L_lgl = list_generics_1L_lgl)
    report(x,
           key_1L_chr = key_1L_chr)
}
  return(x)
}
author.ready4fun_metadata_a <- function(x){
  rlang::exec(write_pkg_setup_fls, !!!x)
}
