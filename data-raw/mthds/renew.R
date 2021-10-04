renew.ready4fun_manifest <- function(x,
                                     type_1L_chr,
<<<<<<< HEAD
                                     key_1L_chr = NULL){
=======
                                     dv_url_pfx_1L_chr = character(0),
                                     key_1L_chr = NULL,
                                     server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
                                     ){
>>>>>>> dev
  if(type_1L_chr == "fns_dmt")
    x_ready4fun_manifest <- add_fns_dmt_tb(pkg_setup_ls = x,
                                         fns_env_ls = NULL,
                                         inc_methods_1L_lgl = T)
  return(x_ready4fun_manifest)
}
