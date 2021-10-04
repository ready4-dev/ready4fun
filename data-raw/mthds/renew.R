renew.ready4fun_manifest <- function(x,
                                     type_1L_chr,
                                     dv_url_pfx_1L_chr = character(0),
                                     key_1L_chr = NULL,
                                     server_1L_chr = Sys.getenv("DATAVERSE_SERVER")
                                     ){
  if(type_1L_chr == "fns_dmt")
    x_ready4fun_manifest <- add_fns_dmt_tb(pkg_setup_ls = x,
                                         dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                         fns_env_ls = NULL,
                                         inc_methods_1L_lgl = T,
                                         key_1L_chr = key_1L_chr,
                                         server_1L_chr = server_1L_chr)
  return(x_ready4fun_manifest)
}
