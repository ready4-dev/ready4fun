author.ready4fun_pkg_setup <- function(x,
                                       dv_url_pfx_1L_chr = NULL,
                                       key_1L_chr = NULL,
                                       list_generics_1L_lgl = F,
                                       self_serve_1L_lgl = F,
                                       self_serve_fn_ls = NULL,
                                       server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  inst_ready4fun_pkg_setup <- write_package(x,
                                          dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                          key_1L_chr = key_1L_chr,
                                          list_generics_1L_lgl = list_generics_1L_lgl,
                                          publish_dv_1L_lgl = T,
                                          self_serve_1L_lgl = self_serve_1L_lgl,
                                          self_serve_fn_ls = self_serve_fn_ls,
                                          server_1L_chr = server_1L_chr)
  return(inst_ready4fun_pkg_setup)
}
