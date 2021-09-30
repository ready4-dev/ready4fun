authorClasses.ready4fun_manifest <- function(x,
                                             dv_url_pfx_1L_chr = NULL,
                                             key_1L_chr = NULL,
                                             self_serve_1L_lgl = F,
                                             self_serve_fn_ls = NULL,
                                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  write_clss(x,
             dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
             key_1L_chr = key_1L_chr,
             self_serve_1L_lgl = self_serve_1L_lgl,
             self_serve_fn_ls = self_serve_fn_ls,
             server_1L_chr = server_1L_chr)
}
