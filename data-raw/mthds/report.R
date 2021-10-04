report.ready4fun_manifest <- function(x,
                                      dv_url_pfx_1L_chr = character(0),
                                      key_1L_chr = NULL,
                                      server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  write_manuals(pkg_setup_ls = x,
                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                key_1L_chr = key_1L_chr,
                server_1L_chr = server_1L_chr)
}
