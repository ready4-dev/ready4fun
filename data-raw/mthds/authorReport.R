authorReport.ready4fun_manifest <- function(x,
                                      key_1L_chr = Sys.getenv("DATAVERSE_KEY")){
  write_manuals(pkg_setup_ls = x,
                key_1L_chr = key_1L_chr)
}
