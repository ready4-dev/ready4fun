ratify.ready4fun_manifest <- function(x,
                                      append_1L_lgl = F) {
  x_ready4fun_manifest <- validate_pkg_setup(x, append_1L_lgl = append_1L_lgl, is_method_1L_lgl = T)
  return(x_ready4fun_manifest)
}
