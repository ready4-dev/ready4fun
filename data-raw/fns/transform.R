transform_cls_type_ls <- function(cls_type_ls){
  lifecycle::deprecate_soft("0.0.0.9467",
                            what = "ready4fun::transform_cls_type_ls()",
                            with = "ready4::transform_cls_type_ls()")
  max_lngth_1L_int <- purrr::map_int(cls_type_ls,
                                     ~ length(.x)) %>%
    max()
  tfmd_cls_type_ls <- cls_type_ls %>% purrr::map(~{
    c(.x,rep(NA_character_,max_lngth_1L_int - length(.x)))
  })
  return(tfmd_cls_type_ls)
}
