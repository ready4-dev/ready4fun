rowbind_all_tbs_in_r4_obj <- function(tbs_r4,
                                         second_tbs_r4,
                                         r4_name_1L_chr){
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::rowbind_all_tbs_in_r4_obj()", "ready4::rowbind_all_tbs_in_r4_obj()")
  tbs_r4 <- purrr::reduce(methods::getSlots(r4_name_1L_chr) %>% names(),
                          .init = tbs_r4,
                          ~ rowbind_tbs_in_r4_obj(tbs_r4 = .x,
                                                     slot_nm_1L_chr = .y,
                                                     second_tbs_r4 = second_tbs_r4,
                                                  r4_name_1L_chr = r4_name_1L_chr
                          ))
  return(tbs_r4)
}
rowbind_tbs_in_r4_obj <- function(tbs_r4,
                                  slot_nm_1L_chr,
                                  second_tbs_r4,
                                  r4_name_1L_chr){
  lifecycle::deprecate_soft("0.0.0.9446", "ready4fun::rowbind_tbs_in_r4_obj()", "ready4::rowbind_tbs_in_r4_obj()")
  if(tibble::is_tibble(methods::slot(tbs_r4,slot_nm_1L_chr))){
    slot(tbs_r4,slot_nm_1L_chr) <- rbind(methods::slot(tbs_r4,slot_nm_1L_chr),
                                         methods::slot(second_tbs_r4,slot_nm_1L_chr))
  }
  return(tbs_r4)
}
