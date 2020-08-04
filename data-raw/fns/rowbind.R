rowbind_all_tbs_in_r4_obj_r4 <- function(tbs_r4, ## Move to ready4class or ready4use
                                     second_tbs_r4,
                                     r4_name_chr){
  tbs_r4 <- purrr::reduce(methods::getSlots(r4_name_chr) %>% names(),
                          .init = tbs_r4,
                          ~ rowbind_tbs_in_r4_obj_r4(tbs_r4 = .x,
                                              slot_nm_chr = .y,
                                              second_tbs_r4 = second_tbs_r4,
                                              r4_name_chr = r4_name_chr
                          ))
  return(tbs_r4)
}
rowbind_tbs_in_r4_obj_r4 <- function(tbs_r4,
                              slot_nm_chr,
                              second_tbs_r4,
                              r4_name_chr){
  if(tibble::is_tibble(methods::slot(tbs_r4,slot_nm_chr))){
    slot(tbs_r4,slot_nm_chr) <- rbind(methods::slot(tbs_r4,slot_nm_chr),
                                      methods::slot(second_tbs_r4,slot_nm_chr))
  }
  return(tbs_r4)
}
