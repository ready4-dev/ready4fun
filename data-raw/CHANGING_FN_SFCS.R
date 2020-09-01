tb <- dplyr::select(fns_dmt_tb,fns_chr) %>%
  dplyr::mutate(new_nm = fns_chr %>% remove_obj_type_from_nm_chr()) %>%
  dplyr::mutate(duplicated_lgl = duplicated(new_nm))
# unchanged_tb <- tb %>% dplyr::filter(duplicated_lgl) # Don't change this
# tb <- tb %>%
#   dplyr::filter(!duplicated_lgl)
tb <- tb %>% dplyr::mutate(new_nm = dplyr::case_when(startsWith(new_nm,"write") & endsWith(new_nm,"_R") ~ new_nm %>% stringr::str_sub(end=-3),
                                                     duplicated_lgl ~ fns_chr,
                                                     new_nm == "assert_does_not_match_terms" ~ "assert_inp_does_not_match_terms",
                                                     new_nm == "force_req_pkg_install" ~ "force_instl_of_reqd_pkg",
                                                     new_nm == "make_and_doc_fn_type_R" ~ "write_dmtd_fn_type_lup",
                                                     new_nm == "make_all_fns_dmt" ~ "make_dmt_for_all_fns",
                                                     new_nm == "make_fns" ~ "make_fn_nms",
                                                     new_nm == "make_fns_type" ~ "make_fn_types",
                                                     new_nm == "replace_1L_and_indefL_sfxs_R" ~	"write_to_rpl_1L_and_indefL_sfcs",
                                                     new_nm == "replace_sfx_pair_R" ~ "write_to_replace_sfx_pair",
                                                     new_nm == "reset_pkg_files_R" ~ "write_to_reset_pkg_files",
                                                     new_nm == "write_pt_lup_db" ~ "write_pt_lup",
                                                     TRUE ~ new_nm))
tb <- tb %>% dplyr::mutate(duplicated_lgl = duplicated(new_nm))
any(tb$duplicated_lgl)



#make_and_doc_generics_tb_R # write_dmtd_fn_type_lup
