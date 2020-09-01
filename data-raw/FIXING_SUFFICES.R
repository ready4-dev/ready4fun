library(ready4fun)
##
write_new_arg_sfxs_R <- function(arg_nms_chr,
                                 fn_type_1L_chr,
                                 dir_path_chr,
                                 pkg_nm_1L_chr,
                                 inc_fns_idx_dbl = NA_real_){
  if(is.na(inc_fns_idx_dbl))
    inc_fns_idx_dbl <- 1:length(ls(paste0("package:",pkg_nm_1L_chr))[ls(paste0("package:",pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)])
  # Update argument names (within package - same function type)
  purrr::walk(arg_nms_chr[order(nchar(arg_nms_chr), arg_nms_chr,  decreasing=T )] %>% unique(),
              ~ replace_1L_and_indefL_sfxs_R(.x,
                                             file_path_chr = paste0(dir_path_chr,"/",fn_type_1L_chr,".R")))
  # Update function names (within package)
  updated_fns_chr <- ls(paste0("package:",pkg_nm_1L_chr))[ls(paste0("package:",pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)][inc_fns_idx_dbl]
  updated_sfxs_chr <- arg_nms_chr[arg_nms_chr %>% endsWith("_vec")] %>% stringr::str_sub(start=-8) %>% unique()
  fn_nms_to_upd_chr <- updated_fns_chr[updated_fns_chr %>% stringr::str_sub(start=-8) %in% updated_sfxs_chr]
  if(ifelse(identical(fn_nms_to_upd_chr, character(0)),
            F,
            !is.na(fn_nms_to_upd_chr)
            )){
    purrr::walk(fn_nms_to_upd_chr,
                ~ replace_1L_and_indefL_sfxs_R(.x,
                                               dir_path_chr = dir_path_chr))
    # Update function names (outside package)
    normalizePath("../../../")
    purrr::walk(paste0(pkg_nm_1L_chr,"::",fn_nms_to_upd_chr),
                ~ replace_1L_and_indefL_sfxs_R(.x,
                                               dir_path_chr = normalizePath("../../../")))
  }
  fn_args_to_rnm_ls <- purrr::map(updated_fns_chr,
                                    ~ {
                                      fn_args_chr <- get_fn_args_chr(eval(parse(text=.x)))
                                      fn_args_chr[purrr::map_lgl(fn_args_chr, ~ .x %in% c(arg_nms_chr,arg_nms_chr %>% stringr::str_sub(end=-5)))]
                                    }) %>% stats::setNames(updated_fns_chr)

  return(fn_args_to_rnm_ls)
}
make_short_long_nms_vec_chr <- function(long_vecs_chr = character(0),
                                        short_vecs_chr = character(0)){
  short_vecs_chr <- paste0(short_vecs_chr,"_vec")
  if(short_vecs_chr[1]=="_vec"){
    short_vecs_chr <- character(0)
  }
  short_and_long_vec_chr <- c(long_vecs_chr, short_vecs_chr)
  return(short_and_long_vec_chr)
}
# FIRST BIT
# pkg_nm_1L_chr <- "ready4fun"
# fn_type_1L_chr <- "add"
# dir_path_chr <- "data-raw/fns"
# arg_nms_chr <- c("phrase_chr_vec","matching_prefix_chr_vec","n_char_matches_dbl_vec",
#                  "indefinite_item_chr_vec","abbreviated_phrase_chr_vec","phrases_chr_vec","words_chr_vec","expanded_chr_vec",
#                  "indefinite_chr_vec","matches_lgl_vec","no_plural_chr_vec","non_standard_chr_vec","standard_chr_vec",
#                  paste0(c("ignore_phrs_not_in_lup_lgl","plural_lgl","match_chr","long_plural_chr","short_plural_chr"
#                  ),"_vec"))
# add_fn_args_to_rnm_ls <- fn_args_to_rnm_ls
# saveRDS(add_fn_args_to_rnm_ls,"data-raw/add_fn_args_to_rnm_ls.RDS")
#
# SECOND BIT
# assert_fn_args_to_rnm_ls <- write_new_arg_sfxs_R(arg_nms_chr = c("input_chr_vec","exclude_if_match_chr_vec"),
#                                                  fn_type_1L_chr = "assert",
#                                                  dir_path_chr = "data-raw/fns",
#                                                  pkg_nm_1L_chr = "ready4fun")
# saveRDS(assert_fn_args_to_rnm_ls,"data-raw/assert_fn_args_to_rnm_ls.RDS")
# THIRD BIT
# force_fn_args_to_rnm_ls <- write_new_arg_sfxs_R(arg_nms_chr = paste0("package_nm_chr","_vec"),
#                                                  fn_type_1L_chr = "force",
#                                                  dir_path_chr = "data-raw/fns",
#                                                  pkg_nm_1L_chr = "ready4fun")
# saveRDS(force_fn_args_to_rnm_ls,"data-raw/force_fn_args_to_rnm_ls.RDS")
# FOURTH BIT
# get_fn_args_to_rnm_ls <- write_new_arg_sfxs_R(arg_nms_chr = c("fn_args_chr_vec","fns_chr_vec","outp_obj_type_chr_vec",
#                                                                 paste0(c("path_to_pkg_rt_chr","dev_pkg_nm_chr", "path_chr","match_var_nm_chr",
#                                                                          "target_var_nm_chr","fn_name_chr", "package_chr", "last_line_chr", "return_chr"
#                                                                 ),"_vec")),
#                                                  fn_type_1L_chr = "get",
#                                                  dir_path_chr = "data-raw/fns",
#                                                  pkg_nm_1L_chr = "ready4fun")
#saveRDS(get_fn_args_to_rnm_ls,"data-raw/get_fn_args_to_rnm_ls.RDS")
# FIFTH BIT
# fn_type_1L_chr <- "import"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("sheet_names_chr_vec"),
#                                                  short_vecs_chr = c("range_chr","path_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                                                  dir_path_chr = "data-raw/fns",
#                                                  pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# SIXTH BIT
# fn_type_1L_chr <- "make"
# make_fns_chr <- ls(paste0("package:",pkg_nm_1L_chr))[ls(paste0("package:",pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)]
# excl_chr <- c("make_and_doc_generics_tb_R","make_obj_lup_tb", "make_fn_type_lup_tb")
# inc_fns_idx_dbl <- (1:length(make_fns_chr))[make_fns_chr %>% purrr::map_lgl(~!.x %in% excl_chr)]
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("args_chr_vec", "arg_desc_chr_vec","arg_desc_spine_chr",
#                                                                    "exclude_if_match_chr_vec",
#                                                                    "fn_args_chr_vec", "fns_chr_vec", "fn_desc_chr_vec", "fn_desc_spine_chr_vec","fn_nms_chr_vec",
#                                                                    "fn_type_chr_vec", ## MANUAL EDIT NEEDED
#                                                                    "fns_path_chr_vec",
#                                                                    "force_true_chr_vec", "force_false_chr_vec",
#                                                                    "get_set_chr_vec",
#                                                                    "long_name_chr_vec",## MANUAL EDIT NEEDED
#                                                                    "match_chr_vec",
#                                                                    "no_plural_chr_vec",
#                                                                   "pfx_matches_chr_vec",
#                                                                    "suffices_chr_vec",
#                                                                    "short_name_chr_vec", ## MANUAL EDIT NEEDED
#                                                                   "text_elements_chr_vec",
#                                                                    "title_chr_vec", ## MANUAL EDIT NEEDED
#                                                                    "undocumented_fns_dir_chr"),
#                                                  short_vecs_chr = c("append_lgl",
#                                                                     "arg_type_chr", "arg_type_abbr_spine_chr",
#                                                                     "argument_nm_chr",
#                                                                     "class_name_chr",
#                                                                     "details_chr",  ## MANUAL EDIT NEEDED
#                                                                     "doc_in_class_lgl",
#                                                                     "example_lgl",
#                                                                     "export_lgl",
#                                                                     "fn_name_chr","fn_out_type_chr","fn_tags_chr","fn_title_chr",
#                                                                     "fns_chr","fns_dir_chr",
#                                                                     "is_generic_lgl",
#                                                                     "overwrite_lgl",
#                                                                     "pkg_nm_chr",
#                                                                     "ref_slot_chr",
#                                                                     "treat_as_chr",
#                                                                     "url_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun",
#                        inc_fns_idx_dbl = inc_fns_idx_dbl)
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# SEVENTH BIT
# fn_type_1L_chr <- "read"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("fns_path_chr_vec"),
#                                                  short_vecs_chr = c("fns_dir_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                                                  dir_path_chr = "data-raw/fns",
#                                                  pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# # Eighth Bit
# fn_type_1L_chr <- "remove"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("names_chr_vec","nms_chr_vec","output_chr_vec","suffices_chr_vec"),
#                                                  short_vecs_chr = c("name_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# Ninth Bit
# fn_type_1L_chr <- "replace"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("args_nm_chr_vec","replacements_chr_vec","sfxs_chr_vec"),
#                                                  short_vecs_chr = c("dir_path_chr","file_path_chr","indefL_arg_nm_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# #Tenth Bit
# fn_type_1L_chr <- "rowbind"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(#long_vecs_chr = c(),
#                                                  short_vecs_chr = c("r4_name_chr","slot_nm_chr")) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# # Eleventh Bit
# fn_type_1L_chr <- "unload"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("loaded_pck_chr_vec","package_chr_vec","unload_chr_vec")#,
#   #short_vecs_chr = c()
#   ) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
# Twelvth Bit
# fn_type_1L_chr <- "update"
# fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("arg_desc_chr_vec",
#                                                                    "arg_ls_to_upd_lgl_vec",
#                                                                    "chr_vars_to_upd_lgl_vec",
#                                                                    "data_chr_vec",
#                                                                    "fn_args_chr_vec",
#                                                                    "import_chr_vec",
#                                                                    "lgl_vars_to_upd_lgl_vec",
#                                                                    "long_name_chr_vec", # MANAUAL ADDITIONS REQUIRED
#                                                                    "old_args_chr_vec",
#                                                                    "no_plural_chr_vec","pfx_rgx_chr",
#                                                                    "short_name_chr_vec","slots_chr_vec",
#                                                                    "variable_chr_vec"),
#                                                  short_vecs_chr = c("append_lgl","fn_dmt_chr","fn_name_chr","fn_nm_chr","fn_type_chr","package_chr","phrase_chr",
#                                                                     "idx_dbl")
# ) %>%
#   write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
#                        dir_path_chr = "data-raw/fns",
#                        pkg_nm_1L_chr = "ready4fun")
# saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
fn_type_1L_chr <- "write"
fn_args_to_rnm_ls <- make_short_long_nms_vec_chr(long_vecs_chr = c("dev_pkgs_chr_vec","files_chr_vec","file_nms_chr_vec",
                                                                   "import_chr_vec","keep_auto_nms_chr_vec","packages_chr_vec",
                                                                   "top_level_chr_vec"),
                                                 short_vecs_chr = c("db_chr","desc_chr","dev_pkg_nm_chr","dest_path_chr","doc_in_class_lgl",
                                                                    "example_lgl","export_lgl",
                                                                    "fn_title_chr","fn_type_chr","fn_name_chr","fn_desc_chr","fn_out_type_chr",
                                                                    "format_chr",
                                                                    "incr_ver_lgl",
                                                                    "lup_dir_chr",
                                                                    "make_tmpl_vignette_lgl","make_pdfs_lgl","overwrite_lgl",
                                                                    "package_chr","path_chr",
                                                                    "path_to_dvpr_dmt_dir_chr","path_to_pkg_rt_chr","path_to_user_dmt_dir_chr","pfx_chr",
                                                                    "r4_name_chr",
                                                                    "r_dir_chr","R_dir_chr",
                                                                    "slot_nm_chr",
                                                                    "temp_path_chr","title_chr","tmp_fn_dir_chr",
                                                                    "url_chr")
) %>%
  write_new_arg_sfxs_R(fn_type_1L_chr = fn_type_1L_chr,
                       dir_path_chr = "data-raw/fns",
                       pkg_nm_1L_chr = "ready4fun")
saveRDS(fn_args_to_rnm_ls, paste0("data-raw/",fn_type_1L_chr,"_fn_args_to_rnm_ls.RDS"))
## Irregular changes for reset.R

# Update argument names (within package - for other function types)
# Update argument names (outside package)
# Do search for ready4fun:: (updated_fns_chr)


