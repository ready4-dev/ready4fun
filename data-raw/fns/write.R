write_abbr_lup <- function(seed_lup = NULL,
                           short_name_chr = NA_character_,
                           long_name_chr = NA_character_,
                           no_plural_chr = NA_character_,
                           custom_plural_ls = NULL,
                           overwrite_1L_lgl = T,
                           object_type_lup = NULL,
                           pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0),
                                                       title_chr = character(0),
                                                       desc_chr = character(0),
                                                       url_chr = character(0)),
                           pkg_nm_1L_chr = get_dev_pkg_nm(),
                           dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                           dv_url_pfx_1L_chr = NULL,
                           key_1L_chr = NULL,
                           server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                           url_1L_chr = deprecated()){
  if (lifecycle::is_present(url_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9323",
                              "ready4fun::write_abbr_lup(url_1L_chr)",
                              details = "Please use `ready4fun::write_abbr_lup(dv_ds_nm_1L_chr)` instead.")
  }
  if(is.null(seed_lup)){
    seed_lup <- get_rds_from_dv("object_type_lup",
                                dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                key_1L_chr = key_1L_chr,
                                server_1L_chr = server_1L_chr)
  }
  if(is.null(object_type_lup)){ # Was seed_lup
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  }
  pkg_dss_tb <- update_abbr_lup(seed_lup,
                  short_name_chr = short_name_chr,
                  long_name_chr = long_name_chr,
                  no_plural_chr = no_plural_chr,
                  custom_plural_ls = custom_plural_ls) %>%
    write_and_doc_ds(db_df = .,
                     overwrite_1L_lgl = overwrite_1L_lgl,
                     db_1L_chr = "abbreviations_lup",
                     title_1L_chr = "Common abbreviations lookup table",
                     desc_1L_chr = paste0("A lookup table for abbreviations commonly used in object names in the ",pkg_nm_1L_chr,"package."),
                     format_1L_chr = "A tibble",
                     url_1L_chr = dv_ds_nm_1L_chr,
                     abbreviations_lup = .,
                     object_type_lup = object_type_lup,
                     pkg_dss_tb = pkg_dss_tb,
                     dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                     dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                     key_1L_chr = key_1L_chr,
                     server_1L_chr = server_1L_chr)
  return(pkg_dss_tb)
}
write_all_tbs_in_tbs_r4_to_csvs <- function(tbs_r4,
                                            r4_name_1L_chr,
                                            lup_dir_1L_chr,
                                            pfx_1L_chr){
  purrr::walk(methods::getSlots(r4_name_1L_chr) %>% names(),
              ~ write_tb_to_csv(tbs_r4 = tbs_r4,
                                slot_nm_1L_chr = .x,
                                r4_name_1L_chr = r4_name_1L_chr,
                                lup_dir_1L_chr = lup_dir_1L_chr,
                                pfx_1L_chr = pfx_1L_chr))
}
write_and_doc_ds <- function(db_df,
                             overwrite_1L_lgl = T,
                             db_1L_chr,
                             title_1L_chr,
                             desc_1L_chr,
                             format_1L_chr = "A tibble",
                             url_1L_chr = NA_character_,
                             vars_ls = NULL,
                             R_dir_1L_chr = "R",
                             simple_lup_1L_lgl = F,
                             abbreviations_lup = NULL,
                             object_type_lup = NULL,
                             pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0),
                                                         title_chr = character(0),
                                                         desc_chr = character(0),
                                                         url_chr = character(0)),
                             dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                             dv_url_pfx_1L_chr = NULL,
                             key_1L_chr = NULL,
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  eval(parse(text=paste0(db_1L_chr,"<-db_df")))
  eval(parse(text=paste0("usethis::use_data(",
                         db_1L_chr,
                         ", overwrite = overwrite_1L_lgl)")))
  sink(paste0(R_dir_1L_chr,"/db_",db_1L_chr,".R"), append = F)
  write_ds_dmt(db_df = db_df,
               db_1L_chr = db_1L_chr,
               title_1L_chr = title_1L_chr,
               desc_1L_chr = desc_1L_chr,
               format_1L_chr = format_1L_chr,
               vars_ls = vars_ls,
               url_1L_chr = url_1L_chr,
               R_dir_1L_chr = R_dir_1L_chr,
               simple_lup_1L_lgl = simple_lup_1L_lgl,
               abbreviations_lup = abbreviations_lup,
               object_type_lup = object_type_lup,
               dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
               dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
               key_1L_chr = key_1L_chr,
               server_1L_chr = server_1L_chr)
  close_open_sinks()
  devtools::document()
  devtools::load_all()
  pkg_dss_tb <- tibble::add_case(pkg_dss_tb,
                                 ds_obj_nm_chr = db_1L_chr,
                                 title_chr = title_1L_chr,
                                 desc_chr = desc_1L_chr,
                                 url_chr = url_1L_chr)
  return(pkg_dss_tb)
}
write_and_doc_fn_fls <- function(fns_dmt_tb,
                                 pkg_setup_ls,
                                 make_pdfs_1L_lgl = T,
                                 update_pkgdown_1L_lgl = T,
                                 path_to_dmt_dir_1L_chr = deprecated(),##
                                 dev_pkgs_chr = deprecated(),
                                 path_to_dvpr_dmt_dir_1L_chr = deprecated(),
                                 path_to_pkg_rt_1L_chr = deprecated(),
                                 path_to_user_dmt_dir_1L_chr = deprecated(),
                                 r_dir_1L_chr = deprecated()
                                 ){
  if (lifecycle::is_present(path_to_dvpr_dmt_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9307",
                              "ready4fun::write_and_doc_fn_fls(path_to_dvpr_dmt_dir_1L_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` to specify the directory to which both 'Developer' and 'User' documentation sub-directories will be written.")
  }
  if (lifecycle::is_present(path_to_user_dmt_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9307",
                              "ready4fun::write_and_doc_fn_fls(path_to_user_dmt_dir_1L_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` to specify the directory to which both 'Developer' and 'User' documentation sub-directories will be written.")
  }
  if (lifecycle::is_present(dev_pkgs_chr)) {
    lifecycle::deprecate_warn("0.0.0.9327",
                              "ready4fun::write_and_doc_fn_fls(dev_pkgs_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` instead.")
  }
  if (lifecycle::is_present(r_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9327",
                              "ready4fun::write_and_doc_fn_fls(r_dir_1L_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` instead.")
  }
  if (lifecycle::is_present(path_to_pkg_rt_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9327",
                              "ready4fun::write_and_doc_fn_fls(path_to_pkg_rt_1L_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` instead.")
  }
  if (lifecycle::is_present(path_to_dmt_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_and_doc_fn_fls(path_to_dmt_dir_1L_chr)",
                              details = "Please use `ready4fun::write_and_doc_fn_fls(pkg_setup_ls)` to pass the path_to_dmt_dir_1L_chr object to this function.")
  }
  add_build_ignore(pkg_setup_ls$subsequent_ls$build_ignore_ls)
  add_addl_pkgs(pkg_setup_ls$subsequent_ls$addl_pkgs_ls)
  dev_pkgs_chr <- pkg_setup_ls$subsequent_ls$dev_pkgs_chr
  r_dir_1L_chr <- paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R")
  write_new_dirs(c(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,
                   paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/Developer"),
                   paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/User")))
  fns_env_ls <- read_fns(make_undmtd_fns_dir_chr(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,
                                                        "/data-raw"),
                                                 drop_empty_1L_lgl = T))
  purrr::walk2(list(paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/Developer"),
                    paste0(pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,"/User")),
               c(T,F),
               ~ {
                 write_new_files(paths_chr = paste0(r_dir_1L_chr,
                                                    "/",
                                                    fns_dmt_tb$file_pfx_chr[1],
                                                    fns_dmt_tb$file_nm_chr %>%
                                                      unique()),
                                 custom_write_ls = list(fn = write_fn_fl,
                                                        args_ls = list(fns_dmt_tb = fns_dmt_tb,
                                                                       fns_env_ls = fns_env_ls,
                                                                       r_dir_1L_chr = r_dir_1L_chr,
                                                                       document_unexp_lgl = .y)))
                 devtools::document()
                 devtools::load_all()
                 write_ns_imps_to_desc(dev_pkgs_chr = dev_pkgs_chr,
                                       incr_ver_1L_lgl = .y)
                 devtools::load_all()
                 if(make_pdfs_1L_lgl)
                 devtools::build_manual(path = .x)
               })
  if(update_pkgdown_1L_lgl){
    datasets_chr <- utils::data(package = get_dev_pkg_nm(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr),
                                envir = environment())$results[,3]
    writeLines(c("development:",
                 "  mode: auto",
                 "reference:",
                 "- title: \"Datasets\"",
                 "- contents:",
                 paste0("  - ",datasets_chr),
                 {
                   if("prototype_lup" %in% datasets_chr){
                     utils::data("prototype_lup",package=get_dev_pkg_nm(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr), envir = environment())
                     fns_chr <- prototype_lup %>% dplyr::filter(pt_ns_chr == get_dev_pkg_nm(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr)) %>%
                       dplyr::pull(fn_to_call_chr)
                     if(length(fns_chr)>0){
                       c( paste0("- title: \"","Classes","\""),
                                      "- contents:",
                                      paste0("  - ",fns_chr))
                   }
                   }
                 },
                 purrr::map2(c("fn_","grp_","mthd_"),c("Functions","Generics","Methods"),
                             ~{
                   fns_chr <- dplyr::filter(fns_dmt_tb, inc_for_main_user_lgl & file_pfx_chr == .x) %>%
                     dplyr::pull(fns_chr)
                   if(length(fns_chr)>0){
                    txt_chr  <- c( paste0("- title: \"",.y,"\""),
                        "- contents:",
                        paste0("  - ",fns_chr))
                   }else{
                     txt_chr  <- ""
                   }
                 }) %>% purrr::flatten_chr() %>% purrr::discard(~.x=="")),
               con = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/_pkgdown.yml"))
  }
}
write_clss <- function(dss_records_ls,
                       pkg_setup_ls,
                       dv_url_pfx_1L_chr = NULL,
                       key_1L_chr = NULL,
                       self_serve_1L_lgl = F,
                       server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                       cls_fn_ls = deprecated()){
  if (lifecycle::is_present(cls_fn_ls)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_clss(cls_fn_ls)",
                              details = "Please use `ready4fun::write_clss(pkg_desc_ls)` to pass the cls_fn_ls object to this function.")
  }
  if(self_serve_1L_lgl){
    fns_env_ls <- read_fns(make_undmtd_fns_dir_chr(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,
                                                          "/data-raw"),
                                                   drop_empty_1L_lgl = T))
    write_new_files(paths_chr = paste0(paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                                       "/",
                                       dss_records_ls$fns_dmt_tb$file_pfx_chr,
                                       dss_records_ls$fns_dmt_tb$file_nm_chr) %>%
                      unique(),
                    custom_write_ls = list(fn = write_fn_fl,
                                           args_ls = list(fns_dmt_tb = dss_records_ls$fns_dmt_tb,
                                                          fns_env_ls = fns_env_ls,
                                                          r_dir_1L_chr = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                                                          document_unexp_lgl = F)))
    devtools::document()
    devtools::load_all()
  }
  if(!is.null(pkg_setup_ls$subsequent_ls$cls_fn_ls)){
    if("dev_pkg_ns_1L_chr" %in% formalArgs(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn) & ! "dev_pkg_ns_1L_chr" %in% names(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls))
      pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$dev_pkg_ns_1L_chr <- pkg_setup_ls$initial_ls$pkg_desc_ls$Package
    if("name_pfx_1L_chr" %in% formalArgs(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn) & ! "name_pfx_1L_chr" %in% names(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls))
      pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$name_pfx_1L_chr <- paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package,"_")
    if("output_dir_1L_chr" %in% formalArgs(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn) & ! "output_dir_1L_chr" %in% names(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls))
      pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$output_dir_1L_chr <- paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R")
    if("abbreviations_lup" %in% formalArgs(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn) & ! "abbreviations_lup" %in% names(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls))
      pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup
    if("object_type_lup" %in% formalArgs(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn) & ! "object_type_lup" %in% names(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls))
      pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$object_type_lup <- pkg_setup_ls$subsequent_ls$object_type_lup
    prototype_lup <- rlang::exec(pkg_setup_ls$subsequent_ls$cls_fn_ls$fn,
                                 !!!pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls) #%>%
    if(!identical(pkg_setup_ls$subsequent_ls$prototype_lup, prototype_lup)){
      write_env_objs_to_dv(list(prototype_lup = prototype_lup),
                           descriptions_chr = "Class prototype lookup table",
                           ds_url_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2],
                           publish_dv_1L_lgl = T)
    }
    # args_ls <-  make_pkg_ds_ls(prototype_lup, db_1L_chr = "prototype_lup",
    #                  abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
    #                  object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
    #                  title_1L_chr = "Class prototype lookup table",
    #                  desc_1L_chr = "Metadata on classes used in ready4 suite")
    # args_ls <- append(args_ls,
    #                   list(overwrite_1L_lgl = T,
    #                        pkg_dss_tb = dss_records_ls$pkg_dss_tb,
    #                        R_dir_1L_chr = "R",
    #                        dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2],
    #                        dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
    #                        key_1L_chr = key_1L_chr,
    #                        server_1L_chr = server_1L_chr))
    # dss_records_ls$pkg_dss_tb <- rlang::exec(write_and_doc_ds,!!!args_ls)
  }
}
write_dmtd_fn_type_lup <- function(fn_types_lup = make_fn_type_lup(),
                                   overwrite_1L_lgl = T,
                                   pkg_nm_1L_chr = get_dev_pkg_nm(),
                                   url_1L_chr = deprecated(),
                                   abbreviations_lup = NULL,
                                   object_type_lup = NULL,
                                   pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0),
                                                               title_chr = character(0),
                                                               desc_chr = character(0),
                                                               url_chr = character(0)),
                                   dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                                   dv_url_pfx_1L_chr = NULL,
                                   key_1L_chr = NULL,
                                   server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if (lifecycle::is_present(url_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9323",
                              "ready4fun::write_dmtd_fn_type_lup(url_1L_chr)",
                              details = "Please use `ready4fun::write_dmtd_fn_type_lup(dv_ds_nm_1L_chr)` instead.")
  }
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup", # Replace with get_rds_from_dv ?
                package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  fn_types_lup %>%
    write_and_doc_ds(overwrite_1L_lgl = overwrite_1L_lgl,
                     db_1L_chr = "fn_types_lup",
                     title_1L_chr = "Function type lookup table",
                     desc_1L_chr = paste0("A lookup table to find descriptions for different types of functions used within the ",pkg_nm_1L_chr," package suite."),
                     format_1L_chr = "A tibble",
                     url_1L_chr = dv_ds_nm_1L_chr,
                     abbreviations_lup = abbreviations_lup,
                     object_type_lup = object_type_lup,
                     pkg_dss_tb = pkg_dss_tb,
                     dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                     dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                     key_1L_chr = key_1L_chr,
                     server_1L_chr = server_1L_chr)
}
write_documented_fns <- function(tmp_fn_dir_1L_chr,
                                   R_dir_1L_chr){
  sinew::makeOxyFile(tmp_fn_dir_1L_chr,
                     verbose = F)
  files_chr <- list.files(tmp_fn_dir_1L_chr) %>%
    purrr::map_chr(~{
      ifelse(startsWith(.x,"oxy-"),.x,NA_character_)
    }) %>% purrr::discard(is.na)
  purrr::walk(files_chr,
              ~{
                target_chr <- paste0(R_dir_1L_chr,"/fn_",.x %>% stringr::str_sub(start=5))
                original_chr <- paste0(tmp_fn_dir_1L_chr,"/",.x)
                if(file.exists(target_chr))
                  file.remove(target_chr)
                file.copy(original_chr,
                          target_chr)
              })
  do.call(file.remove, list(paste0(tmp_fn_dir_1L_chr,"/",files_chr)))
}
write_ds_dmt <- function(db_df,
                         db_1L_chr,
                         title_1L_chr,
                         desc_1L_chr,
                         format_1L_chr = "A tibble",
                         url_1L_chr = NA_character_,
                         vars_ls = NULL,
                         R_dir_1L_chr = "R",
                         simple_lup_1L_lgl = F,
                         abbreviations_lup = NULL,
                         object_type_lup = NULL,
                         dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                         dv_url_pfx_1L_chr = NULL,
                         key_1L_chr = NULL,
                         server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(abbreviations_lup))
    utils::data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  auto_vars_ls <- names(db_df) %>%
    purrr::map(~ ifelse(simple_lup_1L_lgl,
                        get_from_lup_obj(abbreviations_lup,
                                         target_var_nm_1L_chr = "long_name_chr",
                                         match_var_nm_1L_chr = "short_name_chr",
                                         match_value_xx = .x,
                                         evaluate_lgl = F),
                        make_arg_desc(.x,
                                      object_type_lup = object_type_lup,
                                      abbreviations_lup = abbreviations_lup,
                                      dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                      dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                      key_1L_chr = key_1L_chr,
                                      server_1L_chr = server_1L_chr))) %>%
    stats::setNames(names(db_df))
  if(is.null(vars_ls)){
    vars_ls <- auto_vars_ls
  }else{
    keep_auto_nms_chr <- setdiff(names(auto_vars_ls),names(vars_ls))
    vars_ls <- auto_vars_ls %>%
      purrr::map2(names(auto_vars_ls),
                 ~{
                   if(.y %in% keep_auto_nms_chr){
                     .x
                   }else{
                     vars_ls %>% purrr::pluck(.y)
                   }
                 })
  }
  writeLines(paste0("#' ",title_1L_chr,"\n",
                    "#' \n",
                    "#' ",desc_1L_chr,"\n",
                    "#' \n",
                    "#' ",format_1L_chr,"\n",
                    "#' \n",
                    paste0("#' \\describe{\n",
                                  purrr::map2_chr(vars_ls,
                                                  names(vars_ls),
                                                  ~ paste0("#'   \\item{",.y,"}{",.x,"}")) %>%
                                    paste0(collapse = "\n"),
                                  "\n#' }\n"),
                    ifelse(is.na(url_1L_chr),
                           "",
                           paste0("#' @source \\url{",url_1L_chr,"}\n")),
                    "\"",db_1L_chr,"\""))
}
write_env_objs_to_dv <- function(env_objects_ls,
                                 descriptions_chr,
                                 ds_url_1L_chr,
                                 key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                 publish_dv_1L_lgl = F,
                                 server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  tmp_dir <- tempdir()
  paths_chr <- env_objects_ls %>%
    purrr::map2_chr(names(env_objects_ls),
                    ~{
                      path_1L_chr <- paste0(tmp_dir,"/",.y,".RDS")
                      saveRDS(object = .x,
                              file = path_1L_chr)
                      path_1L_chr
                    })
  file_ids_int <- write_fls_to_dv(paths_chr,
                                  descriptions_chr = descriptions_chr,
                                  ds_url_1L_chr = ds_url_1L_chr,
                                  ds_ls = dataverse::get_dataset(ds_url_1L_chr),
                                  key_1L_chr = key_1L_chr,
                                  server_1L_chr = server_1L_chr)
  do.call(file.remove, list(paths_chr))
  unlink(tmp_dir)
  if(publish_dv_1L_lgl){
    write_to_publish_dv_ds(dv_ds_1L_chr = ds_url_1L_chr)
  }
  return(file_ids_int)
}
write_fls_to_dv <- function(file_paths_chr,
                            descriptions_chr = NULL,
                            ds_url_1L_chr,
                            ds_ls = NULL,
                            key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(!identical(file_paths_chr, character(0))){
    message(paste0("Are you sure that you want to upload the following file",
                   ifelse(length(file_paths_chr)>1,"s",""),
                   " to dataverse ",
                   ds_url_1L_chr,
                   ": \n",
                   file_paths_chr %>%
                     purrr::map_chr(~fs::path_file(.x)) %>%
                     paste0(collapse = "\n"),
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Type 'Y' to confirm that you want to upload ",
                                                       ifelse(length(file_paths_chr)>1,
                                                              "these files:",
                                                              "this file:")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr == "Y"){
      if(is.null(ds_ls))
        ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
      is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
      nms_chr <- ds_ls$files$filename
      if(is.null(descriptions_chr))
        descriptions_chr <- purrr::map(file_paths_chr,
                                       ~ NULL)
      ids_int <- file_paths_chr %>%
        purrr::map2_int(descriptions_chr,
                        ~{
                          fl_nm_1L_chr <- fs::path_file(.x)
                          if (fl_nm_1L_chr %in% nms_chr) {
                            id_1L_int <- get_fl_id_from_dv_ls(ds_ls,
                                                              fl_nm_1L_chr = fl_nm_1L_chr,
                                                              nms_chr = nms_chr)
                            if (is_draft_1L_lgl) {
                              id_1L_int %>% dataverse::delete_file()
                              id_1L_int <- dataverse::add_dataset_file(file = .x,
                                                                       dataset = ds_url_1L_chr,
                                                                       description = .y,
                                                                       key = key_1L_chr,
                                                                       server = server_1L_chr)
                            }else {
                              dataverse::update_dataset_file(file = .x,
                                                             dataset = ds_url_1L_chr,
                                                             id = id_1L_int,
                                                             force = T,
                                                             description = .y,
                                                             key = key_1L_chr,
                                                             server = server_1L_chr)
                            }
                          }else {
                            id_1L_int <- dataverse::add_dataset_file(file = .x,
                                                                     dataset = ds_url_1L_chr,
                                                                     description = .y,
                                                                     key = key_1L_chr,
                                                                     server = server_1L_chr)
                          }
                          id_1L_int
                        })
    }else{
      ids_int <- NULL
    }
  }else{
    ids_int <- NULL
  }
  return(ids_int)
}
write_fn_fl <- function(fns_dmt_tb,
                        fns_env_ls,
                        r_dir_1L_chr = "R",
                        document_unexp_lgl = T,
                        consent_1L_chr = NULL){
  file_nms_chr <- fns_dmt_tb$file_nm_chr %>% unique()
  if(is.null(consent_1L_chr)){
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write the files ",
                                                       file_nms_chr %>%
                                                         paste0(collapse = ", ") %>%
                                                         stringi::stri_replace_last(fixed = ",", " and"),
                                                       " to the ",
                                                       r_dir_1L_chr,
                                                       " directory?"),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
  }
  if(consent_1L_chr == "Y"){
    file_nms_chr %>%
      purrr::walk(~
                    {
                      tb <- fns_dmt_tb %>%
                        dplyr::filter(file_nm_chr == .x)
                      first_lgl_vec <- c(T,rep(F,nrow(tb)-1))
                      dest_path_1L_chr <- paste0(r_dir_1L_chr,"/",tb$file_pfx_chr[1],.x)
                      purrr::walk(1:nrow(tb),
                                  ~
                                    {
                                      if(!exists(tb[[.x,1]])){
                                        fn <- fns_env_ls$fns_env[[tb[[.x,1]]]]
                                      }else{
                                        fn <- eval(parse(text=tb[[.x,1]]))
                                      }
                                      # fn <- eval(parse(text=tb[[.x,1]]))
                                      fn_chr <- deparse(fn)
                                      fn_and_cls_chr <- tb[[.x,1]] %>% strsplit("\\.") %>% purrr::pluck(1)
                                      sink(dest_path_1L_chr, append =  !first_lgl_vec[.x])
                                      make_lines_for_fn_dmt(fn_name_1L_chr = tb[[.x,1]],
                                                            fn_type_1L_chr = ifelse(tb$file_pfx_chr[1]=="mthd_",
                                                                                    "meth_std_s3_mthd",
                                                                                    ifelse(tb$file_pfx_chr[1]=="grp_",
                                                                                           "gen_std_s3_mthd",
                                                                                           "fn")),
                                                            fn = fn,
                                                            fn_desc_1L_chr = tb[[.x,3]],
                                                            fn_out_type_1L_chr = tb[[.x,6]],
                                                            fn_title_1L_chr = tb[[.x,2]],
                                                            example_1L_lgl = tb[[.x,7]],
                                                            export_1L_lgl = T,
                                                            class_name_1L_chr = "",
                                                            details_1L_chr = tb[[.x,4]],
                                                            args_ls = tb$args_ls[[.x]] %>% as.list(),
                                                            import_chr = NA_character_,
                                                            doc_in_class_1L_lgl = F,
                                                            abbreviations_lup = abbreviations_lup,
                                                            object_type_lup = abbreviations_lup)
                                      if(tb[[.x,5]] + document_unexp_lgl == 0){
                                        writeLines(paste0("#' @keywords internal"))
                                      }
                                      writeLines(paste0(tb[[.x,1]]," <- ",fn_chr[1]))
                                      writeLines(fn_chr[2:length(fn_chr)])
                                      if(tb$file_pfx_chr[1]=="grp_"){
                                        writeLines(paste0("methods::setGeneric(\"",
                                                          tb[[.x,1]],
                                                          "\")"))
                                      }
                                      if(tb$file_pfx_chr[1]=="mthd_"){
                                        writeLines(paste0("#' @rdname ",fn_and_cls_chr[1],"-methods"))
                                        writeLines(paste0("#' @aliases ",fn_and_cls_chr[1],",",fn_and_cls_chr[2],"-method"))
                                        writeLines(paste0('methods::setMethod(\"', fn_and_cls_chr[1], '\"',
                                                          ', ',paste0('\"',fn_and_cls_chr[2],'\"'),
                                                          ', ', tb[[.x,1]],
                                                          ')'))
                                      }
                                      close_open_sinks()
                                    })
                    }
      )
  }
}
write_fn_type_dirs <- function(path_1L_chr = "data-raw"){
  undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr)
  write_new_dirs(undocumented_fns_dir_chr)
}
write_fns_to_split_dests <- function(pkg_depcy_ls,
                                     pkg_1_core_fns_chr,
                                     fns_dmt_tb,
                                     original_pkg_nm_1L_chr = get_dev_pkg_nm(),
                                     pkg_1_nm_1L_chr = "package_1",
                                     pkg_2_nm_1L_chr = "package_2",
                                     tmp_dir_path_1L_chr = "data-raw/pkg_migration",
                                     path_to_fns_dir_1L_chr = "data-raw/fns"){
  # utils::data("fns_dmt_tb",
  #             package = original_pkg_nm_1L_chr,
  #             envir = environment())
  read_fns(path_to_fns_dir_1L_chr)
  fns_for_pkg_1_chr <- get_all_depcys_of_fns(pkg_depcy_ls = pkg_depcy_ls,
                                             fns_chr = pkg_1_core_fns_chr)
  fns_for_pkg_2_chr <- setdiff(pkg_depcy_ls$Nomfun$label, fns_for_pkg_1_chr)
  migrate_ls <- list(fns_for_pkg_1_chr = fns_for_pkg_1_chr,
                     fns_for_pkg_2_chr = fns_for_pkg_2_chr)
  if(!dir.exists(tmp_dir_path_1L_chr))
    dir.create(tmp_dir_path_1L_chr)
  new_dest_dir_chr <- purrr::map_chr(c(pkg_1_nm_1L_chr,pkg_2_nm_1L_chr),
                                     ~ {
                                       new_dir_1L_chr <- paste0(tmp_dir_path_1L_chr,
                                                                "/",
                                                                .x)
                                       if(!dir.exists(new_dir_1L_chr))
                                         dir.create(new_dir_1L_chr)
                                       new_dir_1L_chr
                                     })
  migrate_ls %>%
    purrr::walk2(new_dest_dir_chr,
                 ~{
                   fns_tb <- fns_dmt_tb %>%
                     dplyr::filter(fns_chr %in% .x) %>%
                     dplyr::select(fns_chr, file_nm_chr)
                   file_nms_chr <- fns_tb$file_nm_chr %>% unique()
                   new_dest_dir_1L_chr <- .y
                   file_nms_chr %>%
                     purrr::walk(~
                                   {
                                     tb <- fns_tb %>%
                                       dplyr::filter(file_nm_chr == .x)
                                     first_lgl_vec <- c(T,rep(F,nrow(tb)-1))
                                     dest_path_1L_chr <- paste0(new_dest_dir_1L_chr,"/",.x)
                                     purrr::walk(1:nrow(tb),
                                                 ~
                                                   {
                                                     fn <- eval(parse(text=tb[[.x,1]]))
                                                     fn_chr <- deparse(fn)
                                                     sink(dest_path_1L_chr, append =  !first_lgl_vec[.x])
                                                     writeLines(paste0(tb[[.x,1]]," <- ",fn_chr[1]))
                                                     writeLines(fn_chr[2:length(fn_chr)])
                                                     close_open_sinks()
                                                   })
                                   }
                     )
                 })
}
write_from_tmp <- function(tmp_paths_chr,
                           dest_paths_chr,
                           edit_fn_ls = list(NULL),
                           args_ls_ls = NULL){
  text_ls <- purrr::pmap(list(tmp_paths_chr,
                              edit_fn_ls,
                              args_ls_ls),
                         ~{
                           fileConn <- file(..1)
                           txt_chr <- readLines(fileConn, warn=FALSE)
                           close(fileConn)
                           if(is.null(..2)){
                             edit_fn <- function(x){x}
                             }else{
                               edit_fn <- ..2
                               }
                rlang::exec(edit_fn, txt_chr, !!!..3)
              })
  write_to_delete_fls(intersect(tmp_paths_chr,dest_paths_chr))
  write_new_files(dest_paths_chr,
                  text_ls = text_ls)
}
write_inst_dir <- function(path_to_pkg_rt_1L_chr = getwd()){
  source_inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr,"/data-raw/inst")
  if(dir.exists(source_inst_dir_1L_chr)){
    inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr,"/inst")
    write_to_delete_dirs(inst_dir_1L_chr)
    write_new_dirs(inst_dir_1L_chr)
    write_new_files(inst_dir_1L_chr,
                    source_paths_ls = list(source_inst_dir_1L_chr))
  }
}
write_links_for_website <- function(path_to_pkg_rt_1L_chr = getwd(),
                                    developer_manual_url_1L_chr = NA_character_,
                                    user_manual_url_1L_chr = NA_character_,
                                    project_website_url_1L_chr = NA_character_){
  write_from_tmp(paste0(path_to_pkg_rt_1L_chr,
                        "/_pkgdown.yml"),
                 dest_paths_chr = paste0(path_to_pkg_rt_1L_chr,
                                           "/_pkgdown.yml"),
                 edit_fn_ls = list(function(txt_chr,
                                            user_manual_url_1L_chr,
                                            developer_manual_url_1L_chr,
                                            project_website_url_1L_chr){
                   idx_1L_int <- which(txt_chr=="home:")
                   if(!identical(idx_1L_int,integer(0))){
                     changes_chr <- c(any(txt_chr == "  - text: User manual (PDF)"),
                                      any(txt_chr == "  - text: Developer version of usual manual (PDF)"),
                                      any(txt_chr == "  - text: Project website"))
                     txt_chr <- txt_chr[-(1:(length(changes_chr[changes_chr==T])*2))]
                   }
                   c("home:",
                     "  links:",
                     ifelse(!is.na(user_manual_url_1L_chr), "  - text: Manual - User (PDF)", NA_character_),
                     ifelse(!is.na(user_manual_url_1L_chr), paste0("    href: ", user_manual_url_1L_chr), NA_character_),
                     ifelse(!is.na(developer_manual_url_1L_chr), "  - text: Manual - Developer (PDF)", NA_character_),
                     ifelse(!is.na(developer_manual_url_1L_chr), paste0("    href: ", developer_manual_url_1L_chr), NA_character_),
                     ifelse(!is.na(project_website_url_1L_chr), "  - text: Project website", NA_character_),
                     ifelse(!is.na(project_website_url_1L_chr), paste0("    href: ", project_website_url_1L_chr), NA_character_),
                     txt_chr) %>% stats::na.omit()
                 }),
                 args_ls_ls = list(list(user_manual_url_1L_chr = user_manual_url_1L_chr,
                                        developer_manual_url_1L_chr = developer_manual_url_1L_chr,
                                        project_website_url_1L_chr = project_website_url_1L_chr)))
}
write_manuals <- function(pkg_setup_ls,
                          path_to_dmt_dir_1L_chr = deprecated(), ##
                          dv_url_pfx_1L_chr = NULL,
                          key_1L_chr = NULL,
                          publish_dv_1L_lgl = T,
                          server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                          pkg_desc_ls = deprecated()){
  if (lifecycle::is_present(pkg_desc_ls)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_manuals(pkg_desc_ls)",
                              details = "Please use `ready4fun::write_manuals(pkg_setup_ls)` to pass the pkg_desc_ls object to this function.")
  }
  write_manuals_to_dv(package_1L_chr = pkg_setup_ls$initial_ls$pkg_desc_ls$Package,
                      path_to_dmt_dir_1L_chr = pkg_setup_ls$subsequent_ls$path_to_dmt_dir_1L_chr,
                      pkg_dmt_dv_ds_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[1],
                      publish_dv_1L_lgl = publish_dv_1L_lgl)
  dmt_urls_chr <- get_dv_fls_urls(file_nms_chr = paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package,
                                                        "_",
                                                        c("Developer","User"),
                                                        ".pdf"),
                                  dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[1],
                                  dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                  key_1L_chr = key_1L_chr,
                                  server_1L_chr = server_1L_chr)
  project_url_1L_chr <- pkg_setup_ls$initial_ls$pkg_desc_ls$URL %>%
    strsplit(",") %>%
    unlist() %>%
    purrr::pluck(3)
  if(is.null(project_url_1L_chr))
    project_url_1L_chr <-  NA_character_
  write_links_for_website(user_manual_url_1L_chr = dmt_urls_chr[2],
                          developer_manual_url_1L_chr = dmt_urls_chr[1],
                          project_website_url_1L_chr = project_url_1L_chr)
}
write_manuals_to_dv <- function(package_1L_chr = get_dev_pkg_nm(getwd()),
                                path_to_dmt_dir_1L_chr,
                                pkg_dmt_dv_ds_1L_chr,
                                publish_dv_1L_lgl = F){
  version_1L_chr <- utils::packageDescription(package_1L_chr)$Version
  purrr::walk(c("Developer","User"),
              ~{
                dir_1L_chr <- paste0(path_to_dmt_dir_1L_chr,
                                     "/",
                                     .x)
                original_1L_chr <- paste0(dir_1L_chr,
                                          "/",
                                          package_1L_chr,
                                          "_",
                                          version_1L_chr,
                                          ".pdf")
                fl_nm_1L_chr <- paste0(package_1L_chr,
                                       "_",
                                       .x,
                                       ".pdf")
                copy_1L_chr <- paste0(dir_1L_chr,
                                      "/",
                                      fl_nm_1L_chr)
                if(file.exists(original_1L_chr)){
                  write_new_files(dir_1L_chr,
                                  source_paths_ls = list(original_1L_chr),
                                  fl_nm_1L_chr = fl_nm_1L_chr)
                }
                # consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to add a copy of ",
                #                                      copy_1L_chr,
                #                                      " to a draft version of dataverse ",
                #                                      pkg_dmt_dv_ds_1L_chr,
                #                                      "?"),
                #                               options_chr = c("Y", "N"),
                #                               force_from_opts_1L_chr = T)
  # if(consent_1L_chr == "Y"){
                write_fls_to_dv(copy_1L_chr,
                                descriptions_chr = paste0("Manual (",
                                                          .x %>% tolower(),
                                                          " version)",
                                                          " describing the contents of the ",
                                                          package_1L_chr,
                                                          " R package."),
                                ds_url_1L_chr = pkg_dmt_dv_ds_1L_chr)
      # dataverse::add_dataset_file(file = copy_1L_chr,
      #                             dataset = pkg_dmt_dv_ds_1L_chr,
      #                             description = paste0("Manual (",
      #                                                  .x %>% tolower(),
      #                                                  " version)",
      #                                                  " describing the contents of the ",
      #                                                  package_1L_chr,
      #                                                  " R package."))


  # }
              })
  if(publish_dv_1L_lgl){
    write_to_publish_dv_ds(dv_ds_1L_chr = pkg_dmt_dv_ds_1L_chr)
  }
}
write_new_abbrs <- function(pkg_setup_ls,
                            long_name_chr = NULL,
                            custom_plural_ls = NULL,
                            key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                            no_plural_chr = NA_character_,
                            publish_dv_1L_lgl = T,
                            pfx_rgx = NA_character_,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(pkg_setup_ls$subsequent_ls$abbreviations_lup)){
    pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$object_type_lup
    was_null_abbrs_1L_lgl <- T
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_abbrs_chr) & !is.null(long_name_chr)){
    pkg_setup_ls <- update_abbrs(pkg_setup_ls,
                                 short_name_chr = pkg_setup_ls$problems_ls$missing_abbrs_chr,
                                 long_name_chr = long_name_chr,
                                 no_plural_chr = no_plural_chr,
                                 custom_plural_ls = custom_plural_ls,
                                 pfx_rgx = pfx_rgx)
    pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                          list_element_1L_chr = "missing_abbrs_chr")
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_class_abbrs_chr)){
    class_desc_chr <- pkg_setup_ls$problems_ls$missing_class_abbrs_chr %>%
      purrr::map_chr(~ get_from_lup_obj(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x,
                                        match_value_xx = stringr::str_remove(.x,
                                                                             paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package,"_")),
                                        match_var_nm_1L_chr = "name_stub_chr",
                                        target_var_nm_1L_chr = "class_desc_chr",
                                        evaluate_lgl = F))
    short_dupls_chr <- intersect(pkg_setup_ls$problems_ls$missing_class_abbrs_chr,
                                 pkg_setup_ls$subsequent_ls$abbreviations_lup$short_name_chr)
    long_dupls_chr <- intersect(class_desc_chr,
                                pkg_setup_ls$subsequent_ls$abbreviations_lup$long_name_chr)
    testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate class name values from 'classes_to_make_tb' to the short_name_chr column:\n",
                          short_dupls_chr %>% make_list_phrase()),
                   identical(short_dupls_chr, character(0)))
    testit::assert(paste0("No duplicates are allowed in the abbreviations lookup table. You are attempting to add the following duplicate class description values from 'classes_to_make_tb' to the long_name_chr column:\n",
                          long_dupls_chr %>% make_list_phrase()),
                   identical(long_dupls_chr, character(0)))
    pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup %>%
      update_abbr_lup(short_name_chr = pkg_setup_ls$problems_ls$missing_class_abbrs_chr,
                      long_name_chr = class_desc_chr,
                      no_plural_chr = class_desc_chr,
                      custom_plural_ls = NULL,
                      pfx_rgx = NA_character_)
    pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                          list_element_1L_chr = "missing_class_abbrs_chr")
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_words_chr)){
   append_ls <- list(treat_as_words_chr = c(pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                                            pkg_setup_ls$problems_ls$missing_words_chr))
   words_desc_1L_chr <- "Additional words for dictionary"
   pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                         list_element_1L_chr = "missing_words_chr")
  }else{
    append_ls <- words_desc_1L_chr <- NULL
  }
  file_ids_int <- write_env_objs_to_dv(append(list(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup),
                                              append_ls),
                                       descriptions_chr = c("Abbreviations lookup table", words_desc_1L_chr),
                                       ds_url_1L_chr = pkg_setup_ls$subsequent_ls$dv_ds_nm_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr,
                                       publish_dv_1L_lgl = publish_dv_1L_lgl)
  return(pkg_setup_ls)
}
write_new_arg_sfcs <- function(arg_nms_chr,
                                 fn_type_1L_chr,
                                 dir_path_chr,
                                 rt_dev_dir_path_1L_chr = normalizePath("../../../"),
                                 pkg_nm_1L_chr,
                                 inc_fns_idx_dbl = NA_real_){
  if(is.na(inc_fns_idx_dbl))
    inc_fns_idx_dbl <- 1:length(ls(paste0("package:",pkg_nm_1L_chr))[ls(paste0("package:",pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)])
  purrr::walk(arg_nms_chr[order(nchar(arg_nms_chr), arg_nms_chr,  decreasing=T )] %>% unique(),
              ~ write_to_rpl_1L_and_indefL_sfcs(.x,
                                             file_path_chr = paste0(dir_path_chr,"/",fn_type_1L_chr,".R")))
  updated_fns_chr <- ls(paste0("package:",pkg_nm_1L_chr))[ls(paste0("package:",pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)][inc_fns_idx_dbl]
  updated_sfcs_chr <- arg_nms_chr[arg_nms_chr %>% endsWith("_vec")] %>% stringr::str_sub(start=-8) %>% unique()
  fn_nms_to_upd_chr <- updated_fns_chr[updated_fns_chr %>% stringr::str_sub(start=-8) %in% updated_sfcs_chr]
  if(ifelse(identical(fn_nms_to_upd_chr, character(0)),
            F,
            !is.na(fn_nms_to_upd_chr)
  )){
    purrr::walk(fn_nms_to_upd_chr,
                ~ write_to_rpl_1L_and_indefL_sfcs(.x,
                                               dir_path_chr = dir_path_chr))
    purrr::walk(paste0(pkg_nm_1L_chr,"::",fn_nms_to_upd_chr),
                ~ write_to_rpl_1L_and_indefL_sfcs(.x,
                                               dir_path_chr = rt_dev_dir_path_1L_chr))
  }
  fn_args_to_rnm_ls <- purrr::map(updated_fns_chr,
                                  ~ {
                                    fn_args_chr <- get_fn_args_chr(eval(parse(text=.x)))
                                    fn_args_chr[purrr::map_lgl(fn_args_chr, ~ .x %in% c(arg_nms_chr,arg_nms_chr %>% stringr::str_sub(end=-5)))]
                                  }) %>% stats::setNames(updated_fns_chr)

  return(fn_args_to_rnm_ls)
}
write_new_dirs <- function(new_dirs_chr){
  new_dirs_chr <- new_dirs_chr[new_dirs_chr %>% purrr::map_lgl(~!dir.exists(.x))]
  if(!identical(new_dirs_chr, character(0))){
    message(paste0("Are you sure that you want to write the following director",
                   ifelse(length(new_dirs_chr)>1,"ies","y"),
                   " to your machine? \n",
                   new_dirs_chr %>% paste0(collapse = "\n")))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write ",
                                                       ifelse(length(new_dirs_chr)>1,
                                                              "these directories?",
                                                              "this directory?")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      paths_ls <- new_dirs_chr %>% purrr::walk(~{
        dir.create(.x)
      })
      message(paste0("New directories created:\n", new_dirs_chr %>% paste0(collapse = "\n")))
    }else{
      message("Write request cancelled - no new directories created")
    }
  }
}
write_new_files <- function(paths_chr,
                            custom_write_ls = NULL,
                            fl_nm_1L_chr = NULL,
                            source_paths_ls = NULL,
                            text_ls = NULL){
  if(!is.null(source_paths_ls)){
    dest_dir_1L_chr <- paths_chr
    paths_chr <- purrr::map(source_paths_ls,
                            ~{
                              if(dir.exists(.x)){
                                list.files(.x)
                              }else{
                                fs::path_file(.x)
                              }
                            }) %>%
      purrr::flatten_chr() %>%
      purrr::map_chr(~paste0(dest_dir_1L_chr,
                             "/",
                             ifelse(is.null(fl_nm_1L_chr),
                                    .x,
                                    fl_nm_1L_chr)
                             ))
    recursive_1L_lgl <- ifelse(paths_chr %>% purrr::map_lgl(~dir.exists(.x)) %>% any(),
                               T,
                               F)
  }
  new_files_chr <- paths_chr[paths_chr %>% purrr::map_lgl(~!file.exists(.x))]
  overwritten_files_chr <- setdiff(paths_chr, new_files_chr)
  if(!identical(paths_chr, character(0))){
    message(paste0("Are you sure that you want to write / overwrite the following file",
                   ifelse(length(paths_chr)>1,"s",""),
                   " to your machine: \n",
                   ifelse(identical(new_files_chr, character(0)),
                          "",
                          paste0("Files that will be created: \n",
                                 new_files_chr %>% paste0(collapse = "\n"))),
                   ifelse(identical(overwritten_files_chr, character(0)),
                          "",
                          paste0("Files that will be overwritten: \n",
                                 overwritten_files_chr %>% paste0(collapse = "\n"))),
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to write ",
                                                       ifelse((length(new_files_chr) + length(overwritten_files_chr))>1,
                                                              "these files:",
                                                              "this file:")),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      if(!is.null(text_ls)){
        purrr::walk2(paths_chr,
                     text_ls,
                     ~ {
                       file_conn <- file(.x)
                       writeLines(.y, file_conn)
                       close(file_conn)
                     })
      }else{
        if(!is.null(source_paths_ls)){
          source_paths_chr <- purrr::map(source_paths_ls,
                        ~{
                          if(dir.exists(.x)){
                            list.files(.x, full.names = T)
                          }else{
                            .x
                          }
                        }) %>%
          purrr::flatten_chr()
          purrr::walk(source_paths_chr,
                       ~ file.copy(.x,
                                   paste0(dest_dir_1L_chr,
                                          ifelse(is.null(fl_nm_1L_chr),
                                                 "",
                                                 paste0("/",fl_nm_1L_chr))),
                                   overwrite = T,
                                   recursive = recursive_1L_lgl))
        }
        if(!is.null(custom_write_ls)){
          if("consent_1L_chr" %in% formalArgs(custom_write_ls$fn))
            custom_write_ls$args_ls$consent_1L_chr <- consent_1L_chr
          rlang::exec(custom_write_ls$fn,
                      !!!custom_write_ls$args_ls)
        }
      }
    }else{
      message("Write request cancelled - no new directories created")
    }
  }
}
write_new_fn_types <- function(pkg_setup_ls,
                               fn_type_desc_chr = NA_character_,
                               first_arg_desc_chr = NA_character_,
                               is_generic_lgl = F,
                               is_method_lgl = F,
                               key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                               second_arg_desc_chr = NA_character_,
                               server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                               publish_dv_1L_lgl = F){
  pkg_setup_ls$subsequent_ls$fn_types_lup <- pkg_setup_ls$subsequent_ls$fn_types_lup %>%
    add_rows_to_fn_type_lup(fn_type_nm_chr = pkg_setup_ls$problems_ls$missing_fn_types_chr,
                            fn_type_desc_chr = fn_type_desc_chr,
                            first_arg_desc_chr = first_arg_desc_chr,
                            second_arg_desc_chr = second_arg_desc_chr,
                            is_generic_lgl = is_generic_lgl,
                            is_method_lgl = is_method_lgl)
  file_ids_int <- write_env_objs_to_dv(list(fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup),
                                       descriptions_chr = c("Function types lookup table"),
                                       ds_url_1L_chr = pkg_setup_ls$subsequent_ls$dv_ds_nm_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr,
                                       publish_dv_1L_lgl = publish_dv_1L_lgl)
  pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                        list_element_1L_chr = "missing_fn_types_chr")
  return(pkg_setup_ls)
}
write_ns_imps_to_desc <- function(dev_pkgs_chr = NA_character_,
                                  incr_ver_1L_lgl = T){
  devtools::document()
  packages_chr <- readLines("NAMESPACE") %>%
    purrr::map_chr(~ifelse(startsWith(.x,"import"),
                           ifelse(startsWith(.x,"importFrom"),
                                  stringr::str_replace(.x,"importFrom\\(","") %>%
                                    stringr::str_sub(end=stringr::str_locate(.,",")[1,1]-1),
                                  stringr::str_replace(.x,"import\\(","") %>%
                                    stringr::str_sub(end=-2)),
                           NA_character_)) %>%
    purrr::discard(is.na) %>%
    unique()
  if(!is.na(dev_pkgs_chr)){
    dev_pkgs_chr <- intersect(packages_chr,dev_pkgs_chr) %>% sort()
    packages_chr <- setdiff(packages_chr,dev_pkgs_chr) %>% sort()
    purrr::walk(dev_pkgs_chr,
                ~usethis::use_dev_package(.x))
  }
  purrr::walk(packages_chr,
              ~usethis::use_package(.x))
  devtools::document()
  if(incr_ver_1L_lgl)
    usethis::use_version()
}
write_new_obj_types <- function(pkg_setup_ls,
                                long_name_chr = NULL,
                                atomic_element_lgl = F,
                                custom_plural_ls = NULL,
                                key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                no_plural_chr = NA_character_,
                                publish_dv_1L_lgl = T,
                                pfx_rgx = NA_character_,
                                r3_can_extend_lgl = F,
                                server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  was_null_seed_1L_lgl <- was_null_obj_type_1L_lgl <- update_abbrs_1L_lgl <- F
  if(is.null(pkg_setup_ls$subsequent_ls$seed_obj_type_lup)){
    pkg_setup_ls$subsequent_ls$seed_obj_type_lup <- make_obj_lup_spine()
    was_null_seed_1L_lgl <- T
  }
  if(is.null(pkg_setup_ls$subsequent_ls$object_type_lup)){
    pkg_setup_ls$subsequent_ls$object_type_lup <- make_obj_lup(obj_lup_spine = pkg_setup_ls$subsequent_ls$seed_obj_type_lup)
    was_null_obj_type_1L_lgl <- update_abbrs_1L_lgl <- T
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_obj_types_chr) & !is.null(long_name_chr)){
    short_dupls_chr <- intersect(pkg_setup_ls$problems_ls$missing_obj_types_chr,
                                 pkg_setup_ls$subsequent_ls$object_type_lup$short_name_chr)
    long_dupls_chr <- intersect(long_name_chr,
                                pkg_setup_ls$subsequent_ls$object_type_lup$long_name_chr)
    testit::assert(paste0("No duplicates are allowed in the object type lookup table. You are attempting to add the following duplicate values to the short_name_chr column:\n",
                          short_dupls_chr %>% make_list_phrase()),
                   identical(short_dupls_chr, character(0)))
    testit::assert(paste0("No duplicates are allowed in the object type lookup table. You are attempting to add the following duplicate values from the 'long_name_chr' argument to the long_name_chr column of the abbreviations lookup tbale:\n",
                          long_dupls_chr %>% make_list_phrase()),
                   identical(long_dupls_chr, character(0)))
    pkg_setup_ls$subsequent_ls$seed_obj_type_lup <- make_obj_lup_spine(pkg_setup_ls$subsequent_ls$seed_obj_type_lup,
                                                                       new_entries_tb = tibble::tibble(short_name_chr = pkg_setup_ls$problems_ls$missing_obj_types_chr,
                                                                                                       long_name_chr = long_name_chr,
                                                                                                       atomic_element_lgl = atomic_element_lgl,
                                                                                                       r3_can_extend_lgl = r3_can_extend_lgl))

    updated_obj_type_lup <- make_obj_lup(pkg_setup_ls$subsequent_ls$seed_obj_type_lup)
    obj_type_new_cses_tb <- get_obj_type_new_cses(updated_obj_type_lup = updated_obj_type_lup,
                                                  old_obj_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)


    pkg_setup_ls$subsequent_ls$object_type_lup <- pkg_setup_ls$subsequent_ls$object_type_lup %>%
      update_abbr_lup(short_name_chr = obj_type_new_cses_tb$short_name_chr,
                      long_name_chr = obj_type_new_cses_tb$long_name_chr,
                      no_plural_chr = obj_type_new_cses_tb$long_name_chr,
                      custom_plural_ls = custom_plural_ls,
                      pfx_rgx = pfx_rgx)
    update_abbrs_1L_lgl <- T
  }
  if(update_abbrs_1L_lgl){
    if(was_null_obj_type_1L_lgl){
      obj_type_new_cses_tb <- pkg_setup_ls$subsequent_ls$object_type_lup
    }
    if(is.null(pkg_setup_ls$subsequent_ls$abbreviations_lup)){
      pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$object_type_lup
    }else{
      pkg_setup_ls$subsequent_ls$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup %>%
        update_abbr_lup(short_name_chr = obj_type_new_cses_tb$short_name_chr,
                        long_name_chr = obj_type_new_cses_tb$long_name_chr,
                        no_plural_chr = obj_type_new_cses_tb$long_name_chr,
                        custom_plural_ls = custom_plural_ls,
                        pfx_rgx = pfx_rgx)
    }
    if(!is.null(pkg_setup_ls$problems_ls$missing_obj_types_chr) & !is.null(long_name_chr))
      pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                            list_element_1L_chr = "missing_obj_types_chr")
  }
  if(!is.null(pkg_setup_ls$problems_ls$missing_words_chr)){
    append_ls <- list(treat_as_words_chr = c(pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                                             pkg_setup_ls$problems_ls$missing_words_chr))
    words_desc_1L_chr <- "Additional words for dictionary"
    pkg_setup_ls <- update_pkg_setup_msgs(pkg_setup_ls,
                                          list_element_1L_chr = "missing_words_chr")
  }else{
    append_ls <- words_desc_1L_chr <- NULL
  }
  if(was_null_seed_1L_lgl){
    append_ls <- append(append_ls,
                        list(seed_obj_type_lup = pkg_setup_ls$subsequent_ls$seed_obj_type_lup))
    seed_desc_1L_chr <- "Seed object type lookup table"
  }else{
    seed_desc_1L_chr <- NULL
  }
  if(update_abbrs_1L_lgl){
    append_ls <- append(append_ls,
                        list(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup))
    abbrs_desc_1L_chr <- "Abbreviations lookup table"
  }else{
    abbrs_desc_1L_chr <- NULL
  }
  file_ids_int <- write_env_objs_to_dv(append(list(object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup),
                                              append_ls),
                                       descriptions_chr = c("Object type lookup table",
                                                            words_desc_1L_chr, seed_desc_1L_chr, abbrs_desc_1L_chr),
                                       ds_url_1L_chr = pkg_setup_ls$subsequent_ls$dv_ds_nm_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr,
                                       publish_dv_1L_lgl = publish_dv_1L_lgl)
  return(pkg_setup_ls)
}
write_package <- function(pkg_setup_ls,
                          dv_url_pfx_1L_chr = NULL,
                          key_1L_chr = NULL,
                          publish_dv_1L_lgl = T,
                          self_serve_1L_lgl = F,
                          server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                          cls_fn_ls = deprecated(),
                          path_to_dmt_dir_1L_chr = deprecated(),
                          pkg_desc_ls = deprecated(),
                          pkg_ds_ls_ls = deprecated()){
  if (lifecycle::is_present(pkg_desc_ls)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_package(pkg_desc_ls)",
                              details = "Please use `ready4fun::write_package(pkg_setup_ls)` to pass the pkg_desc_ls object to this function.")
  }
  if (lifecycle::is_present(pkg_ds_ls_ls)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_package(pkg_ds_ls_ls)",
                              details = "Please use `ready4fun::write_package(pkg_setup_ls)` to pass the pkg_ds_ls_ls object to this function.")
  }
  if (lifecycle::is_present(cls_fn_ls)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_package(cls_fn_ls)",
                              details = "Please use `ready4fun::write_package(pkg_setup_ls)` to pass the cls_fn_ls object to this function.")
  }
  if (lifecycle::is_present(path_to_dmt_dir_1L_chr)) {
    lifecycle::deprecate_warn("0.0.0.9333",
                              "ready4fun::write_package(path_to_dmt_dir_1L_chr)",
                              details = "Please use `ready4fun::write_package(pkg_setup_ls)` to pass the path_to_dmt_dir_1L_chr object to this function.")
  }
  rlang::exec(write_pkg_setup_fls, !!!pkg_setup_ls$initial_ls)
  dss_records_ls <- write_pkg_dss(pkg_setup_ls,
                                  pkg_url_1L_chr = pkg_setup_ls$initial_ls$pkg_desc_ls$URL %>%
                                    strsplit(",") %>%
                                    unlist() %>%
                                    purrr::pluck(1),
                                  dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2])
  write_clss(dss_records_ls = dss_records_ls,
             pkg_setup_ls = pkg_setup_ls,
             dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
             key_1L_chr = key_1L_chr,
             self_serve_1L_lgl = self_serve_1L_lgl,
             server_1L_chr = server_1L_chr)
  write_and_doc_fn_fls(fns_dmt_tb = dss_records_ls$fns_dmt_tb,
                       pkg_setup_ls = pkg_setup_ls,
                       update_pkgdown_1L_lgl = T)
  write_manuals(pkg_setup_ls = pkg_setup_ls,
                dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                key_1L_chr = key_1L_chr,
                server_1L_chr = server_1L_chr)
}
write_pkg <- function(package_1L_chr,
                        R_dir_1L_chr = "R"){
  lifecycle::deprecate_soft("0.0.0.9298",
                            what = "ready4fun::write_pkg()")
  write_from_tmp(system.file("pkg_tmp.R", package="ready4fun"),
                 dest_paths_chr = paste0(R_dir_1L_chr,"/pkg_",package_1L_chr,".R"),
                 edit_fn_ls = list(function(txt_chr,
                                    package_1L_chr){
                     pkg_desc_ls <- utils::packageDescription(package_1L_chr)
                     # txt_chr <- purrr::map_chr(txt_chr,
                     #                           ~ stringr::str_replace_all(.x,
                     #                                                      "ready4fun",
                     #                                                      package_1L_chr))
                     # txt_chr[1] <- paste0("#' ",package_1L_chr,": ",pkg_desc_ls$Title %>%
                     #                        stringr::str_replace_all("\n","\n#' "))
                     # txt_chr[3] <- paste0("#' ",pkg_desc_ls$Description %>%
                     #                        stringr::str_replace_all("\n","\n#' "))
                     txt_chr
                   }),
                   args_ls_ls = list(list(package_1L_chr = package_1L_chr)))
}
write_pkg_dss <- function(pkg_setup_ls,
                          #abbreviations_lup = NULL,
                          args_ls_ls = NULL,
                          #cls_fn_ls = NULL,
                          details_ls = NULL,
                          dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()), # Deprecate
                          dv_ds_nm_1L_chr, # Deprecate
                          #fn_types_lup = NULL,
                          inc_all_mthds_1L_lgl = T,
                          inc_pkg_meta_data_1L_lgl = F, # Deprecate
                          #object_type_lup = NULL,
                          paths_ls = make_fn_nms(),
                          pkg_url_1L_chr = NA_character_,
                          R_dir_1L_chr = "R",
                          undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
                          #url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                          dv_url_pfx_1L_chr = NULL,
                          key_1L_chr = NULL,
                          server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                          pkg_ds_ls_ls = deprecated()){
  pkg_dss_tb <- tibble::tibble(ds_obj_nm_chr = character(0),
                               title_chr = character(0), desc_chr = character(0), url_chr = character(0))
  if(inc_pkg_meta_data_1L_lgl){
    pkg_dss_tb <- write_abbr_lup(seed_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                 pkg_dss_tb = pkg_dss_tb,
                                 pkg_nm_1L_chr = dev_pkg_nm_1L_chr,
                                 dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                 object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup)
    #utils::data("abbreviations_lup", envir = environment())
    pkg_dss_tb <- pkg_setup_ls$subsequent_ls$fn_types_lup %>%
      write_dmtd_fn_type_lup(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                             object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
                             pkg_dss_tb = pkg_dss_tb,
                             dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,#url_1L_chr,
                             dv_url_pfx_1L_chr = NULL,
                             key_1L_chr = NULL,
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
    # utils::data("fn_types_lup", envir = environment())
  }
  if(!is.null(pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls)){
    pkg_dss_tb <- purrr::reduce(pkg_setup_ls$subsequent_ls$pkg_ds_ls_ls,
                                .init = pkg_dss_tb,
                                ~ {
                                  if(is.null(.y$abbreviations_lup))
                                    .y$abbreviations_lup <- pkg_setup_ls$subsequent_ls$abbreviations_lup
                                  if(is.null(.y$object_type_lup))
                                    .y$object_type_lup <- pkg_setup_ls$subsequent_ls$object_type_lup
                                  args_ls <- append(.y,
                                                    list(overwrite_1L_lgl = T,
                                                         pkg_dss_tb = .x,
                                                         R_dir_1L_chr = R_dir_1L_chr,
                                                         dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                                         dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                                         key_1L_chr = key_1L_chr,
                                                         server_1L_chr = server_1L_chr))
                                  rlang::exec(write_and_doc_ds,!!!args_ls)
                                })
  }
  fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls,
                                     abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                     custom_dmt_ls = list(details_ls = details_ls,#list(add_indefartls_to_phrases = "TEST DETAILS",close_open_sinks = "ANOTHER TEST"),
                                                          inc_for_main_user_lgl_ls = list(force_true_chr = pkg_setup_ls$subsequent_ls$user_manual_fns_chr,
                                                                                          force_false_chr = NA_character_),
                                                          args_ls_ls = args_ls_ls#list(add_indefartls_to_phrases = NA_character_#c(abbreviated_phrase_chr_vec = "TEST_ARG_DESC_1",ignore_phrs_not_in_lup_1L_lgl = "TEST_ARG_DESC_3"))
                                                     ),
                                     fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup,
                                     inc_all_mthds_1L_lgl = inc_all_mthds_1L_lgl,
                                     object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
                                     undocumented_fns_dir_chr = undocumented_fns_dir_chr)
  if(inc_pkg_meta_data_1L_lgl){
  pkg_dss_tb <- fns_dmt_tb %>%
    write_and_doc_ds(overwrite_1L_lgl = T,
                     db_1L_chr = "fns_dmt_tb",
                     title_1L_chr = paste0(dev_pkg_nm_1L_chr," function documentation table"),
                     desc_1L_chr = paste0("A table with the summary information on functions included in the ",dev_pkg_nm_1L_chr," package."),
                     format_1L_chr = "A tibble",
                     url_1L_chr = pkg_url_1L_chr,
                     abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                     object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
                     pkg_dss_tb = pkg_dss_tb,
                     dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                     dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                     key_1L_chr = key_1L_chr,
                     server_1L_chr = server_1L_chr)
  }
  dss_records_ls <- list(pkg_dss_tb = pkg_dss_tb,
                         fns_dmt_tb = fns_dmt_tb)
  return(dss_records_ls)
}
write_pkg_setup_fls <- function(pkg_desc_ls,
                                copyright_holders_chr,
                                gh_repo_1L_chr,
                                addl_badges_ls = NULL,
                                badges_lup = NULL,
                                check_type_1L_chr = "none",
                                delete_r_dir_cnts_1L_lgl = F,
                                lifecycle_stage_1L_chr = "experimental",
                                incr_ver_1L_lgl = T,
                                on_cran_1L_lgl = F,
                                path_to_pkg_logo_1L_chr = NA_character_,
                                add_gh_site_1L_lgl = T,
                                dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()),
                                path_to_pkg_rt_1L_chr = getwd()){
  options(usethis.description = pkg_desc_ls)
  use_gh_cmd_check_1L_lgl <- (check_type_1L_chr %in% c("gh","full","release","standard"))
  if(is.null(badges_lup)){
    utils::data("badges_lup",envir = environment())
  }
  if(delete_r_dir_cnts_1L_lgl)
    write_to_reset_pkg_files(delete_contents_of_1L_chr = "R",
                             package_1L_chr = dev_pkg_nm_1L_chr,
                             package_dir_1L_chr = path_to_pkg_rt_1L_chr)
  update_desc_fl_1L_lgl <- !is.na(dev_pkg_nm_1L_chr)
  if(!update_desc_fl_1L_lgl)
    dev_pkg_nm_1L_chr <- get_dev_pkg_nm(path_to_pkg_rt_1L_chr)
  devtools::load_all(path_to_pkg_rt_1L_chr)
  write_std_imp(paste0(path_to_pkg_rt_1L_chr,"/R"),
                package_1L_chr = dev_pkg_nm_1L_chr)
  if(update_desc_fl_1L_lgl){
    desc_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"))
    desc_1L_chr[1] <- paste0("Package: ",dev_pkg_nm_1L_chr)
    write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"),
                    text_ls = list(desc_1L_chr))
  }
  if(!file.exists(paste0(path_to_pkg_rt_1L_chr,
                         "/vignettes/",
                         get_dev_pkg_nm(),
                         ".Rmd")))
    write_vignette(dev_pkg_nm_1L_chr, pkg_rt_dir_chr = path_to_pkg_rt_1L_chr)
  if(incr_ver_1L_lgl){
    usethis::use_version()
  }
  write_inst_dir(path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr)
  usethis::use_gpl3_license()
  license_1L_chr <- c(paste0(dev_pkg_nm_1L_chr,
                          " - ",
                          desc::desc_get("Title") %>%
                            as.vector()),
    readLines(paste0(path_to_pkg_rt_1L_chr,"/License.md"))[556:569]) %>%
    purrr::map_chr(~stringr::str_trim(.x) %>%
                     stringr::str_replace_all("<year>",as.character(Sys.Date() %>% lubridate::year())) %>%
                     stringr::str_replace_all("<name of author>",paste0(copyright_holders_chr,collapse = "and "))) %>%
    paste0(collapse = "\n")
  write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr,"/LICENSE"),
                  text_ls = list(license_1L_chr))
  desc::desc_set("License", "GPL-3 + file LICENSE")
  usethis::use_pkgdown()
  usethis::use_build_ignore(files = "_pkgdown.yml")
  usethis::use_package("testthat")
  usethis::use_package("knitr")
  desc::desc_set("VignetteBuilder", "knitr")
  usethis::use_build_ignore(paste0(paste0("data-raw/"),
                            list.files(paste0(path_to_pkg_rt_1L_chr,"/data-raw"), recursive = T)))
  if(!is.na(path_to_pkg_logo_1L_chr)){
    write_new_dirs(paste0(path_to_pkg_rt_1L_chr,"/man/figures/"))
    write_new_files(paste0(path_to_pkg_rt_1L_chr,"/man/figures"),
                    source_paths_ls = list(path_to_pkg_logo_1L_chr),
                    fl_nm_1L_chr = "logo.png")
  }
  if(on_cran_1L_lgl){
    cran_install_chr <- c("To install the latest production version of this software, run the following command in your R console:",
                          "",
                          "```r",
                          "utils::install.packages(\"",
                          dev_pkg_nm_1L_chr,
                          "\")",
                          "",
                          "```",
                          "")
  }else{
    cran_install_chr <- character(0)
  }
  readme_chr <- c(paste0("# ",
                         dev_pkg_nm_1L_chr,
                         ifelse(is.na(path_to_pkg_logo_1L_chr),
                                "",
                                " <img src=\"man/figures/fav120.png\" align=\"right\" />")),
                  "",
                  paste0("## ",utils::packageDescription(dev_pkg_nm_1L_chr,
                                                         fields ="Title") %>%
                           stringr::str_replace_all("\n"," ")),
                  "",
                  "<!-- badges: start -->",
                  "<!-- badges: end -->" ,
                  "",
                  utils::packageDescription(dev_pkg_nm_1L_chr,fields ="Description"),
                  "",
                  cran_install_chr,
                  "To install a development version of this software, run the following commands in your R console:",
                  "",
                  "```r",
                  "utils::install.packages(\"devtools\")",
                  "",
                  paste0("devtools::install_github(\"",gh_repo_1L_chr,"\")"),
                  "",
                  "```")
  write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr,"/README.md"),
                  text_ls = list(readme_chr))
  if(add_gh_site_1L_lgl)
    usethis::use_github_action("pkgdown")
  if(check_type_1L_chr %in% c("gh","full","release","standard")){
    if(check_type_1L_chr %in% c("gh","standard")){
      usethis::use_github_action_check_standard()
    }else{
      if(check_type_1L_chr == "full"){
        usethis::use_github_action_check_full()
      }else{
        usethis::use_github_action_check_release()
      }
    }
  }
  if(!is.na(path_to_pkg_logo_1L_chr) & !file.exists(paste0(path_to_pkg_rt_1L_chr,"/pkgdown/favicon/apple-touch-icon-120x120.png"))){
    pkgdown::build_favicons()
  }
  write_new_files(paste0(path_to_pkg_rt_1L_chr,"/man/figures"),
                  source_paths_ls = list(paste0(path_to_pkg_rt_1L_chr,"/pkgdown/favicon/apple-touch-icon-120x120.png")),
                  fl_nm_1L_chr = "fav120.png")
  usethis::use_lifecycle()
  usethis::use_lifecycle_badge(lifecycle_stage_1L_chr)
  if(!is.null(addl_badges_ls)){
    badges_chr <- purrr::map2_chr(addl_badges_ls,
                      names(addl_badges_ls),
                      ~{
                        badges_lup %>%
                          dplyr::filter(badge_names_chr == .y) %>%
                          get_from_lup_obj(match_value_xx = .x,
                                           match_var_nm_1L_chr = "label_names_chr",
                                           target_var_nm_1L_chr = "badges_chr",
                                           evaluate_lgl = F)
                      }) %>% unname()
    purrr::walk2(badges_chr,
                 names(addl_badges_ls),
                ~ {
                  badge_1L_chr <- .x
                  badge_nm_1L_chr <- .y
                  break_points_ls <- badge_1L_chr  %>% stringr::str_locate_all("\\]\\(")
                  purrr::walk(break_points_ls,
                              ~ {
                                src_1L_chr <- stringr::str_sub(badge_1L_chr, start = .x[1,2] %>% as.vector() + 1,
                                                 end = .x[2,1] %>% as.vector() - 2)
                                href_1L_chr <- stringr::str_sub(badge_1L_chr, start = .x[2,2] %>% as.vector() + 1,
                                                                end = -2)
                                usethis::use_badge(badge_name = badge_nm_1L_chr,
                                                   src = src_1L_chr,
                                                   href = href_1L_chr)
                              })
                })

  }
  devtools::document()
  devtools::load_all()
}
write_pt_lup_db <- function(R_dir_1L_chr = "R"){
  write_from_tmp(system.file("db_pt_lup.R",package="ready4fun"),
                 dest_paths_chr = paste0(R_dir_1L_chr,"/db_pt_lup.R"))
}
write_std_imp <- function(R_dir_1L_chr = "R",
                          package_1L_chr){
  write_from_tmp(c(system.file("pkg_tmp.R", package="ready4fun"),
                   system.file("imp_fns_tmp.R",package="ready4fun"),
                   system.file("imp_mthds_tmp.R",package="ready4fun")),
                 dest_paths_chr = c(paste0(R_dir_1L_chr,"/pkg_",package_1L_chr,".R"),
                                    paste0(R_dir_1L_chr,"/imp_fns.R"),
                                    paste0(R_dir_1L_chr,"/imp_mthds.R")),
                 edit_fn_ls = list(function(txt_chr,
                                            package_1L_chr){
                   pkg_desc_ls <- utils::packageDescription(package_1L_chr)
                   # txt_chr <- purrr::map_chr(txt_chr,
                   #                           ~ stringr::str_replace_all(.x,
                   #                                                      "ready4fun",
                   #                                                      package_1L_chr))
                   # txt_chr[1] <- paste0("#' ",
                   #                      package_1L_chr,
                   #                      ": ",
                   #                      pkg_desc_ls$Title %>%
                   #                        stringr::str_replace_all("\n","\n#' "))
                   # txt_chr[3] <- paste0("#' ",
                   #                      pkg_desc_ls$Description %>%
                   #                        stringr::str_replace_all("\n","\n#' "))
                   txt_chr
                   },
                   NULL,
                   NULL),
                 args_ls_ls = list(list(package_1L_chr = package_1L_chr),
                                   NULL,
                                   NULL)
  )
}
write_tb_to_csv <- function(tbs_r4,
                            slot_nm_1L_chr,
                            r4_name_1L_chr,
                            lup_dir_1L_chr,
                            pfx_1L_chr){
  methods::slot(tbs_r4,slot_nm_1L_chr) %>%
    dplyr::mutate_if(is.list,.funs = dplyr::funs(ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
    utils::write.csv(file = paste0(lup_dir_1L_chr,"/",pfx_1L_chr,"_",slot_nm_1L_chr,".csv"),
              row.names = F)
}
write_to_delete_dirs <- function(dir_paths_chr){
  dir_paths_chr <- dir_paths_chr[dir_paths_chr %>% purrr::map_lgl(~dir.exists(.x))]
  if(!identical(dir_paths_chr, character(0))){
    fls_to_be_purged_chr <- dir_paths_chr %>%
      purrr::map(~list.files(.x, full.names = TRUE)) %>%
      purrr::flatten_chr()
    message(paste0("Are you sure that you want to delete the following director",
                   ifelse(length(dir_paths_chr)>1,"ies","y"),
                   ":\n",
                   dir_paths_chr %>% paste0(collapse = "\n"),
                   ifelse(identical(fls_to_be_purged_chr, character(0)),
                          "",
                          paste0(" and the following file",
                                 ifelse(length(fls_to_be_purged_chr)>1,
                                        "s:\n",
                                        ":\n"),
                                 fls_to_be_purged_chr %>% paste0(collapse = "\n"))),
                   " from your machine: \n",
                   "?"))
    consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to delete ",
                                                       ifelse(length(dir_paths_chr) > 1,
                                                              "these directories",
                                                              "this directory"),
                                                       ifelse(length(fls_to_be_purged_chr) > 0,
                                                              ifelse(length(fls_to_be_purged_chr) > 0,
                                                                     " and files",
                                                                     " and file"),
                                                              ""),
                                                       ":"),
                                  options_chr = c("Y", "N"),
                                  force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      dir_paths_chr %>%
        purrr::walk(~unlink(.x, recursive=TRUE))
    }else{
      message("Delete directory request cancelled - no directories deleted")
    }
  }
}
write_to_delete_fls <- function(file_paths_chr){
  file_paths_chr <- file_paths_chr[file_paths_chr %>% purrr::map_lgl(~file.exists(.x))]
  if(!identical(file_paths_chr, character(0))){
    message(paste0("Are you sure that you want to delete the following file",
                   ifelse(length(file_paths_chr)>1,"s",""),
                   " from your machine: \n",
                   file_paths_chr %>% paste0(collapse = "\n"),
                   "?"))
    consent_1L_chr <-  make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you want to delete ",
                                                        ifelse(length(file_paths_chr)>1,"these files:","this file:")),
                                   options_chr = c("Y", "N"),
                                   force_from_opts_1L_chr = T)
    if(consent_1L_chr %in% c("Y")){
      paths_ls <- do.call(file.remove, list(file_paths_chr))
    }else{
      message("Delete files request cancelled - no files deleted")
    }
  }
}
write_to_publish_dv_ds <- function(dv_ds_1L_chr){
  consent_1L_chr <- make_prompt(prompt_1L_chr=paste0("Do you confirm ('Y') that you wish to publish the current draft of dataverse ",
                                                     dv_ds_1L_chr,
                                                     "?"),
                                options_chr = c("Y", "N"),
                                force_from_opts_1L_chr = T)
  if(consent_1L_chr == "Y"){
    dataverse::publish_dataset(dv_ds_1L_chr,
                               minor = F)
  }
}
write_to_remove_collate <- function(description_chr){
  if(!identical(which(description_chr=="Collate: "),integer(0)))
    description_chr <- description_chr[1:(which(description_chr=="Collate: ")-1)]
  return(description_chr)
}
write_to_replace_fn_nms <- function(rename_tb,
                                    undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(),
                                    rt_dev_dir_path_1L_chr = normalizePath("../../../"),
                                    dev_pkg_nm_1L_chr = get_dev_pkg_nm()){
  if(any(rename_tb$duplicated_lgl))
    stop("Duplicates in rename table")
  rename_tb <- rename_tb %>%
    dplyr::filter(fns_chr != new_nm) %>%
    dplyr::select(fns_chr,new_nm)
  purrr::pwalk(rename_tb, ~  {
    pattern_1L_chr <- ..1
    replacement_1L_chr <- ..2
    purrr::walk(undocumented_fns_dir_chr,
                ~ xfun::gsub_dir(undocumented_fns_dir_chr,
                                 pattern = pattern_1L_chr,
                                 replacement = replacement_1L_chr))
    xfun::gsub_dir(dir = rt_dev_dir_path_1L_chr,
                   pattern = paste0(dev_pkg_nm_1L_chr,"::",pattern_1L_chr),
                   replacement = paste0(dev_pkg_nm_1L_chr,"::",replacement_1L_chr),
                   ext = "R",
                   fixed = T)
  })
}
write_to_replace_sfx_pair <- function(args_nm_chr,
                                      sfcs_chr,
                                      replacements_chr,
                                      file_path_1L_chr = NA_character_,
                                      dir_path_1L_chr = NA_character_){
  fn <- ifelse(is.na(file_path_1L_chr),xfun::gsub_dir,xfun::gsub_file)
  path_chr <- ifelse(is.na(file_path_1L_chr),dir_path_1L_chr,file_path_1L_chr)
  args_ls <- list(pattern = paste0(args_nm_chr[1],
                                   "(?!",
                                   stringr::str_remove(sfcs_chr[2],
                                                       sfcs_chr[1]),
                                   ")"),
                  replacement = paste0(stringr::str_remove(args_nm_chr[1],
                                                           sfcs_chr[1]),
                                       replacements_chr[1]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
  args_ls <- list(pattern = args_nm_chr[2],
                  replacement = paste0(stringr::str_remove(args_nm_chr[2],
                                                           sfcs_chr[2]),
                                       replacements_chr[2]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
}
write_to_rpl_1L_and_indefL_sfcs <- function(indefL_arg_nm_1L_chr,
                                            file_path_1L_chr = NA_character_,
                                            dir_path_1L_chr = NA_character_){
  sfcs_chr <- c(indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8, end=-5),
                indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8))
  write_to_replace_sfx_pair(args_nm_chr =   paste0(indefL_arg_nm_1L_chr %>% stringr::str_sub(end=-9),
                                                   sfcs_chr),
                            sfcs_chr = sfcs_chr,
                            replacements_chr = paste0(c("_1L",""),sfcs_chr[1]),
                            file_path_1L_chr = file_path_1L_chr,
                            dir_path_1L_chr = dir_path_1L_chr)

}
write_to_reset_pkg_files <- function(delete_contents_of_1L_chr,
                                     package_1L_chr = get_dev_pkg_nm(getwd()),
                                     package_dir_1L_chr = getwd(),
                                     description_ls = NULL,
                                     keep_version_lgl = T
                                     ){
  devtools::load_all()
  if(keep_version_lgl){
    desc_ls <- utils::packageDescription(package_1L_chr)
    description_ls$Version <- desc_ls$Version
  }
  usethis::use_description(fields = description_ls)
  #file.remove(paste0(package_dir_1L_chr,"/NAMESPACE"))
  file_paths_chr <- c(paste0(package_dir_1L_chr,"/NAMESPACE"),
                         list.files(paste0(package_dir_1L_chr,"/",delete_contents_of_1L_chr), full.names = TRUE))
  write_to_delete_fls(file_paths_chr)
  # do.call(file.remove, fl_paths_chr)
  devtools::document()
  devtools::load_all()
}
write_vignette <- function(package_1L_chr,
                           pkg_rt_dir_chr = "."){
  write_new_dirs(paste0(pkg_rt_dir_chr,"/vignettes"))
  # if(!dir.exists(paste0(pkg_rt_dir_chr,"/vignettes")))
  #   dir.create(paste0(pkg_rt_dir_chr,"/vignettes"))
  # write_from_tmp(system.file("ready4fun.Rmd",package="ready4fun"),
  #                dest_paths_chr = paste0(pkg_rt_dir_chr,"/vignettes/",package_1L_chr,".Rmd"),
  #                edit_fn_ls = list(function(txt_chr,
  #                                           package_1L_chr){
  #                  txt_chr <- purrr::map_chr(txt_chr,
  #                                            ~ stringr::str_replace_all(.x,
  #                                                                         "ready4fun",
  #                                                                         package_1L_chr))
  #                    txt_chr
  #                  },
  #                  args_ls_ls = list(list(package_1L_chr = package_1L_chr))))
  # write_from_tmp(system.file(".gitignore",package="ready4fun"),
  #                dest_paths_chr = paste0(pkg_rt_dir_chr,"/vignettes/",".gitignore"),
  #                  edit_fn_ls = list(function(txt_chr, package_1L_chr){
  #                    txt_chr
  #                  }),
  #                  args_ls_ls = list(list(package_1L_chr = package_1L_chr)))
  write_from_tmp(c(system.file("ready4fun.Rmd",package="ready4fun"),
                   system.file(".gitignore",package="ready4fun")),
                 dest_paths_chr = c(paste0(pkg_rt_dir_chr,"/vignettes/",package_1L_chr,".Rmd"),
                                    paste0(pkg_rt_dir_chr,"/vignettes/",".gitignore")),
                 edit_fn_ls = list(function(txt_chr,
                                            package_1L_chr){
                   txt_chr <- purrr::map_chr(txt_chr,
                                             ~ stringr::str_replace_all(.x,
                                                                        "ready4fun",
                                                                        package_1L_chr))
                   txt_chr
                   },
                   function(txt_chr, package_1L_chr){
                   txt_chr
                     }),
                 args_ls_ls = list(list(package_1L_chr = package_1L_chr),
                                   list(list(package_1L_chr = package_1L_chr)))
                 )
}
write_ws <- function(path_1L_chr){
  top_level_chr <- paste0(path_1L_chr,
                          "/ready4/",
                          c("Code", "Data","Documentation", "Insight"))
  code_top_lvl_chr <- c("Application","Authoring","Brochure","Description","Modelling","Prediction") %>%
    purrr::map_chr(~paste0(top_level_chr[1],"/",.x))
  code_sub_dirs_chr <- c(paste0(code_top_lvl_chr[2],"/Workflows",c("","/R")),
                         paste0(code_top_lvl_chr[3],"/HTML"),
                         paste0(code_top_lvl_chr[4],"/Datatypes",c("","/R")),
                         paste0(code_top_lvl_chr[5],"/Templates",c("","/R")),
                         paste0(code_top_lvl_chr[6],"/Example",c("","/Toolkit_1","/Toolkit_1/R")))
  data_top_lvl_chr <- c("Dataverse","Project","R_Format","Raw_Format") %>%
    purrr::map_chr(~paste0(top_level_chr[2],"/",.x))
  data_sub_dirs_chr <- c("Agents","Attributes","Geometries","Metadata") %>%
    purrr::map_chr(~paste0(data_top_lvl_chr[4],"/",.x))
  dcmntn_top_lvl_chr <- c("Code", "Data","Images") %>%
    purrr::map_chr(~paste0(top_level_chr[3],"/",.x))
  dcmntn_sub_dirs_chr <- c("Developer", "User") %>%
    purrr::map_chr(~paste0(dcmntn_top_lvl_chr[1],"/",.x))
  insight_top_lvl_chr <- c("Analysis","Science","Team") %>%
    purrr::map_chr(~paste0(top_level_chr[4],"/",.x))
  new_dirs_chr <- c(paste0(path_1L_chr,"/ready4"),
                    top_level_chr,
                    code_top_lvl_chr, code_sub_dirs_chr,
                    data_top_lvl_chr, data_sub_dirs_chr,
                    dcmntn_top_lvl_chr, dcmntn_sub_dirs_chr,
                    insight_top_lvl_chr)
  write_new_dirs(new_dirs_chr)
}
