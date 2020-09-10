write_abbr_lup <- function(short_name_chr = NA_character_,
                           long_name_chr = NA_character_,
                           no_plural_chr = NA_character_,
                           custom_plural_ls = NULL,
                           overwrite_1L_lgl = T,
                           seed_lup = NULL,
                           url_1L_chr,
                           pkg_nm_1L_chr = get_dev_pkg_nm()){
  if(is.null(seed_lup)){
    data("object_type_lup",package="ready4fun",envir = environment())
    seed_lup <- object_type_lup
  }
  update_abbr_lup(seed_lup,
                  short_name_chr = short_name_chr,
                  long_name_chr = long_name_chr,
                  no_plural_chr = no_plural_chr,
                  custom_plural_ls = custom_plural_ls) %>%
    write_and_doc_ds(db = .,
                     overwrite_1L_lgl = overwrite_1L_lgl,
                     db_1L_chr = "abbreviations_lup",
                     title_1L_chr = "Common abbreviations lookup table",
                     desc_1L_chr = paste0("A lookup table for abbreviations commonly used in object names in the ",pkg_nm_1L_chr,"package."),
                     format_1L_chr = "A tibble",
                     url_1L_chr = url_1L_chr,
                     abbreviations_lup = .)
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
write_and_doc_ds <- function(db,
                               overwrite_1L_lgl = T,
                               db_1L_chr,
                               title_1L_chr,
                               desc_1L_chr,
                               format_1L_chr = "A tibble",
                               url_1L_chr = NA_character_,
                               vars_ls = NULL,
                               R_dir_1L_chr = "R",
                               abbreviations_lup = NULL,
                               object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  eval(parse(text=paste0(db_1L_chr,"<-db")))
  eval(parse(text=paste0("usethis::use_data(",
                         db_1L_chr,
                         ", overwrite = overwrite_1L_lgl)")))
  sink(paste0(R_dir_1L_chr,"/db_",db_1L_chr,".R"), append = F)
  write_ds_dmt(db=db,
                 db_1L_chr = db_1L_chr,
                 title_1L_chr = title_1L_chr,
                 desc_1L_chr = desc_1L_chr,
                 format_1L_chr = format_1L_chr,
                 vars_ls = vars_ls,
                 url_1L_chr = url_1L_chr,
                 R_dir_1L_chr = R_dir_1L_chr,
                 abbreviations_lup = abbreviations_lup,
                 object_type_lup = object_type_lup)
  close_open_sinks()
  devtools::document()
  devtools::load_all()
}
write_and_doc_fn_fls <- function(fns_dmt_tb,
                                   r_dir_1L_chr = "R",
                                 path_to_pkg_rt_1L_chr = ".",
                                   path_to_user_dmt_dir_1L_chr = "../../../../Documentation/Code/User",
                                   path_to_dvpr_dmt_dir_1L_chr = "../../../../Documentation/Code/Developer",
                                   make_pdfs_1L_lgl = T,
                                 dev_pkgs_chr = NA_character_,
                                 update_pkgdown_1L_lgl = T){
  purrr::walk2(list(path_to_dvpr_dmt_dir_1L_chr,
                    path_to_user_dmt_dir_1L_chr),
               c(T,F),
               ~ {
                 write_fn_fl(fns_dmt_tb,
                               r_dir_1L_chr = r_dir_1L_chr,
                               document_unexp_lgl = .y)
                 devtools::document()
                 devtools::load_all()
                 write_ns_imps_to_desc(dev_pkgs_chr = dev_pkgs_chr,
                                       incr_ver_1L_lgl = .y)
                 devtools::load_all()
                 if(make_pdfs_1L_lgl)
                 devtools::build_manual(path = .x)
               })
  if(update_pkgdown_1L_lgl){
    writeLines(c("development:",
                 "  mode: auto",
                 "reference:",
                 "- title: \"Datasets\"",
                 "- contents:",
                 paste0("  - ",data(package=get_dev_pkg_nm())$results[,3]),
                 {
                   fns_chr <- dplyr::filter(fns_dmt_tb, inc_for_main_user_lgl & file_pfx_chr == "fn_") %>%
                     dplyr::pull(fns_chr)
                   if(length(fns_chr)>0)
                     c( "- title: \"Functions\"",
                        "- contents:",
                        paste0("  - ",fns_chr))
                 }),
               con = paste0(path_to_pkg_rt_1L_chr,"/_pkgdown.yml"))
    pkgdown::build_site()
  }
}
write_dmtd_fn_type_lup <- function(fn_type_lup_tb = make_fn_type_lup(),
                                   overwrite_1L_lgl = T,
                                   pkg_nm_1L_chr = get_dev_pkg_nm(),
                                   url_1L_chr = url_1L_chr,
                                   abbreviations_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  fn_type_lup_tb %>%
    write_and_doc_ds(overwrite_1L_lgl = overwrite_1L_lgl,
                     db_1L_chr = "fn_type_lup_tb",
                     title_1L_chr = "Function type lookup table",
                     desc_1L_chr = paste0("A lookup table to find descriptions for different types of functions used within the ",pkg_nm_1L_chr," package suite."),
                     format_1L_chr = "A tibble",
                     url_1L_chr = url_1L_chr,
                     abbreviations_lup = abbreviations_lup)
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
write_ds_dmt <- function(db,
                           db_1L_chr,
                           title_1L_chr,
                           desc_1L_chr,
                           format_1L_chr = "A tibble",
                           url_1L_chr = NA_character_,
                           vars_ls = NULL,
                           R_dir_1L_chr = "R",
                           abbreviations_lup = NULL,
                           object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  auto_vars_ls <- names(db) %>%
    purrr::map(~ make_arg_desc(.x,
                               object_type_lup = object_type_lup,
                               abbreviations_lup = abbreviations_lup)) %>%
    stats::setNames(names(db))
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
write_fn_fl <- function(fns_dmt_tb,
                          r_dir_1L_chr = "R",
                          document_unexp_lgl = T){
  file_nms_chr <- fns_dmt_tb$file_nm_chr %>% unique()
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
                                    fn <- eval(parse(text=tb[[.x,1]]))
                                    fn_chr <- deparse(fn)
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
                                    close_open_sinks()
                                  })
                  }
    )
}
write_fn_type_dirs <- function(path_1L_chr = "data-raw"){
  undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr)
  paths_ls <- undocumented_fns_dir_chr %>% purrr::walk(~{
    if(!dir.exists(.x))
      dir.create(.x)
  })
}
write_from_tmp <- function(temp_path_1L_chr,
                             dest_path_1L_chr,
                             edit_fn = function(x){x},
                             args_ls = NULL){
  fileConn <- file(temp_path_1L_chr)
  txt_chr <- readLines(fileConn)
  close(fileConn)
  txt_chr <- rlang::exec(edit_fn, txt_chr, !!!args_ls)
  if(temp_path_1L_chr == dest_path_1L_chr)
    file.remove(temp_path_1L_chr)
  fileConn <- file(dest_path_1L_chr)
  writeLines(txt_chr, fileConn)
  close(fileConn)
}
write_new_arg_sfxs <- function(arg_nms_chr,
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
  updated_sfxs_chr <- arg_nms_chr[arg_nms_chr %>% endsWith("_vec")] %>% stringr::str_sub(start=-8) %>% unique()
  fn_nms_to_upd_chr <- updated_fns_chr[updated_fns_chr %>% stringr::str_sub(start=-8) %in% updated_sfxs_chr]
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
write_pkg <- function(package_1L_chr,
                        R_dir_1L_chr = "R"){
  write_from_tmp(system.file("pkg_ready_fun.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/pkg_",package_1L_chr,".R"),
                   edit_fn = function(txt_chr,
                                      package_1L_chr){
                     pkg_desc_ls <- packageDescription(package_1L_chr)
                     txt_chr <- purrr::map_chr(txt_chr,
                                               ~ stringr::str_replace_all(.x,
                                                                          "ready4fun",
                                                                          package_1L_chr))
                     txt_chr[1] <- paste0("#' ",package_1L_chr,": ",pkg_desc_ls$Title %>%
                                            stringr::str_replace_all("\n","\n#' "))
                     txt_chr[3] <- paste0("#' ",pkg_desc_ls$Description %>%
                                            stringr::str_replace_all("\n","\n#' "))
                     txt_chr
                   },
                   args_ls = list(package_1L_chr = package_1L_chr))
}
write_pkg_setup_fls <- function(path_to_pkg_rt_1L_chr = ".",
                                  dev_pkg_nm_1L_chr = get_dev_pkg_nm(),
                                  incr_ver_1L_lgl = T,
                                delete_contents_of_R_dir = F,
                                copyright_holders_chr,
                                use_travis_1L_lgl = T,
                                path_to_pkg_logo_1L_chr = NA_character_,
                                github_repo_1L_chr,
                                lifecycle_stage_1L_chr = "experimental"){
  if(delete_contents_of_R_dir)
    write_to_reset_pkg_files(paste0(path_to_pkg_rt_1L_chr,"/R"))
  update_desc_fl_1L_lgl <- !is.na(dev_pkg_nm_1L_chr)
  if(!update_desc_fl_1L_lgl)
    dev_pkg_nm_1L_chr <- get_dev_pkg_nm(path_to_pkg_rt_1L_chr)
  devtools::load_all(path_to_pkg_rt_1L_chr)
  write_pkg(dev_pkg_nm_1L_chr,R_dir_1L_chr = paste0(path_to_pkg_rt_1L_chr,"/R"))
  write_std_imp(paste0(path_to_pkg_rt_1L_chr,"/R"))
  if(update_desc_fl_1L_lgl){
    desc_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"))
    desc_1L_chr[1] <- paste0("Package: ",dev_pkg_nm_1L_chr)
    sink(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"), append = F)
    writeLines(desc_1L_chr)
    close_open_sinks()
  }
  if(!file.exists(paste0(path_to_pkg_rt_1L_chr,
                         "/vignettes/",
                         get_dev_pkg_nm(),
                         ".Rmd")))
    write_vignette(dev_pkg_nm_1L_chr, pkg_rt_dir_chr = path_to_pkg_rt_1L_chr)
  if(incr_ver_1L_lgl){
    usethis::use_version()
  }
  usethis::use_gpl3_license(copyright_holders_chr)
  usethis::use_pkgdown()
  if(!is.na(path_to_pkg_logo_1L_chr)){
    if(!dir.exists(paste0(path_to_pkg_rt_1L_chr,"/man/figures/")))
      dir.create(paste0(path_to_pkg_rt_1L_chr,"/man/figures/"))
    file.copy(path_to_pkg_logo_1L_chr,
              paste0(path_to_pkg_rt_1L_chr,"/man/figures/logo.png"))
  }
  writeLines(c(paste0("# ",get_dev_pkg_nm(),ifelse(is.na(path_to_pkg_logo_1L_chr),
                                                   "",
                                                   " <img src=\"man/figures/fav120.png\" align=\"right\" />")),
               "",
               paste0("## ",packageDescription(get_dev_pkg_nm(),fields ="Title") %>% stringr::str_replace_all("\n"," ")),
               "",
               packageDescription(get_dev_pkg_nm(),fields ="Description"),
               "",
               "If you plan on testing this software you can install it by running the following commands in your R console:",
               "",
               "install.packages(\"devtools\")",
               "",
               paste0("devtools::install_github(\"",github_repo_1L_chr,"\")"),
               "",
               "<!-- badges: start -->",
               "<!-- badges: end -->" ),
             con = "README.md")
  if(use_travis_1L_lgl){
    usethis::use_travis()
    sink(file = paste0(path_to_pkg_rt_1L_chr,
                       "/.travis.yml"), append = T)
    writeLines("warnings_are_errors: false")
    close_open_sinks()
  }
  if(!is.na(path_to_pkg_logo_1L_chr) & !file.exists(paste0(path_to_pkg_rt_1L_chr,"/pkgdown/favicon/apple-touch-icon-120x120.png"))){
    pkgdown::build_favicons()
    file.copy(paste0(path_to_pkg_rt_1L_chr,"/pkgdown/favicon/apple-touch-icon-120x120.png"),
              paste0(path_to_pkg_rt_1L_chr,"/man/figures/fav120.png"))
  }
  usethis::use_lifecycle()
  usethis::use_lifecycle_badge(lifecycle_stage_1L_chr)
}
write_pt_lup_db <- function(R_dir_1L_chr = "R"){
  write_from_tmp(system.file("db_pt_lup.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/db_pt_lup.R"))
}
write_std_imp <- function(R_dir_1L_chr = "R"){
  write_from_tmp(system.file("imp_pipe_tmp.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/imp_pipe.R"))
  write_from_tmp(system.file("imp_mthds_tmp.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/imp_mthds.R"))
}
write_tb_to_csv <- function(tbs_r4,
                            slot_nm_1L_chr,
                            r4_name_1L_chr,
                            lup_dir_1L_chr,
                            pfx_1L_chr){
  methods::slot(tbs_r4,slot_nm_1L_chr) %>%
    dplyr::mutate_if(is.list,.funs = dplyr::funs(ifelse(stringr::str_c(.)=="NULL",NA_character_ , stringr::str_c (.)))) %>%
    write.csv(file = paste0(lup_dir_1L_chr,"/",pfx_1L_chr,"_",slot_nm_1L_chr,".csv"),
              row.names = F)
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
                                      sfxs_chr,
                                      replacements_chr,
                                      file_path_1L_chr = NA_character_,
                                      dir_path_1L_chr = NA_character_){
  fn <- ifelse(is.na(file_path_1L_chr),xfun::gsub_dir,xfun::gsub_file)
  path_chr <- ifelse(is.na(file_path_1L_chr),dir_path_1L_chr,file_path_1L_chr)
  args_ls <- list(pattern = paste0(args_nm_chr[1],
                                   "(?!",
                                   stringr::str_remove(sfxs_chr[2],
                                                       sfxs_chr[1]),
                                   ")"),
                  replacement = paste0(stringr::str_remove(args_nm_chr[1],
                                                           sfxs_chr[1]),
                                       replacements_chr[1]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
  args_ls <- list(pattern = args_nm_chr[2],
                  replacement = paste0(stringr::str_remove(args_nm_chr[2],
                                                           sfxs_chr[2]),
                                       replacements_chr[2]),
                  perl=T)
  rlang::exec(fn, path_chr, !!!args_ls)
}
write_to_rpl_1L_and_indefL_sfcs <- function(indefL_arg_nm_1L_chr,
                                            file_path_1L_chr = NA_character_,
                                            dir_path_1L_chr = NA_character_){
  sfxs_chr <- c(indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8, end=-5),
                indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8))
  write_to_replace_sfx_pair(args_nm_chr =   paste0(indefL_arg_nm_1L_chr %>% stringr::str_sub(end=-9),
                                                   sfxs_chr),
                            sfxs_chr = sfxs_chr,
                            replacements_chr = paste0(c("_1L",""),sfxs_chr[1]),
                            file_path_1L_chr = file_path_1L_chr,
                            dir_path_1L_chr = dir_path_1L_chr)

}
write_to_reset_pkg_files <- function(delete_contents_of_1L_chr,
                                     package_1L_chr = get_dev_pkg_nm(),
                                     package_dir_1L_chr = getwd(),
                                     description_ls = NULL,
                                     keep_version_lgl = T
                                     ){
  devtools::load_all()
  if(keep_version_lgl){
    desc_ls <- packageDescription(package_1L_chr)
    description_ls$Version <- desc_ls$Version
  }
  usethis::use_description(fields = description_ls)
  file.remove(paste0(package_dir_1L_chr,"/NAMESPACE"))
  do.call(file.remove, list(list.files(paste0(package_dir_1L_chr,"/",delete_contents_of_1L_chr), full.names = TRUE)))
  devtools::document()
  devtools::load_all()
}
write_vignette <- function(package_1L_chr,
                             pkg_rt_dir_chr = "."){
  if(!dir.exists(paste0(pkg_rt_dir_chr,"/vignettes")))
    dir.create(paste0(pkg_rt_dir_chr,"/vignettes"))
  write_from_tmp(system.file("ready4fun.Rmd",package="ready4fun"),
                   dest_path_1L_chr = paste0(pkg_rt_dir_chr,"/vignettes/",package_1L_chr,".Rmd"),
                   edit_fn = function(txt_chr,
                                      package_1L_chr){
                     txt_chr <- purrr::map_chr(txt_chr,
                                               ~ stringr::str_replace_all(.x,
                                                                          "ready4fun",
                                                                          package_1L_chr))
                     txt_chr
                   },
                   args_ls = list(package_1L_chr = package_1L_chr))
  write_from_tmp(system.file(".gitignore",package="ready4fun"),
                   dest_path_1L_chr = paste0(pkg_rt_dir_chr,"/vignettes/",".gitignore"),
                   edit_fn = function(txt_chr, package_1L_chr){
                     txt_chr
                   },
                   args_ls = list(package_1L_chr = package_1L_chr))
}
write_ws <- function(path_1L_chr){
  dir.create(paste0(path_1L_chr,"/Readyforwhatsnext"))
  top_level_chr <- paste0(path_1L_chr,"/Readyforwhatsnext/",c("Code", "Data","Documentation", "Insight"))
  top_level_chr %>%
    purrr::walk(~ dir.create(.x))
  c("Framework", "Models") %>%
    purrr::walk(~ {
      dir.create(paste0(top_level_chr[1],"/",.x))
      dir.create(paste0(top_level_chr[1],"/",.x,"/R"))
    })
  c("Dataverse","Project","R_Format","Raw_Format") %>%
    purrr::walk(~dir.create(paste0(top_level_chr[2],"/",.x)))
  c("Agents","Attributes","Geometries","Metadata") %>%
    purrr::walk(~dir.create(paste0(top_level_chr[2],"/Raw_Format/",.x)))
  c("Code", "Data") %>%
    purrr::walk(~dir.create(paste0(top_level_chr[3],"/",.x)))
  c("Developer", "User") %>%
    purrr::walk(~dir.create(paste0(top_level_chr[3],"/Code/",.x)))
  c("Analysis","Apps","Pages","Scientific Summaries") %>%
    purrr::walk(~dir.create(paste0(top_level_chr[4],"/",.x)))
}
