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
write_and_doc_ds_R <- function(db,
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
  write_ds_dmt_R(db=db,
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
write_and_doc_fn_fls_R <- function(fns_dmt_tb,
                                   r_dir_1L_chr = "R",
                                   path_to_user_dmt_dir_1L_chr = "../../../../Documentation/Code/User",
                                   path_to_dvpr_dmt_dir_1L_chr = "../../../../Documentation/Code/Developer",
                                   make_pdfs_1L_lgl = T){
  purrr::walk2(list(path_to_dvpr_dmt_dir_1L_chr,
                    path_to_user_dmt_dir_1L_chr),
               c(T,F),
               ~ {
                 write_fn_fl_R(fns_dmt_tb,
                               r_dir_1L_chr = r_dir_1L_chr,
                               document_unexp_lgl = .y)
                 devtools::document()
                 devtools::load_all()
                 if(make_pdfs_1L_lgl)
                 devtools::build_manual(path = .x)
               })

}
write_documented_fns_R <- function(tmp_fn_dir_1L_chr,
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
write_ds_dmt_R <- function(db,
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
    purrr::map(~ make_arg_desc_1L_chr(paste0(.x#,"_vec"
                                          ),
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

write_fn_dmt <- function(fn_name_1L_chr,
                         fn_type_1L_chr,
                         fn = NULL,
                         fn_desc_1L_chr = NA_character_,
                         fn_out_type_1L_chr = NA_character_,
                         fn_title_1L_chr = NA_character_,
                         example_1L_lgl = F,
                         export_1L_lgl = T,
                         class_name_chr = "",
                         details_chr = "DETAILS",
                         args_ls = NULL,
                         import_chr = NA_character_,
                         doc_in_class_1L_lgl = F,
                         abbreviations_lup = NULL,
                         object_type_lup = NULL){
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  fn_tags_spine_ls <- make_fn_dmt_spine_chr_ls(fn_name_1L_chr = fn_name_1L_chr,
                                               fn_type_1L_chr = fn_type_1L_chr,
                                               fn_title_1L_chr = fn_title_1L_chr,
                                               fn = fn,
                                               example_1L_lgl = example_1L_lgl,
                                               export_1L_lgl = export_1L_lgl,
                                               details_chr = details_chr,
                                               class_name_chr = class_name_chr,
                                               doc_in_class_1L_lgl = doc_in_class_1L_lgl)
  new_tag_chr_ls <- make_new_fn_dmt_chr_ls(fn_type_1L_chr = fn_type_1L_chr,
                                           fn_name_1L_chr = fn_name_1L_chr,
                                           fn_desc_1L_chr = fn_desc_1L_chr,
                                           fn_det_chr = details_chr,
                                           fn_out_type_1L_chr = fn_out_type_1L_chr,
                                           args_ls = args_ls,
                                           fn,
                                           abbreviations_lup = abbreviations_lup,
                                           object_type_lup = object_type_lup)
  fn_tags_chr <- update_fn_dmt_chr(fn_tags_spine_ls = fn_tags_spine_ls,
                                   new_tag_chr_ls = new_tag_chr_ls,
                                   fn_name_1L_chr = fn_name_1L_chr,
                                   fn_type_1L_chr = fn_type_1L_chr,
                                   import_chr = import_chr)
  writeLines(fn_tags_chr)
}
write_fn_fl_R <- function(fns_dmt_tb,
                          r_dir_1L_chr = "R",
                          document_unexp_lgl = T){
  file_nms_chr <- fns_dmt_tb$file_nm_chr %>% unique()
  file_nms_chr %>%
    purrr::walk(~
                  {
                    tb <- fns_dmt_tb %>%
                      dplyr::filter(file_nm_chr == .x)
                    first_lgl_vec <- c(T,rep(F,nrow(tb)-1))
                    dest_path_1L_chr <- paste0(r_dir_1L_chr,"/",tb$file_pfx_1L_chr[1],.x)
                    purrr::walk(1:nrow(tb),
                                ~
                                  {
                                    fn <- eval(parse(text=tb[[.x,1]]))
                                    fn_chr <- deparse(fn)
                                    sink(dest_path_1L_chr, append =  !first_lgl_vec[.x])
                                    write_fn_dmt(fn_name_1L_chr = tb[[.x,1]],
                                                 fn_type_1L_chr = "fn", ####
                                                 fn = fn,
                                                 fn_desc_1L_chr = tb[[.x,3]],
                                                 fn_out_type_1L_chr = tb[[.x,6]],
                                                 fn_title_1L_chr = tb[[.x,2]],
                                                 example_1L_lgl = tb[[.x,7]],
                                                 export_1L_lgl = T,#tb[[.x,5]],
                                                 class_name_chr = "",
                                                 details_chr = tb[[.x,4]],
                                                 args_ls = tb$args_ls[[.x]] %>% as.list(),
                                                 import_chr = NA_character_,
                                                 doc_in_class_1L_lgl = F)
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
write_from_tmp_R <- function(temp_path_1L_chr,
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
write_pkg_R <- function(package_1L_chr,
                        R_dir_1L_chr = "R"){
  write_from_tmp_R(system.file("pkg_ready_fun.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/pkg_",package_1L_chr,".R"),
                   edit_fn = function(txt_chr,
                                      package_1L_chr){
                     pkg_desc_ls <- packageDescription(package_1L_chr)
                     txt_chr <- purrr::map_chr(txt_chr,
                                               ~ stringr::str_replace_all(.x,
                                                                          "ready4fun",
                                                                          package_1L_chr))
                     txt_chr[1] <- paste0("#' ",package_1L_chr,": ",pkg_desc_ls$Title)
                     txt_chr[3] <- paste0("#' ",pkg_desc_ls$Description)
                     txt_chr
                   },
                   args_ls = list(package_1L_chr = package_1L_chr))
}
write_pkg_setup_fls_R <- function(path_to_pkg_rt_1L_chr = ".",
                                  dev_pkg_nm_1L_chr = NA_character_,
                                  make_tmpl_vignette_1L_lgl = F,
                                  incr_ver_1L_lgl = T){
  update_desc_fl_1L_lgl <- !is.na(dev_pkg_nm_1L_chr)
  if(!update_desc_fl_1L_lgl)
    dev_pkg_nm_1L_chr <- get_dev_pkg_nm_1L_chr(path_to_pkg_rt_1L_chr)
  devtools::load_all(path_to_pkg_rt_1L_chr)
  write_pkg_R(dev_pkg_nm_1L_chr,R_dir_1L_chr = paste0(path_to_pkg_rt_1L_chr,"/R"))
  write_std_imp_R(paste0(path_to_pkg_rt_1L_chr,"/R"))
  if(update_desc_fl_1L_lgl){
    desc_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"))
    desc_1L_chr[1] <- paste0("Package: ",dev_pkg_nm_1L_chr)
    sink(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"), append = F)
    writeLines(desc_1L_chr)
    close_open_sinks()
  }
  if(make_tmpl_vignette_1L_lgl)
    write_vignette_R(dev_pkg_nm_1L_chr, pkg_rt_dir_chr = path_to_pkg_rt_1L_chr)
  if(incr_ver_1L_lgl){
    usethis::use_version()
  }
}
write_pt_lup_db_R <- function(R_dir_1L_chr = "R"){
  write_from_tmp_R(system.file("db_pt_lup.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/db_pt_lup.R"))
}
write_std_imp_R <- function(R_dir_1L_chr = "R"){
  write_from_tmp_R(system.file("imp_pipe_tmp.R",package="ready4fun"),
                   dest_path_1L_chr = paste0(R_dir_1L_chr,"/imp_pipe.R"))
  write_from_tmp_R(system.file("imp_mthds_tmp.R",package="ready4fun"),
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
write_vignette_R <- function(package_1L_chr,
                             pkg_rt_dir_chr = "."){
  if(!dir.exists(paste0(pkg_rt_dir_chr,"/vignettes")))
    dir.create(paste0(pkg_rt_dir_chr,"/vignettes"))
  write_from_tmp_R(system.file("ready4fun.Rmd",package="ready4fun"),
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
  write_from_tmp_R(system.file(".gitignore",package="ready4fun"),
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
