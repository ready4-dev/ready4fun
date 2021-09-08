get_all_depcys_of_fns <- function(pkg_depcy_ls,
                                  fns_chr){
  arg_ls <- list(new_dbl = pkg_depcy_ls$Nomfun %>%
                   dplyr::filter(label %in% fns_chr) %>%
                   dplyr::pull(id) %>% as.numeric(),
                 solo_dbl = numeric(0),
                 upper_tb = data.frame(from = numeric(0),to=numeric(0)))
  while(!identical(arg_ls$new_dbl,numeric(0))){
    arg_ls <- make_depnt_fns_ls(arg_ls, pkg_depcy_ls = pkg_depcy_ls)
  }
  fn_idcs_dbl <- c(arg_ls$upper_tb$to,arg_ls$solo_dbl) %>% unique() %>% sort()
  fns_to_keep_chr <- pkg_depcy_ls$Nomfun %>% dplyr::filter(id %in% fn_idcs_dbl) %>% dplyr::pull(2)
  return(fns_to_keep_chr)
}
get_arg_obj_type <- function(argument_nm_1L_chr,
                             dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                             dv_url_pfx_1L_chr = NULL,
                             key_1L_chr = NULL,
                             object_type_lup = NULL,
                             server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  nchar_int <- nchar(object_type_lup$short_name_chr)
  match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_1L_chr,
                                                      paste0(ifelse(nchar(argument_nm_1L_chr)==nchar_int,"","_"),
                                                             object_type_lup$short_name_chr))]
  if(!identical(match_chr,character(0))){
    arg_obj_type_1L_chr <- dplyr::filter(object_type_lup,
                                         long_name_chr %in% match_chr) %>%
      dplyr::mutate(nchar_int = nchar(short_name_chr)) %>%
      dplyr::filter(nchar_int == max(nchar_int)) %>%
      dplyr::pull(long_name_chr)
  }else{
    arg_obj_type_1L_chr <- character(0)
  }
  return(arg_obj_type_1L_chr)
}
get_dev_pkg_nm <- function(path_to_pkg_rt_1L_chr = "."){
  dev_pkg_nm_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr,"/DESCRIPTION"))[1] %>% stringr::str_sub(start=10)
  return(dev_pkg_nm_1L_chr)
}
get_dv_fls_urls <- function(file_nms_chr,
                            dv_ds_nm_1L_chr,
                            dv_url_pfx_1L_chr = NULL,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                            key_1L_chr = NULL){
  if(is.null(dv_url_pfx_1L_chr))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label)
  urls_chr <- file_nms_chr %>%
    purrr::map_chr(~{
      idx_1L_int <- which(all_items_chr == .x)
      if(identical(idx_1L_int, integer(0))){
        NA_character_
      }else{
        paste0(dv_url_pfx_1L_chr,ds_ls[[idx_1L_int]]$dataFile$id)
      }
    })
  return(urls_chr)
}
get_fl_id_from_dv_ls <-  function (ds_ls, fl_nm_1L_chr, nms_chr = NA_character_)
{
  if (is.na(nms_chr[1])) {
    nms_chr <- purrr::map2_chr(ds_ls$files$originalFileName,
                               ds_ls$files$filename, ~ifelse(is.na(.x), .y, .x))
  }
  if (fl_nm_1L_chr %in% nms_chr) {
    id_1L_chr <- get_from_lup_obj(ds_ls$files[, names(ds_ls$files) %>% unique()] %>%
                                    tibble::as_tibble(),
                                  match_var_nm_1L_chr = ifelse(fl_nm_1L_chr %in% ds_ls$files$originalFileName,
                                                               "originalFileName",
                                                               "filename"),
                                  match_value_xx = fl_nm_1L_chr,
                                  target_var_nm_1L_chr = "id",
                                  evaluate_lgl = F)
  }
  else {
    id_1L_chr <- NA_character_
  }
  return(id_1L_chr)
}
get_fn_args <- function(fn){
  fn_args_chr <- as.list(args(fn)) %>%
    names() %>%
    purrr::discard({.==""})
  return(fn_args_chr)
}
get_fn_nms_in_file <- function(path_1L_chr){
  source(path_1L_chr, local=T)
  local_chr <- ls()
  local_chr <- local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text=.x))))]
  return(local_chr)
}
get_from_lup_obj <- function(data_lookup_tb,
                             match_value_xx,
                             match_var_nm_1L_chr,
                             target_var_nm_1L_chr,
                             evaluate_lgl = TRUE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(match_var_nm_1L_chr)==match_value_xx) %>%
    dplyr::select(!!target_var_nm_1L_chr) %>%
    dplyr::pull()
  if(evaluate_lgl){
    if(stringr::str_detect(return_object_ref,"::")){
      colon_positions <- stringr::str_locate(return_object_ref,
                                             "::")
      namespace_ref <- stringr::str_sub(return_object_ref,
                                        start=1,
                                        end=colon_positions[1,"start"]-1)
      object_ref <- stringr::str_sub(return_object_ref,
                                     start=colon_positions[1,"end"]+1)

      if(sum(stringr::str_detect(search(),paste0("package:",
                                                 namespace_ref))) == 0){
        namespace_ref_sym <- rlang::sym(namespace_ref)
        attachNamespace(namespace_ref)
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
        detach(paste0("package:",
                      namespace_ref),
               character.only = TRUE)
      }else{
        return_object_xx <- get(x = object_ref,
                                envir = as.environment(paste0("package:",
                                                              namespace_ref)))
      }
    }else{
      return_object_xx <- get(x = return_object_ref)
    }
  }else{
    return_object_xx <- return_object_ref
  }
  return(return_object_xx)
}
get_new_abbrs <- function(pkg_setup_ls,
                          classes_to_make_tb = NULL,
                          fns_env_ls = NULL,
                          inc_all_mthds_1L_lgl = T,
                          paths_ls = make_fn_nms(),
                          pkg_ds_ls_ls = NULL,
                          transformations_chr = NULL,
                          undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T),
                          use_last_1L_int = NULL){
  if(is.null(fns_env_ls))
    fns_env_ls <- read_fns()
  fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls,
                                     abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                     custom_dmt_ls = list(details_ls = NULL,
                                                          inc_for_main_user_lgl_ls = list(force_true_chr = pkg_setup_ls$subsequent_ls$user_manual_fns_chr,
                                                                                          force_false_chr = NA_character_),
                                                          args_ls_ls = NULL),
                                     fns_env_ls = fns_env_ls,
                                     fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup,
                                     inc_all_mthds_1L_lgl = inc_all_mthds_1L_lgl,
                                     object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
                                     undocumented_fns_dir_chr = undocumented_fns_dir_chr)
  if(is.null(use_last_1L_int)){
    new_fn_abbrs_chr <- fns_dmt_tb$fns_chr %>%
      get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                          drop_first_1L_lgl = T,
                          treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                          use_last_1L_int = use_last_1L_int)
  }else{
    new_fn_abbrs_chr <- character(0)
  }
  new_arg_abbrs_chr <- fns_dmt_tb$args_ls %>%
    purrr::map(~names(.x) %>%
                 get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                                     treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                                     use_last_1L_int = use_last_1L_int)) %>%
    purrr::flatten_chr() %>%
    unique()
  if(!is.null(pkg_ds_ls_ls)){
    new_ds_abbrs_chr <- pkg_ds_ls_ls %>%
      purrr::map(~c(names(.x$db_df)),
                 .x$db_1L_chr) %>%
      purrr::flatten_chr() %>%
      get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                          treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                          use_last_1L_int = use_last_1L_int)
  }else{
    new_ds_abbrs_chr <- character(0)
  }
  if(!is.null(classes_to_make_tb)){
    new_clss_abbrs_chr <- classes_to_make_tb$vals_ls %>%
      purrr::discard(is.null) %>%
      purrr::map(~names(.x)) %>%
      purrr::flatten_chr() %>%
      unique() %>%
      get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                          treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr,
                          use_last_1L_int = use_last_1L_int)
  }else{
    new_clss_abbrs_chr <- character(0)
  }
  new_abbrs_chr <- c(new_fn_abbrs_chr,
                     new_arg_abbrs_chr,
                     new_clss_abbrs_chr,
                     new_ds_abbrs_chr) %>%
    unique() %>%
    sort()
  if(!is.null(transformations_chr)){
    new_abbrs_chr <- c(setdiff(new_abbrs_chr,
                               transformations_chr),
                       names(transformations_chr)) %>%
      sort()
  }
  return(new_abbrs_chr)
}
get_new_abbrs_cndts <- function(text_chr,
                                abbreviations_lup,
                                drop_first_1L_lgl = F,
                                use_last_1L_int = NULL,
                                treat_as_words_chr = character(0)){
  new_abbrs_cndts_chr <- text_chr %>%
    purrr::map(~{
      candidates_chr <- strsplit(.x,"_")[[1]]
      if(drop_first_1L_lgl)
        candidates_chr <- candidates_chr[-1]
      if(!is.null(use_last_1L_int))
        candidates_chr <- candidates_chr %>%
          tail(use_last_1L_int)
      candidates_chr
    }) %>%
    purrr::flatten_chr() %>%
    unique() %>%
    sort() %>%
    setdiff(abbreviations_lup$short_name_chr)
  data("GradyAugmented", package = "qdapDictionaries", envir = environment())
  new_abbrs_cndts_chr <- setdiff(new_abbrs_cndts_chr[suppressWarnings(is.na(as.numeric(new_abbrs_cndts_chr)))],
                                  c(c(GradyAugmented, treat_as_words_chr),
                                    c(GradyAugmented, treat_as_words_chr) %>% toupper(),
                                    c(GradyAugmented, treat_as_words_chr) %>% Hmisc::capitalize()))
  return(new_abbrs_cndts_chr)
}
get_new_fn_types <- function(pkg_setup_ls,
                             #abbreviations_lup, # NOTE: Needs to be updated to read S4 generics and methods
                             #dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                             #dv_url_pfx_1L_chr = NULL,
                             #key_1L_chr = NULL,
                             #fn_types_lup = NULL,
                             fn_nms_ls = make_fn_nms(),
                             #server_1L_chr = Sys.getenv("DATAVERSE_SERVER"),
                             undmtd_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
                             #,
                             #object_type_lup = NULL
                             ){

  new_fn_types_chr <- purrr::map2(fn_nms_ls[names(fn_nms_ls)!="gnrcs"],
                                  undmtd_fns_dir_chr[undmtd_fns_dir_chr %>%
                                                       purrr::map_lgl(~!endsWith(.x,"gnrcs"))],
                                  ~stringr::str_remove(.x,paste0(.y,"/")) %>% stringr::str_sub(end=-3)) %>%
    purrr::flatten_chr()
  generics_dir_1L_chr <- undmtd_fns_dir_chr[undmtd_fns_dir_chr %>%
                                              purrr::map_lgl(~endsWith(.x,"gnrcs"))]
  if(!identical(generics_dir_1L_chr,
                character(0)))
  new_fn_types_chr <- new_fn_types_chr %>%
    c(get_fn_nms_in_file(paste0(generics_dir_1L_chr,"/generics.R")))
  new_fn_types_chr <- new_fn_types_chr %>%
    unique() %>%
    sort() %>%
    make_fn_title(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup,
                  object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup,
                  is_generic_lgl = T) %>%
    tools::toTitleCase()
  if(!is.null(pkg_setup_ls$subsequent_ls$fn_types_lup))
    new_fn_types_chr <- new_fn_types_chr %>%
    setdiff(pkg_setup_ls$subsequent_ls$fn_types_lup$fn_type_nm_chr)
  return(new_fn_types_chr)
}
get_obj_type_new_cses <- function(updated_obj_type_lup,
                                  dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                                  dv_url_pfx_1L_chr = NULL,
                                  excluded_chr = NA_character_,
                                  key_1L_chr = NULL,
                                  old_obj_type_lup = NULL,
                                  server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(old_obj_type_lup))
    old_obj_type_lup <- get_rds_from_dv("object_type_lup",
                                           dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                           dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                           key_1L_chr = key_1L_chr,
                                           server_1L_chr = server_1L_chr)
  obj_type_lup_new_cses_tb <- updated_obj_type_lup %>%
    dplyr::filter(!short_name_chr %in% old_obj_type_lup$short_name_chr)
  if(!is.na(excluded_chr[1]))
    obj_type_lup_new_cses_tb <- obj_type_lup_new_cses_tb %>%
      dplyr::filter(!short_name_chr %in% excluded_chr)
  return(obj_type_lup_new_cses_tb)
}
get_outp_obj_type <- function(fns_chr,
                              dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                              dv_url_pfx_1L_chr = NULL,
                              fns_env_ls = NULL,
                              key_1L_chr = NULL,
                              object_type_lup = NULL,
                              server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(fns_env_ls))
    fns_env_ls <- read_fns(fns_env = environment())
  if(is.null(object_type_lup))
    object_type_lup <- get_rds_from_dv("object_type_lup",
                                       dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                       dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                       key_1L_chr = key_1L_chr,
                                       server_1L_chr = server_1L_chr)
  outp_obj_type_chr <- purrr::map_chr(fns_chr,
                                          ~ {
                                            if(!exists(.x)){
                                              fn <- fns_env_ls$fns_env[[.x]]
                                            }else{
                                              fn <- eval(parse(text=.x))
                                            }
                                            return_obj_chr <- get_return_obj_nm(fn) %>%
                                              make_arg_desc(object_type_lup = object_type_lup,
                                                            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
                                                            dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
                                                            key_1L_chr = key_1L_chr,
                                                            server_1L_chr = server_1L_chr)
                                            ifelse(return_obj_chr  == "NO MATCH","NULL", return_obj_chr)
                                          })
  return(outp_obj_type_chr)
}
get_r4_obj_slots <- function(fn_name_1L_chr,
                                     package_1L_chr = ""){
  slots_ls <- className(fn_name_1L_chr,update_ns(package_1L_chr)) %>% methods::getSlots()
  slots_chr <- purrr::map_chr(slots_ls, ~ .x)
  return(slots_chr)
}
get_rds_from_dv <- function(file_nm_1L_chr,
                            dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9",
                            dv_url_pfx_1L_chr = NULL,
                            key_1L_chr = NULL,
                            server_1L_chr = Sys.getenv("DATAVERSE_SERVER")){
  if(is.null(dv_url_pfx_1L_chr))
    dv_url_pfx_1L_chr <- paste0("https://",
                                server_1L_chr,
                                "/api/access/datafile/")
  ds_ls <- dataverse::dataset_files(dv_ds_nm_1L_chr,
                                    server = server_1L_chr,
                                    key = key_1L_chr)
  all_items_chr <- purrr::map_chr(ds_ls,~.x$label)
  idx_1L_int <- which(all_items_chr == paste0(file_nm_1L_chr,".RDS"))
  if(identical(idx_1L_int, integer(0))){
    r_object_xx <- NULL
  }else{
    r_object_xx <- readRDS(url(paste0(dv_url_pfx_1L_chr,
                                      ds_ls[[idx_1L_int]]$dataFile$id)))
  }
  return(r_object_xx)
}
get_return_obj_nm <- function(fn){
  fn_chr <- deparse(fn)
  last_line_1L_chr <- fn_chr[length(fn_chr)-1] %>%
    trimws()
  if(startsWith(last_line_1L_chr,"return(")){
    return_1L_chr <- stringr::str_replace(last_line_1L_chr,"return","") %>%
      stringr::str_sub(start=2,end=-2)
  }else{
    return_1L_chr <- NA_character_
  }
  return(return_1L_chr)
}

