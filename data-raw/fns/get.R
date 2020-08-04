get_fn_args_chr_vec <- function(fn){
  fn_args_chr_vec <- as.list(args(fn)) %>%
    names() %>%
    purrr::discard({.==""})
  return(fn_args_chr_vec)
}
get_fn_nms_in_file_chr <- function(path_chr){
  source(path_chr, local=T)
  local_chr <- ls()
  local_chr <-local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text=.x))))]
  return(local_chr)
}
get_from_lup_obj <- function(data_lookup_tb,
                             match_value_xx,
                             match_var_nm_chr,
                             target_var_nm_chr,
                             evaluate_lgl = TRUE){
  return_object_ref <- data_lookup_tb %>%
    dplyr::filter(!!rlang::sym(match_var_nm_chr)==match_value_xx) %>%
    dplyr::select(!!target_var_nm_chr) %>%
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
get_outp_obj_type_chr_vec <- function(fns_chr_vec){
  outp_obj_type_chr_vec <- purrr::map_chr(fns_chr_vec,
                                          ~ {
                                            return_obj_chr <- get_return_obj_nm_chr(eval(parse(text=.x))) %>%
                                              make_arg_desc_chr_vec()
                                            ifelse(return_obj_chr  == "NO MATCH","NULL", return_obj_chr)
                                          })
  return(outp_obj_type_chr_vec)
}
get_r4_obj_slots_chr_vec <- function(fn_name_chr,
                                     package_chr = ""){
  slots_ls <- className(fn_name_chr,update_ns_chr(package_chr)) %>% methods::getSlots()
  slots_chr_vec <- purrr::map_chr(slots_ls, ~ .x)
  return(slots_chr_vec)
}
get_return_obj_nm_chr <- function(fn){
  fn_chr <- deparse(fn)
  last_line_chr <- fn_chr[length(fn_chr)-1] %>%
    trimws()
  if(startsWith(last_line_chr,"return(")){
    return_chr <- stringr::str_replace(last_line_chr,"return","") %>%
      stringr::str_sub(start=2,end=-2)
  }else{
    return_chr <- NA_character_
  }
  return(return_chr)
}
