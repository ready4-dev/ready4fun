remove_collate <- function(description_chr){
  if(!identical(which(description_chr=="Collate: "),integer(0)))
    description_chr <- description_chr[1:(which(description_chr=="Collate: ")-1)]
  return(description_chr)
}
remove_obj_type_from_nm <- function(nms_chr,
                                            object_type_lup = NULL,
                                            abbreviations_lup = NULL,
                                            is_generic_lgl = F){
  if(is.null(object_type_lup))
    data("object_type_lup",package="ready4fun",envir = environment())
  if(is.null(abbreviations_lup))
    data("abbreviations_lup",package="ready4fun",envir = environment())
  output_chr <- make_arg_type_abbr(nms_chr,
                                               abbreviations_lup = abbreviations_lup,
                                               object_type_lup = object_type_lup)
  suffices_chr <- output_chr %>% purrr::map2_chr(is_generic_lgl,~{
    ifelse(.x=="NO MATCH"|.y,
           "",
           .x)

  })
  names_chr <- purrr::map2_chr(nms_chr,
                                   suffices_chr,
                                   ~ {
                                     name_1L_chr <- .x
                                     ifelse(purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                           ~ endsWith(name_1L_chr,paste0(".",.x))) %>% any(),
                                            paste0(name_1L_chr %>% stringr::str_remove(paste0(".",abbreviations_lup$short_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                                             ~ endsWith(name_1L_chr,paste0(".",.x)))])),
                                                   " method applied to ",
                                                    abbreviations_lup$long_name_chr[purrr::map_lgl(abbreviations_lup$short_name_chr,
                                                                                                   ~ endsWith(name_1L_chr,paste0(".",.x)))],
                                                   "."),
                                            ifelse(.y=="",
                                                   .x,
                                                   stringi::stri_replace_last_fixed(.x,
                                                                                    paste0("_",.y),
                                                                                    "")))
                                   }

                                   )
  return(names_chr)

}
