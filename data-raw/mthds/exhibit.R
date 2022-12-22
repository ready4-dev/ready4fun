exhibit.ready4fun_abbreviations <- function(x,
                                            caption_1L_chr = NULL,
                                            mkdn_tbl_ref_1L_chr = NULL,
                                            output_type_1L_chr = "HTML",
                                            select_int = NA_integer_,
                                            use_lbls_as_col_nms_1L_lgl = T,
                                            ...){
  var_desc_chr <- c("Abbreviation","Description","Plural")
  if(!is.na(select_int[1])){
    x <- dplyr::select(x,
                       select_int)
    var_desc_chr <- var_desc_chr[select_int]
  }
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
exhibit.ready4fun_functions <- function(x,
                                        caption_1L_chr = NULL,
                                        mkdn_tbl_ref_1L_chr = NULL,
                                        output_type_1L_chr = "HTML",
                                        select_int = NA_integer_,
                                        use_lbls_as_col_nms_1L_lgl = T,
                                        ...){
  var_desc_chr = c("Verb","Description","Argument One","Argument Two", "Generic", "Method")
  if(!is.na(select_int[1])){
    x <- dplyr::select(x,
                       select_int)
    var_desc_chr <- var_desc_chr[select_int]
  }
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
exhibit.ready4fun_objects <- function(x,
                                      caption_1L_chr = NULL,
                                      mkdn_tbl_ref_1L_chr = NULL,
                                      output_type_1L_chr = "HTML",
                                      select_int = NA_integer_,
                                      use_lbls_as_col_nms_1L_lgl = T,
                                      ...){
  var_desc_chr = c("Suffix","Description","Atomic","Extendable")
  if(!is.na(select_int[1])){
    x <- dplyr::select(x,
                       select_int)
    var_desc_chr <- var_desc_chr[select_int]
  }
  x %>%
    print_from_chunk(caption_1L_chr = caption_1L_chr,
                     mkdn_tbl_ref_1L_chr = mkdn_tbl_ref_1L_chr,
                     output_type_1L_chr = output_type_1L_chr,
                     use_lbls_as_col_nms_1L_lgl = use_lbls_as_col_nms_1L_lgl,
                     var_desc_chr = var_desc_chr,
                     ...)
}
