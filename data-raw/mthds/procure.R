procure.ready4fun_abbreviations <- function(x,
                                            match_1L_chr = character(0),
                                            type_1L_chr = c("abbreviation","extension"),
                                            what_1L_chr = c("table","string")){
  type_1L_chr <- match.arg(type_1L_chr)
  what_1L_chr <- match.arg(what_1L_chr)
  object_xx <- NULL
  if(identical(match_1L_chr, character(0))){
    warning("No value supplied to `match_1L_chr`. NULL value returned")
  }else{
    object_xx <- get_abbrs(match_1L_chr, type_1L_chr = type_1L_chr, abbreviations_lup = x)
    if(what_1L_chr == "string"){
      target_1L_chr <- ifelse(type_1L_chr == "abbreviation", "long_name_chr","short_name_chr")
      object_xx <- object_xx %>% dplyr::pull(!!rlang::sym(target_1L_chr))
    }
  }
  return(object_xx)
}
procure.ready4fun_manifest <- function(x,
                                       type_1L_chr) {
  if (type_1L_chr == "problems") {
    value_xx <- x$problems_ls
  }
  return(value_xx)
}
