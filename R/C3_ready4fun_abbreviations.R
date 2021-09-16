
setOldClass(c("ready4fun_abbreviations","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of abbreviations.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of abbreviations., Default: make_pt_ready4fun_abbreviations()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @details ready4 S3 class for tibble object lookup table of abbreviations.
#' @rdname ready4fun_abbreviations
#' @export 

ready4fun_abbreviations <- function(x = make_pt_ready4fun_abbreviations()){ 
validate_ready4fun_abbreviations(make_new_ready4fun_abbreviations(x))
}
#' Make new ready4fun abbreviations ready4 S3 class for tibble object lookup table of abbreviations.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of abbreviations.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @details ready4 S3 class for tibble object lookup table of abbreviations.
#' @rdname make_new_ready4fun_abbreviations
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4fun_abbreviations <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4fun_abbreviations",setdiff(make_pt_ready4fun_abbreviations() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun abbreviations ready4 S3 class for tibble object lookup table of abbreviations.
#' @description Create a new prototype for the ready4 S3 class for tibble object lookup table of abbreviations.
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param plural_lgl Plural (a logical vector), Default: logical(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of abbreviations.
#' @details ready4 S3 class for tibble object lookup table of abbreviations.
#' @rdname make_pt_ready4fun_abbreviations
#' @export 
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4fun_abbreviations <- function(short_name_chr = character(0),
long_name_chr = character(0),
plural_lgl = logical(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr,
plural_lgl = plural_lgl) %>% update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4fun abbreviations ready4 S3 class for tibble object lookup table of abbreviations.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of abbreviations.
#' @details ready4 S3 class for tibble object lookup table of abbreviations.
#' @rdname validate_ready4fun_abbreviations
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all arrange filter pull
#' @importFrom tidyr gather
#' @importFrom purrr map2_chr
validate_ready4fun_abbreviations <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_abbreviations())],
names(make_pt_ready4fun_abbreviations())))!=length(names(make_pt_ready4fun_abbreviations()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4fun_abbreviations()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_abbreviations() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_abbreviations())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_abbreviations() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_abbreviations() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4fun abbreviations ready4 S3 class for tibble object lookup table of abbreviations.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of abbreviations.
#' @details ready4 S3 class for tibble object lookup table of abbreviations.
#' @rdname is_ready4fun_abbreviations
#' @export 

is_ready4fun_abbreviations <- function(x) inherits(validate_ready4fun_abbreviations(x), "ready4fun_abbreviations")
