
#' ready4 S4 class for declaring package description file data..1
#' @description Create a new valid instance of the ready4 S4 class for declaring package description file data..1
#' @param x A prototype for the ready4 S4 class for declaring package description file data..1, Default: make_pt_ready4fun_pkg_ds()
#' @return A validated instance of the ready4 S4 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname ready4fun_pkg_ds
#' @export 

ready4fun_pkg_ds <- function(x = make_pt_ready4fun_pkg_ds()){ 
validate_ready4fun_pkg_ds(make_new_ready4fun_pkg_ds(x))
}
#' Make new ready4 S4 class for declaring package description file data..1
#' @description Create a new unvalidated instance of the ready4 S4 class for declaring package description file data..1
#' @param x A prototype for the ready4 S4 class for declaring package description file data..1
#' @return An unvalidated instance of the ready4 S4 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_new_ready4fun_pkg_ds
#' @export 

make_new_ready4fun_pkg_ds <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_pkg_ds",setdiff(make_pt_ready4fun_pkg_ds() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4 S4 class for declaring package description file data..1
#' @description Create a new prototype for the ready4 S4 class for declaring package description file data..1
#' @param db_df Database (a data.frame), Default: data.frame()
#' @param db_1L_chr Database (a character vector of length one), Default: character(0)
#' @param title_1L_chr Title (a character vector of length one), Default: character(0)
#' @param desc_1L_chr Description (a character vector of length one), Default: character(0)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: ready4fun_abbreviations()
#' @param format_1L_chr Format (a character vector of length one), Default: character(0)
#' @param object_type_lup Object type (a lookup table), Default: ready4fun_abbreviations()
#' @param simple_lup_1L_lgl Simple lookup table (a logical vector of length one), Default: logical(0)
#' @param url_1L_chr Url (a character vector of length one), Default: character(0)
#' @param vars_ls Variables (a list), Default: list()
#' @return A prototype for ready4 S4 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_pt_ready4fun_pkg_ds
#' @export 
#' @importFrom ready4class update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_pkg_ds <- function(db_df = data.frame(),
db_1L_chr = character(0),
title_1L_chr = character(0),
desc_1L_chr = character(0),
abbreviations_lup = ready4fun_abbreviations(),
format_1L_chr = character(0),
object_type_lup = ready4fun_abbreviations(),
simple_lup_1L_lgl = logical(0),
url_1L_chr = character(0),
vars_ls = list()){ 
args_ls <- list(db_df = db_df,
db_1L_chr = db_1L_chr,
title_1L_chr = title_1L_chr,
desc_1L_chr = desc_1L_chr,
abbreviations_lup = abbreviations_lup,
format_1L_chr = format_1L_chr,
object_type_lup = object_type_lup,
simple_lup_1L_lgl = simple_lup_1L_lgl,
url_1L_chr = url_1L_chr,
vars_ls = vars_ls) %>% ready4class::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4 S4 class for declaring package description file data..1
#' @description Validate an instance of the ready4 S4 class for declaring package description file data..1
#' @param x An unvalidated instance of the ready4 S4 class for declaring package description file data..1
#' @return A prototpe for ready4 S4 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname validate_ready4fun_pkg_ds
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4fun_pkg_ds <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_pkg_ds())],
names(make_pt_ready4fun_pkg_ds())))!=length(names(make_pt_ready4fun_pkg_ds()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_pkg_ds()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_pkg_ds() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_pkg_ds())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_pkg_ds() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_pkg_ds() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4 S4 class for declaring package description file data..1
#' @description Check whether an object is a valid instance of the ready4 S4 class for declaring package description file data..1
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S4 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname is_ready4fun_pkg_ds
#' @export 

is_ready4fun_pkg_ds <- function(x) inherits(validate_ready4fun_pkg_ds(x), "ready4fun_pkg_ds")
