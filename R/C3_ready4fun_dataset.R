
#' for declaring package description file data..1
#' @description Create a new valid instance of the ready4 S3 class for declaring package description file data..1
#' @param x A prototype for the ready4 S3 class for declaring package description file data..1, Default: make_pt_ready4fun_dataset()
#' @return A validated instance of the ready4 S3 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname ready4fun_dataset
#' @export 
ready4fun_dataset <- function(x = make_pt_ready4fun_dataset()){ 
validate_ready4fun_dataset(make_new_ready4fun_dataset(x))
}
#' make new ready4fun package dataset ready4 S3 class for declaring package description file data..1
#' @description Create a new unvalidated instance of the ready4 S3 class for declaring package description file data..1
#' @param x A prototype for the ready4 S3 class for declaring package description file data..1
#' @return An unvalidated instance of the ready4 S3 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_new_ready4fun_dataset
#' @export 
#' @keywords internal
make_new_ready4fun_dataset <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_dataset",setdiff(make_pt_ready4fun_dataset() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package dataset ready4 S3 class for declaring package description file data..1
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
#' @return A prototype for ready4 S3 class for declaring package description file data..1
#' 
#' @rdname ready4fun_dataset
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_dataset <- function(db_df = data.frame(),
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
vars_ls = vars_ls) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' validate ready4fun package dataset ready4 S3 class for declaring package description file data..1
#' @description Validate an instance of the ready4 S3 class for declaring package description file data..1
#' @param x An unvalidated instance of the ready4 S3 class for declaring package description file data..1
#' @return A prototpe for ready4 S3 class for declaring package description file data..1
#' @details ready4 S3 class for declaring package description file data.
#' @rdname validate_ready4fun_dataset
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4 transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_dataset <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_dataset())],
names(make_pt_ready4fun_dataset())))!=length(names(make_pt_ready4fun_dataset()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_dataset()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_dataset() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_dataset())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_dataset() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class))
  vars_chr <- class_lup %>% dplyr::pull(1) %>% unique()
  classes_chr <- vars_chr %>%  purrr::map_chr(~dplyr::filter(class_lup, variable == .x) %>%  dplyr::pull(2) %>% paste0(collapse = ", "))
purrr::map2_chr(vars_chr,
classes_chr,
~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", 
")
}),
call. = FALSE)
}

x}
#' is ready4fun package dataset ready4 S3 class for declaring package description file data..1
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for declaring package description file data..1
#' 
#' @rdname ready4fun_dataset
#' @export 
is_ready4fun_dataset <- function(x) inherits(validate_ready4fun_dataset(x), "ready4fun_dataset")
