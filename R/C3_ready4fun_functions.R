
setOldClass(c("ready4fun_functions","tbl_df", "tbl", "data.frame"))
#' Function types lookup table.
#' @description Create a new valid instance of the Function types lookup table.
#' @param x A prototype for the Function types lookup table., Default: make_pt_ready4fun_functions()
#' @return A validated instance of the Function types lookup table.
#' @details Function types lookup table.
#' @rdname ready4fun_functions
#' @export 
ready4fun_functions <- function(x = make_pt_ready4fun_functions()){ 
validate_ready4fun_functions(make_new_ready4fun_functions(x))
}
#' make new ready4fun package functions Function types lookup table.
#' @description Create a new unvalidated instance of the Function types lookup table.
#' @param x A prototype for the Function types lookup table.
#' @return An unvalidated instance of the Function types lookup table.
#' @details Function types lookup table.
#' @rdname make_new_ready4fun_functions
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4fun_functions <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4fun_functions",setdiff(make_pt_ready4fun_functions() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package functions Function types lookup table.
#' @param fn_type_nm_chr Function type name (a character vector), Default: character(0)
#' @param fn_type_desc_chr Function type description (a character vector), Default: character(0)
#' @param first_arg_desc_chr First argument description (a character vector), Default: character(0)
#' @param second_arg_desc_chr Second argument description (a character vector), Default: character(0)
#' @param is_generic_lgl Is generic (a logical vector), Default: logical(0)
#' @param is_method_lgl Is method (a logical vector), Default: logical(0)
#' @return A prototype for Function types lookup table.
#' 
#' @rdname ready4fun_functions
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4fun_functions <- function(fn_type_nm_chr = character(0),
fn_type_desc_chr = character(0),
first_arg_desc_chr = character(0),
second_arg_desc_chr = character(0),
is_generic_lgl = logical(0),
is_method_lgl = logical(0)){ 
args_ls <- list(fn_type_nm_chr = fn_type_nm_chr,
fn_type_desc_chr = fn_type_desc_chr,
first_arg_desc_chr = first_arg_desc_chr,
second_arg_desc_chr = second_arg_desc_chr,
is_generic_lgl = is_generic_lgl,
is_method_lgl = is_method_lgl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4fun package functions Function types lookup table.
#' @description Validate an instance of the Function types lookup table.
#' @param x An unvalidated instance of the Function types lookup table.
#' @return A prototpe for Function types lookup table.
#' @details Function types lookup table.
#' @rdname validate_ready4fun_functions
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_functions <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_functions())],
names(make_pt_ready4fun_functions())))!=length(names(make_pt_ready4fun_functions()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4fun_functions()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_functions() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_functions())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_functions() %>% 
dplyr::summarise_all(class) %>% 
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
#' is ready4fun package functions Function types lookup table.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Function types lookup table.
#' 
#' @rdname ready4fun_functions
#' @export 
is_ready4fun_functions <- function(x) inherits(validate_ready4fun_functions(x), "ready4fun_functions")
