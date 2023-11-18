
setOldClass(c("ready4fun_objects","tbl_df", "tbl", "data.frame"))
#' Object types lookup table.
#' @description Create a new valid instance of the Object types lookup table.
#' @param x A prototype for the Object types lookup table., Default: make_pt_ready4fun_objects()
#' @return A validated instance of the Object types lookup table.
#' @details Object types lookup table.
#' @rdname ready4fun_objects
#' @export 
ready4fun_objects <- function(x = make_pt_ready4fun_objects()){ 
validate_ready4fun_objects(make_new_ready4fun_objects(x))
}
#' make new ready4fun package objects Object types lookup table.
#' @description Create a new unvalidated instance of the Object types lookup table.
#' @param x A prototype for the Object types lookup table.
#' @return An unvalidated instance of the Object types lookup table.
#' @details Object types lookup table.
#' @rdname make_new_ready4fun_objects
#' @export 
#' @importFrom tibble is_tibble
#' @keywords internal
make_new_ready4fun_objects <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4fun_objects",setdiff(make_pt_ready4fun_objects() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package objects Object types lookup table.
#' @param short_name_chr Short name (a character vector), Default: character(0)
#' @param long_name_chr Long name (a character vector), Default: character(0)
#' @param atomic_element_lgl Atomic element (a logical vector), Default: logical(0)
#' @param r3_can_extend_lgl Ready4 submodule can extend (a logical vector), Default: logical(0)
#' @return A prototype for Object types lookup table.
#' 
#' @rdname ready4fun_objects
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4fun_objects <- function(short_name_chr = character(0),
long_name_chr = character(0),
atomic_element_lgl = logical(0),
r3_can_extend_lgl = logical(0)){ 
args_ls <- list(short_name_chr = short_name_chr,
long_name_chr = long_name_chr,
atomic_element_lgl = atomic_element_lgl,
r3_can_extend_lgl = r3_can_extend_lgl) %>% ready4::update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' validate ready4fun package objects Object types lookup table.
#' @description Validate an instance of the Object types lookup table.
#' @param x An unvalidated instance of the Object types lookup table.
#' @return A prototpe for Object types lookup table.
#' @details Object types lookup table.
#' @rdname validate_ready4fun_objects
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_objects <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_objects())],
names(make_pt_ready4fun_objects())))!=length(names(make_pt_ready4fun_objects()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4fun_objects()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_objects() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_objects())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_objects() %>% 
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
#' is ready4fun package objects Object types lookup table.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the Object types lookup table.
#' 
#' @rdname ready4fun_objects
#' @export 
is_ready4fun_objects <- function(x) inherits(validate_ready4fun_objects(x), "ready4fun_objects")
