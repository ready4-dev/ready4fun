
#' ready4 submodule class for list object specifying function arguments and function.
#' @description Create a new valid instance of the ready4 submodule class for list object specifying function arguments and function.
#' @param x A prototype for the ready4 submodule class for list object specifying function arguments and function., Default: make_pt_ready4fun_executor()
#' @return A validated instance of the ready4 submodule class for list object specifying function arguments and function.
#' @details Function and function arguments pair.
#' @rdname ready4fun_executor
#' @export 
ready4fun_executor <- function(x = make_pt_ready4fun_executor()){ 
validate_ready4fun_executor(make_new_ready4fun_executor(x))
}
#' make new ready4fun package executor ready4 submodule class for list object specifying function arguments and function.
#' @description Create a new unvalidated instance of the ready4 submodule class for list object specifying function arguments and function.
#' @param x A prototype for the ready4 submodule class for list object specifying function arguments and function.
#' @return An unvalidated instance of the ready4 submodule class for list object specifying function arguments and function.
#' @details Function and function arguments pair.
#' @rdname make_new_ready4fun_executor
#' @export 
#' @keywords internal
make_new_ready4fun_executor <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_executor",setdiff(make_pt_ready4fun_executor() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package executor ready4 submodule class for list object specifying function arguments and function.
#' @param args_ls Arguments (a list), Default: list()
#' @param fn Function (a function), Default: identity
#' @return A prototype for ready4 submodule class for list object specifying function arguments and function.
#' 
#' @rdname ready4fun_executor
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_executor <- function(args_ls = list(),
fn = identity){ 
args_ls <- list(args_ls = args_ls,
fn = fn) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' validate ready4fun package executor ready4 submodule class for list object specifying function arguments and function.
#' @description Validate an instance of the ready4 submodule class for list object specifying function arguments and function.
#' @param x An unvalidated instance of the ready4 submodule class for list object specifying function arguments and function.
#' @return A prototpe for ready4 submodule class for list object specifying function arguments and function.
#' @details Function and function arguments pair.
#' @rdname validate_ready4fun_executor
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4 transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_executor <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_executor())],
names(make_pt_ready4fun_executor())))!=length(names(make_pt_ready4fun_executor()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_executor()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_executor() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_executor())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_executor() %>% 
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
#' is ready4fun package executor ready4 submodule class for list object specifying function arguments and function.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 submodule class for list object specifying function arguments and function.
#' 
#' @rdname ready4fun_executor
#' @export 
is_ready4fun_executor <- function(x) inherits(validate_ready4fun_executor(x), "ready4fun_executor")
