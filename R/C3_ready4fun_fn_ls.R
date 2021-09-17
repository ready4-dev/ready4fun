
#' ready4 S3 class for declaring package description file data.
#' @description Create a new valid instance of the ready4 S3 class for declaring package description file data.
#' @param x A prototype for the ready4 S3 class for declaring package description file data., Default: make_pt_ready4fun_fn_ls()
#' @return A validated instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname ready4fun_fn_ls
#' @export 

ready4fun_fn_ls <- function(x = make_pt_ready4fun_fn_ls()){ 
validate_ready4fun_fn_ls(make_new_ready4fun_fn_ls(x))
}
#' Make new ready4fun function list ready4 S3 class for declaring package description file data.
#' @description Create a new unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @param x A prototype for the ready4 S3 class for declaring package description file data.
#' @return An unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_new_ready4fun_fn_ls
#' @export 

make_new_ready4fun_fn_ls <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_fn_ls",setdiff(make_pt_ready4fun_fn_ls() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun function list ready4 S3 class for declaring package description file data.
#' @description Create a new prototype for the ready4 S3 class for declaring package description file data.
#' @param args_ls Arguments (a list), Default: list()
#' @param fn Function (a function), Default: identity
#' @return A prototype for ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_pt_ready4fun_fn_ls
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_fn_ls <- function(args_ls = list(),
fn = identity){ 
args_ls <- list(args_ls = args_ls,
fn = fn) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun function list ready4 S3 class for declaring package description file data.
#' @description Validate an instance of the ready4 S3 class for declaring package description file data.
#' @param x An unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @return A prototpe for ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname validate_ready4fun_fn_ls
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4fun_fn_ls <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_fn_ls())],
names(make_pt_ready4fun_fn_ls())))!=length(names(make_pt_ready4fun_fn_ls()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_fn_ls()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_fn_ls() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_fn_ls())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_fn_ls() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_fn_ls() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4fun function list ready4 S3 class for declaring package description file data.
#' @description Check whether an object is a valid instance of the ready4 S3 class for declaring package description file data.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname is_ready4fun_fn_ls
#' @export 

is_ready4fun_fn_ls <- function(x) inherits(validate_ready4fun_fn_ls(x), "ready4fun_fn_ls")
