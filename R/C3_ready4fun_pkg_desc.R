
#' ready4 S4 class for declaring package description file data.
#' @description Create a new valid instance of the ready4 S4 class for declaring package description file data.
#' @param x A prototype for the ready4 S4 class for declaring package description file data., Default: make_pt_ready4fun_pkg_desc()
#' @return A validated instance of the ready4 S4 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname ready4fun_pkg_desc
#' @export 

ready4fun_pkg_desc <- function(x = make_pt_ready4fun_pkg_desc()){ 
validate_ready4fun_pkg_desc(make_new_ready4fun_pkg_desc(x))
}
#' Make new ready4fun package description ready4 S4 class for declaring package description file data.
#' @description Create a new unvalidated instance of the ready4 S4 class for declaring package description file data.
#' @param x A prototype for the ready4 S4 class for declaring package description file data.
#' @return An unvalidated instance of the ready4 S4 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_new_ready4fun_pkg_desc
#' @export 

make_new_ready4fun_pkg_desc <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_pkg_desc",setdiff(make_pt_ready4fun_pkg_desc() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package description ready4 S4 class for declaring package description file data.
#' @description Create a new prototype for the ready4 S4 class for declaring package description file data.
#' @param Package PARAM_DESCRIPTION, Default: character(0)
#' @param Title PARAM_DESCRIPTION, Default: character(0)
#' @param Description PARAM_DESCRIPTION, Default: character(0)
#' @param License PARAM_DESCRIPTION, Default: logical(0)
#' @param URL PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for ready4 S4 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_pt_ready4fun_pkg_desc
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_pkg_desc <- function(Package = character(0),
Title = character(0),
Description = character(0),
License = logical(0),
URL = character(0)){ 
args_ls <- list(Package = Package,
Title = Title,
Description = Description,
License = License,
URL = URL) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package description ready4 S4 class for declaring package description file data.
#' @description Validate an instance of the ready4 S4 class for declaring package description file data.
#' @param x An unvalidated instance of the ready4 S4 class for declaring package description file data.
#' @return A prototpe for ready4 S4 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname validate_ready4fun_pkg_desc
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4fun_pkg_desc <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_pkg_desc())],
names(make_pt_ready4fun_pkg_desc())))!=length(names(make_pt_ready4fun_pkg_desc()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_pkg_desc()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_pkg_desc() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_pkg_desc())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_pkg_desc() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_pkg_desc() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4fun package description ready4 S4 class for declaring package description file data.
#' @description Check whether an object is a valid instance of the ready4 S4 class for declaring package description file data.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S4 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname is_ready4fun_pkg_desc
#' @export 

is_ready4fun_pkg_desc <- function(x) inherits(validate_ready4fun_pkg_desc(x), "ready4fun_pkg_desc")
