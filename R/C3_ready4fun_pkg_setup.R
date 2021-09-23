
#' ready4 S4 class for package metadata required for package set-up.
#' @description Create a new valid instance of the ready4 S4 class for package metadata required for package set-up.
#' @param x A prototype for the ready4 S4 class for package metadata required for package set-up., Default: make_pt_ready4fun_pkg_setup()
#' @return A validated instance of the ready4 S4 class for package metadata required for package set-up.
#' @details ready4 S3 class for package metadata required for package set-up.
#' @rdname ready4fun_pkg_setup
#' @export 

ready4fun_pkg_setup <- function(x = make_pt_ready4fun_pkg_setup()){ 
validate_ready4fun_pkg_setup(make_new_ready4fun_pkg_setup(x))
}
#' Make new ready4fun package package setup ready4 S4 class for package metadata required for package set-up.
#' @description Create a new unvalidated instance of the ready4 S4 class for package metadata required for package set-up.
#' @param x A prototype for the ready4 S4 class for package metadata required for package set-up.
#' @return An unvalidated instance of the ready4 S4 class for package metadata required for package set-up.
#' @details ready4 S3 class for package metadata required for package set-up.
#' @rdname make_new_ready4fun_pkg_setup
#' @export 

make_new_ready4fun_pkg_setup <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_pkg_setup",setdiff(make_pt_ready4fun_pkg_setup() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package package setup ready4 S4 class for package metadata required for package set-up.
#' @description Create a new prototype for the ready4 S4 class for package metadata required for package set-up.
#' @param initial_ls Initial (a list), Default: ready4fun_pkg_setup_one()
#' @param subsequent_ls Subsequent (a list), Default: ready4fun_pkg_setup_two()
#' @return A prototype for ready4 S4 class for package metadata required for package set-up.
#' @details ready4 S3 class for package metadata required for package set-up.
#' @rdname make_pt_ready4fun_pkg_setup
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_pkg_setup <- function(initial_ls = ready4fun_pkg_setup_one(),
subsequent_ls = ready4fun_pkg_setup_two()){ 
args_ls <- list(initial_ls = initial_ls,
subsequent_ls = subsequent_ls) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package package setup ready4 S4 class for package metadata required for package set-up.
#' @description Validate an instance of the ready4 S4 class for package metadata required for package set-up.
#' @param x An unvalidated instance of the ready4 S4 class for package metadata required for package set-up.
#' @return A prototpe for ready4 S4 class for package metadata required for package set-up.
#' @details ready4 S3 class for package metadata required for package set-up.
#' @rdname validate_ready4fun_pkg_setup
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4class transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4fun_pkg_setup <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_pkg_setup())],
names(make_pt_ready4fun_pkg_setup())))!=length(names(make_pt_ready4fun_pkg_setup()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_pkg_setup()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_pkg_setup() %>% 
lapply(class) %>% ready4class::transform_cls_type_ls() %>%tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4class::transform_cls_type_ls() %>%tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_pkg_setup())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_pkg_setup() %>% 
lapply(class) %>% ready4class::transform_cls_type_ls() %>%tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_pkg_setup() %>% 
lapply(class) %>% ready4class::transform_cls_type_ls() %>%tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4fun package package setup ready4 S4 class for package metadata required for package set-up.
#' @description Check whether an object is a valid instance of the ready4 S4 class for package metadata required for package set-up.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S4 class for package metadata required for package set-up.
#' @details ready4 S3 class for package metadata required for package set-up.
#' @rdname is_ready4fun_pkg_setup
#' @export 

is_ready4fun_pkg_setup <- function(x) inherits(validate_ready4fun_pkg_setup(x), "ready4fun_pkg_setup")
