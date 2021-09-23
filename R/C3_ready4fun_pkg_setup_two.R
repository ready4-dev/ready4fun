
#' ready4 S4 class for package metadata required for second package set-up step.
#' @description Create a new valid instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @param x A prototype for the ready4 S4 class for package metadata required for second package set-up step., Default: make_pt_ready4fun_pkg_setup_two()
#' @return A validated instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @details ready4 S3 class for package metadata required for second package set-up step.
#' @rdname ready4fun_pkg_setup_two
#' @export 

ready4fun_pkg_setup_two <- function(x = make_pt_ready4fun_pkg_setup_two()){ 
validate_ready4fun_pkg_setup_two(make_new_ready4fun_pkg_setup_two(x))
}
#' Make new ready4fun package package setup two ready4 S4 class for package metadata required for second package set-up step.
#' @description Create a new unvalidated instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @param x A prototype for the ready4 S4 class for package metadata required for second package set-up step.
#' @return An unvalidated instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @details ready4 S3 class for package metadata required for second package set-up step.
#' @rdname make_new_ready4fun_pkg_setup_two
#' @export 

make_new_ready4fun_pkg_setup_two <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_pkg_setup_two",setdiff(make_pt_ready4fun_pkg_setup_two() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package package setup two ready4 S4 class for package metadata required for second package set-up step.
#' @description Create a new prototype for the ready4 S4 class for package metadata required for second package set-up step.
#' @param addl_pkgs_ls Additional packages (a list), Default: list()
#' @param build_ignore_ls Build ignore (a list), Default: list()
#' @param dev_pkgs_chr Development packages (a character vector), Default: character(0)
#' @param pkg_dmt_dv_dss_chr Package documentation dataverse datasets (a character vector), Default: character(0)
#' @param user_manual_fns_chr User manual functions (a character vector), Default: character(0)
#' @return A prototype for ready4 S4 class for package metadata required for second package set-up step.
#' @details ready4 S3 class for package metadata required for second package set-up step.
#' @rdname make_pt_ready4fun_pkg_setup_two
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_pkg_setup_two <- function(addl_pkgs_ls = list(),
build_ignore_ls = list(),
dev_pkgs_chr = character(0),
pkg_dmt_dv_dss_chr = character(0),
user_manual_fns_chr = character(0)){ 
args_ls <- list(addl_pkgs_ls = addl_pkgs_ls,
build_ignore_ls = build_ignore_ls,
dev_pkgs_chr = dev_pkgs_chr,
pkg_dmt_dv_dss_chr = pkg_dmt_dv_dss_chr,
user_manual_fns_chr = user_manual_fns_chr) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package package setup two ready4 S4 class for package metadata required for second package set-up step.
#' @description Validate an instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @param x An unvalidated instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @return A prototpe for ready4 S4 class for package metadata required for second package set-up step.
#' @details ready4 S3 class for package metadata required for second package set-up step.
#' @rdname validate_ready4fun_pkg_setup_two
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map_chr map2_chr
validate_ready4fun_pkg_setup_two <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_pkg_setup_two())],
names(make_pt_ready4fun_pkg_setup_two())))!=length(names(make_pt_ready4fun_pkg_setup_two()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_pkg_setup_two()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_pkg_setup_two() %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_pkg_setup_two())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_pkg_setup_two() %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class)
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
#' Is ready4fun package package setup two ready4 S4 class for package metadata required for second package set-up step.
#' @description Check whether an object is a valid instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S4 class for package metadata required for second package set-up step.
#' @details ready4 S3 class for package metadata required for second package set-up step.
#' @rdname is_ready4fun_pkg_setup_two
#' @export 

is_ready4fun_pkg_setup_two <- function(x) inherits(validate_ready4fun_pkg_setup_two(x), "ready4fun_pkg_setup_two")
