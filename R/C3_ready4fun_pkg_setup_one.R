
#' ready4 S4 class for package metadata required for initial package set-up step.
#' @description Create a new valid instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @param x A prototype for the ready4 S4 class for package metadata required for initial package set-up step., Default: make_pt_ready4fun_pkg_setup_one()
#' @return A validated instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @details ready4 S3 class for package metadata required for initial package set-up step.
#' @rdname ready4fun_pkg_setup_one
#' @export 

ready4fun_pkg_setup_one <- function(x = make_pt_ready4fun_pkg_setup_one()){ 
validate_ready4fun_pkg_setup_one(make_new_ready4fun_pkg_setup_one(x))
}
#' Make new ready4fun package package setup one ready4 S4 class for package metadata required for initial package set-up step.
#' @description Create a new unvalidated instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @param x A prototype for the ready4 S4 class for package metadata required for initial package set-up step.
#' @return An unvalidated instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @details ready4 S3 class for package metadata required for initial package set-up step.
#' @rdname make_new_ready4fun_pkg_setup_one
#' @export 

make_new_ready4fun_pkg_setup_one <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_pkg_setup_one",setdiff(make_pt_ready4fun_pkg_setup_one() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package package setup one ready4 S4 class for package metadata required for initial package set-up step.
#' @description Create a new prototype for the ready4 S4 class for package metadata required for initial package set-up step.
#' @param pkg_desc_ls Package description (a list), Default: list()
#' @param copyright_holders_chr Copyright holders (a character vector), Default: character(0)
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: character(0)
#' @param add_gh_site_1L_lgl Add github site (a logical vector of length one), Default: character(0)
#' @param addl_badges_ls Additional badges (a list), Default: list()
#' @param badges_lup Badges (a lookup table), Default: ready4_badges()
#' @param check_type_1L_chr Check type (a character vector of length one), Default: character(0)
#' @param delete_r_dir_cnts_1L_lgl Delete r directory contents (a logical vector of length one), Default: logical(0)
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: character(0)
#' @param lifecycle_stage_1L_chr Lifecycle stage (a character vector of length one), Default: character(0)
#' @param incr_ver_1L_lgl Increment version (a logical vector of length one), Default: logical(0)
#' @param on_cran_1L_lgl On cran (a logical vector of length one), Default: logical(0)
#' @param path_to_pkg_logo_1L_chr Path to package logo (a character vector of length one), Default: character(0)
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: character(0)
#' @return A prototype for ready4 S4 class for package metadata required for initial package set-up step.
#' @details ready4 S3 class for package metadata required for initial package set-up step.
#' @rdname make_pt_ready4fun_pkg_setup_one
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_pkg_setup_one <- function(pkg_desc_ls = list(),
copyright_holders_chr = character(0),
gh_repo_1L_chr = character(0),
add_gh_site_1L_lgl = character(0),
addl_badges_ls = list(),
badges_lup = ready4_badges(),
check_type_1L_chr = character(0),
delete_r_dir_cnts_1L_lgl = logical(0),
dev_pkg_nm_1L_chr = character(0),
lifecycle_stage_1L_chr = character(0),
incr_ver_1L_lgl = logical(0),
on_cran_1L_lgl = logical(0),
path_to_pkg_logo_1L_chr = character(0),
path_to_pkg_rt_1L_chr = character(0)){ 
args_ls <- list(pkg_desc_ls = pkg_desc_ls,
copyright_holders_chr = copyright_holders_chr,
gh_repo_1L_chr = gh_repo_1L_chr,
add_gh_site_1L_lgl = add_gh_site_1L_lgl,
addl_badges_ls = addl_badges_ls,
badges_lup = badges_lup,
check_type_1L_chr = check_type_1L_chr,
delete_r_dir_cnts_1L_lgl = delete_r_dir_cnts_1L_lgl,
dev_pkg_nm_1L_chr = dev_pkg_nm_1L_chr,
lifecycle_stage_1L_chr = lifecycle_stage_1L_chr,
incr_ver_1L_lgl = incr_ver_1L_lgl,
on_cran_1L_lgl = on_cran_1L_lgl,
path_to_pkg_logo_1L_chr = path_to_pkg_logo_1L_chr,
path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package package setup one ready4 S4 class for package metadata required for initial package set-up step.
#' @description Validate an instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @param x An unvalidated instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @return A prototpe for ready4 S4 class for package metadata required for initial package set-up step.
#' @details ready4 S3 class for package metadata required for initial package set-up step.
#' @rdname validate_ready4fun_pkg_setup_one
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr arrange filter pull
#' @importFrom purrr map2_chr
validate_ready4fun_pkg_setup_one <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_pkg_setup_one())],
names(make_pt_ready4fun_pkg_setup_one())))!=length(names(make_pt_ready4fun_pkg_setup_one()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_pkg_setup_one()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_pkg_setup_one() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_pkg_setup_one())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
purrr::map2_chr(make_pt_ready4fun_pkg_setup_one() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(1),
 make_pt_ready4fun_pkg_setup_one() %>% 
lapply(class) %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
dplyr::pull(2),
 ~ paste0(.x,": ",.y)) %>% 
stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

x}
#' Is ready4fun package package setup one ready4 S4 class for package metadata required for initial package set-up step.
#' @description Check whether an object is a valid instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S4 class for package metadata required for initial package set-up step.
#' @details ready4 S3 class for package metadata required for initial package set-up step.
#' @rdname is_ready4fun_pkg_setup_one
#' @export 

is_ready4fun_pkg_setup_one <- function(x) inherits(validate_ready4fun_pkg_setup_one(x), "ready4fun_pkg_setup_one")
