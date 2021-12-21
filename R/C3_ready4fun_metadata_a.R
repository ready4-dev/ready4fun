
#' ready4 S3 class for package metadata required for initial package set-up step.
#' @description Create a new valid instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @param x A prototype for the ready4 S3 class for package metadata required for initial package set-up step., Default: make_pt_ready4fun_metadata_a()
#' @return A validated instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @details Package metadata (A).
#' @rdname ready4fun_metadata_a
#' @export 
ready4fun_metadata_a <- function(x = make_pt_ready4fun_metadata_a()){ 
validate_ready4fun_metadata_a(make_new_ready4fun_metadata_a(x))
}
#' make new ready4fun package metadata a ready4 S3 class for package metadata required for initial package set-up step.
#' @description Create a new unvalidated instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @param x A prototype for the ready4 S3 class for package metadata required for initial package set-up step.
#' @return An unvalidated instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @details Package metadata (A).
#' @rdname make_new_ready4fun_metadata_a
#' @export 
#' @keywords internal
make_new_ready4fun_metadata_a <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_metadata_a",setdiff(make_pt_ready4fun_metadata_a() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package metadata a ready4 S3 class for package metadata required for initial package set-up step.
#' @param pkg_desc_ls Package description (a list), Default: ready4fun_description()
#' @param copyright_holders_chr Copyright holders (a character vector), Default: character(0)
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: character(0)
#' @param add_gh_site_1L_lgl Add github site (a logical vector of length one), Default: logical(0)
#' @param addl_badges_ls Additional badges (a list), Default: list()
#' @param badges_lup Badges (a lookup table), Default: ready4fun_badges()
#' @param check_type_1L_chr Check type (a character vector of length one), Default: character(0)
#' @param delete_r_dir_cnts_1L_lgl Delete r directory contents (a logical vector of length one), Default: logical(0)
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: character(0)
#' @param lifecycle_stage_1L_chr Lifecycle stage (a character vector of length one), Default: character(0)
#' @param incr_ver_1L_lgl Increment version (a logical vector of length one), Default: logical(0)
#' @param on_cran_1L_lgl On cran (a logical vector of length one), Default: logical(0)
#' @param path_to_pkg_logo_1L_chr Path to package logo (a character vector of length one), Default: character(0)
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: character(0)
#' @return A prototype for ready4 S3 class for package metadata required for initial package set-up step.
#' 
#' @rdname ready4fun_metadata_a
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_metadata_a <- function(pkg_desc_ls = ready4fun_description(),
copyright_holders_chr = character(0),
gh_repo_1L_chr = character(0),
add_gh_site_1L_lgl = logical(0),
addl_badges_ls = list(),
badges_lup = ready4fun_badges(),
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
path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' validate ready4fun package metadata a ready4 S3 class for package metadata required for initial package set-up step.
#' @description Validate an instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @param x An unvalidated instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' @return A prototpe for ready4 S3 class for package metadata required for initial package set-up step.
#' @details Package metadata (A).
#' @rdname validate_ready4fun_metadata_a
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4 transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_metadata_a <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_metadata_a())],
names(make_pt_ready4fun_metadata_a())))!=length(names(make_pt_ready4fun_metadata_a()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_metadata_a()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_metadata_a() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_metadata_a())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_metadata_a() %>% 
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
#' is ready4fun package metadata a ready4 S3 class for package metadata required for initial package set-up step.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for package metadata required for initial package set-up step.
#' 
#' @rdname ready4fun_metadata_a
#' @export 
is_ready4fun_metadata_a <- function(x) inherits(validate_ready4fun_metadata_a(x), "ready4fun_metadata_a")
