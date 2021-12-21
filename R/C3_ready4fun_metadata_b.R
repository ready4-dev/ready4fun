
#' ready4 S3 class for package metadata required for second package set-up step.
#' @description Create a new valid instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @param x A prototype for the ready4 S3 class for package metadata required for second package set-up step., Default: make_pt_ready4fun_metadata_b()
#' @return A validated instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @details Package metadata (B).
#' @rdname ready4fun_metadata_b
#' @export 
ready4fun_metadata_b <- function(x = make_pt_ready4fun_metadata_b()){ 
validate_ready4fun_metadata_b(make_new_ready4fun_metadata_b(x))
}
#' make new ready4fun package metadata b ready4 S3 class for package metadata required for second package set-up step.
#' @description Create a new unvalidated instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @param x A prototype for the ready4 S3 class for package metadata required for second package set-up step.
#' @return An unvalidated instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @details Package metadata (B).
#' @rdname make_new_ready4fun_metadata_b
#' @export 
#' @keywords internal
make_new_ready4fun_metadata_b <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_metadata_b",setdiff(make_pt_ready4fun_metadata_b() %>% class(),class(x))),
class(x))
x
}
#' make prototype ready4fun package metadata b ready4 S3 class for package metadata required for second package set-up step.
#' @param abbreviations_lup Abbreviations (a lookup table), Default: ready4fun_abbreviations()
#' @param addl_pkgs_ls Additional packages (a list), Default: list()
#' @param build_ignore_ls Build ignore (a list), Default: list()
#' @param cls_fn_ls Class (a list of functions), Default: ready4fun_executor()
#' @param custom_dmt_ls Custom documentation (a list), Default: list()
#' @param dev_pkgs_chr Development packages (a character vector), Default: character(0)
#' @param dss_records_ls Datasets records (a list), Default: list()
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: character(0)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: character(0)
#' @param fn_types_lup Function types (a lookup table), Default: tibble::tibble()
#' @param fns_dmt_tb Functions documentation (a tibble), Default: tibble::tibble()
#' @param import_from_chr Import from (a character vector), Default: character(0)
#' @param inc_pkg_meta_data_1L_lgl Include package meta data (a logical vector of length one), Default: logical(0)
#' @param object_type_lup Object type (a lookup table), Default: ready4fun_abbreviations()
#' @param path_to_dmt_dir_1L_chr Path to documentation directory (a character vector of length one), Default: character(0)
#' @param piggyback_to_1L_chr Piggyback to (a character vector of length one), Default: character(0)
#' @param pkg_dmt_dv_dss_chr Package documentation dataverse datasets (a character vector), Default: character(0)
#' @param pkg_ds_ls_ls Package dataset (a list of lists), Default: list()
#' @param seed_obj_type_lup Seed object type (a lookup table), Default: tibble::tibble()
#' @param server_1L_chr Server (a character vector of length one), Default: character(0)
#' @param s4_fns_ls S4 functions (a list), Default: ready4fun_executor()
#' @param treat_as_words_chr Treat as words (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for package metadata required for second package set-up step.
#' 
#' @rdname ready4fun_metadata_b
#' @export 
#' @importFrom tibble tibble
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_metadata_b <- function(abbreviations_lup = ready4fun_abbreviations(),
addl_pkgs_ls = list(),
build_ignore_ls = list(),
cls_fn_ls = ready4fun_executor(),
custom_dmt_ls = list(),
dev_pkgs_chr = character(0),
dss_records_ls = list(),
dv_ds_nm_1L_chr = character(0),
dv_url_pfx_1L_chr = character(0),
fn_types_lup = tibble::tibble(),
fns_dmt_tb = tibble::tibble(),
import_from_chr = character(0),
inc_pkg_meta_data_1L_lgl = logical(0),
object_type_lup = ready4fun_abbreviations(),
path_to_dmt_dir_1L_chr = character(0),
piggyback_to_1L_chr = character(0),
pkg_dmt_dv_dss_chr = character(0),
pkg_ds_ls_ls = list(),
seed_obj_type_lup = tibble::tibble(),
server_1L_chr = character(0),
s4_fns_ls = ready4fun_executor(),
treat_as_words_chr = character(0)){ 
args_ls <- list(abbreviations_lup = abbreviations_lup,
addl_pkgs_ls = addl_pkgs_ls,
build_ignore_ls = build_ignore_ls,
cls_fn_ls = cls_fn_ls,
custom_dmt_ls = custom_dmt_ls,
dev_pkgs_chr = dev_pkgs_chr,
dss_records_ls = dss_records_ls,
dv_ds_nm_1L_chr = dv_ds_nm_1L_chr,
dv_url_pfx_1L_chr = dv_url_pfx_1L_chr,
fn_types_lup = fn_types_lup,
fns_dmt_tb = fns_dmt_tb,
import_from_chr = import_from_chr,
inc_pkg_meta_data_1L_lgl = inc_pkg_meta_data_1L_lgl,
object_type_lup = object_type_lup,
path_to_dmt_dir_1L_chr = path_to_dmt_dir_1L_chr,
piggyback_to_1L_chr = piggyback_to_1L_chr,
pkg_dmt_dv_dss_chr = pkg_dmt_dv_dss_chr,
pkg_ds_ls_ls = pkg_ds_ls_ls,
seed_obj_type_lup = seed_obj_type_lup,
server_1L_chr = server_1L_chr,
s4_fns_ls = s4_fns_ls,
treat_as_words_chr = treat_as_words_chr) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' validate ready4fun package metadata b ready4 S3 class for package metadata required for second package set-up step.
#' @description Validate an instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @param x An unvalidated instance of the ready4 S3 class for package metadata required for second package set-up step.
#' @return A prototpe for ready4 S3 class for package metadata required for second package set-up step.
#' @details Package metadata (B).
#' @rdname validate_ready4fun_metadata_b
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom ready4 transform_cls_type_ls
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
#' @keywords internal
validate_ready4fun_metadata_b <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_metadata_b())],
names(make_pt_ready4fun_metadata_b())))!=length(names(make_pt_ready4fun_metadata_b()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_metadata_b()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_metadata_b() %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% ready4::transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_metadata_b())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_metadata_b() %>% 
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
#' is ready4fun package metadata b ready4 S3 class for package metadata required for second package set-up step.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for package metadata required for second package set-up step.
#' 
#' @rdname ready4fun_metadata_b
#' @export 
is_ready4fun_metadata_b <- function(x) inherits(validate_ready4fun_metadata_b(x), "ready4fun_metadata_b")
