
#' ready4 S3 class for encapsulating the metadata required for package set-up.
#' @description Create a new valid instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param x A prototype for the ready4 S3 class for encapsulating the metadata required for package set-up., Default: make_pt_ready4fun_manifest()
#' @return A validated instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @details ready4 S3 class for encapsulating the metadata required for package set-up.
#' @rdname ready4fun_manifest
#' @export 

ready4fun_manifest <- function(x = make_pt_ready4fun_manifest()){ 
validate_ready4fun_manifest(make_new_ready4fun_manifest(x))
}
#' Make new ready4fun package manifest ready4 S3 class for encapsulating the metadata required for package set-up.
#' @description Create a new unvalidated instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param x A prototype for the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @return An unvalidated instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @details ready4 S3 class for encapsulating the metadata required for package set-up.
#' @rdname make_new_ready4fun_manifest
#' @export 

make_new_ready4fun_manifest <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_manifest",setdiff(make_pt_ready4fun_manifest() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package manifest ready4 S3 class for encapsulating the metadata required for package set-up.
#' @description Create a new prototype for the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param initial_ls Initial (a list), Default: ready4fun_manifest_one()
#' @param subsequent_ls Subsequent (a list), Default: ready4fun_manifest_two()
#' @return A prototype for ready4 S3 class for encapsulating the metadata required for package set-up.
#' @details ready4 S3 class for encapsulating the metadata required for package set-up.
#' @rdname make_pt_ready4fun_manifest
#' @export 
#' @importFrom rlang exec
make_pt_ready4fun_manifest <- function(initial_ls = ready4fun_manifest_one(),
subsequent_ls = ready4fun_manifest_two()){ 
args_ls <- list(initial_ls = initial_ls,
subsequent_ls = subsequent_ls) %>% update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package manifest ready4 S3 class for encapsulating the metadata required for package set-up.
#' @description Validate an instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param x An unvalidated instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @return A prototpe for ready4 S3 class for encapsulating the metadata required for package set-up.
#' @details ready4 S3 class for encapsulating the metadata required for package set-up.
#' @rdname validate_ready4fun_manifest
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
validate_ready4fun_manifest <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_manifest())],
names(make_pt_ready4fun_manifest())))!=length(names(make_pt_ready4fun_manifest()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_manifest()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_manifest() %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_manifest())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_manifest() %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
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
#' Is ready4fun package manifest ready4 S3 class for encapsulating the metadata required for package set-up.
#' @description Check whether an object is a valid instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for encapsulating the metadata required for package set-up.
#' @details ready4 S3 class for encapsulating the metadata required for package set-up.
#' @rdname is_ready4fun_manifest
#' @export 

is_ready4fun_manifest <- function(x) inherits(validate_ready4fun_manifest(x), "ready4fun_manifest")
