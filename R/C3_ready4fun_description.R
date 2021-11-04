
#' ready4 S3 class for declaring package description file data.
#' @description Create a new valid instance of the ready4 S3 class for declaring package description file data.
#' @param x A prototype for the ready4 S3 class for declaring package description file data., Default: make_pt_ready4fun_description()
#' @return A validated instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname ready4fun_description
#' @export 
ready4fun_description <- function(x = make_pt_ready4fun_description()){ 
validate_ready4fun_description(make_new_ready4fun_description(x))
}
#' Make new ready4fun package description ready4 S3 class for declaring package description file data.
#' @description Create a new unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @param x A prototype for the ready4 S3 class for declaring package description file data.
#' @return An unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_new_ready4fun_description
#' @export 
make_new_ready4fun_description <- function(x){ 
stopifnot(is.list(x))
class(x) <- append(c("ready4fun_description",setdiff(make_pt_ready4fun_description() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package description ready4 S3 class for declaring package description file data.
#' @description Create a new prototype for the ready4 S3 class for declaring package description file data.
#' @param Package PARAM_DESCRIPTION, Default: character(0)
#' @param Title PARAM_DESCRIPTION, Default: character(0)
#' @param Description PARAM_DESCRIPTION, Default: character(0)
#' @param License PARAM_DESCRIPTION, Default: logical(0)
#' @param URL PARAM_DESCRIPTION, Default: character(0)
#' @return A prototype for ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname make_pt_ready4fun_description
#' @export 
#' @importFrom ready4 update_pt_fn_args_ls
#' @importFrom rlang exec
make_pt_ready4fun_description <- function(Package = character(0),
Title = character(0),
Description = character(0),
License = logical(0),
URL = character(0)){ 
args_ls <- list(Package = Package,
Title = Title,
Description = Description,
License = License,
URL = URL) %>% ready4::update_pt_fn_args_ls()
rlang::exec(list,!!!args_ls)
}
#' Validate ready4fun package description ready4 S3 class for declaring package description file data.
#' @description Validate an instance of the ready4 S3 class for declaring package description file data.
#' @param x An unvalidated instance of the ready4 S3 class for declaring package description file data.
#' @return A prototpe for ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname validate_ready4fun_description
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom dplyr filter arrange pull
#' @importFrom purrr map_chr map2_chr
validate_ready4fun_description <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_description())],
names(make_pt_ready4fun_description())))!=length(names(make_pt_ready4fun_description()))){
stop(paste0("LIST must include elements named: ",
names(make_pt_ready4fun_description()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_description() %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
lapply(class) %>% transform_cls_type_ls() %>% tibble::as_tibble() %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_description())) %>% dplyr::arrange(variable))){
stop(paste0("LIST elements should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_description() %>% 
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
#' Is ready4fun package description ready4 S3 class for declaring package description file data.
#' @description Check whether an object is a valid instance of the ready4 S3 class for declaring package description file data.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for declaring package description file data.
#' @details ready4 S3 class for declaring package description file data.
#' @rdname is_ready4fun_description
#' @export 
is_ready4fun_description <- function(x) inherits(validate_ready4fun_description(x), "ready4fun_description")
