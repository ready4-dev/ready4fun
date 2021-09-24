
setOldClass(c("ready4fun_badges","tbl_df", "tbl", "data.frame"))
#' ready4 S3 class for tibble object lookup table of badges metadata.
#' @description Create a new valid instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of badges metadata., Default: make_pt_ready4fun_badges()
#' @return A validated instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @details ready4 S3 class for tibble object lookup table of badges metadata.
#' @rdname ready4fun_badges
#' @export 

ready4fun_badges <- function(x = make_pt_ready4fun_badges()){ 
validate_ready4fun_badges(make_new_ready4fun_badges(x))
}
#' Make new ready4fun package badges ready4 S3 class for tibble object lookup table of badges metadata.
#' @description Create a new unvalidated instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @param x A prototype for the ready4 S3 class for tibble object lookup table of badges metadata.
#' @return An unvalidated instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @details ready4 S3 class for tibble object lookup table of badges metadata.
#' @rdname make_new_ready4fun_badges
#' @export 
#' @importFrom tibble is_tibble
make_new_ready4fun_badges <- function(x){ 
stopifnot(tibble::is_tibble(x))
class(x) <- append(c("ready4fun_badges",setdiff(make_pt_ready4fun_badges() %>% class(),class(x))),
class(x))
x
}
#' Make prototype ready4fun package badges ready4 S3 class for tibble object lookup table of badges metadata.
#' @description Create a new prototype for the ready4 S3 class for tibble object lookup table of badges metadata.
#' @param badge_names_chr Badge names (a character vector), Default: character(0)
#' @param label_names_chr Label names (a character vector), Default: character(0)
#' @param colours_chr Colours (a character vector), Default: character(0)
#' @param badges_chr Badges (a character vector), Default: character(0)
#' @return A prototype for ready4 S3 class for tibble object lookup table of badges metadata.
#' @details ready4 S3 class for tibble object lookup table of badges metadata.
#' @rdname make_pt_ready4fun_badges
#' @export 
#' @importFrom rlang exec
#' @importFrom tibble tibble
make_pt_ready4fun_badges <- function(badge_names_chr = character(0),
label_names_chr = character(0),
colours_chr = character(0),
badges_chr = character(0)){ 
args_ls <- list(badge_names_chr = badge_names_chr,
label_names_chr = label_names_chr,
colours_chr = colours_chr,
badges_chr = badges_chr) %>% update_pt_fn_args_ls()
rlang::exec(tibble::tibble,!!!args_ls)
}
#' Validate ready4fun package badges ready4 S3 class for tibble object lookup table of badges metadata.
#' @description Validate an instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @param x An unvalidated instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @return A prototpe for ready4 S3 class for tibble object lookup table of badges metadata.
#' @details ready4 S3 class for tibble object lookup table of badges metadata.
#' @rdname validate_ready4fun_badges
#' @export 
#' @importFrom stringr str_detect str_c
#' @importFrom dplyr summarise_all filter arrange pull
#' @importFrom tidyr gather
#' @importFrom purrr map_chr map2_chr
validate_ready4fun_badges <- function(x){
if(sum(stringr::str_detect(names(x)[names(x) %in% names(make_pt_ready4fun_badges())],
names(make_pt_ready4fun_badges())))!=length(names(make_pt_ready4fun_badges()))){
stop(paste0("TIBBLE must include columns named: ",
names(make_pt_ready4fun_badges()) %>% stringr::str_c(sep="", collapse = ", ")),
call. = FALSE)
}

 if(!identical(make_pt_ready4fun_badges() %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
 dplyr::filter(!is.na(class)) %>% 
dplyr::arrange(variable),
x %>% 
dplyr::summarise_all(class) %>% 
 tidyr::gather(variable,class) %>% 
dplyr::filter(variable %in% names(make_pt_ready4fun_badges())) %>% dplyr::arrange(variable))){
stop(paste0("TIBBLE columns should be of the following classes: ",
"",
{
class_lup <- make_pt_ready4fun_badges() %>% 
dplyr::summarise_all(class) %>% 
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
#' Is ready4fun package badges ready4 S3 class for tibble object lookup table of badges metadata.
#' @description Check whether an object is a valid instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @param x An object of any type
#' @return A logical value, TRUE if a valid instance of the ready4 S3 class for tibble object lookup table of badges metadata.
#' @details ready4 S3 class for tibble object lookup table of badges metadata.
#' @rdname is_ready4fun_badges
#' @export 

is_ready4fun_badges <- function(x) inherits(validate_ready4fun_badges(x), "ready4fun_badges")
