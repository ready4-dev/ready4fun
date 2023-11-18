#' Get house style abbreviations
#' @description An aspect of the ready4 framework is a consistent house style for code. Retrieve details on framework abbreviations with `get_abbrs`.
#' @param what_1L_chr What (a character vector of length one), Default: character(0)
#' @param type_1L_chr Type (a character vector of length one), Default: c("abbreviation", "extension")
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one), Default: 'NA'
#' @param dv_ds_metadata_ls Dataverse dataset metadata (a list), Default: list(list())
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'NA'
#' @param dv_server_1L_chr Dataverse server (a character vector of length one), Default: 'NA'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'NA'
#' @param search_descs_1L_lgl Search descriptions (a logical vector of length one), Default: deprecated()
#' @return Abbreviations (a lookup table)
#' @rdname get_abbrs
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @importFrom ready4 ingest
#' @importFrom dplyr filter
#' @importFrom purrr map_lgl
#' @importFrom stringr str_detect
#' @example man/examples/get_abbrs.R
get_abbrs <- function (what_1L_chr = character(0), type_1L_chr = c("abbreviation", 
    "extension"), abbreviations_lup = NULL, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", dv_nm_1L_chr = NA_character_, 
    dv_ds_metadata_ls = list(list()), dv_ds_nm_1L_chr = NA_character_, 
    dv_server_1L_chr = NA_character_, dv_url_pfx_1L_chr = NA_character_, 
    search_descs_1L_lgl = deprecated()) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    search_descs_1L_lgl <- ifelse(type_1L_chr == "extension", 
        F, T)
    if (is.null(abbreviations_lup)) {
        abbreviations_lup <- ready4use::Ready4useRepos(dv_nm_1L_chr = dv_nm_1L_chr, 
            dv_ds_metadata_ls = dv_ds_metadata_ls, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
            dv_server_1L_chr = dv_server_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            gh_repo_1L_chr = gh_repo_1L_chr, gh_tag_1L_chr = gh_tag_1L_chr) %>% 
            ready4::ingest(fls_to_ingest_chr = c("abbreviations_lup"), 
                metadata_1L_lgl = F)
    }
    if (!identical(what_1L_chr, character(0))) {
        if (!search_descs_1L_lgl) {
            abbreviations_lup <- abbreviations_lup %>% dplyr::filter(short_name_chr %>% 
                purrr::map_lgl(~startsWith(.x, what_1L_chr)))
        }
        else {
            abbreviations_lup <- abbreviations_lup %>% dplyr::filter(long_name_chr %>% 
                purrr::map_lgl(~stringr::str_detect(.x, what_1L_chr)))
        }
    }
    return(abbreviations_lup)
}
#' Get all dependencies of functions
#' @description get_all_depcys_of_fns() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get all dependencies of functions. The function returns Functions to keep (a character vector).
#' @param pkg_depcy_ls Package dependency (a list)
#' @param fns_chr Functions (a character vector)
#' @return Functions to keep (a character vector)
#' @rdname get_all_depcys_of_fns
#' @export 
#' @importFrom dplyr filter pull
#' @keywords internal
get_all_depcys_of_fns <- function (pkg_depcy_ls, fns_chr) 
{
    arg_ls <- list(new_dbl = pkg_depcy_ls$Nomfun %>% dplyr::filter(label %in% 
        fns_chr) %>% dplyr::pull(id) %>% as.numeric(), solo_dbl = numeric(0), 
        upper_tb = data.frame(from = numeric(0), to = numeric(0)))
    while (!identical(arg_ls$new_dbl, numeric(0))) {
        arg_ls <- make_depnt_fns_ls(arg_ls, pkg_depcy_ls = pkg_depcy_ls)
    }
    fn_idcs_dbl <- c(arg_ls$upper_tb$to, arg_ls$solo_dbl) %>% 
        unique() %>% sort()
    fns_to_keep_chr <- pkg_depcy_ls$Nomfun %>% dplyr::filter(id %in% 
        fn_idcs_dbl) %>% dplyr::pull(2)
    return(fns_to_keep_chr)
}
#' Get argument object type
#' @description get_arg_obj_type() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get argument object type. The function returns Argument object type (a character vector of length one).
#' @param argument_nm_1L_chr Argument name (a character vector of length one)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: deprecated()
#' @param key_1L_chr Key (a character vector of length one), Default: deprecated()
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: deprecated()
#' @return Argument object type (a character vector of length one)
#' @rdname get_arg_obj_type
#' @export 
#' @importFrom dplyr filter mutate pull
#' @keywords internal
get_arg_obj_type <- function (argument_nm_1L_chr, dv_ds_nm_1L_chr = "ready4-dev/ready4", 
    dv_url_pfx_1L_chr = deprecated(), key_1L_chr = deprecated(), 
    object_type_lup = NULL, server_1L_chr = deprecated()) 
{
    if (is.null(object_type_lup)) {
        object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    }
    nchar_int <- nchar(object_type_lup$short_name_chr)
    match_chr <- object_type_lup$long_name_chr[endsWith(argument_nm_1L_chr, 
        paste0(ifelse(nchar(argument_nm_1L_chr) == nchar_int, 
            "", "_"), object_type_lup$short_name_chr))]
    if (!identical(match_chr, character(0))) {
        arg_obj_type_1L_chr <- dplyr::filter(object_type_lup, 
            long_name_chr %in% match_chr) %>% dplyr::mutate(nchar_int = nchar(short_name_chr)) %>% 
            dplyr::filter(nchar_int == max(nchar_int)) %>% dplyr::pull(long_name_chr)
    }
    else {
        arg_obj_type_1L_chr <- character(0)
    }
    return(arg_obj_type_1L_chr)
}
#' Get development package name
#' @description get_dev_pkg_nm() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get development package name. The function returns Development package name (a character vector of length one).
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: '.'
#' @return Development package name (a character vector of length one)
#' @rdname get_dev_pkg_nm
#' @export 
#' @importFrom stringr str_sub
#' @keywords internal
get_dev_pkg_nm <- function (path_to_pkg_rt_1L_chr = ".") 
{
    dev_pkg_nm_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr, 
        "/DESCRIPTION"))[1] %>% stringr::str_sub(start = 10)
    return(dev_pkg_nm_1L_chr)
}
#' Get function arguments
#' @description get_fn_args() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get function arguments. The function returns Function arguments (a character vector).
#' @param fn Function (a function)
#' @return Function arguments (a character vector)
#' @rdname get_fn_args
#' @export 
#' @importFrom purrr discard
#' @keywords internal
get_fn_args <- function (fn) 
{
    fn_args_chr <- as.list(args(fn)) %>% names() %>% purrr::discard({
        . == ""
    })
    return(fn_args_chr)
}
#' Get function names in file
#' @description get_fn_nms_in_file() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get function names in file. The function returns Local (a character vector).
#' @param path_1L_chr Path (a character vector of length one)
#' @return Local (a character vector)
#' @rdname get_fn_nms_in_file
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
get_fn_nms_in_file <- function (path_1L_chr) 
{
    source(path_1L_chr, local = T)
    local_chr <- ls()
    local_chr <- local_chr[local_chr %>% purrr::map_lgl(~is.function(eval(parse(text = .x))))]
    return(local_chr)
}
#' Get function types
#' @description get_fn_types() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get function types. The function returns Function types (a lookup table).
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one), Default: 'NA'
#' @param dv_ds_metadata_ls Dataverse dataset metadata (a list), Default: list(list())
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'NA'
#' @param dv_server_1L_chr Dataverse server (a character vector of length one), Default: 'NA'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'NA'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param type_1L_chr Type (a character vector of length one), Default: c("submodule", "simple", "partial")
#' @param what_1L_chr What (a character vector of length one), Default: c("regular", "method", "all")
#' @return Function types (a lookup table)
#' @rdname get_fn_types
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @importFrom ready4 ingest
#' @importFrom dplyr filter select
#' @keywords internal
get_fn_types <- function (dv_nm_1L_chr = NA_character_, dv_ds_metadata_ls = list(list()), 
    dv_ds_nm_1L_chr = NA_character_, dv_server_1L_chr = NA_character_, 
    dv_url_pfx_1L_chr = NA_character_, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", type_1L_chr = c("submodule", 
        "simple", "partial"), what_1L_chr = c("regular", "method", 
        "all")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    fn_types_lup <- ready4use::Ready4useRepos(dv_nm_1L_chr = dv_nm_1L_chr, 
        dv_ds_metadata_ls = dv_ds_metadata_ls, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
        dv_server_1L_chr = dv_server_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        fl_nms_chr = "fn_types_lup", gh_repo_1L_chr = gh_repo_1L_chr, 
        gh_tag_1L_chr = gh_tag_1L_chr) %>% ready4::ingest(fls_to_ingest_chr = "fn_types_lup", 
        metadata_1L_lgl = F)
    if (what_1L_chr %in% c("regular")) {
        fn_types_lup <- dplyr::filter(fn_types_lup, !is_generic_lgl)
    }
    if (what_1L_chr == "method") {
        fn_types_lup <- dplyr::filter(fn_types_lup, is_generic_lgl)
    }
    if (type_1L_chr == "submodule") {
        fn_types_lup <- ready4fun_functions(fn_types_lup)
    }
    if (type_1L_chr == "partial") {
        fn_types_lup <- fn_types_lup %>% dplyr::select(fn_type_nm_chr, 
            fn_type_desc_chr, is_method_lgl)
    }
    if (type_1L_chr == "simple") {
        fn_types_lup <- fn_types_lup %>% dplyr::select(fn_type_nm_chr, 
            fn_type_desc_chr)
    }
    return(fn_types_lup)
}
#' Get method title
#' @description get_mthd_title() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get method title. The function returns Method title (a character vector of length one).
#' @param mthd_nm_1L_chr Method name (a character vector of length one)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: 'ready4'
#' @return Method title (a character vector of length one)
#' @rdname get_mthd_title
#' @export 
#' @importFrom stringr str_sub
#' @importFrom tools Rd_db
#' @importFrom purrr pluck
#' @keywords internal
get_mthd_title <- function (mthd_nm_1L_chr, pkg_nm_1L_chr = "ready4") 
{
    df <- mthd_nm_1L_chr %>% str_locate("\\.")
    if (!is.na(df[[1, 1]])) {
        mthd_nm_1L_chr <- stringr::str_sub(mthd_nm_1L_chr, end = (df[[1, 
            1]] - 1))
    }
    gnrc_dmt_ls <- tools::Rd_db("ready4") %>% purrr::pluck(paste0(mthd_nm_1L_chr, 
        "-methods.Rd"))
    mthd_title_1L_chr <- ifelse(!is.null(gnrc_dmt_ls), gnrc_dmt_ls %>% 
        purrr::pluck(1) %>% purrr::pluck(1) %>% as.vector(), 
        mthd_nm_1L_chr)
    return(mthd_title_1L_chr)
}
#' Get new abbreviations
#' @description get_new_abbrs() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get new abbreviations. The function returns New abbreviations (a character vector).
#' @param pkg_setup_ls Package setup (a list)
#' @param append_1L_lgl Append (a logical vector of length one), Default: T
#' @param classes_to_make_tb Classes to make (a tibble), Default: NULL
#' @param inc_all_mthds_1L_lgl Include all methods (a logical vector of length one), Default: T
#' @param paths_ls Paths (a list), Default: make_fn_nms()
#' @param pkg_ds_ls_ls Package dataset (a list of lists), Default: NULL
#' @param transformations_chr Transformations (a character vector), Default: NULL
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @param use_last_1L_int Use last (an integer vector of length one), Default: NULL
#' @param fns_dmt_tb Functions documentation (a tibble), Default: deprecated()
#' @return New abbreviations (a character vector)
#' @rdname get_new_abbrs
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom tibble tibble
#' @importFrom purrr map flatten_chr discard
#' @keywords internal
get_new_abbrs <- function (pkg_setup_ls, append_1L_lgl = T, classes_to_make_tb = NULL, 
    inc_all_mthds_1L_lgl = T, paths_ls = make_fn_nms(), pkg_ds_ls_ls = NULL, 
    transformations_chr = NULL, undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T), 
    use_last_1L_int = NULL, fns_dmt_tb = deprecated()) 
{
    if (lifecycle::is_present(fns_dmt_tb)) {
        lifecycle::deprecate_warn("0.0.0.9421", "ready4fun::get_new_abbrs(fns_dmt_tb)", 
            details = "Please use `ready4fun::get_new_abbrs(pkg_desc_ls)` to pass the fns_dmt_tb object to this function.")
    }
    if (identical(pkg_setup_ls$subsequent_ls$fns_dmt_tb, tibble::tibble())) {
        pkg_setup_ls$subsequent_ls$fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls, 
            abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            append_1L_lgl = append_1L_lgl, custom_dmt_ls = pkg_setup_ls$subsequent_ls$custom_dmt_ls, 
            fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
            inc_all_mthds_1L_lgl = inc_all_mthds_1L_lgl, object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
            undocumented_fns_dir_chr = undocumented_fns_dir_chr)
    }
    if (is.null(use_last_1L_int)) {
        new_fn_abbrs_chr <- pkg_setup_ls$subsequent_ls$fns_dmt_tb$fns_chr %>% 
            get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
                drop_first_1L_lgl = T, treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr, 
                use_last_1L_int = use_last_1L_int)
    }
    else {
        new_fn_abbrs_chr <- character(0)
    }
    new_arg_abbrs_chr <- pkg_setup_ls$subsequent_ls$fns_dmt_tb$args_ls %>% 
        purrr::map(~names(.x) %>% get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr, 
            use_last_1L_int = use_last_1L_int)) %>% purrr::flatten_chr() %>% 
        unique()
    if (!is.null(pkg_ds_ls_ls)) {
        new_ds_abbrs_chr <- pkg_ds_ls_ls %>% purrr::map(~c(names(.x$db_df)), 
            .x$db_1L_chr) %>% purrr::flatten_chr() %>% get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr, 
            use_last_1L_int = use_last_1L_int)
    }
    else {
        new_ds_abbrs_chr <- character(0)
    }
    if (!is.null(classes_to_make_tb)) {
        new_clss_abbrs_chr <- classes_to_make_tb$vals_ls %>% 
            purrr::discard(is.null) %>% purrr::map(~names(.x)) %>% 
            purrr::flatten_chr() %>% unique() %>% get_new_abbrs_cndts(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
            treat_as_words_chr = pkg_setup_ls$subsequent_ls$treat_as_words_chr, 
            use_last_1L_int = use_last_1L_int)
    }
    else {
        new_clss_abbrs_chr <- character(0)
    }
    new_abbrs_chr <- c(new_fn_abbrs_chr, new_arg_abbrs_chr, new_clss_abbrs_chr, 
        new_ds_abbrs_chr) %>% unique() %>% sort()
    if (!is.null(transformations_chr)) {
        new_abbrs_chr <- c(setdiff(new_abbrs_chr, transformations_chr), 
            names(transformations_chr)) %>% sort()
    }
    return(new_abbrs_chr)
}
#' Get new abbreviations candidates
#' @description get_new_abbrs_cndts() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get new abbreviations candidates. The function returns New abbreviations candidates (a character vector).
#' @param text_chr Text (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param drop_first_1L_lgl Drop first (a logical vector of length one), Default: F
#' @param use_last_1L_int Use last (an integer vector of length one), Default: NULL
#' @param treat_as_words_chr Treat as words (a character vector), Default: character(0)
#' @return New abbreviations candidates (a character vector)
#' @rdname get_new_abbrs_cndts
#' @export 
#' @importFrom purrr map flatten_chr
#' @importFrom Hmisc capitalize
#' @keywords internal
get_new_abbrs_cndts <- function (text_chr, abbreviations_lup, drop_first_1L_lgl = F, 
    use_last_1L_int = NULL, treat_as_words_chr = character(0)) 
{
    new_abbrs_cndts_chr <- text_chr %>% purrr::map(~{
        candidates_chr <- strsplit(.x, "_")[[1]] %>% purrr::map(~strsplit(.x, 
            "\\.")[[1]]) %>% purrr::flatten_chr()
        if (drop_first_1L_lgl) {
            candidates_chr <- candidates_chr[-1]
        }
        if (!is.null(use_last_1L_int)) {
            candidates_chr <- candidates_chr %>% tail(use_last_1L_int)
        }
        candidates_chr
    }) %>% purrr::flatten_chr() %>% unique() %>% sort() %>% setdiff(abbreviations_lup$short_name_chr)
    data("GradyAugmented", package = "qdapDictionaries", envir = environment())
    new_abbrs_cndts_chr <- setdiff(setdiff(new_abbrs_cndts_chr[suppressWarnings(is.na(as.numeric(new_abbrs_cndts_chr)))], 
        c(c(GradyAugmented, treat_as_words_chr), c(GradyAugmented, 
            treat_as_words_chr) %>% toupper(), c(GradyAugmented, 
            treat_as_words_chr) %>% Hmisc::capitalize())), "")
    return(new_abbrs_cndts_chr)
}
#' Get new class prototypes
#' @description get_new_cls_pts() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get new class prototypes. The function returns New class prototypes (a character vector).
#' @param pkg_setup_ls Package setup (a list)
#' @return New class prototypes (a character vector)
#' @rdname get_new_cls_pts
#' @export 
#' @importFrom purrr flatten flatten_chr pmap_chr
#' @importFrom Hmisc capitalize
#' @keywords internal
get_new_cls_pts <- function (pkg_setup_ls) 
{
    incdd_clss_chr <- c(pkg_setup_ls$subsequent_ls$prototype_lup$type_chr, 
        pkg_setup_ls$subsequent_ls$prototype_lup$fn_to_call_chr) %>% 
        unique()
    incdd_clss_chr <- incdd_clss_chr[incdd_clss_chr != ""]
    new_cls_pts_chr <- setdiff(pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x$pt_ls %>% 
        purrr::flatten() %>% purrr::flatten_chr() %>% unique(), 
        c(incdd_clss_chr, pkg_setup_ls$subsequent_ls$cls_fn_ls$args_ls$x %>% 
            purrr::pmap_chr(~ifelse(..1, paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package, 
                "_", ..2), paste0(pkg_setup_ls$initial_ls$pkg_desc_ls$Package %>% 
                Hmisc::capitalize(), ..2)))))
    return(new_cls_pts_chr)
}
#' Get new function types
#' @description get_new_fn_types() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get new function types. The function returns New function types (a character vector).
#' @param pkg_setup_ls Package setup (a list)
#' @param fn_nms_ls Function names (a list), Default: make_fn_nms()
#' @param undmtd_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @return New function types (a character vector)
#' @rdname get_new_fn_types
#' @export 
#' @importFrom purrr map2 map_lgl flatten_chr
#' @importFrom stringr str_remove str_sub
#' @importFrom dplyr filter pull
#' @importFrom tools toTitleCase
#' @keywords internal
get_new_fn_types <- function (pkg_setup_ls, fn_nms_ls = make_fn_nms(), undmtd_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)) 
{
    new_fn_types_chr <- purrr::map2(fn_nms_ls[names(fn_nms_ls) != 
        "gnrcs"], undmtd_fns_dir_chr[undmtd_fns_dir_chr %>% purrr::map_lgl(~!endsWith(.x, 
        "gnrcs"))], ~stringr::str_remove(.x, paste0(.y, "/")) %>% 
        stringr::str_sub(end = -3)) %>% purrr::flatten_chr()
    methods_chr <- intersect(new_fn_types_chr, pkg_setup_ls$subsequent_ls$fn_types_lup %>% 
        dplyr::filter(is_generic_lgl) %>% dplyr::pull(fn_type_nm_chr))
    new_fn_types_chr <- c(new_fn_types_chr %>% setdiff(methods_chr) %>% 
        unique() %>% sort() %>% make_fn_title(abbreviations_lup = pkg_setup_ls$subsequent_ls$abbreviations_lup, 
        fn_types_lup = pkg_setup_ls$subsequent_ls$fn_types_lup, 
        object_type_lup = pkg_setup_ls$subsequent_ls$object_type_lup, 
        is_generic_lgl = T) %>% tools::toTitleCase(), methods_chr) %>% 
        sort()
    generics_dir_1L_chr <- undmtd_fns_dir_chr[undmtd_fns_dir_chr %>% 
        purrr::map_lgl(~endsWith(.x, "gnrcs"))]
    if (!identical(generics_dir_1L_chr, character(0))) {
        new_fn_types_chr <- new_fn_types_chr %>% c(get_fn_nms_in_file(paste0(generics_dir_1L_chr, 
            "/generics.R"))) %>% unique() %>% sort()
    }
    if (!is.null(pkg_setup_ls$subsequent_ls$fn_types_lup)) {
        new_fn_types_chr <- new_fn_types_chr %>% setdiff(pkg_setup_ls$subsequent_ls$fn_types_lup$fn_type_nm_chr)
    }
    return(new_fn_types_chr)
}
#' Get object type new cases
#' @description get_obj_type_new_cses() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get object type new cases. The function returns Object type lookup table new cases (a tibble).
#' @param updated_obj_type_lup Updated object type (a lookup table)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: deprecated()
#' @param excluded_chr Excluded (a character vector), Default: 'NA'
#' @param key_1L_chr Key (a character vector of length one), Default: deprecated()
#' @param old_obj_type_lup Old object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: deprecated()
#' @return Object type lookup table new cases (a tibble)
#' @rdname get_obj_type_new_cses
#' @export 
#' @importFrom dplyr filter
#' @keywords internal
get_obj_type_new_cses <- function (updated_obj_type_lup, dv_ds_nm_1L_chr = "ready4-dev/ready4", 
    dv_url_pfx_1L_chr = deprecated(), excluded_chr = NA_character_, 
    key_1L_chr = deprecated(), old_obj_type_lup = NULL, server_1L_chr = deprecated()) 
{
    if (is.null(old_obj_type_lup)) {
        old_obj_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    }
    obj_type_lup_new_cses_tb <- updated_obj_type_lup %>% dplyr::filter(!short_name_chr %in% 
        old_obj_type_lup$short_name_chr)
    if (!is.na(excluded_chr[1])) {
        obj_type_lup_new_cses_tb <- obj_type_lup_new_cses_tb %>% 
            dplyr::filter(!short_name_chr %in% excluded_chr)
    }
    return(obj_type_lup_new_cses_tb)
}
#' Get object types
#' @description get_obj_types() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get object types. The function returns Object type (a lookup table).
#' @param dv_nm_1L_chr Dataverse name (a character vector of length one), Default: 'NA'
#' @param dv_ds_metadata_ls Dataverse dataset metadata (a list), Default: list(list())
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'NA'
#' @param dv_server_1L_chr Dataverse server (a character vector of length one), Default: 'NA'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'NA'
#' @param gh_repo_1L_chr Github repository (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param gh_tag_1L_chr Github tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param type_1L_chr Type (a character vector of length one), Default: c("submodule", "tibble")
#' @param what_1L_chr What (a character vector of length one), Default: c("seed", "all")
#' @return Object type (a lookup table)
#' @rdname get_obj_types
#' @export 
#' @importFrom ready4use Ready4useRepos
#' @importFrom ready4 ingest
#' @keywords internal
get_obj_types <- function (dv_nm_1L_chr = NA_character_, dv_ds_metadata_ls = list(list()), 
    dv_ds_nm_1L_chr = NA_character_, dv_server_1L_chr = NA_character_, 
    dv_url_pfx_1L_chr = NA_character_, gh_repo_1L_chr = "ready4-dev/ready4", 
    gh_tag_1L_chr = "Documentation_0.0", type_1L_chr = c("submodule", 
        "tibble"), what_1L_chr = c("seed", "all")) 
{
    type_1L_chr <- match.arg(type_1L_chr)
    what_1L_chr <- match.arg(what_1L_chr)
    obj_type_lup <- ready4use::Ready4useRepos(dv_nm_1L_chr = dv_nm_1L_chr, 
        dv_ds_metadata_ls = dv_ds_metadata_ls, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
        dv_server_1L_chr = dv_server_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        fl_nms_chr = ifelse(what_1L_chr == "seed", "seed_obj_type_lup", 
            "object_type_lup"), gh_repo_1L_chr = gh_repo_1L_chr, 
        gh_tag_1L_chr = gh_tag_1L_chr) %>% ready4::ingest(fls_to_ingest_chr = ifelse(what_1L_chr == 
        "seed", "seed_obj_type_lup", "object_type_lup"), metadata_1L_lgl = F)
    if (type_1L_chr == "submodule") {
        if (what_1L_chr == "seed") {
            obj_type_lup <- ready4fun_objects(obj_type_lup)
        }
        else {
            obj_type_lup <- ready4fun_abbreviations(obj_type_lup)
        }
    }
    return(obj_type_lup)
}
#' Get output object type
#' @description get_outp_obj_type() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get output object type. The function returns Output object type (a character vector).
#' @param fns_chr Functions (a character vector)
#' @param abbreviations_lup Abbreviations (a lookup table)
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'ready4-dev/ready4'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: deprecated()
#' @param fns_env_ls Functions (a list of environments)
#' @param is_generic_lgl Is generic (a logical vector), Default: F
#' @param key_1L_chr Key (a character vector of length one), Default: deprecated()
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: deprecated()
#' @return Output object type (a character vector)
#' @rdname get_outp_obj_type
#' @export 
#' @importFrom purrr map2_chr
#' @keywords internal
get_outp_obj_type <- function (fns_chr, abbreviations_lup, dv_ds_nm_1L_chr = "ready4-dev/ready4", 
    dv_url_pfx_1L_chr = deprecated(), fns_env_ls, is_generic_lgl = F, 
    key_1L_chr = deprecated(), object_type_lup = NULL, server_1L_chr = deprecated()) 
{
    if (is.null(object_type_lup)) {
        object_type_lup <- get_rds_from_pkg_dmt(fl_nm_1L_chr = "object_type_lup", 
            piggyback_to_1L_chr = dv_ds_nm_1L_chr)
    }
    outp_obj_type_chr <- purrr::map2_chr(fns_chr, is_generic_lgl, 
        ~{
            if (.y) {
                "NULL"
            }
            else {
                if (!is.null(fns_env_ls$fns_env[[.x]])) {
                  fn <- fns_env_ls$fns_env[[.x]]
                }
                else {
                  fn <- eval(parse(text = .x))
                }
                return_obj_chr <- get_return_obj_nm(fn) %>% make_arg_desc(abbreviations_lup = abbreviations_lup, 
                  object_type_lup = object_type_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
                  dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
                  server_1L_chr = server_1L_chr)
                ifelse(return_obj_chr == "NO MATCH", "NULL", 
                  return_obj_chr)
            }
        })
    return(outp_obj_type_chr)
}
#' Get rds from package documentation
#' @description get_rds_from_pkg_dmt() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get rds from package documentation. The function returns R object (an output object of multiple potential types).
#' @param pkg_setup_ls Package setup (a list), Default: NULL
#' @param fl_nm_1L_chr File name (a character vector of length one)
#' @param piggyback_to_1L_chr Piggyback to (a character vector of length one), Default: character(0)
#' @param piggyback_tag_1L_chr Piggyback tag (a character vector of length one), Default: 'Documentation_0.0'
#' @param piggyback_token_1L_chr Piggyback token (a character vector of length one), Default: ''
#' @return R object (an output object of multiple potential types)
#' @rdname get_rds_from_pkg_dmt
#' @export 
#' @importFrom piggyback pb_download_url
#' @keywords internal
get_rds_from_pkg_dmt <- function (pkg_setup_ls = NULL, fl_nm_1L_chr, piggyback_to_1L_chr = character(0), 
    piggyback_tag_1L_chr = "Documentation_0.0", piggyback_token_1L_chr = "") 
{
    if (!is.null(pkg_setup_ls)) {
        piggyback_to_1L_chr <- pkg_setup_ls$subsequent_ls$piggyback_to_1L_chr
    }
    dmt_urls_chr <- piggyback::pb_download_url(repo = piggyback_to_1L_chr, 
        tag = piggyback_tag_1L_chr, .token = piggyback_token_1L_chr)
    dmt_url_1L_chr <- dmt_urls_chr[dmt_urls_chr %>% endsWith(paste0(fl_nm_1L_chr, 
        ".RDS")) | dmt_urls_chr %>% endsWith(paste0(fl_nm_1L_chr, 
        ".Rds")) | dmt_urls_chr %>% endsWith(paste0(fl_nm_1L_chr, 
        ".rds"))]
    r_object_xx <- readRDS(url(dmt_url_1L_chr))
    return(r_object_xx)
}
#' Get return object name
#' @description get_return_obj_nm() is a Get function that extracts data from an object. Specifically, this function implements an algorithm to get return object name. The function returns Return (a character vector of length one).
#' @param fn Function (a function)
#' @return Return (a character vector of length one)
#' @rdname get_return_obj_nm
#' @export 
#' @importFrom stringr str_replace str_sub
#' @keywords internal
get_return_obj_nm <- function (fn) 
{
    fn_chr <- deparse(fn)
    last_line_1L_chr <- fn_chr[length(fn_chr) - 1] %>% trimws()
    if (startsWith(last_line_1L_chr, "return(")) {
        return_1L_chr <- stringr::str_replace(last_line_1L_chr, 
            "return", "") %>% stringr::str_sub(start = 2, end = -2)
    }
    else {
        return_1L_chr <- NA_character_
    }
    return(return_1L_chr)
}
