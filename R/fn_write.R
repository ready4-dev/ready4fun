#' Write abbreviation
#' @description write_abbr_lup() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write abbreviation lookup table. The function returns Package datasets (a tibble).
#' @param seed_lup Seed (a lookup table), Default: NULL
#' @param url_1L_chr Url (a character vector of length one), Default: deprecated()
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param no_plural_chr No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Package datasets (a tibble)
#' @rdname write_abbr_lup
#' @export 
#' @importFrom tibble tibble
#' @importFrom lifecycle is_present deprecate_warn
#' @keywords internal
write_abbr_lup <- function (seed_lup = NULL, url_1L_chr = deprecated(), pkg_nm_1L_chr = get_dev_pkg_nm(), 
    short_name_chr = NA_character_, long_name_chr = NA_character_, 
    no_plural_chr = NA_character_, custom_plural_ls = NULL, overwrite_1L_lgl = T, 
    object_type_lup = NULL, pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0)), 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (lifecycle::is_present(url_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9323", "ready4fun::write_abbr_lup(url_1L_chr)", 
            details = "Please use `ready4fun::write_abbr_lup(dv_ds_nm_1L_chr)` instead.")
    }
    if (is.null(seed_lup)) {
        seed_lup <- get_rds_from_dv("object_type_lup", dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
            dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr)
    }
    if (is.null(object_type_lup)) {
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    }
    pkg_dss_tb <- update_abbr_lup(seed_lup, short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr, no_plural_chr = no_plural_chr, 
        custom_plural_ls = custom_plural_ls) %>% write_and_doc_ds(db_df = ., 
        overwrite_1L_lgl = overwrite_1L_lgl, db_1L_chr = "abbreviations_lup", 
        title_1L_chr = "Common abbreviations lookup table", desc_1L_chr = paste0("A lookup table for abbreviations commonly used in object names in the ", 
            pkg_nm_1L_chr, "package."), format_1L_chr = "A tibble", 
        url_1L_chr = dv_ds_nm_1L_chr, abbreviations_lup = ., 
        object_type_lup = object_type_lup, pkg_dss_tb = pkg_dss_tb, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    return(pkg_dss_tb)
}
#' Write all tibbles in tibbles ready4 S4 to comma separated variables files
#' @description write_all_tbs_in_tbs_r4_to_csvs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write all tibbles in tibbles ready4 s4 to comma separated variables files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param tbs_r4 Tibbles (a ready4 S4)
#' @param r4_name_1L_chr Ready4 S4 name (a character vector of length one)
#' @param lup_dir_1L_chr Lookup table directory (a character vector of length one)
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @return NULL
#' @rdname write_all_tbs_in_tbs_r4_to_csvs
#' @export 
#' @importFrom purrr walk
#' @importFrom methods getSlots
#' @keywords internal
write_all_tbs_in_tbs_r4_to_csvs <- function (tbs_r4, r4_name_1L_chr, lup_dir_1L_chr, pfx_1L_chr) 
{
    purrr::walk(methods::getSlots(r4_name_1L_chr) %>% names(), 
        ~write_tb_to_csv(tbs_r4 = tbs_r4, slot_nm_1L_chr = .x, 
            r4_name_1L_chr = r4_name_1L_chr, lup_dir_1L_chr = lup_dir_1L_chr, 
            pfx_1L_chr = pfx_1L_chr))
}
#' Write and document dataset
#' @description write_and_doc_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write and document dataset. The function returns Package datasets (a tibble).
#' @param db_df Database (a data.frame)
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @param db_1L_chr Database (a character vector of length one)
#' @param title_1L_chr Title (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param format_1L_chr Format (a character vector of length one), Default: 'A tibble'
#' @param url_1L_chr Url (a character vector of length one), Default: 'NA'
#' @param vars_ls Variables (a list), Default: NULL
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param simple_lup_1L_lgl Simple lookup table (a logical vector of length one), Default: F
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Package datasets (a tibble)
#' @rdname write_and_doc_ds
#' @export 
#' @importFrom tibble tibble add_case
#' @importFrom utils data
#' @importFrom devtools document load_all
#' @keywords internal
write_and_doc_ds <- function (db_df, overwrite_1L_lgl = T, db_1L_chr, title_1L_chr, 
    desc_1L_chr, format_1L_chr = "A tibble", url_1L_chr = NA_character_, 
    vars_ls = NULL, R_dir_1L_chr = "R", simple_lup_1L_lgl = F, 
    abbreviations_lup = NULL, object_type_lup = NULL, pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0)), 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    eval(parse(text = paste0(db_1L_chr, "<-db_df")))
    eval(parse(text = paste0("usethis::use_data(", db_1L_chr, 
        ", overwrite = overwrite_1L_lgl)")))
    sink(paste0(R_dir_1L_chr, "/db_", db_1L_chr, ".R"), append = F)
    write_ds_dmt(db_df = db_df, db_1L_chr = db_1L_chr, title_1L_chr = title_1L_chr, 
        desc_1L_chr = desc_1L_chr, format_1L_chr = format_1L_chr, 
        vars_ls = vars_ls, url_1L_chr = url_1L_chr, R_dir_1L_chr = R_dir_1L_chr, 
        simple_lup_1L_lgl = simple_lup_1L_lgl, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
        dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
        server_1L_chr = server_1L_chr)
    close_open_sinks()
    devtools::document()
    devtools::load_all()
    pkg_dss_tb <- tibble::add_case(pkg_dss_tb, ds_obj_nm_chr = db_1L_chr, 
        title_chr = title_1L_chr, desc_chr = desc_1L_chr, url_chr = url_1L_chr)
    return(pkg_dss_tb)
}
#' Write and document function files
#' @description write_and_doc_fn_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write and document function files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @param path_to_dmt_dir_1L_chr Path to documentation directory (a character vector of length one)
#' @param path_to_dvpr_dmt_dir_1L_chr Path to developer documentation directory (a character vector of length one), Default: deprecated()
#' @param path_to_user_dmt_dir_1L_chr Path to user documentation directory (a character vector of length one), Default: deprecated()
#' @param make_pdfs_1L_lgl Make pdfs (a logical vector of length one), Default: T
#' @param dev_pkgs_chr Development packages (a character vector), Default: 'NA'
#' @param update_pkgdown_1L_lgl Update pkgdown (a logical vector of length one), Default: T
#' @return NULL
#' @rdname write_and_doc_fn_fls
#' @export 
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom purrr walk2 map2 flatten_chr discard
#' @importFrom devtools document load_all build_manual
#' @importFrom utils data
#' @importFrom dplyr filter pull
#' @keywords internal
write_and_doc_fn_fls <- function (fns_dmt_tb, r_dir_1L_chr = "R", path_to_pkg_rt_1L_chr = getwd(), 
    path_to_dmt_dir_1L_chr, path_to_dvpr_dmt_dir_1L_chr = deprecated(), 
    path_to_user_dmt_dir_1L_chr = deprecated(), make_pdfs_1L_lgl = T, 
    dev_pkgs_chr = NA_character_, update_pkgdown_1L_lgl = T) 
{
    if (lifecycle::is_present(path_to_dvpr_dmt_dir_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9307", "ready4fun::write_and_doc_fn_fls(path_to_dvpr_dmt_dir_1L_chr)", 
            details = "Please use `ready4fun::write_and_doc_fn_fls(path_to_dmt_dir_1L_chr)` to specify the directory to which both 'Developer' and 'User' documentation sub-directories will be written.")
    }
    if (lifecycle::is_present(path_to_user_dmt_dir_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9307", "ready4fun::write_and_doc_fn_fls(path_to_user_dmt_dir_1L_chr)", 
            details = "Please use `ready4fun::write_and_doc_fn_fls(path_to_dmt_dir_1L_chr)` to specify the directory to which both 'Developer' and 'User' documentation sub-directories will be written.")
    }
    write_new_dirs(c(path_to_dmt_dir_1L_chr, paste0(path_to_dmt_dir_1L_chr, 
        "/Developer"), paste0(path_to_dmt_dir_1L_chr, "/User")))
    purrr::walk2(list(paste0(path_to_dmt_dir_1L_chr, "/Developer"), 
        paste0(path_to_dmt_dir_1L_chr, "/User")), c(T, F), ~{
        write_new_files(paths_chr = paste0(r_dir_1L_chr, "/", 
            fns_dmt_tb$file_pfx_chr[1], fns_dmt_tb$file_nm_chr %>% 
                unique()), custom_write_ls = list(fn = write_fn_fl, 
            args_ls = list(fns_dmt_tb, r_dir_1L_chr = r_dir_1L_chr, 
                document_unexp_lgl = .y)))
        devtools::document()
        devtools::load_all()
        write_ns_imps_to_desc(dev_pkgs_chr = dev_pkgs_chr, incr_ver_1L_lgl = .y)
        devtools::load_all()
        if (make_pdfs_1L_lgl) 
            devtools::build_manual(path = .x)
    })
    if (update_pkgdown_1L_lgl) {
        datasets_chr <- utils::data(package = get_dev_pkg_nm(path_to_pkg_rt_1L_chr), 
            envir = environment())$results[, 3]
        writeLines(c("development:", "  mode: auto", "reference:", 
            "- title: \"Datasets\"", "- contents:", paste0("  - ", 
                datasets_chr), {
                if ("prototype_lup" %in% datasets_chr) {
                  utils::data("prototype_lup", package = get_dev_pkg_nm(path_to_pkg_rt_1L_chr), 
                    envir = environment())
                  fns_chr <- prototype_lup %>% dplyr::filter(pt_ns_chr == 
                    get_dev_pkg_nm(path_to_pkg_rt_1L_chr)) %>% 
                    dplyr::pull(fn_to_call_chr)
                  if (length(fns_chr) > 0) {
                    c(paste0("- title: \"", "Classes", "\""), 
                      "- contents:", paste0("  - ", fns_chr))
                  }
                }
            }, purrr::map2(c("fn_", "grp_", "mthd_"), c("Functions", 
                "Generics", "Methods"), ~{
                fns_chr <- dplyr::filter(fns_dmt_tb, inc_for_main_user_lgl & 
                  file_pfx_chr == .x) %>% dplyr::pull(fns_chr)
                if (length(fns_chr) > 0) {
                  txt_chr <- c(paste0("- title: \"", .y, "\""), 
                    "- contents:", paste0("  - ", fns_chr))
                } else {
                  txt_chr <- ""
                }
            }) %>% purrr::flatten_chr() %>% purrr::discard(~.x == 
                "")), con = paste0(path_to_pkg_rt_1L_chr, "/_pkgdown.yml"))
    }
}
#' Write documented function type
#' @description write_dmtd_fn_type_lup() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write documented function type lookup table. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: make_fn_type_lup()
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @param url_1L_chr Url (a character vector of length one), Default: deprecated()
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname write_dmtd_fn_type_lup
#' @export 
#' @importFrom tibble tibble
#' @importFrom lifecycle is_present deprecate_warn
#' @importFrom utils data
#' @keywords internal
write_dmtd_fn_type_lup <- function (fn_type_lup_tb = make_fn_type_lup(), overwrite_1L_lgl = T, 
    pkg_nm_1L_chr = get_dev_pkg_nm(), url_1L_chr = deprecated(), 
    abbreviations_lup = NULL, object_type_lup = NULL, pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0)), 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (lifecycle::is_present(url_1L_chr)) {
        lifecycle::deprecate_warn("0.0.0.9323", "ready4fun::write_dmtd_fn_type_lup(url_1L_chr)", 
            details = "Please use `ready4fun::write_dmtd_fn_type_lup(dv_ds_nm_1L_chr)` instead.")
    }
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    fn_type_lup_tb %>% write_and_doc_ds(overwrite_1L_lgl = overwrite_1L_lgl, 
        db_1L_chr = "fn_type_lup_tb", title_1L_chr = "Function type lookup table", 
        desc_1L_chr = paste0("A lookup table to find descriptions for different types of functions used within the ", 
            pkg_nm_1L_chr, " package suite."), format_1L_chr = "A tibble", 
        url_1L_chr = dv_ds_nm_1L_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, pkg_dss_tb = pkg_dss_tb, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
}
#' Write documented functions
#' @description write_documented_fns() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write documented functions. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param tmp_fn_dir_1L_chr Temporary function directory (a character vector of length one)
#' @param R_dir_1L_chr R directory (a character vector of length one)
#' @return NULL
#' @rdname write_documented_fns
#' @export 
#' @importFrom sinew makeOxyFile
#' @importFrom purrr map_chr discard walk
#' @importFrom stringr str_sub
#' @keywords internal
write_documented_fns <- function (tmp_fn_dir_1L_chr, R_dir_1L_chr) 
{
    sinew::makeOxyFile(tmp_fn_dir_1L_chr, verbose = F)
    files_chr <- list.files(tmp_fn_dir_1L_chr) %>% purrr::map_chr(~{
        ifelse(startsWith(.x, "oxy-"), .x, NA_character_)
    }) %>% purrr::discard(is.na)
    purrr::walk(files_chr, ~{
        target_chr <- paste0(R_dir_1L_chr, "/fn_", .x %>% stringr::str_sub(start = 5))
        original_chr <- paste0(tmp_fn_dir_1L_chr, "/", .x)
        if (file.exists(target_chr)) 
            file.remove(target_chr)
        file.copy(original_chr, target_chr)
    })
    do.call(file.remove, list(paste0(tmp_fn_dir_1L_chr, "/", 
        files_chr)))
}
#' Write dataset documentation
#' @description write_ds_dmt() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write dataset documentation. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param db_df Database (a data.frame)
#' @param db_1L_chr Database (a character vector of length one)
#' @param title_1L_chr Title (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param format_1L_chr Format (a character vector of length one), Default: 'A tibble'
#' @param url_1L_chr Url (a character vector of length one), Default: 'NA'
#' @param vars_ls Variables (a list), Default: NULL
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param simple_lup_1L_lgl Simple lookup table (a logical vector of length one), Default: F
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one), Default: 'https://doi.org/10.7910/DVN/2Y9VF9'
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return NULL
#' @rdname write_ds_dmt
#' @export 
#' @importFrom utils data
#' @importFrom purrr map map2 pluck map2_chr
#' @importFrom stats setNames
#' @keywords internal
write_ds_dmt <- function (db_df, db_1L_chr, title_1L_chr, desc_1L_chr, format_1L_chr = "A tibble", 
    url_1L_chr = NA_character_, vars_ls = NULL, R_dir_1L_chr = "R", 
    simple_lup_1L_lgl = F, abbreviations_lup = NULL, object_type_lup = NULL, 
    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9", dv_url_pfx_1L_chr = NULL, 
    key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(abbreviations_lup)) 
        utils::data("abbreviations_lup", package = "ready4fun", 
            envir = environment())
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    auto_vars_ls <- names(db_df) %>% purrr::map(~ifelse(simple_lup_1L_lgl, 
        get_from_lup_obj(abbreviations_lup, target_var_nm_1L_chr = "long_name_chr", 
            match_var_nm_1L_chr = "short_name_chr", match_value_xx = .x, 
            evaluate_lgl = F), make_arg_desc(.x, object_type_lup = object_type_lup, 
            abbreviations_lup = abbreviations_lup, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
            dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr))) %>% stats::setNames(names(db_df))
    if (is.null(vars_ls)) {
        vars_ls <- auto_vars_ls
    }
    else {
        keep_auto_nms_chr <- setdiff(names(auto_vars_ls), names(vars_ls))
        vars_ls <- auto_vars_ls %>% purrr::map2(names(auto_vars_ls), 
            ~{
                if (.y %in% keep_auto_nms_chr) {
                  .x
                }
                else {
                  vars_ls %>% purrr::pluck(.y)
                }
            })
    }
    writeLines(paste0("#' ", title_1L_chr, "\n", "#' \n", "#' ", 
        desc_1L_chr, "\n", "#' \n", "#' ", format_1L_chr, "\n", 
        "#' \n", paste0("#' \\describe{\n", purrr::map2_chr(vars_ls, 
            names(vars_ls), ~paste0("#'   \\item{", .y, "}{", 
                .x, "}")) %>% paste0(collapse = "\n"), "\n#' }\n"), 
        ifelse(is.na(url_1L_chr), "", paste0("#' @source \\url{", 
            url_1L_chr, "}\n")), "\"", db_1L_chr, "\""))
}
#' Write files to dataverse
#' @description write_fls_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write files to dataverse. The function returns Identities (an integer vector).
#' @param file_paths_chr File paths (a character vector)
#' @param descriptions_chr Descriptions (a character vector), Default: NULL
#' @param ds_url_1L_chr Dataset url (a character vector of length one)
#' @param ds_ls Dataset (a list), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: Sys.getenv("DATAVERSE_KEY")
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Identities (an integer vector)
#' @rdname write_fls_to_dv
#' @export 
#' @importFrom purrr map_chr map map2_int
#' @importFrom fs path_file
#' @importFrom dataverse get_dataset delete_file add_dataset_file update_dataset_file
#' @keywords internal
write_fls_to_dv <- function (file_paths_chr, descriptions_chr = NULL, ds_url_1L_chr, 
    ds_ls = NULL, key_1L_chr = Sys.getenv("DATAVERSE_KEY"), server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (!identical(file_paths_chr, character(0))) {
        message(paste0("Are you sure that you want to upload the following file", 
            ifelse(length(file_paths_chr) > 1, "s", ""), " to dataverse ", 
            ds_url_1L_chr, ": \n", file_paths_chr %>% purrr::map_chr(~fs::path_file(.x)) %>% 
                paste0(collapse = "\n"), "?"))
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Type 'Y' to confirm that you want to upload ", 
            ifelse(length(file_paths_chr) > 1, "these files:", 
                "this file:")), options_chr = c("Y", "N"), force_from_opts_1l_chr = T)
        if (consent_1L_chr == "Y") {
            if (is.null(ds_ls)) 
                ds_ls <- dataverse::get_dataset(ds_url_1L_chr)
            is_draft_1L_lgl <- ds_ls$versionState == "DRAFT"
            nms_chr <- ds_ls$files$filename
            if (is.null(descriptions_chr)) 
                descriptions_chr <- purrr::map(file_paths_chr, 
                  ~NULL)
            ids_int <- file_paths_chr %>% purrr::map2_int(descriptions_chr, 
                ~{
                  fl_nm_1L_chr <- fs::path_file(.x)
                  if (fl_nm_1L_chr %in% nms_chr) {
                    id_1L_int <- get_fl_id_from_dv_ls(ds_ls, 
                      fl_nm_1L_chr = fl_nm_1L_chr, nms_chr = nms_chr)
                    if (is_draft_1L_lgl) {
                      id_1L_int %>% dataverse::delete_file()
                      id_1L_int <- dataverse::add_dataset_file(file = .x, 
                        dataset = ds_url_1L_chr, description = .y, 
                        key = key_1L_chr, server = server_1L_chr)
                    }
                    else {
                      dataverse::update_dataset_file(file = .x, 
                        dataset = ds_url_1L_chr, id = id_1L_int, 
                        force = T, description = .y, key = key_1L_chr, 
                        server = server_1L_chr)
                    }
                  }
                  else {
                    id_1L_int <- dataverse::add_dataset_file(file = .x, 
                      dataset = ds_url_1L_chr, description = .y, 
                      key = key_1L_chr, server = server_1L_chr)
                  }
                  id_1L_int
                })
        }
    }
    else {
        ids_int <- NULL
    }
    return(ids_int)
}
#' Write function file
#' @description write_fn_fl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write function file. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param document_unexp_lgl Document unexported (a logical vector), Default: T
#' @param consent_1L_chr Consent (a character vector of length one), Default: NULL
#' @return NULL
#' @rdname write_fn_fl
#' @export 
#' @importFrom stringi stri_replace_last
#' @importFrom purrr walk pluck
#' @importFrom dplyr filter
#' @keywords internal
write_fn_fl <- function (fns_dmt_tb, r_dir_1L_chr = "R", document_unexp_lgl = T, 
    consent_1L_chr = NULL) 
{
    file_nms_chr <- fns_dmt_tb$file_nm_chr %>% unique()
    if (is.null(consent_1L_chr)) {
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write the files ", 
            file_nms_chr %>% paste0(collapse = ", ") %>% stringi::stri_replace_last(fixed = ",", 
                " and"), " to the ", r_dir_1L_chr, " directory?"), 
            options_chr = c("Y", "N"), force_from_opts_1l_chr = T)
    }
    if (consent_1L_chr == "Y") {
        file_nms_chr %>% purrr::walk(~{
            tb <- fns_dmt_tb %>% dplyr::filter(file_nm_chr == 
                .x)
            first_lgl_vec <- c(T, rep(F, nrow(tb) - 1))
            dest_path_1L_chr <- paste0(r_dir_1L_chr, "/", tb$file_pfx_chr[1], 
                .x)
            purrr::walk(1:nrow(tb), ~{
                fn <- eval(parse(text = tb[[.x, 1]]))
                fn_chr <- deparse(fn)
                fn_and_cls_chr <- tb[[.x, 1]] %>% strsplit("\\.") %>% 
                  purrr::pluck(1)
                sink(dest_path_1L_chr, append = !first_lgl_vec[.x])
                make_lines_for_fn_dmt(fn_name_1L_chr = tb[[.x, 
                  1]], fn_type_1L_chr = ifelse(tb$file_pfx_chr[1] == 
                  "mthd_", "meth_std_s3_mthd", ifelse(tb$file_pfx_chr[1] == 
                  "grp_", "gen_std_s3_mthd", "fn")), fn = fn, 
                  fn_desc_1L_chr = tb[[.x, 3]], fn_out_type_1L_chr = tb[[.x, 
                    6]], fn_title_1L_chr = tb[[.x, 2]], example_1L_lgl = tb[[.x, 
                    7]], export_1L_lgl = T, class_name_1L_chr = "", 
                  details_1L_chr = tb[[.x, 4]], args_ls = tb$args_ls[[.x]] %>% 
                    as.list(), import_chr = NA_character_, doc_in_class_1L_lgl = F, 
                  abbreviations_lup = abbreviations_lup, object_type_lup = abbreviations_lup)
                if (tb[[.x, 5]] + document_unexp_lgl == 0) {
                  writeLines(paste0("#' @keywords internal"))
                }
                writeLines(paste0(tb[[.x, 1]], " <- ", fn_chr[1]))
                writeLines(fn_chr[2:length(fn_chr)])
                if (tb$file_pfx_chr[1] == "grp_") {
                  writeLines(paste0("methods::setGeneric(\"", 
                    tb[[.x, 1]], "\")"))
                }
                if (tb$file_pfx_chr[1] == "mthd_") {
                  writeLines(paste0("#' @rdname ", fn_and_cls_chr[1], 
                    "-methods"))
                  writeLines(paste0("#' @aliases ", fn_and_cls_chr[1], 
                    ",", fn_and_cls_chr[2], "-method"))
                  writeLines(paste0("methods::setMethod(\"", 
                    fn_and_cls_chr[1], "\"", ", ", paste0("\"", 
                      fn_and_cls_chr[2], "\""), ", ", tb[[.x, 
                      1]], ")"))
                }
                close_open_sinks()
            })
        })
    }
}
#' Write function type directories
#' @description write_fn_type_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write function type directories. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_1L_chr Path (a character vector of length one), Default: 'data-raw'
#' @return NULL
#' @rdname write_fn_type_dirs
#' @export 

#' @keywords internal
write_fn_type_dirs <- function (path_1L_chr = "data-raw") 
{
    undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr)
    write_new_dirs(undocumented_fns_dir_chr)
}
#' Write functions to split destns
#' @description write_fns_to_split_destns() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write functions to split destns. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param pkg_depcy_ls Package dependency (a list)
#' @param pkg_1_core_fns_chr Package 1 core functions (a character vector)
#' @param original_pkg_nm_1L_chr Original package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @param pkg_1_nm_1L_chr Package 1 name (a character vector of length one), Default: 'package_1'
#' @param pkg_2_nm_1L_chr Package 2 name (a character vector of length one), Default: 'package_2'
#' @param tmp_dir_path_1L_chr Temporary directory path (a character vector of length one), Default: 'data-raw/pkg_migration'
#' @param path_to_fns_dir_1L_chr Path to functions directory (a character vector of length one), Default: 'data-raw/fns'
#' @return NULL
#' @rdname write_fns_to_split_destns
#' @export 
#' @importFrom utils data
#' @importFrom purrr map_chr walk2 walk
#' @importFrom dplyr filter select
#' @keywords internal
write_fns_to_split_destns <- function (pkg_depcy_ls, pkg_1_core_fns_chr, original_pkg_nm_1L_chr = get_dev_pkg_nm(), 
    pkg_1_nm_1L_chr = "package_1", pkg_2_nm_1L_chr = "package_2", 
    tmp_dir_path_1L_chr = "data-raw/pkg_migration", path_to_fns_dir_1L_chr = "data-raw/fns") 
{
    utils::data("fns_dmt_tb", package = original_pkg_nm_1L_chr, 
        envir = environment())
    read_fns(path_to_fns_dir_1L_chr)
    fns_for_pkg_1_chr <- get_all_depcys_of_fns(pkg_depcy_ls = pkg_depcy_ls, 
        fns_chr = pkg_1_core_fns_chr)
    fns_for_pkg_2_chr <- setdiff(pkg_depcy_ls$Nomfun$label, fns_for_pkg_1_chr)
    migrate_ls <- list(fns_for_pkg_1_chr = fns_for_pkg_1_chr, 
        fns_for_pkg_2_chr = fns_for_pkg_2_chr)
    if (!dir.exists(tmp_dir_path_1L_chr)) 
        dir.create(tmp_dir_path_1L_chr)
    new_dest_dir_chr <- purrr::map_chr(c(pkg_1_nm_1L_chr, pkg_2_nm_1L_chr), 
        ~{
            new_dir_1L_chr <- paste0(tmp_dir_path_1L_chr, "/", 
                .x)
            if (!dir.exists(new_dir_1L_chr)) 
                dir.create(new_dir_1L_chr)
            new_dir_1L_chr
        })
    migrate_ls %>% purrr::walk2(new_dest_dir_chr, ~{
        fns_tb <- fns_dmt_tb %>% dplyr::filter(fns_chr %in% .x) %>% 
            dplyr::select(fns_chr, file_nm_chr)
        file_nms_chr <- fns_tb$file_nm_chr %>% unique()
        new_dest_dir_1L_chr <- .y
        file_nms_chr %>% purrr::walk(~{
            tb <- fns_tb %>% dplyr::filter(file_nm_chr == .x)
            first_lgl_vec <- c(T, rep(F, nrow(tb) - 1))
            dest_path_1L_chr <- paste0(new_dest_dir_1L_chr, "/", 
                .x)
            purrr::walk(1:nrow(tb), ~{
                fn <- eval(parse(text = tb[[.x, 1]]))
                fn_chr <- deparse(fn)
                sink(dest_path_1L_chr, append = !first_lgl_vec[.x])
                writeLines(paste0(tb[[.x, 1]], " <- ", fn_chr[1]))
                writeLines(fn_chr[2:length(fn_chr)])
                close_open_sinks()
            })
        })
    })
}
#' Write from temporary
#' @description write_from_tmp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write from temporary. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param temp_paths_chr Temporary paths (a character vector)
#' @param dest_paths_chr Dest paths (a character vector)
#' @param edit_fn_ls Edit (a list of functions), Default: list(NULL)
#' @param args_ls_ls Arguments (a list of lists), Default: NULL
#' @return NULL
#' @rdname write_from_tmp
#' @export 
#' @importFrom purrr pmap
#' @importFrom rlang exec
#' @keywords internal
write_from_tmp <- function (temp_paths_chr, dest_paths_chr, edit_fn_ls = list(NULL), 
    args_ls_ls = NULL) 
{
    text_ls <- purrr::pmap(list(temp_paths_chr, edit_fn_ls, args_ls_ls), 
        ~{
            fileConn <- file(..1)
            txt_chr <- readLines(fileConn, warn = FALSE)
            close(fileConn)
            if (is.null(..2)) {
                edit_fn <- function(x) {
                  x
                }
            }
            else {
                edit_fn <- ..2
            }
            rlang::exec(edit_fn, txt_chr, !!!..3)
        })
    write_to_delete_fls(intersect(temp_paths_chr, dest_paths_chr))
    write_new_files(dest_paths_chr, text_ls = text_ls)
}
#' Write instance directory
#' @description write_inst_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write instance directory. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @return NULL
#' @rdname write_inst_dir
#' @export 

#' @keywords internal
write_inst_dir <- function (path_to_pkg_rt_1L_chr = getwd()) 
{
    source_inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr, "/data-raw/inst")
    if (dir.exists(source_inst_dir_1L_chr)) {
        inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr, "/inst")
        write_to_delete_dirs(inst_dir_1L_chr)
        write_new_dirs(inst_dir_1L_chr)
        write_new_files(inst_dir_1L_chr, source_paths_ls = list(source_inst_dir_1L_chr))
    }
}
#' Write links for website
#' @description write_links_for_website() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write links for website. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @param developer_manual_url_1L_chr Developer manual url (a character vector of length one), Default: 'NA'
#' @param user_manual_url_1L_chr User manual url (a character vector of length one), Default: 'NA'
#' @param project_website_url_1L_chr Project website url (a character vector of length one), Default: 'NA'
#' @return NULL
#' @rdname write_links_for_website
#' @export 
#' @importFrom stats na.omit
#' @keywords internal
write_links_for_website <- function (path_to_pkg_rt_1L_chr = getwd(), developer_manual_url_1L_chr = NA_character_, 
    user_manual_url_1L_chr = NA_character_, project_website_url_1L_chr = NA_character_) 
{
    write_from_tmp(paste0(path_to_pkg_rt_1L_chr, "/_pkgdown.yml"), 
        dest_paths_chr = paste0(path_to_pkg_rt_1L_chr, "/_pkgdown.yml"), 
        edit_fn_ls = list(function(txt_chr, user_manual_url_1L_chr, 
            developer_manual_url_1L_chr, project_website_url_1L_chr) {
            idx_1L_int <- which(txt_chr == "home:")
            if (!identical(idx_1L_int, integer(0))) {
                changes_chr <- c(any(txt_chr == "  - text: User manual (PDF)"), 
                  any(txt_chr == "  - text: Developer version of usual manual (PDF)"), 
                  any(txt_chr == "  - text: Project website"))
                txt_chr <- txt_chr[-(1:(length(changes_chr[changes_chr == 
                  T]) * 2))]
            }
            c("home:", "  links:", ifelse(!is.na(user_manual_url_1L_chr), 
                "  - text: Manual - User (PDF)", NA_character_), 
                ifelse(!is.na(user_manual_url_1L_chr), paste0("    href: ", 
                  user_manual_url_1L_chr), NA_character_), ifelse(!is.na(developer_manual_url_1L_chr), 
                  "  - text: Manual - Developer (PDF)", NA_character_), 
                ifelse(!is.na(developer_manual_url_1L_chr), paste0("    href: ", 
                  developer_manual_url_1L_chr), NA_character_), 
                ifelse(!is.na(project_website_url_1L_chr), "  - text: Project website", 
                  NA_character_), ifelse(!is.na(project_website_url_1L_chr), 
                  paste0("    href: ", project_website_url_1L_chr), 
                  NA_character_), txt_chr) %>% stats::na.omit()
        }), args_ls_ls = list(list(user_manual_url_1L_chr = user_manual_url_1L_chr, 
            developer_manual_url_1L_chr = developer_manual_url_1L_chr, 
            project_website_url_1L_chr = project_website_url_1L_chr)))
}
#' Write manuals to dataverse
#' @description write_manuals_to_dv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write manuals to dataverse. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param package_1L_chr Package (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param path_to_dmt_dir_1L_chr Path to documentation directory (a character vector of length one)
#' @param pkg_dmt_dv_ds_1L_chr Package documentation dataverse dataset (a character vector of length one)
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: F
#' @return NULL
#' @rdname write_manuals_to_dv
#' @export 
#' @importFrom utils packageDescription
#' @importFrom purrr walk
#' @keywords internal
write_manuals_to_dv <- function (package_1L_chr = get_dev_pkg_nm(getwd()), path_to_dmt_dir_1L_chr, 
    pkg_dmt_dv_ds_1L_chr, publish_dv_1L_lgl = F) 
{
    version_1L_chr <- utils::packageDescription(package_1L_chr)$Version
    purrr::walk(c("Developer", "User"), ~{
        dir_1L_chr <- paste0(path_to_dmt_dir_1L_chr, "/", .x)
        original_1L_chr <- paste0(dir_1L_chr, "/", package_1L_chr, 
            "_", version_1L_chr, ".pdf")
        fl_nm_1L_chr <- paste0(package_1L_chr, "_", .x, ".pdf")
        copy_1L_chr <- paste0(dir_1L_chr, "/", fl_nm_1L_chr)
        if (file.exists(original_1L_chr)) {
            write_new_files(dir_1L_chr, source_paths_ls = list(original_1L_chr), 
                filename_1L_chr = fl_nm_1L_chr)
        }
        write_fls_to_dv(copy_1L_chr, descriptions_chr = paste0("Manual (", 
            .x %>% tolower(), " version)", " describing the contents of the ", 
            package_1L_chr, " R package."), ds_url_1L_chr = pkg_dmt_dv_ds_1L_chr)
    })
    if (publish_dv_1L_lgl) {
        write_to_publish_dv_ds(dv_ds_1L_chr = pkg_dmt_dv_ds_1L_chr)
    }
}
#' Write new argument sfxs
#' @description write_new_arg_sfxs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new argument sfxs. The function returns Function arguments to rnm (a list).
#' @param arg_nms_chr Argument names (a character vector)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param dir_path_chr Directory path (a character vector)
#' @param rt_dev_dir_path_1L_chr Root development directory path (a character vector of length one), Default: normalizePath("../../../")
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param inc_fns_idx_dbl Include functions index (a double vector), Default: NA
#' @return Function arguments to rnm (a list)
#' @rdname write_new_arg_sfxs
#' @export 
#' @importFrom purrr walk map map_lgl
#' @importFrom stringr str_sub
#' @importFrom stats setNames
#' @keywords internal
write_new_arg_sfxs <- function (arg_nms_chr, fn_type_1L_chr, dir_path_chr, rt_dev_dir_path_1L_chr = normalizePath("../../../"), 
    pkg_nm_1L_chr, inc_fns_idx_dbl = NA_real_) 
{
    if (is.na(inc_fns_idx_dbl)) 
        inc_fns_idx_dbl <- 1:length(ls(paste0("package:", pkg_nm_1L_chr))[ls(paste0("package:", 
            pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)])
    purrr::walk(arg_nms_chr[order(nchar(arg_nms_chr), arg_nms_chr, 
        decreasing = T)] %>% unique(), ~write_to_rpl_1L_and_indefL_sfcs(.x, 
        file_path_chr = paste0(dir_path_chr, "/", fn_type_1L_chr, 
            ".R")))
    updated_fns_chr <- ls(paste0("package:", pkg_nm_1L_chr))[ls(paste0("package:", 
        pkg_nm_1L_chr)) %>% startsWith(fn_type_1L_chr)][inc_fns_idx_dbl]
    updated_sfxs_chr <- arg_nms_chr[arg_nms_chr %>% endsWith("_vec")] %>% 
        stringr::str_sub(start = -8) %>% unique()
    fn_nms_to_upd_chr <- updated_fns_chr[updated_fns_chr %>% 
        stringr::str_sub(start = -8) %in% updated_sfxs_chr]
    if (ifelse(identical(fn_nms_to_upd_chr, character(0)), F, 
        !is.na(fn_nms_to_upd_chr))) {
        purrr::walk(fn_nms_to_upd_chr, ~write_to_rpl_1L_and_indefL_sfcs(.x, 
            dir_path_chr = dir_path_chr))
        purrr::walk(paste0(pkg_nm_1L_chr, "::", fn_nms_to_upd_chr), 
            ~write_to_rpl_1L_and_indefL_sfcs(.x, dir_path_chr = rt_dev_dir_path_1L_chr))
    }
    fn_args_to_rnm_ls <- purrr::map(updated_fns_chr, ~{
        fn_args_chr <- get_fn_args_chr(eval(parse(text = .x)))
        fn_args_chr[purrr::map_lgl(fn_args_chr, ~.x %in% c(arg_nms_chr, 
            arg_nms_chr %>% stringr::str_sub(end = -5)))]
    }) %>% stats::setNames(updated_fns_chr)
    return(fn_args_to_rnm_ls)
}
#' Write new directories
#' @description write_new_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new directories. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param new_dirs_chr New directories (a character vector)
#' @return NULL
#' @rdname write_new_dirs
#' @export 
#' @importFrom purrr map_lgl walk
#' @keywords internal
write_new_dirs <- function (new_dirs_chr) 
{
    new_dirs_chr <- new_dirs_chr[new_dirs_chr %>% purrr::map_lgl(~!dir.exists(.x))]
    if (!identical(new_dirs_chr, character(0))) {
        message(paste0("Are you sure that you want to write the following director", 
            ifelse(length(new_dirs_chr) > 1, "ies", "y"), " to your machine? \n", 
            new_dirs_chr %>% paste0(collapse = "\n")))
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write ", 
            ifelse(length(new_dirs_chr) > 1, "these directories?", 
                "this directory?")), options_chr = c("Y", "N"), 
            force_from_opts_1l_chr = T)
        if (consent_1L_chr %in% c("Y")) {
            paths_ls <- new_dirs_chr %>% purrr::walk(~{
                dir.create(.x)
            })
            message(paste0("New directories created:\n", new_dirs_chr %>% 
                paste0(collapse = "\n")))
        }
        else {
            message("Write request cancelled - no new directories created")
        }
    }
}
#' Write new files
#' @description write_new_files() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param paths_chr Paths (a character vector)
#' @param custom_write_ls Custom write (a list), Default: NULL
#' @param filename_1L_chr Filename (a character vector of length one), Default: NULL
#' @param source_paths_ls Source paths (a list), Default: NULL
#' @param text_ls Text (a list), Default: NULL
#' @return NULL
#' @rdname write_new_files
#' @export 
#' @importFrom purrr map flatten_chr map_chr map_lgl walk2 walk
#' @importFrom fs path_file
#' @importFrom rlang exec
#' @keywords internal
write_new_files <- function (paths_chr, custom_write_ls = NULL, filename_1L_chr = NULL, 
    source_paths_ls = NULL, text_ls = NULL) 
{
    if (!is.null(source_paths_ls)) {
        dest_dir_1L_chr <- paths_chr
        paths_chr <- purrr::map(source_paths_ls, ~{
            if (dir.exists(.x)) {
                list.files(.x)
            }
            else {
                fs::path_file(.x)
            }
        }) %>% purrr::flatten_chr() %>% purrr::map_chr(~paste0(dest_dir_1L_chr, 
            "/", ifelse(is.null(filename_1L_chr), .x, filename_1L_chr)))
        recursive_1L_lgl <- ifelse(paths_chr %>% purrr::map_lgl(~dir.exists(.x)) %>% 
            any(), T, F)
    }
    new_files_chr <- paths_chr[paths_chr %>% purrr::map_lgl(~!file.exists(.x))]
    overwritten_files_chr <- setdiff(paths_chr, new_files_chr)
    if (!identical(paths_chr, character(0))) {
        message(paste0("Are you sure that you want to write / overwrite the following file", 
            ifelse(length(paths_chr) > 1, "s", ""), " to your machine: \n", 
            ifelse(identical(new_files_chr, character(0)), "", 
                paste0("Files that will be created: \n", new_files_chr %>% 
                  paste0(collapse = "\n"))), ifelse(identical(overwritten_files_chr, 
                character(0)), "", paste0("Files that will be overwritten: \n", 
                overwritten_files_chr %>% paste0(collapse = "\n"))), 
            "?"))
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to write ", 
            ifelse((length(new_files_chr) + length(overwritten_files_chr)) > 
                1, "these files:", "this file:")), options_chr = c("Y", 
            "N"), force_from_opts_1l_chr = T)
        if (consent_1L_chr %in% c("Y")) {
            if (!is.null(text_ls)) {
                purrr::walk2(paths_chr, text_ls, ~{
                  file_conn <- file(.x)
                  writeLines(.y, file_conn)
                  close(file_conn)
                })
            }
            else {
                if (!is.null(source_paths_ls)) {
                  source_paths_chr <- purrr::map(source_paths_ls, 
                    ~{
                      if (dir.exists(.x)) {
                        list.files(.x, full.names = T)
                      }
                      else {
                        .x
                      }
                    }) %>% purrr::flatten_chr()
                  purrr::walk(source_paths_chr, ~file.copy(.x, 
                    paste0(dest_dir_1L_chr, ifelse(is.null(filename_1L_chr), 
                      "", paste0("/", filename_1L_chr))), overwrite = T, 
                    recursive = recursive_1L_lgl))
                }
                if (!is.null(custom_write_ls)) {
                  custom_write_ls$args_ls$consent_1L_chr <- consent_1L_chr
                  rlang::exec(custom_write_ls$fn, !!!custom_write_ls$args_ls)
                }
            }
        }
        else {
            message("Write request cancelled - no new directories created")
        }
    }
}
#' Write namespace imports to description
#' @description write_ns_imps_to_desc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write namespace imports to description. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param dev_pkgs_chr Development packages (a character vector), Default: 'NA'
#' @param incr_ver_1L_lgl Incr ver (a logical vector of length one), Default: T
#' @return NULL
#' @rdname write_ns_imps_to_desc
#' @export 
#' @importFrom devtools document
#' @importFrom purrr map_chr discard walk
#' @importFrom stringr str_replace str_sub str_locate
#' @importFrom usethis use_dev_package use_package use_version
#' @keywords internal
write_ns_imps_to_desc <- function (dev_pkgs_chr = NA_character_, incr_ver_1L_lgl = T) 
{
    devtools::document()
    packages_chr <- readLines("NAMESPACE") %>% purrr::map_chr(~ifelse(startsWith(.x, 
        "import"), ifelse(startsWith(.x, "importFrom"), stringr::str_replace(.x, 
        "importFrom\\(", "") %>% stringr::str_sub(end = stringr::str_locate(., 
        ",")[1, 1] - 1), stringr::str_replace(.x, "import\\(", 
        "") %>% stringr::str_sub(end = -2)), NA_character_)) %>% 
        purrr::discard(is.na) %>% unique()
    if (!is.na(dev_pkgs_chr)) {
        dev_pkgs_chr <- intersect(packages_chr, dev_pkgs_chr) %>% 
            sort()
        packages_chr <- setdiff(packages_chr, dev_pkgs_chr) %>% 
            sort()
        purrr::walk(dev_pkgs_chr, ~usethis::use_dev_package(.x))
    }
    purrr::walk(packages_chr, ~usethis::use_package(.x))
    devtools::document()
    if (incr_ver_1L_lgl) 
        usethis::use_version()
}
#' Write package
#' @description write_package() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param pkg_desc_ls Package description (a list)
#' @param pkg_ds_ls_ls Package dataset (a list of lists)
#' @param pkg_setup_ls Package setup (a list)
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: 'https://dataverse.harvard.edu/api/access/datafile/'
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param path_to_dmt_dir_1L_chr Path to documentation directory (a character vector of length one), Default: normalizePath("../../../../../Documentation/Code")
#' @param publish_dv_1L_lgl Publish dataverse (a logical vector of length one), Default: F
#' @return NULL
#' @rdname write_package
#' @export 
#' @importFrom rlang exec
#' @importFrom purrr pluck
#' @keywords internal
write_package <- function (pkg_desc_ls, pkg_ds_ls_ls, pkg_setup_ls, abbreviations_lup = NULL, 
    dv_url_pfx_1L_chr = "https://dataverse.harvard.edu/api/access/datafile/", 
    fn_type_lup_tb = NULL, object_type_lup = NULL, path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"), 
    publish_dv_1L_lgl = F) 
{
    rlang::exec(write_pkg_setup_fls, !!!pkg_setup_ls$initial_ls)
    dss_records_ls <- write_pkg_dss(pkg_ds_ls_ls, pkg_url_1L_chr = pkg_desc_ls$URL %>% 
        strsplit(",") %>% unlist() %>% purrr::pluck(1), abbreviations_lup = abbreviations_lup, 
        dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[2], 
        fn_type_lup_tb = fn_type_lup_tb, object_type_lup = object_type_lup)
    add_build_ignore(pkg_setup_ls$subsequent_ls$build_ignore_ls)
    add_addl_pkgs(pkg_setup_ls$subsequent_ls$addl_pkgs_ls)
    write_and_doc_fn_fls(fns_dmt_tb = dss_records_ls$fns_dmt_tb, 
        dev_pkgs_chr = pkg_setup_ls$subsequent_ls$dev_pkgs_chr, 
        path_to_dmt_dir_1L_chr = path_to_dmt_dir_1L_chr, r_dir_1L_chr = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr, 
            "/R"), update_pkgdown_1L_lgl = T)
    write_manuals_to_dv(package_1L_chr = pkg_desc_ls$Package, 
        path_to_dmt_dir_1L_chr = path_to_dmt_dir_1L_chr, pkg_dmt_dv_ds_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[1], 
        publish_dv_1L_lgl = publish_dv_1L_lgl)
    dmt_urls_chr <- get_dv_fls_urls(file_nms_chr = paste0(pkg_desc_ls$Package, 
        "_", c("Developer", "User"), ".pdf"), dv_ds_nm_1L_chr = pkg_setup_ls$subsequent_ls$pkg_dmt_dv_dss_chr[1], 
        dv_url_pfx_1L_chr = dv_url_pfx_1L_chr)
    project_url_1L_chr <- pkg_desc_ls$URL %>% strsplit(",") %>% 
        unlist() %>% purrr::pluck(3)
    if (is.null(project_url_1L_chr)) 
        project_url_1L_chr <- NA_character_
    write_links_for_website(user_manual_url_1L_chr = dmt_urls_chr[2], 
        developer_manual_url_1L_chr = dmt_urls_chr[1], project_website_url_1L_chr = project_url_1L_chr)
}
#' Write package
#' @description write_pkg() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param package_1L_chr Package (a character vector of length one)
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return NULL
#' @rdname write_pkg
#' @export 
#' @importFrom lifecycle deprecate_soft
#' @importFrom utils packageDescription
#' @keywords internal
write_pkg <- function (package_1L_chr, R_dir_1L_chr = "R") 
{
    lifecycle::deprecate_soft("0.0.0.9298", what = "ready4fun::write_pkg()")
    write_from_tmp(system.file("pkg_tmp.R", package = "ready4fun"), 
        dest_paths_chr = paste0(R_dir_1L_chr, "/pkg_", package_1L_chr, 
            ".R"), edit_fn_ls = list(function(txt_chr, package_1L_chr) {
            pkg_desc_ls <- utils::packageDescription(package_1L_chr)
            txt_chr
        }), args_ls_ls = list(list(package_1L_chr = package_1L_chr)))
}
#' Write package datasets
#' @description write_pkg_dss() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package datasets. The function returns Datasets records (a list).
#' @param pkg_ds_ls_ls Package dataset (a list of lists), Default: NULL
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param args_ls_ls Arguments (a list of lists), Default: NULL
#' @param details_ls Details (a list), Default: NULL
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param dv_ds_nm_1L_chr Dataverse dataset name (a character vector of length one)
#' @param fn_type_lup_tb Function type lookup table (a tibble), Default: NULL
#' @param inc_all_mthds_1L_lgl Include all methods (a logical vector of length one), Default: T
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param paths_ls Paths (a list), Default: make_fn_nms()
#' @param pkg_url_1L_chr Package url (a character vector of length one), Default: 'NA'
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T)
#' @param dv_url_pfx_1L_chr Dataverse url prefix (a character vector of length one), Default: NULL
#' @param key_1L_chr Key (a character vector of length one), Default: NULL
#' @param server_1L_chr Server (a character vector of length one), Default: Sys.getenv("DATAVERSE_SERVER")
#' @return Datasets records (a list)
#' @rdname write_pkg_dss
#' @export 
#' @importFrom utils data
#' @importFrom purrr reduce
#' @importFrom rlang exec
#' @keywords internal
write_pkg_dss <- function (pkg_ds_ls_ls = NULL, abbreviations_lup = NULL, args_ls_ls = NULL, 
    details_ls = NULL, dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()), 
    dv_ds_nm_1L_chr, fn_type_lup_tb = NULL, inc_all_mthds_1L_lgl = T, 
    object_type_lup = NULL, paths_ls = make_fn_nms(), pkg_url_1L_chr = NA_character_, 
    R_dir_1L_chr = "R", undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(drop_empty_1L_lgl = T), 
    dv_url_pfx_1L_chr = NULL, key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER")) 
{
    if (is.null(object_type_lup)) 
        object_type_lup <- get_rds_from_dv("object_type_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(abbreviations_lup)) 
        abbreviations_lup <- get_rds_from_dv("abbreviations_lup", 
            dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
            key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    if (is.null(fn_type_lup_tb)) 
        fn_type_lup_tb <- get_rds_from_dv("fn_type_lup_tb", dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
            dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, key_1L_chr = key_1L_chr, 
            server_1L_chr = server_1L_chr)
    pkg_dss_tb <- write_abbr_lup(seed_lup = abbreviations_lup, 
        pkg_nm_1L_chr = dev_pkg_nm_1L_chr, dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, 
        object_type_lup = object_type_lup)
    utils::data("abbreviations_lup", envir = environment())
    pkg_dss_tb <- fn_type_lup_tb %>% write_dmtd_fn_type_lup(abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, pkg_dss_tb = pkg_dss_tb, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = NULL, 
        key_1L_chr = NULL, server_1L_chr = Sys.getenv("DATAVERSE_SERVER"))
    utils::data("fn_type_lup_tb", envir = environment())
    if (!is.null(pkg_ds_ls_ls)) {
        pkg_dss_tb <- purrr::reduce(pkg_ds_ls_ls, .init = pkg_dss_tb, 
            ~{
                if (is.null(.y$abbreviations_lup)) 
                  .y$abbreviations_lup <- abbreviations_lup
                if (is.null(.y$object_type_lup)) 
                  .y$object_type_lup <- object_type_lup
                args_ls <- append(.y, list(overwrite_1L_lgl = T, 
                  pkg_dss_tb = .x, R_dir_1L_chr = R_dir_1L_chr, 
                  dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
                  key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr))
                rlang::exec(write_and_doc_ds, !!!args_ls)
            })
    }
    fns_dmt_tb <- make_dmt_for_all_fns(paths_ls = paths_ls, abbreviations_lup = abbreviations_lup, 
        custom_dmt_ls = list(details_ls = details_ls, inc_for_main_user_lgl_ls = list(force_true_chr = pkg_setup_ls$subsequent_ls$user_manual_fns_chr, 
            force_false_chr = NA_character_), args_ls_ls = args_ls_ls), 
        fn_type_lup_tb = fn_type_lup_tb, inc_all_mthds_1L_lgl = inc_all_mthds_1L_lgl, 
        object_type_lup = object_type_lup, undocumented_fns_dir_chr = undocumented_fns_dir_chr)
    pkg_dss_tb <- fns_dmt_tb %>% write_and_doc_ds(overwrite_1L_lgl = T, 
        db_1L_chr = "fns_dmt_tb", title_1L_chr = paste0(dev_pkg_nm_1L_chr, 
            " function documentation table"), desc_1L_chr = paste0("A table with the summary information on functions included in the ", 
            dev_pkg_nm_1L_chr, " package."), format_1L_chr = "A tibble", 
        url_1L_chr = pkg_url_1L_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup, pkg_dss_tb = pkg_dss_tb, 
        dv_ds_nm_1L_chr = dv_ds_nm_1L_chr, dv_url_pfx_1L_chr = dv_url_pfx_1L_chr, 
        key_1L_chr = key_1L_chr, server_1L_chr = server_1L_chr)
    dss_records_ls <- list(pkg_dss_tb = pkg_dss_tb, fns_dmt_tb = fns_dmt_tb)
    return(dss_records_ls)
}
#' Write package setup files
#' @description write_pkg_setup_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package setup files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param pkg_desc_ls Package description (a list)
#' @param copyright_holders_chr Copyright holders (a character vector)
#' @param github_repo_1L_chr Github repo (a character vector of length one)
#' @param addl_badges_ls Addl badges (a list), Default: NULL
#' @param badges_lup Badges (a lookup table), Default: NULL
#' @param check_type_1L_chr Check type (a character vector of length one), Default: 'none'
#' @param delete_r_dir_cnts_1L_lgl Delete r directory contents (a logical vector of length one), Default: F
#' @param lifecycle_stage_1L_chr Lifecycle stage (a character vector of length one), Default: 'experimental'
#' @param incr_ver_1L_lgl Incr ver (a logical vector of length one), Default: T
#' @param on_cran_1L_lgl On cran (a logical vector of length one), Default: F
#' @param path_to_pkg_logo_1L_chr Path to package logo (a character vector of length one), Default: 'NA'
#' @param add_gh_site_1L_lgl Add gh site (a logical vector of length one), Default: T
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @return NULL
#' @rdname write_pkg_setup_fls
#' @export 
#' @importFrom utils data packageDescription
#' @importFrom devtools load_all document
#' @importFrom usethis use_version use_gpl3_license use_pkgdown use_build_ignore use_package use_github_action use_github_action_check_standard use_github_action_check_full use_github_action_check_release use_lifecycle use_lifecycle_badge use_badge
#' @importFrom desc desc_get desc_set
#' @importFrom purrr map_chr map2_chr walk2 walk
#' @importFrom stringr str_trim str_replace_all str_locate_all str_sub
#' @importFrom lubridate year
#' @importFrom pkgdown build_favicons
#' @importFrom dplyr filter
#' @keywords internal
write_pkg_setup_fls <- function (pkg_desc_ls, copyright_holders_chr, github_repo_1L_chr, 
    addl_badges_ls = NULL, badges_lup = NULL, check_type_1L_chr = "none", 
    delete_r_dir_cnts_1L_lgl = F, lifecycle_stage_1L_chr = "experimental", 
    incr_ver_1L_lgl = T, on_cran_1L_lgl = F, path_to_pkg_logo_1L_chr = NA_character_, 
    add_gh_site_1L_lgl = T, dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()), 
    path_to_pkg_rt_1L_chr = getwd()) 
{
    options(usethis.description = pkg_desc_ls)
    use_gh_cmd_check_1L_lgl <- (check_type_1L_chr %in% c("gh", 
        "full", "release", "standard"))
    if (is.null(badges_lup)) {
        utils::data("badges_lup", envir = environment())
    }
    if (delete_r_dir_cnts_1L_lgl) 
        write_to_reset_pkg_files(delete_contents_of_1L_chr = "R", 
            package_1L_chr = dev_pkg_nm_1L_chr, package_dir_1L_chr = path_to_pkg_rt_1L_chr)
    update_desc_fl_1L_lgl <- !is.na(dev_pkg_nm_1L_chr)
    if (!update_desc_fl_1L_lgl) 
        dev_pkg_nm_1L_chr <- get_dev_pkg_nm(path_to_pkg_rt_1L_chr)
    devtools::load_all(path_to_pkg_rt_1L_chr)
    write_std_imp(paste0(path_to_pkg_rt_1L_chr, "/R"), package_1L_chr = dev_pkg_nm_1L_chr)
    if (update_desc_fl_1L_lgl) {
        desc_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr, 
            "/DESCRIPTION"))
        desc_1L_chr[1] <- paste0("Package: ", dev_pkg_nm_1L_chr)
        write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr, 
            "/DESCRIPTION"), text_ls = list(desc_1L_chr))
    }
    if (!file.exists(paste0(path_to_pkg_rt_1L_chr, "/vignettes/", 
        get_dev_pkg_nm(), ".Rmd"))) 
        write_vignette(dev_pkg_nm_1L_chr, pkg_rt_dir_chr = path_to_pkg_rt_1L_chr)
    if (incr_ver_1L_lgl) {
        usethis::use_version()
    }
    write_inst_dir(path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr)
    usethis::use_gpl3_license()
    license_1L_chr <- c(paste0(dev_pkg_nm_1L_chr, " - ", desc::desc_get("Title") %>% 
        as.vector()), readLines(paste0(path_to_pkg_rt_1L_chr, 
        "/License.md"))[556:569]) %>% purrr::map_chr(~stringr::str_trim(.x) %>% 
        stringr::str_replace_all("<year>", as.character(Sys.Date() %>% 
            lubridate::year())) %>% stringr::str_replace_all("<name of author>", 
        paste0(copyright_holders_chr, collapse = "and "))) %>% 
        paste0(collapse = "\n")
    write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr, 
        "/LICENSE"), text_ls = list(license_1L_chr))
    desc::desc_set("License", "GPL-3 + file LICENSE")
    usethis::use_pkgdown()
    usethis::use_build_ignore(files = "_pkgdown.yml")
    usethis::use_package("testthat")
    usethis::use_package("knitr")
    desc::desc_set("VignetteBuilder", "knitr")
    usethis::use_build_ignore(paste0(paste0("data-raw/"), list.files(paste0(path_to_pkg_rt_1L_chr, 
        "/data-raw"), recursive = T)))
    if (!is.na(path_to_pkg_logo_1L_chr)) {
        write_new_dirs(paste0(path_to_pkg_rt_1L_chr, "/man/figures/"))
        write_new_files(paste0(path_to_pkg_rt_1L_chr, "/man/figures"), 
            source_paths_ls = list(path_to_pkg_logo_1L_chr), 
            filename_1L_chr = "logo.png")
    }
    if (on_cran_1L_lgl) {
        cran_install_chr <- c("To install the latest production version of this software, run the following command in your R console:", 
            "", "```r", "utils::install.packages(\"", dev_pkg_nm_1L_chr, 
            "\")", "", "```", "")
    }
    else {
        cran_install_chr <- character(0)
    }
    readme_chr <- c(paste0("# ", dev_pkg_nm_1L_chr, ifelse(is.na(path_to_pkg_logo_1L_chr), 
        "", " <img src=\"man/figures/fav120.png\" align=\"right\" />")), 
        "", paste0("## ", utils::packageDescription(dev_pkg_nm_1L_chr, 
            fields = "Title") %>% stringr::str_replace_all("\n", 
            " ")), "", "<!-- badges: start -->", "<!-- badges: end -->", 
        "", utils::packageDescription(dev_pkg_nm_1L_chr, fields = "Description"), 
        "", cran_install_chr, "To install a development version of this software, run the following commands in your R console:", 
        "", "```r", "utils::install.packages(\"devtools\")", 
        "", paste0("devtools::install_github(\"", github_repo_1L_chr, 
            "\")"), "", "```")
    write_new_files(paths_chr = paste0(path_to_pkg_rt_1L_chr, 
        "/README.md"), text_ls = list(readme_chr))
    if (add_gh_site_1L_lgl) 
        usethis::use_github_action("pkgdown")
    if (check_type_1L_chr %in% c("gh", "full", "release", "standard")) {
        if (check_type_1L_chr %in% c("gh", "standard")) {
            usethis::use_github_action_check_standard()
        }
        else {
            if (check_type_1L_chr == "full") {
                usethis::use_github_action_check_full()
            }
            else {
                usethis::use_github_action_check_release()
            }
        }
    }
    if (!is.na(path_to_pkg_logo_1L_chr) & !file.exists(paste0(path_to_pkg_rt_1L_chr, 
        "/pkgdown/favicon/apple-touch-icon-120x120.png"))) {
        pkgdown::build_favicons()
    }
    write_new_files(paste0(path_to_pkg_rt_1L_chr, "/man/figures"), 
        source_paths_ls = list(paste0(path_to_pkg_rt_1L_chr, 
            "/pkgdown/favicon/apple-touch-icon-120x120.png")), 
        filename_1L_chr = "fav120.png")
    usethis::use_lifecycle()
    usethis::use_lifecycle_badge(lifecycle_stage_1L_chr)
    if (!is.null(addl_badges_ls)) {
        badges_chr <- purrr::map2_chr(addl_badges_ls, names(addl_badges_ls), 
            ~{
                badges_lup %>% dplyr::filter(badge_names_chr == 
                  .y) %>% get_from_lup_obj(match_value_xx = .x, 
                  match_var_nm_1L_chr = "label_names_chr", target_var_nm_1L_chr = "badges_chr", 
                  evaluate_lgl = F)
            }) %>% unname()
        purrr::walk2(badges_chr, names(addl_badges_ls), ~{
            badge_1L_chr <- .x
            badge_nm_1L_chr <- .y
            break_points_ls <- badge_1L_chr %>% stringr::str_locate_all("\\]\\(")
            purrr::walk(break_points_ls, ~{
                src_1L_chr <- stringr::str_sub(badge_1L_chr, 
                  start = .x[1, 2] %>% as.vector() + 1, end = .x[2, 
                    1] %>% as.vector() - 2)
                href_1L_chr <- stringr::str_sub(badge_1L_chr, 
                  start = .x[2, 2] %>% as.vector() + 1, end = -2)
                usethis::use_badge(badge_name = badge_nm_1L_chr, 
                  src = src_1L_chr, href = href_1L_chr)
            })
        })
    }
    devtools::document()
    devtools::load_all()
}
#' Write prototype lookup table database
#' @description write_pt_lup_db() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write prototype lookup table database. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return NULL
#' @rdname write_pt_lup_db
#' @export 

#' @keywords internal
write_pt_lup_db <- function (R_dir_1L_chr = "R") 
{
    write_from_tmp(system.file("db_pt_lup.R", package = "ready4fun"), 
        dest_paths_chr = paste0(R_dir_1L_chr, "/db_pt_lup.R"))
}
#' Write standard import
#' @description write_std_imp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write standard import. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param package_1L_chr Package (a character vector of length one)
#' @return NULL
#' @rdname write_std_imp
#' @export 
#' @importFrom utils packageDescription
#' @keywords internal
write_std_imp <- function (R_dir_1L_chr = "R", package_1L_chr) 
{
    write_from_tmp(c(system.file("pkg_tmp.R", package = "ready4fun"), 
        system.file("imp_fns_tmp.R", package = "ready4fun"), 
        system.file("imp_mthds_tmp.R", package = "ready4fun")), 
        dest_paths_chr = c(paste0(R_dir_1L_chr, "/pkg_", package_1L_chr, 
            ".R"), paste0(R_dir_1L_chr, "/imp_fns.R"), paste0(R_dir_1L_chr, 
            "/imp_mthds.R")), edit_fn_ls = list(function(txt_chr, 
            package_1L_chr) {
            pkg_desc_ls <- utils::packageDescription(package_1L_chr)
            txt_chr
        }, NULL, NULL), args_ls_ls = list(list(package_1L_chr = package_1L_chr), 
            NULL, NULL))
}
#' Write tibble to comma separated variables file
#' @description write_tb_to_csv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write tibble to comma separated variables file. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param tbs_r4 Tibbles (a ready4 S4)
#' @param slot_nm_1L_chr Slot name (a character vector of length one)
#' @param r4_name_1L_chr Ready4 S4 name (a character vector of length one)
#' @param lup_dir_1L_chr Lookup table directory (a character vector of length one)
#' @param pfx_1L_chr Prefix (a character vector of length one)
#' @return NULL
#' @rdname write_tb_to_csv
#' @export 
#' @importFrom methods slot
#' @importFrom dplyr mutate_if funs
#' @importFrom stringr str_c
#' @importFrom utils write.csv
#' @keywords internal
write_tb_to_csv <- function (tbs_r4, slot_nm_1L_chr, r4_name_1L_chr, lup_dir_1L_chr, 
    pfx_1L_chr) 
{
    methods::slot(tbs_r4, slot_nm_1L_chr) %>% dplyr::mutate_if(is.list, 
        .funs = dplyr::funs(ifelse(stringr::str_c(.) == "NULL", 
            NA_character_, stringr::str_c(.)))) %>% utils::write.csv(file = paste0(lup_dir_1L_chr, 
        "/", pfx_1L_chr, "_", slot_nm_1L_chr, ".csv"), row.names = F)
}
#' Write to delete directories
#' @description write_to_delete_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete directories. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param dir_paths_chr Directory paths (a character vector)
#' @return NULL
#' @rdname write_to_delete_dirs
#' @export 
#' @importFrom purrr map_lgl map flatten_chr walk
#' @keywords internal
write_to_delete_dirs <- function (dir_paths_chr) 
{
    dir_paths_chr <- dir_paths_chr[dir_paths_chr %>% purrr::map_lgl(~dir.exists(.x))]
    if (!identical(dir_paths_chr, character(0))) {
        fls_to_be_purged_chr <- dir_paths_chr %>% purrr::map(~list.files(.x, 
            full.names = TRUE)) %>% purrr::flatten_chr()
        message(paste0("Are you sure that you want to delete the following director", 
            ifelse(length(dir_paths_chr) > 1, "ies", "y"), ":\n", 
            dir_paths_chr %>% paste0(collapse = "\n"), ifelse(identical(fls_to_be_purged_chr, 
                character(0)), "", paste0(" and the following file", 
                ifelse(length(fls_to_be_purged_chr) > 1, "s:\n", 
                  ":\n"), fls_to_be_purged_chr %>% paste0(collapse = "\n"))), 
            " from your machine: \n", "?"))
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to delete ", 
            ifelse(length(dir_paths_chr) > 1, "these directories", 
                "this directory"), ifelse(length(fls_to_be_purged_chr) > 
                0, ifelse(length(fls_to_be_purged_chr) > 0, " and files", 
                " and file"), ""), ":"), options_chr = c("Y", 
            "N"), force_from_opts_1l_chr = T)
        if (consent_1L_chr %in% c("Y")) {
            dir_paths_chr %>% purrr::walk(~unlink(.x, recursive = TRUE))
        }
        else {
            message("Delete directory request cancelled - no directories deleted")
        }
    }
}
#' Write to delete files
#' @description write_to_delete_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to delete files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param file_paths_chr File paths (a character vector)
#' @return NULL
#' @rdname write_to_delete_fls
#' @export 
#' @importFrom purrr map_lgl
#' @keywords internal
write_to_delete_fls <- function (file_paths_chr) 
{
    file_paths_chr <- file_paths_chr[file_paths_chr %>% purrr::map_lgl(~file.exists(.x))]
    if (!identical(file_paths_chr, character(0))) {
        message(paste0("Are you sure that you want to delete the following file", 
            ifelse(length(file_paths_chr) > 1, "s", ""), " from your machine: \n", 
            file_paths_chr %>% paste0(collapse = "\n"), "?"))
        consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you want to delete ", 
            ifelse(length(file_paths_chr) > 1, "these files:", 
                "this file:")), options_chr = c("Y", "N"), force_from_opts_1l_chr = T)
        if (consent_1L_chr %in% c("Y")) {
            paths_ls <- do.call(file.remove, list(file_paths_chr))
        }
        else {
            message("Delete files request cancelled - no files deleted")
        }
    }
}
#' Write to publish dataverse dataset
#' @description write_to_publish_dv_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to publish dataverse dataset. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param dv_ds_1L_chr Dataverse dataset (a character vector of length one)
#' @return NULL
#' @rdname write_to_publish_dv_ds
#' @export 
#' @importFrom dataverse publish_dataset
#' @keywords internal
write_to_publish_dv_ds <- function (dv_ds_1L_chr) 
{
    consent_1L_chr <- make_prompt(prompt_1L_chr = paste0("Do you confirm ('Y') that you wish to publish the current draft of dataverse ", 
        dv_ds_1L_chr, "?"), options_chr = c("Y", "N"), force_from_opts_1l_chr = T)
    if (consent_1L_chr == "Y") {
        dataverse::publish_dataset(dv_ds_1L_chr, minor = F)
    }
}
#' Write to remove collate
#' @description write_to_remove_collate() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to remove collate. The function returns Description (a character vector).
#' @param description_chr Description (a character vector)
#' @return Description (a character vector)
#' @rdname write_to_remove_collate
#' @export 

#' @keywords internal
write_to_remove_collate <- function (description_chr) 
{
    if (!identical(which(description_chr == "Collate: "), integer(0))) 
        description_chr <- description_chr[1:(which(description_chr == 
            "Collate: ") - 1)]
    return(description_chr)
}
#' Write to replace function names
#' @description write_to_replace_fn_nms() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to replace function names. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param rename_tb Rename (a tibble)
#' @param undocumented_fns_dir_chr Undocumented functions directory (a character vector), Default: make_undmtd_fns_dir_chr()
#' @param rt_dev_dir_path_1L_chr Root development directory path (a character vector of length one), Default: normalizePath("../../../")
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @return NULL
#' @rdname write_to_replace_fn_nms
#' @export 
#' @importFrom dplyr filter select
#' @importFrom purrr pwalk walk
#' @importFrom xfun gsub_dir
#' @keywords internal
write_to_replace_fn_nms <- function (rename_tb, undocumented_fns_dir_chr = make_undmtd_fns_dir_chr(), 
    rt_dev_dir_path_1L_chr = normalizePath("../../../"), dev_pkg_nm_1L_chr = get_dev_pkg_nm()) 
{
    if (any(rename_tb$duplicated_lgl)) 
        stop("Duplicates in rename table")
    rename_tb <- rename_tb %>% dplyr::filter(fns_chr != new_nm) %>% 
        dplyr::select(fns_chr, new_nm)
    purrr::pwalk(rename_tb, ~{
        pattern_1L_chr <- ..1
        replacement_1L_chr <- ..2
        purrr::walk(undocumented_fns_dir_chr, ~xfun::gsub_dir(undocumented_fns_dir_chr, 
            pattern = pattern_1L_chr, replacement = replacement_1L_chr))
        xfun::gsub_dir(dir = rt_dev_dir_path_1L_chr, pattern = paste0(dev_pkg_nm_1L_chr, 
            "::", pattern_1L_chr), replacement = paste0(dev_pkg_nm_1L_chr, 
            "::", replacement_1L_chr), ext = "R", fixed = T)
    })
}
#' Write to replace suffix pair
#' @description write_to_replace_sfx_pair() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to replace suffix pair. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param args_nm_chr Arguments name (a character vector)
#' @param sfxs_chr Sfxs (a character vector)
#' @param replacements_chr Replacements (a character vector)
#' @param file_path_1L_chr File path (a character vector of length one), Default: 'NA'
#' @param dir_path_1L_chr Directory path (a character vector of length one), Default: 'NA'
#' @return NULL
#' @rdname write_to_replace_sfx_pair
#' @export 
#' @importFrom xfun gsub_dir gsub_file
#' @importFrom stringr str_remove
#' @importFrom rlang exec
#' @keywords internal
write_to_replace_sfx_pair <- function (args_nm_chr, sfxs_chr, replacements_chr, file_path_1L_chr = NA_character_, 
    dir_path_1L_chr = NA_character_) 
{
    fn <- ifelse(is.na(file_path_1L_chr), xfun::gsub_dir, xfun::gsub_file)
    path_chr <- ifelse(is.na(file_path_1L_chr), dir_path_1L_chr, 
        file_path_1L_chr)
    args_ls <- list(pattern = paste0(args_nm_chr[1], "(?!", stringr::str_remove(sfxs_chr[2], 
        sfxs_chr[1]), ")"), replacement = paste0(stringr::str_remove(args_nm_chr[1], 
        sfxs_chr[1]), replacements_chr[1]), perl = T)
    rlang::exec(fn, path_chr, !!!args_ls)
    args_ls <- list(pattern = args_nm_chr[2], replacement = paste0(stringr::str_remove(args_nm_chr[2], 
        sfxs_chr[2]), replacements_chr[2]), perl = T)
    rlang::exec(fn, path_chr, !!!args_ls)
}
#' Write to reset package files
#' @description write_to_reset_pkg_files() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to reset package files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param delete_contents_of_1L_chr Delete contents of (a character vector of length one)
#' @param package_1L_chr Package (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param package_dir_1L_chr Package directory (a character vector of length one), Default: getwd()
#' @param description_ls Description (a list), Default: NULL
#' @param keep_version_lgl Keep version (a logical vector), Default: T
#' @return NULL
#' @rdname write_to_reset_pkg_files
#' @export 
#' @importFrom devtools load_all document
#' @importFrom utils packageDescription
#' @importFrom usethis use_description
#' @keywords internal
write_to_reset_pkg_files <- function (delete_contents_of_1L_chr, package_1L_chr = get_dev_pkg_nm(getwd()), 
    package_dir_1L_chr = getwd(), description_ls = NULL, keep_version_lgl = T) 
{
    devtools::load_all()
    if (keep_version_lgl) {
        desc_ls <- utils::packageDescription(package_1L_chr)
        description_ls$Version <- desc_ls$Version
    }
    usethis::use_description(fields = description_ls)
    file_paths_chr <- c(paste0(package_dir_1L_chr, "/NAMESPACE"), 
        list.files(paste0(package_dir_1L_chr, "/", delete_contents_of_1L_chr), 
            full.names = TRUE))
    write_to_delete_fls(file_paths_chr)
    devtools::document()
    devtools::load_all()
}
#' Write to replace length one and indefinite length suffices
#' @description write_to_rpl_1L_and_indefL_sfcs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to replace length one and indefinite length suffices. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param indefL_arg_nm_1L_chr Indefinite length argument name (a character vector of length one)
#' @param file_path_1L_chr File path (a character vector of length one), Default: 'NA'
#' @param dir_path_1L_chr Directory path (a character vector of length one), Default: 'NA'
#' @return NULL
#' @rdname write_to_rpl_1L_and_indefL_sfcs
#' @export 
#' @importFrom stringr str_sub
#' @keywords internal
write_to_rpl_1L_and_indefL_sfcs <- function (indefL_arg_nm_1L_chr, file_path_1L_chr = NA_character_, 
    dir_path_1L_chr = NA_character_) 
{
    sfxs_chr <- c(indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8, 
        end = -5), indefL_arg_nm_1L_chr %>% stringr::str_sub(start = -8))
    write_to_replace_sfx_pair(args_nm_chr = paste0(indefL_arg_nm_1L_chr %>% 
        stringr::str_sub(end = -9), sfxs_chr), sfxs_chr = sfxs_chr, 
        replacements_chr = paste0(c("_1L", ""), sfxs_chr[1]), 
        file_path_1L_chr = file_path_1L_chr, dir_path_1L_chr = dir_path_1L_chr)
}
#' Write vignette
#' @description write_vignette() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write vignette. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param package_1L_chr Package (a character vector of length one)
#' @param pkg_rt_dir_chr Package root directory (a character vector), Default: '.'
#' @return NULL
#' @rdname write_vignette
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
#' @keywords internal
write_vignette <- function (package_1L_chr, pkg_rt_dir_chr = ".") 
{
    write_new_dirs(paste0(pkg_rt_dir_chr, "/vignettes"))
    write_from_tmp(c(system.file("ready4fun.Rmd", package = "ready4fun"), 
        system.file(".gitignore", package = "ready4fun")), dest_paths_chr = c(paste0(pkg_rt_dir_chr, 
        "/vignettes/", package_1L_chr, ".Rmd"), paste0(pkg_rt_dir_chr, 
        "/vignettes/", ".gitignore")), edit_fn_ls = list(function(txt_chr, 
        package_1L_chr) {
        txt_chr <- purrr::map_chr(txt_chr, ~stringr::str_replace_all(.x, 
            "ready4fun", package_1L_chr))
        txt_chr
    }, function(txt_chr, package_1L_chr) {
        txt_chr
    }), args_ls_ls = list(list(package_1L_chr = package_1L_chr), 
        list(list(package_1L_chr = package_1L_chr))))
}
#' Write workspace
#' @description write_ws() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write workspace. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_1L_chr Path (a character vector of length one)
#' @return NULL
#' @rdname write_ws
#' @export 
#' @importFrom purrr map_chr
write_ws <- function (path_1L_chr) 
{
    top_level_chr <- paste0(path_1L_chr, "/ready4/", c("Code", 
        "Data", "Documentation", "Insight"))
    code_top_lvl_chr <- c("Application", "Authoring", "Brochure", 
        "Description", "Modelling", "Prediction") %>% purrr::map_chr(~paste0(top_level_chr[1], 
        "/", .x))
    code_sub_dirs_chr <- c(paste0(code_top_lvl_chr[2], "/Workflows", 
        c("", "/R")), paste0(code_top_lvl_chr[3], "/HTML"), paste0(code_top_lvl_chr[4], 
        "/Datatypes", c("", "/R")), paste0(code_top_lvl_chr[5], 
        "/Templates", c("", "/R")), paste0(code_top_lvl_chr[6], 
        "/Example", c("", "/Toolkit_1", "/Toolkit_1/R")))
    data_top_lvl_chr <- c("Dataverse", "Project", "R_Format", 
        "Raw_Format") %>% purrr::map_chr(~paste0(top_level_chr[2], 
        "/", .x))
    data_sub_dirs_chr <- c("Agents", "Attributes", "Geometries", 
        "Metadata") %>% purrr::map_chr(~paste0(data_top_lvl_chr[4], 
        "/", .x))
    dcmntn_top_lvl_chr <- c("Code", "Data", "Images") %>% purrr::map_chr(~paste0(top_level_chr[3], 
        "/", .x))
    dcmntn_sub_dirs_chr <- c("Developer", "User") %>% purrr::map_chr(~paste0(dcmntn_top_lvl_chr[1], 
        "/", .x))
    insight_top_lvl_chr <- c("Analysis", "Science", "Team") %>% 
        purrr::map_chr(~paste0(top_level_chr[4], "/", .x))
    new_dirs_chr <- c(paste0(path_1L_chr, "/ready4"), top_level_chr, 
        code_top_lvl_chr, code_sub_dirs_chr, data_top_lvl_chr, 
        data_sub_dirs_chr, dcmntn_top_lvl_chr, dcmntn_sub_dirs_chr, 
        insight_top_lvl_chr)
    write_new_dirs(new_dirs_chr)
}
