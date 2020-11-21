#' Write abbreviation
#' @description write_abbr_lup() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write abbreviation lookup table. The function returns Package datasets (a tibble).
#' @param short_name_chr Short name (a character vector), Default: 'NA'
#' @param long_name_chr Long name (a character vector), Default: 'NA'
#' @param no_plural_chr No plural (a character vector), Default: 'NA'
#' @param custom_plural_ls Custom plural (a list), Default: NULL
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @param seed_lup Seed (a lookup table), Default: NULL
#' @param url_1L_chr Url (a character vector of length one)
#' @param pkg_nm_1L_chr Package name (a character vector of length one), Default: get_dev_pkg_nm()
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @return Package datasets (a tibble)
#' @rdname write_abbr_lup
#' @export 
#' @importFrom tibble tibble
write_abbr_lup <- function (short_name_chr = NA_character_, long_name_chr = NA_character_, 
    no_plural_chr = NA_character_, custom_plural_ls = NULL, overwrite_1L_lgl = T, 
    seed_lup = NULL, url_1L_chr, pkg_nm_1L_chr = get_dev_pkg_nm(), 
    pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0))) 
{
    if (is.null(seed_lup)) {
        data("object_type_lup", package = "ready4fun", envir = environment())
        seed_lup <- object_type_lup
    }
    pkg_dss_tb <- update_abbr_lup(seed_lup, short_name_chr = short_name_chr, 
        long_name_chr = long_name_chr, no_plural_chr = no_plural_chr, 
        custom_plural_ls = custom_plural_ls) %>% write_and_doc_ds(db = ., 
        overwrite_1L_lgl = overwrite_1L_lgl, db_1L_chr = "abbreviations_lup", 
        title_1L_chr = "Common abbreviations lookup table", desc_1L_chr = paste0("A lookup table for abbreviations commonly used in object names in the ", 
            pkg_nm_1L_chr, "package."), format_1L_chr = "A tibble", 
        url_1L_chr = url_1L_chr, abbreviations_lup = ., pkg_dss_tb = pkg_dss_tb)
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
write_all_tbs_in_tbs_r4_to_csvs <- function (tbs_r4, r4_name_1L_chr, lup_dir_1L_chr, pfx_1L_chr) 
{
    purrr::walk(methods::getSlots(r4_name_1L_chr) %>% names(), 
        ~write_tb_to_csv(tbs_r4 = tbs_r4, slot_nm_1L_chr = .x, 
            r4_name_1L_chr = r4_name_1L_chr, lup_dir_1L_chr = lup_dir_1L_chr, 
            pfx_1L_chr = pfx_1L_chr))
}
#' Write and document dataset
#' @description write_and_doc_ds() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write and document dataset. The function returns Package datasets (a tibble).
#' @param db PARAM_DESCRIPTION
#' @param overwrite_1L_lgl Overwrite (a logical vector of length one), Default: T
#' @param db_1L_chr Database (a character vector of length one)
#' @param title_1L_chr Title (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param format_1L_chr Format (a character vector of length one), Default: 'A tibble'
#' @param url_1L_chr Url (a character vector of length one), Default: 'NA'
#' @param vars_ls Vars (a list), Default: NULL
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @return Package datasets (a tibble)
#' @rdname write_and_doc_ds
#' @export 
#' @importFrom tibble tibble add_case
#' @importFrom devtools document load_all
write_and_doc_ds <- function (db, overwrite_1L_lgl = T, db_1L_chr, title_1L_chr, 
    desc_1L_chr, format_1L_chr = "A tibble", url_1L_chr = NA_character_, 
    vars_ls = NULL, R_dir_1L_chr = "R", abbreviations_lup = NULL, 
    object_type_lup = NULL, pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0))) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    eval(parse(text = paste0(db_1L_chr, "<-db")))
    eval(parse(text = paste0("usethis::use_data(", db_1L_chr, 
        ", overwrite = overwrite_1L_lgl)")))
    sink(paste0(R_dir_1L_chr, "/db_", db_1L_chr, ".R"), append = F)
    write_ds_dmt(db = db, db_1L_chr = db_1L_chr, title_1L_chr = title_1L_chr, 
        desc_1L_chr = desc_1L_chr, format_1L_chr = format_1L_chr, 
        vars_ls = vars_ls, url_1L_chr = url_1L_chr, R_dir_1L_chr = R_dir_1L_chr, 
        abbreviations_lup = abbreviations_lup, object_type_lup = object_type_lup)
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
#' @param path_to_user_dmt_dir_1L_chr Path to user documentation directory (a character vector of length one), Default: '../../../../Documentation/Code/User'
#' @param path_to_dvpr_dmt_dir_1L_chr Path to developer documentation directory (a character vector of length one), Default: '../../../../Documentation/Code/Developer'
#' @param make_pdfs_1L_lgl Make pdfs (a logical vector of length one), Default: T
#' @param dev_pkgs_chr Development packages (a character vector), Default: 'NA'
#' @param update_pkgdown_1L_lgl Update pkgdown (a logical vector of length one), Default: T
#' @return NULL
#' @rdname write_and_doc_fn_fls
#' @export 
#' @importFrom purrr walk2 map2 flatten_chr discard
#' @importFrom devtools document load_all build_manual
#' @importFrom dplyr filter pull
write_and_doc_fn_fls <- function (fns_dmt_tb, r_dir_1L_chr = "R", path_to_pkg_rt_1L_chr = getwd(), 
    path_to_user_dmt_dir_1L_chr = "../../../../Documentation/Code/User", 
    path_to_dvpr_dmt_dir_1L_chr = "../../../../Documentation/Code/Developer", 
    make_pdfs_1L_lgl = T, dev_pkgs_chr = NA_character_, update_pkgdown_1L_lgl = T) 
{
    purrr::walk2(list(path_to_dvpr_dmt_dir_1L_chr, path_to_user_dmt_dir_1L_chr), 
        c(T, F), ~{
            write_fn_fl(fns_dmt_tb, r_dir_1L_chr = r_dir_1L_chr, 
                document_unexp_lgl = .y)
            devtools::document()
            devtools::load_all()
            write_ns_imps_to_desc(dev_pkgs_chr = dev_pkgs_chr, 
                incr_ver_1L_lgl = .y)
            devtools::load_all()
            if (make_pdfs_1L_lgl) 
                devtools::build_manual(path = .x)
        })
    if (update_pkgdown_1L_lgl) {
        datasets_chr <- data(package = get_dev_pkg_nm(path_to_pkg_rt_1L_chr), 
            envir = environment())$results[, 3]
        writeLines(c("development:", "  mode: auto", "reference:", 
            "- title: \"Datasets\"", "- contents:", paste0("  - ", 
                datasets_chr), {
                if ("prototype_lup" %in% datasets_chr) {
                  data("prototype_lup", package = get_dev_pkg_nm(path_to_pkg_rt_1L_chr), 
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
#' @param url_1L_chr Url (a character vector of length one), Default: url_1L_chr
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param pkg_dss_tb Package datasets (a tibble), Default: tibble::tibble(ds_obj_nm_chr = character(0), title_chr = character(0), 
#'    desc_chr = character(0), url_chr = character(0))
#' @return NULL
#' @rdname write_dmtd_fn_type_lup
#' @export 
#' @importFrom tibble tibble
write_dmtd_fn_type_lup <- function (fn_type_lup_tb = make_fn_type_lup(), overwrite_1L_lgl = T, 
    pkg_nm_1L_chr = get_dev_pkg_nm(), url_1L_chr = url_1L_chr, 
    abbreviations_lup = NULL, pkg_dss_tb = tibble::tibble(ds_obj_nm_chr = character(0), 
        title_chr = character(0), desc_chr = character(0), url_chr = character(0))) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    fn_type_lup_tb %>% write_and_doc_ds(overwrite_1L_lgl = overwrite_1L_lgl, 
        db_1L_chr = "fn_type_lup_tb", title_1L_chr = "Function type lookup table", 
        desc_1L_chr = paste0("A lookup table to find descriptions for different types of functions used within the ", 
            pkg_nm_1L_chr, " package suite."), format_1L_chr = "A tibble", 
        url_1L_chr = url_1L_chr, abbreviations_lup = abbreviations_lup, 
        pkg_dss_tb = pkg_dss_tb)
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
#' @param db PARAM_DESCRIPTION
#' @param db_1L_chr Database (a character vector of length one)
#' @param title_1L_chr Title (a character vector of length one)
#' @param desc_1L_chr Description (a character vector of length one)
#' @param format_1L_chr Format (a character vector of length one), Default: 'A tibble'
#' @param url_1L_chr Url (a character vector of length one), Default: 'NA'
#' @param vars_ls Vars (a list), Default: NULL
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_ds_dmt
#' @export 
#' @importFrom purrr map map2 pluck map2_chr
#' @importFrom stats setNames
write_ds_dmt <- function (db, db_1L_chr, title_1L_chr, desc_1L_chr, format_1L_chr = "A tibble", 
    url_1L_chr = NA_character_, vars_ls = NULL, R_dir_1L_chr = "R", 
    abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    auto_vars_ls <- names(db) %>% purrr::map(~make_arg_desc(.x, 
        object_type_lup = object_type_lup, abbreviations_lup = abbreviations_lup)) %>% 
        stats::setNames(names(db))
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
#' Write function file
#' @description write_fn_fl() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write function file. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param fns_dmt_tb Functions documentation (a tibble)
#' @param r_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @param document_unexp_lgl Document unexported (a logical vector), Default: T
#' @return NULL
#' @rdname write_fn_fl
#' @export 
#' @importFrom purrr walk pluck
#' @importFrom dplyr filter
write_fn_fl <- function (fns_dmt_tb, r_dir_1L_chr = "R", document_unexp_lgl = T) 
{
    file_nms_chr <- fns_dmt_tb$file_nm_chr %>% unique()
    file_nms_chr %>% purrr::walk(~{
        tb <- fns_dmt_tb %>% dplyr::filter(file_nm_chr == .x)
        first_lgl_vec <- c(T, rep(F, nrow(tb) - 1))
        dest_path_1L_chr <- paste0(r_dir_1L_chr, "/", tb$file_pfx_chr[1], 
            .x)
        purrr::walk(1:nrow(tb), ~{
            fn <- eval(parse(text = tb[[.x, 1]]))
            fn_chr <- deparse(fn)
            fn_and_cls_chr <- tb[[.x, 1]] %>% strsplit("\\.") %>% 
                purrr::pluck(1)
            sink(dest_path_1L_chr, append = !first_lgl_vec[.x])
            make_lines_for_fn_dmt(fn_name_1L_chr = tb[[.x, 1]], 
                fn_type_1L_chr = ifelse(tb$file_pfx_chr[1] == 
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
                writeLines(paste0("methods::setGeneric(\"", tb[[.x, 
                  1]], "\")"))
            }
            if (tb$file_pfx_chr[1] == "mthd_") {
                writeLines(paste0("methods::setMethod(\"", fn_and_cls_chr[1], 
                  "\"", ", ", paste0("\"", fn_and_cls_chr[2], 
                    "\""), ", ", tb[[.x, 1]], ")"))
            }
            close_open_sinks()
        })
    })
}
#' Write function type directories
#' @description write_fn_type_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write function type directories. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_1L_chr Path (a character vector of length one), Default: 'data-raw'
#' @return NULL
#' @rdname write_fn_type_dirs
#' @export 
#' @importFrom purrr walk
write_fn_type_dirs <- function (path_1L_chr = "data-raw") 
{
    undocumented_fns_dir_chr <- make_undmtd_fns_dir_chr(path_1L_chr)
    paths_ls <- undocumented_fns_dir_chr %>% purrr::walk(~{
        if (!dir.exists(.x)) 
            dir.create(.x)
    })
}
#' Write from temporary
#' @description write_from_tmp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write from temporary. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param temp_path_1L_chr Temp path (a character vector of length one)
#' @param dest_path_1L_chr Dest path (a character vector of length one)
#' @param edit_fn Edit (a function), Default: function(x) {
#'    x
#'}
#' @param args_ls Arguments (a list), Default: NULL
#' @return NULL
#' @rdname write_from_tmp
#' @export 
#' @importFrom rlang exec
write_from_tmp <- function (temp_path_1L_chr, dest_path_1L_chr, edit_fn = function(x) {
    x
}, args_ls = NULL) 
{
    fileConn <- file(temp_path_1L_chr)
    txt_chr <- readLines(fileConn)
    close(fileConn)
    txt_chr <- rlang::exec(edit_fn, txt_chr, !!!args_ls)
    if (temp_path_1L_chr == dest_path_1L_chr) 
        file.remove(temp_path_1L_chr)
    fileConn <- file(dest_path_1L_chr)
    writeLines(txt_chr, fileConn)
    close(fileConn)
}
#' Write inst directory
#' @description write_inst_dir() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write inst directory. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @return NULL
#' @rdname write_inst_dir
#' @export 

write_inst_dir <- function (path_to_pkg_rt_1L_chr = getwd()) 
{
    source_inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr, "/data-raw/inst")
    if (dir.exists(source_inst_dir_1L_chr)) {
        inst_dir_1L_chr <- paste0(path_to_pkg_rt_1L_chr, "/inst")
        if (dir.exists(inst_dir_1L_chr)) 
            unlink(inst_dir_1L_chr, recursive = TRUE)
        dir.create(inst_dir_1L_chr)
        file.copy(source_inst_dir_1L_chr, path_to_pkg_rt_1L_chr, 
            recursive = TRUE)
    }
}
#' Write links for website
#' @description write_links_for_website() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write links for website. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @param user_manual_url_1L_chr User manual url (a character vector of length one)
#' @param developer_manual_url_1L_chr Developer manual url (a character vector of length one)
#' @param project_website_url_1L_chr Project website url (a character vector of length one), Default: 'https://readyforwhatsnext.github.io/readyforwhatsnext/'
#' @return NULL
#' @rdname write_links_for_website
#' @export 

write_links_for_website <- function (path_to_pkg_rt_1L_chr = getwd(), user_manual_url_1L_chr, 
    developer_manual_url_1L_chr, project_website_url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/") 
write_from_tmp(paste0(path_to_pkg_rt_1L_chr, "/_pkgdown.yml"), 
    dest_path_1L_chr = paste0(path_to_pkg_rt_1L_chr, "/_pkgdown.yml"), 
    edit_fn = function(txt_chr) {
        c("home:", "  links:", "  - text: User manual (PDF)", 
            paste0("    href: ", user_manual_url_1L_chr), "  - text: Developer version of usual manual (PDF)", 
            paste0("    href: ", developer_manual_url_1L_chr), 
            "  - text: Project website", paste0("    href: ", 
                project_website_url_1L_chr), txt_chr)
    })
#' Write new argument sfxs
#' @description write_new_arg_sfxs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write new argument sfxs. The function returns Function arguments to rnm (a list).
#' @param arg_nms_chr Argument names (a character vector)
#' @param fn_type_1L_chr Function type (a character vector of length one)
#' @param dir_path_chr Directory path (a character vector)
#' @param rt_dev_dir_path_1L_chr Root development directory path (a character vector of length one), Default: normalizePath("../../../")
#' @param pkg_nm_1L_chr Package name (a character vector of length one)
#' @param inc_fns_idx_dbl Inc functions idx (a double vector), Default: NA
#' @return Function arguments to rnm (a list)
#' @rdname write_new_arg_sfxs
#' @export 
#' @importFrom purrr walk map map_lgl
#' @importFrom stringr str_sub
#' @importFrom stats setNames
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
#' @description write_pkg() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param package_1L_chr Package (a character vector of length one)
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return NULL
#' @rdname write_pkg
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
write_pkg <- function (package_1L_chr, R_dir_1L_chr = "R") 
{
    write_from_tmp(system.file("pkg_ready_fun.R", package = "ready4fun"), 
        dest_path_1L_chr = paste0(R_dir_1L_chr, "/pkg_", package_1L_chr, 
            ".R"), edit_fn = function(txt_chr, package_1L_chr) {
            pkg_desc_ls <- packageDescription(package_1L_chr)
            txt_chr <- purrr::map_chr(txt_chr, ~stringr::str_replace_all(.x, 
                "ready4fun", package_1L_chr))
            txt_chr[1] <- paste0("#' ", package_1L_chr, ": ", 
                pkg_desc_ls$Title %>% stringr::str_replace_all("\n", 
                  "\n#' "))
            txt_chr[3] <- paste0("#' ", pkg_desc_ls$Description %>% 
                stringr::str_replace_all("\n", "\n#' "))
            txt_chr
        }, args_ls = list(package_1L_chr = package_1L_chr))
}
#' Write package setup files
#' @description write_pkg_setup_fls() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write package setup files. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_to_pkg_rt_1L_chr Path to package root (a character vector of length one), Default: getwd()
#' @param dev_pkg_nm_1L_chr Development package name (a character vector of length one), Default: get_dev_pkg_nm(getwd())
#' @param incr_ver_1L_lgl Incr ver (a logical vector of length one), Default: T
#' @param delete_contents_of_R_dir PARAM_DESCRIPTION, Default: F
#' @param copyright_holders_chr Copyright holders (a character vector)
#' @param use_travis_1L_lgl Use travis (a logical vector of length one), Default: T
#' @param path_to_pkg_logo_1L_chr Path to package logo (a character vector of length one), Default: 'NA'
#' @param github_repo_1L_chr Github repo (a character vector of length one)
#' @param lifecycle_stage_1L_chr Lifecycle stage (a character vector of length one), Default: 'experimental'
#' @return NULL
#' @rdname write_pkg_setup_fls
#' @export 
#' @importFrom devtools load_all
#' @importFrom usethis use_version use_gpl3_license use_pkgdown use_build_ignore use_travis use_github_action use_lifecycle use_lifecycle_badge
#' @importFrom stringr str_replace_all
#' @importFrom pkgdown build_favicons
write_pkg_setup_fls <- function (path_to_pkg_rt_1L_chr = getwd(), dev_pkg_nm_1L_chr = get_dev_pkg_nm(getwd()), 
    incr_ver_1L_lgl = T, delete_contents_of_R_dir = F, copyright_holders_chr, 
    use_travis_1L_lgl = T, path_to_pkg_logo_1L_chr = NA_character_, 
    github_repo_1L_chr, lifecycle_stage_1L_chr = "experimental") 
{
    if (delete_contents_of_R_dir) 
        write_to_reset_pkg_files(delete_contents_of_1L_chr = "R", 
            package_1L_chr = dev_pkg_nm_1L_chr, package_dir_1L_chr = path_to_pkg_rt_1L_chr)
    update_desc_fl_1L_lgl <- !is.na(dev_pkg_nm_1L_chr)
    if (!update_desc_fl_1L_lgl) 
        dev_pkg_nm_1L_chr <- get_dev_pkg_nm(path_to_pkg_rt_1L_chr)
    devtools::load_all(path_to_pkg_rt_1L_chr)
    write_pkg(dev_pkg_nm_1L_chr, R_dir_1L_chr = paste0(path_to_pkg_rt_1L_chr, 
        "/R"))
    write_std_imp(paste0(path_to_pkg_rt_1L_chr, "/R"))
    if (update_desc_fl_1L_lgl) {
        desc_1L_chr <- readLines(paste0(path_to_pkg_rt_1L_chr, 
            "/DESCRIPTION"))
        desc_1L_chr[1] <- paste0("Package: ", dev_pkg_nm_1L_chr)
        sink(paste0(path_to_pkg_rt_1L_chr, "/DESCRIPTION"), append = F)
        writeLines(desc_1L_chr)
        close_open_sinks()
    }
    if (!file.exists(paste0(path_to_pkg_rt_1L_chr, "/vignettes/", 
        get_dev_pkg_nm(), ".Rmd"))) 
        write_vignette(dev_pkg_nm_1L_chr, pkg_rt_dir_chr = path_to_pkg_rt_1L_chr)
    if (incr_ver_1L_lgl) {
        usethis::use_version()
    }
    write_inst_dir(path_to_pkg_rt_1L_chr = path_to_pkg_rt_1L_chr)
    usethis::use_gpl3_license(copyright_holders_chr)
    usethis::use_pkgdown()
    usethis::use_build_ignore(files = "_pkgdown.yml")
    if (!is.na(path_to_pkg_logo_1L_chr)) {
        if (!dir.exists(paste0(path_to_pkg_rt_1L_chr, "/man/figures/"))) 
            dir.create(paste0(path_to_pkg_rt_1L_chr, "/man/figures/"))
        file.copy(path_to_pkg_logo_1L_chr, paste0(path_to_pkg_rt_1L_chr, 
            "/man/figures/logo.png"))
    }
    writeLines(c(paste0("# ", dev_pkg_nm_1L_chr, ifelse(is.na(path_to_pkg_logo_1L_chr), 
        "", " <img src=\"man/figures/fav120.png\" align=\"right\" />")), 
        "", paste0("## ", packageDescription(dev_pkg_nm_1L_chr, 
            fields = "Title") %>% stringr::str_replace_all("\n", 
            " ")), "", "<!-- badges: start -->", "<!-- badges: end -->", 
        "", packageDescription(dev_pkg_nm_1L_chr, fields = "Description"), 
        "", "If you plan on testing this software you can install it by running the following commands in your R console:", 
        "", "```r", "install.packages(\"devtools\")", "", paste0("devtools::install_github(\"", 
            github_repo_1L_chr, "\")"), "", "```"), con = paste0(path_to_pkg_rt_1L_chr, 
        "/README.md"))
    if (use_travis_1L_lgl) {
        usethis::use_travis()
        write_from_tmp(paste0(path_to_pkg_rt_1L_chr, "/.travis.yml"), 
            dest_path_1L_chr = paste0(path_to_pkg_rt_1L_chr, 
                "/.travis.yml"), edit_fn = function(txt_chr) {
                c(txt_chr, "warnings_are_errors: false")
            })
        usethis::use_github_action("pkgdown")
        pkg_path_1L_chr <- paste0(path_to_pkg_rt_1L_chr, "/R/", 
            "pkg_", dev_pkg_nm_1L_chr, ".R")
        write_from_tmp(pkg_path_1L_chr, dest_path_1L_chr = pkg_path_1L_chr, 
            edit_fn = function(txt_chr) {
                c(txt_chr, "## usethis namespace: start", "#' @importFrom lifecycle deprecate_soft", 
                  "## usethis namespace: end", "NULL")
            })
    }
    if (!is.na(path_to_pkg_logo_1L_chr) & !file.exists(paste0(path_to_pkg_rt_1L_chr, 
        "/pkgdown/favicon/apple-touch-icon-120x120.png"))) {
        pkgdown::build_favicons()
    }
    file.copy(paste0(path_to_pkg_rt_1L_chr, "/pkgdown/favicon/apple-touch-icon-120x120.png"), 
        paste0(path_to_pkg_rt_1L_chr, "/man/figures/fav120.png"))
    usethis::use_lifecycle()
    usethis::use_lifecycle_badge(lifecycle_stage_1L_chr)
}
#' Write prototype lookup table database
#' @description write_pt_lup_db() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write prototype lookup table database. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return NULL
#' @rdname write_pt_lup_db
#' @export 

write_pt_lup_db <- function (R_dir_1L_chr = "R") 
{
    write_from_tmp(system.file("db_pt_lup.R", package = "ready4fun"), 
        dest_path_1L_chr = paste0(R_dir_1L_chr, "/db_pt_lup.R"))
}
#' Write standard import
#' @description write_std_imp() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write standard import. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param R_dir_1L_chr R directory (a character vector of length one), Default: 'R'
#' @return NULL
#' @rdname write_std_imp
#' @export 

write_std_imp <- function (R_dir_1L_chr = "R") 
{
    write_from_tmp(system.file("imp_pipe_tmp.R", package = "ready4fun"), 
        dest_path_1L_chr = paste0(R_dir_1L_chr, "/imp_pipe.R"))
    write_from_tmp(system.file("imp_mthds_tmp.R", package = "ready4fun"), 
        dest_path_1L_chr = paste0(R_dir_1L_chr, "/imp_mthds.R"))
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
write_tb_to_csv <- function (tbs_r4, slot_nm_1L_chr, r4_name_1L_chr, lup_dir_1L_chr, 
    pfx_1L_chr) 
{
    methods::slot(tbs_r4, slot_nm_1L_chr) %>% dplyr::mutate_if(is.list, 
        .funs = dplyr::funs(ifelse(stringr::str_c(.) == "NULL", 
            NA_character_, stringr::str_c(.)))) %>% write.csv(file = paste0(lup_dir_1L_chr, 
        "/", pfx_1L_chr, "_", slot_nm_1L_chr, ".csv"), row.names = F)
}
#' Write to remove collate
#' @description write_to_remove_collate() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write to remove collate. The function returns Description (a character vector).
#' @param description_chr Description (a character vector)
#' @return Description (a character vector)
#' @rdname write_to_remove_collate
#' @export 

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
#' @importFrom usethis use_description
write_to_reset_pkg_files <- function (delete_contents_of_1L_chr, package_1L_chr = get_dev_pkg_nm(getwd()), 
    package_dir_1L_chr = getwd(), description_ls = NULL, keep_version_lgl = T) 
{
    devtools::load_all()
    if (keep_version_lgl) {
        desc_ls <- packageDescription(package_1L_chr)
        description_ls$Version <- desc_ls$Version
    }
    usethis::use_description(fields = description_ls)
    file.remove(paste0(package_dir_1L_chr, "/NAMESPACE"))
    do.call(file.remove, list(list.files(paste0(package_dir_1L_chr, 
        "/", delete_contents_of_1L_chr), full.names = TRUE)))
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
write_vignette <- function (package_1L_chr, pkg_rt_dir_chr = ".") 
{
    if (!dir.exists(paste0(pkg_rt_dir_chr, "/vignettes"))) 
        dir.create(paste0(pkg_rt_dir_chr, "/vignettes"))
    write_from_tmp(system.file("ready4fun.Rmd", package = "ready4fun"), 
        dest_path_1L_chr = paste0(pkg_rt_dir_chr, "/vignettes/", 
            package_1L_chr, ".Rmd"), edit_fn = function(txt_chr, 
            package_1L_chr) {
            txt_chr <- purrr::map_chr(txt_chr, ~stringr::str_replace_all(.x, 
                "ready4fun", package_1L_chr))
            txt_chr
        }, args_ls = list(package_1L_chr = package_1L_chr))
    write_from_tmp(system.file(".gitignore", package = "ready4fun"), 
        dest_path_1L_chr = paste0(pkg_rt_dir_chr, "/vignettes/", 
            ".gitignore"), edit_fn = function(txt_chr, package_1L_chr) {
            txt_chr
        }, args_ls = list(package_1L_chr = package_1L_chr))
}
#' Write workspace
#' @description write_ws() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write workspace. The function is called for its side effects and does not return a value. WARNING: This function writes R scripts to your local environment. Make sure to only use if you want this behaviour
#' @param path_1L_chr Path (a character vector of length one)
#' @return NULL
#' @rdname write_ws
#' @export 
#' @importFrom purrr walk
write_ws <- function (path_1L_chr) 
{
    dir.create(paste0(path_1L_chr, "/ready4"))
    top_level_chr <- paste0(path_1L_chr, "/ready4/", c("Code", 
        "Data", "Documentation", "Insight"))
    top_level_chr %>% purrr::walk(~dir.create(.x))
    c("Framework", "Models") %>% purrr::walk(~{
        dir.create(paste0(top_level_chr[1], "/", .x))
        dir.create(paste0(top_level_chr[1], "/", .x, "/R"))
    })
    c("Dataverse", "Project", "R_Format", "Raw_Format") %>% purrr::walk(~dir.create(paste0(top_level_chr[2], 
        "/", .x)))
    c("Agents", "Attributes", "Geometries", "Metadata") %>% purrr::walk(~dir.create(paste0(top_level_chr[2], 
        "/Raw_Format/", .x)))
    c("Code", "Data") %>% purrr::walk(~dir.create(paste0(top_level_chr[3], 
        "/", .x)))
    c("Developer", "User") %>% purrr::walk(~dir.create(paste0(top_level_chr[3], 
        "/Code/", .x)))
    c("Analysis", "Apps", "Pages", "Scientific Summaries") %>% 
        purrr::walk(~dir.create(paste0(top_level_chr[4], "/", 
            .x)))
}
