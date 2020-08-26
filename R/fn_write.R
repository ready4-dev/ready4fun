#' Write all tibbles in tibbles readyforwhatsnext S4 to csvs
#' @description write_all_tbs_in_tbs_r4_to_csvs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write all a tibbles in a tibbles readyforwhatsnext S4 to csvs.NA
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param r4_name_chr Readyforwhatsnext S4 name (a character vector of length 1)
#' @param lup_dir_chr Lookup table directory (a character vector of length 1)
#' @param pfx_chr Prefix (a character vector of length 1)
#' @return NULL
#' @rdname write_all_tbs_in_tbs_r4_to_csvs
#' @export 
#' @importFrom purrr walk
#' @importFrom methods getSlots
write_all_tbs_in_tbs_r4_to_csvs <- function (tbs_r4, r4_name_chr, lup_dir_chr, pfx_chr) 
{
    purrr::walk(methods::getSlots(r4_name_chr) %>% names(), ~write_tb_to_csv(tbs_r4 = tbs_r4, 
        slot_nm_chr = .x, r4_name_chr = r4_name_chr, lup_dir_chr = lup_dir_chr, 
        pfx_chr = pfx_chr))
}
#' Write and document dataset
#' @description write_and_doc_ds_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write and a document dataset R.NA
#' @param db PARAM_DESCRIPTION
#' @param overwrite_lgl Overwrite (a logical vector of length 1), Default: T
#' @param db_chr Database (a character vector of length 1)
#' @param title_chr Title (a character vector of length 1)
#' @param desc_chr Description (a character vector of length 1)
#' @param format_chr Format (a character vector of length 1), Default: 'A tibble'
#' @param url_chr Url (a character vector of length 1), Default: 'NA'
#' @param vars_ls Vars (a list), Default: NULL
#' @param R_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_and_doc_ds_R
#' @export 
#' @importFrom devtools document load_all
write_and_doc_ds_R <- function (db, overwrite_lgl = T, db_chr, title_chr, desc_chr, 
    format_chr = "A tibble", url_chr = NA_character_, vars_ls = NULL, 
    R_dir_chr = "R", abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    eval(parse(text = paste0(db_chr, "<-db")))
    eval(parse(text = paste0("usethis::use_data(", db_chr, ", overwrite = overwrite_lgl)")))
    sink(paste0(R_dir_chr, "/db_", db_chr, ".R"), append = F)
    write_ds_dmt_R(db = db, db_chr = db_chr, title_chr = title_chr, 
        desc_chr = desc_chr, format_chr = format_chr, vars_ls = vars_ls, 
        url_chr = url_chr, R_dir_chr = R_dir_chr, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup)
    close_open_sinks()
    devtools::document()
    devtools::load_all()
}
#' Write and document function files
#' @description write_and_doc_fn_fls_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write and a document function files R.NA
#' @param fns_dmt_tb Functions dmt (a tibble)
#' @param r_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @param path_to_user_dmt_dir_chr Path to user dmt directory (a character vector of length 1), Default: '../../../../Documentation/Code/User'
#' @param path_to_dvpr_dmt_dir_chr Path to developer dmt directory (a character vector of length 1), Default: '../../../../Documentation/Code/Developer'
#' @param make_pdfs_lgl Make pdfs (a logical vector of length 1), Default: T
#' @return NULL
#' @rdname write_and_doc_fn_fls_R
#' @export 
#' @importFrom purrr walk2
#' @importFrom devtools document load_all build_manual
write_and_doc_fn_fls_R <- function (fns_dmt_tb, r_dir_chr = "R", path_to_user_dmt_dir_chr = "../../../../Documentation/Code/User", 
    path_to_dvpr_dmt_dir_chr = "../../../../Documentation/Code/Developer", 
    make_pdfs_lgl = T) 
{
    purrr::walk2(list(path_to_dvpr_dmt_dir_chr, path_to_user_dmt_dir_chr), 
        c(T, F), ~{
            write_fn_fl_R(fns_dmt_tb, r_dir_chr = r_dir_chr, 
                document_unexp_lgl = .y)
            devtools::document()
            devtools::load_all()
            if (make_pdfs_lgl) 
                devtools::build_manual(path = .x)
        })
}
#' Write documented functions
#' @description write_documented_fns_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a documented functions R.NA
#' @param tmp_fn_dir_chr Tmp function directory (a character vector of length 1)
#' @param R_dir_chr R directory (a character vector of length 1)
#' @return NULL
#' @rdname write_documented_fns_R
#' @export 
#' @importFrom sinew makeOxyFile
#' @importFrom purrr map_chr discard walk
#' @importFrom stringr str_sub
#' @keywords internal
write_documented_fns_R <- function (tmp_fn_dir_chr, R_dir_chr) 
{
    sinew::makeOxyFile(tmp_fn_dir_chr, verbose = F)
    files_chr_vec <- list.files(tmp_fn_dir_chr) %>% purrr::map_chr(~{
        ifelse(startsWith(.x, "oxy-"), .x, NA_character_)
    }) %>% purrr::discard(is.na)
    purrr::walk(files_chr_vec, ~{
        target_chr <- paste0(R_dir_chr, "/fn_", .x %>% stringr::str_sub(start = 5))
        original_chr <- paste0(tmp_fn_dir_chr, "/", .x)
        if (file.exists(target_chr)) 
            file.remove(target_chr)
        file.copy(original_chr, target_chr)
    })
    do.call(file.remove, list(paste0(tmp_fn_dir_chr, "/", files_chr_vec)))
}
#' Write dataset dmt
#' @description write_ds_dmt_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a dataset dmt R.NA
#' @param db PARAM_DESCRIPTION
#' @param db_chr Database (a character vector of length 1)
#' @param title_chr Title (a character vector of length 1)
#' @param desc_chr Description (a character vector of length 1)
#' @param format_chr Format (a character vector of length 1), Default: 'A tibble'
#' @param url_chr Url (a character vector of length 1), Default: 'NA'
#' @param vars_ls Vars (a list), Default: NULL
#' @param R_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_ds_dmt_R
#' @export 
#' @importFrom purrr map map2 pluck map2_chr
#' @importFrom stats setNames
#' @keywords internal
write_ds_dmt_R <- function (db, db_chr, title_chr, desc_chr, format_chr = "A tibble", 
    url_chr = NA_character_, vars_ls = NULL, R_dir_chr = "R", 
    abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    auto_vars_ls <- names(db) %>% purrr::map(~make_arg_desc_chr_vec(paste0(.x, 
        "_vec"), object_type_lup = object_type_lup, abbreviations_lup = abbreviations_lup)) %>% 
        stats::setNames(names(db))
    if (is.null(vars_ls)) {
        vars_ls <- auto_vars_ls
    }
    else {
        keep_auto_nms_chr_vec <- setdiff(names(auto_vars_ls), 
            names(vars_ls))
        vars_ls <- auto_vars_ls %>% purrr::map2(names(auto_vars_ls), 
            ~{
                if (.y %in% keep_auto_nms_chr_vec) {
                  .x
                }
                else {
                  vars_ls %>% purrr::pluck(.y)
                }
            })
    }
    writeLines(paste0("#' ", title_chr, "\n", "#' \n", "#' ", 
        desc_chr, "\n", "#' \n", "#' ", format_chr, "\n", "#' \n", 
        paste0("#' \\describe{\n", purrr::map2_chr(vars_ls, names(vars_ls), 
            ~paste0("#'   \\item{", .y, "}{", .x, "}")) %>% paste0(collapse = "\n"), 
            "\n#' }\n"), ifelse(is.na(url_chr), "", paste0("#' @source \\url{", 
            url_chr, "}\n")), "\"", db_chr, "\""))
}
#' Write function dmt
#' @description write_fn_dmt() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a function dmt.NA
#' @param fn_name_chr Function name (a character vector of length 1)
#' @param fn_type_chr Function type (a character vector of length 1)
#' @param fn Function (a function), Default: NULL
#' @param fn_desc_chr Function description (a character vector of length 1), Default: 'NA'
#' @param fn_out_type_chr Function out type (a character vector of length 1), Default: 'NA'
#' @param fn_title_chr Function title (a character vector of length 1), Default: 'NA'
#' @param example_lgl Example (a logical vector of length 1), Default: F
#' @param export_lgl Export (a logical vector of length 1), Default: T
#' @param class_name_chr Class name (a character vector of length 1), Default: ''
#' @param details_chr Details (a character vector of length 1), Default: 'DETAILS'
#' @param args_ls Arguments (a list), Default: NULL
#' @param import_chr_vec Import (a character vector), Default: 'NA'
#' @param doc_in_class_lgl Document in class (a logical vector of length 1), Default: F
#' @param abbreviations_lup Abbreviations (a lookup table), Default: NULL
#' @param object_type_lup Object type (a lookup table), Default: NULL
#' @return NULL
#' @rdname write_fn_dmt
#' @export 

write_fn_dmt <- function (fn_name_chr, fn_type_chr, fn = NULL, fn_desc_chr = NA_character_, 
    fn_out_type_chr = NA_character_, fn_title_chr = NA_character_, 
    example_lgl = F, export_lgl = T, class_name_chr = "", details_chr = "DETAILS", 
    args_ls = NULL, import_chr_vec = NA_character_, doc_in_class_lgl = F, 
    abbreviations_lup = NULL, object_type_lup = NULL) 
{
    if (is.null(abbreviations_lup)) 
        data("abbreviations_lup", package = "ready4fun", envir = environment())
    if (is.null(object_type_lup)) 
        data("object_type_lup", package = "ready4fun", envir = environment())
    fn_tags_spine_ls <- make_fn_dmt_spine_chr_ls(fn_name_chr = fn_name_chr, 
        fn_type_chr = fn_type_chr, fn_title_chr = fn_title_chr, 
        fn = fn, example_lgl = example_lgl, export_lgl = export_lgl, 
        details_chr = details_chr, class_name_chr = class_name_chr, 
        doc_in_class_lgl = doc_in_class_lgl)
    new_tag_chr_ls <- make_new_fn_dmt_chr_ls(fn_type_chr = fn_type_chr, 
        fn_name_chr = fn_name_chr, fn_desc_chr = fn_desc_chr, 
        fn_det_chr = details_chr, fn_out_type_chr = fn_out_type_chr, 
        args_ls = args_ls, fn, abbreviations_lup = abbreviations_lup, 
        object_type_lup = object_type_lup)
    fn_tags_chr <- update_fn_dmt_chr(fn_tags_spine_ls = fn_tags_spine_ls, 
        new_tag_chr_ls = new_tag_chr_ls, fn_name_chr = fn_name_chr, 
        fn_type_chr = fn_type_chr, import_chr_vec = import_chr_vec)
    writeLines(fn_tags_chr)
}
#' Write function file
#' @description write_fn_fl_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a function file R.NA
#' @param fns_dmt_tb Functions dmt (a tibble)
#' @param r_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @param document_unexp_lgl Document unexported (a logical vector of length 1), Default: T
#' @return NULL
#' @rdname write_fn_fl_R
#' @export 
#' @importFrom purrr walk
#' @importFrom dplyr filter
#' @keywords internal
write_fn_fl_R <- function (fns_dmt_tb, r_dir_chr = "R", document_unexp_lgl = T) 
{
    file_nms_chr_vec <- fns_dmt_tb$file_nm_chr %>% unique()
    file_nms_chr_vec %>% purrr::walk(~{
        tb <- fns_dmt_tb %>% dplyr::filter(file_nm_chr == .x)
        first_lgl_vec <- c(T, rep(F, nrow(tb) - 1))
        dest_path_chr <- paste0(r_dir_chr, "/", tb$file_pfx_chr[1], 
            .x)
        purrr::walk(1:nrow(tb), ~{
            fn <- eval(parse(text = tb[[.x, 1]]))
            fn_chr <- deparse(fn)
            sink(dest_path_chr, append = !first_lgl_vec[.x])
            write_fn_dmt(fn_name_chr = tb[[.x, 1]], fn_type_chr = "fn", 
                fn = fn, fn_desc_chr = tb[[.x, 3]], fn_out_type_chr = tb[[.x, 
                  6]], fn_title_chr = tb[[.x, 2]], example_lgl = tb[[.x, 
                  7]], export_lgl = T, class_name_chr = "", details_chr = tb[[.x, 
                  4]], args_ls = tb$args_ls[[.x]] %>% as.list(), 
                import_chr_vec = NA_character_, doc_in_class_lgl = F)
            if (tb[[.x, 5]] + document_unexp_lgl == 0) {
                writeLines(paste0("#' @keywords internal"))
            }
            writeLines(paste0(tb[[.x, 1]], " <- ", fn_chr[1]))
            writeLines(fn_chr[2:length(fn_chr)])
            close_open_sinks()
        })
    })
}
#' Write function type directories
#' @description write_fn_type_dirs() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a function type directories.NA
#' @param path_1L_chr Path 1L (a character vector of length 1), Default: 'data-raw'
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
#' Write from tmp
#' @description write_from_tmp_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write from tmp R.NA
#' @param temp_path_chr Temp path (a character vector of length 1)
#' @param dest_path_chr Dest path (a character vector of length 1)
#' @param edit_fn Edit (a function), Default: function(x) {
#'    x
#'}
#' @param args_ls Arguments (a list), Default: NULL
#' @return NULL
#' @rdname write_from_tmp_R
#' @export 
#' @importFrom rlang exec
#' @keywords internal
write_from_tmp_R <- function (temp_path_chr, dest_path_chr, edit_fn = function(x) {
    x
}, args_ls = NULL) 
{
    fileConn <- file(temp_path_chr)
    txt_chr <- readLines(fileConn)
    close(fileConn)
    txt_chr <- rlang::exec(edit_fn, txt_chr, !!!args_ls)
    if (temp_path_chr == dest_path_chr) 
        file.remove(temp_path_chr)
    fileConn <- file(dest_path_chr)
    writeLines(txt_chr, fileConn)
    close(fileConn)
}
#' Write namespace imports to description
#' @description write_ns_imps_to_desc() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a namespace imports to a description.NA
#' @param dev_pkgs_chr_vec Dev packages (a character vector), Default: 'NA'
#' @return NULL
#' @rdname write_ns_imps_to_desc
#' @export 
#' @importFrom devtools document
#' @importFrom purrr map_chr discard walk
#' @importFrom stringr str_replace str_sub str_locate
#' @importFrom usethis use_dev_package use_package
write_ns_imps_to_desc <- function (dev_pkgs_chr_vec = NA_character_) 
{
    devtools::document()
    packages_chr_vec <- readLines("NAMESPACE") %>% purrr::map_chr(~ifelse(startsWith(.x, 
        "import"), ifelse(startsWith(.x, "importFrom"), stringr::str_replace(.x, 
        "importFrom\\(", "") %>% stringr::str_sub(end = stringr::str_locate(., 
        ",")[1, 1] - 1), stringr::str_replace(.x, "import\\(", 
        "") %>% stringr::str_sub(end = -2)), NA_character_)) %>% 
        purrr::discard(is.na) %>% unique()
    if (!is.na(dev_pkgs_chr_vec)) {
        dev_pkgs_chr_vec <- intersect(packages_chr_vec, dev_pkgs_chr_vec) %>% 
            sort()
        packages_chr_vec <- setdiff(packages_chr_vec, dev_pkgs_chr_vec) %>% 
            sort()
        purrr::walk(dev_pkgs_chr_vec, ~usethis::use_dev_package(.x))
    }
    purrr::walk(packages_chr_vec, ~usethis::use_package(.x))
    devtools::document()
}
#' Write package
#' @description write_pkg_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a package R.NA
#' @param package_chr Package (a character vector of length 1)
#' @param R_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @return NULL
#' @rdname write_pkg_R
#' @export 
#' @importFrom purrr map_chr
#' @importFrom stringr str_replace_all
write_pkg_R <- function (package_chr, R_dir_chr = "R") 
{
    write_from_tmp_R(system.file("pckg_ready_fun.R", package = "ready4fun"), 
        dest_path_chr = paste0(R_dir_chr, "/pckg_", package_chr, 
            ".R"), edit_fn = function(txt_chr, package_chr) {
            pkg_desc_ls <- packageDescription(package_chr)
            txt_chr <- purrr::map_chr(txt_chr, ~stringr::str_replace_all(.x, 
                "ready4fun", package_chr))
            txt_chr[1] <- paste0("#' ", package_chr, ": ", pkg_desc_ls$Title)
            txt_chr[3] <- paste0("#' ", pkg_desc_ls$Description)
            txt_chr
        }, args_ls = list(package_chr = package_chr))
}
#' Write prototype lookup table database
#' @description write_pt_lup_db_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a prototype lookup table database R.NA
#' @param R_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @return NULL
#' @rdname write_pt_lup_db_R
#' @export 

write_pt_lup_db_R <- function (R_dir_chr = "R") 
{
    write_from_tmp_R(system.file("db_pt_lup.R", package = "ready4fun"), 
        dest_path_chr = paste0(R_dir_chr, "/db_pt_lup.R"))
}
#' Write standard import
#' @description write_std_imp_R() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a standard import R.NA
#' @param R_dir_chr R directory (a character vector of length 1), Default: 'R'
#' @return NULL
#' @rdname write_std_imp_R
#' @export 

write_std_imp_R <- function (R_dir_chr = "R") 
{
    write_from_tmp_R(system.file("imp_pipe_tmp.R", package = "ready4fun"), 
        dest_path_chr = paste0(R_dir_chr, "/imp_pipe.R"))
    write_from_tmp_R(system.file("imp_mthds_tmp.R", package = "ready4fun"), 
        dest_path_chr = paste0(R_dir_chr, "/imp_mthds.R"))
}
#' Write tibble to csv
#' @description write_tb_to_csv() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a tibble to csv.NA
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param slot_nm_chr Slot name (a character vector of length 1)
#' @param r4_name_chr Readyforwhatsnext S4 name (a character vector of length 1)
#' @param lup_dir_chr Lookup table directory (a character vector of length 1)
#' @param pfx_chr Prefix (a character vector of length 1)
#' @return NULL
#' @rdname write_tb_to_csv
#' @export 
#' @importFrom methods slot
#' @importFrom dplyr mutate_if funs
#' @importFrom stringr str_c
write_tb_to_csv <- function (tbs_r4, slot_nm_chr, r4_name_chr, lup_dir_chr, pfx_chr) 
{
    methods::slot(tbs_r4, slot_nm_chr) %>% dplyr::mutate_if(is.list, 
        .funs = dplyr::funs(ifelse(stringr::str_c(.) == "NULL", 
            NA_character_, stringr::str_c(.)))) %>% write.csv(file = paste0(lup_dir_chr, 
        "/", pfx_chr, "_", slot_nm_chr, ".csv"), row.names = F)
}
#' Write workspace
#' @description write_ws() is a Write function that writes a file to a specified local directory. Specifically, this function implements an algorithm to write a workspace.NA
#' @param path_chr Path (a character vector of length 1)
#' @return NULL
#' @rdname write_ws
#' @export 
#' @importFrom purrr walk
write_ws <- function (path_chr) 
{
    dir.create(paste0(path_chr, "/Readyforwhatsnext"))
    top_level_chr_vec <- paste0(path_chr, "/Readyforwhatsnext/", 
        c("Code", "Data", "Documentation", "Insight"))
    top_level_chr_vec %>% purrr::walk(~dir.create(.x))
    c("Framework", "Models") %>% purrr::walk(~{
        dir.create(paste0(top_level_chr_vec[1], "/", .x))
        dir.create(paste0(top_level_chr_vec[1], "/", .x, "/R"))
    })
    c("Dataverse", "Project", "R_Format", "Raw_Format") %>% purrr::walk(~dir.create(paste0(top_level_chr_vec[2], 
        "/", .x)))
    c("Agents", "Attributes", "Geometries", "Metadata") %>% purrr::walk(~dir.create(paste0(top_level_chr_vec[2], 
        "/Raw_Format/", .x)))
    c("Code", "Data") %>% purrr::walk(~dir.create(paste0(top_level_chr_vec[3], 
        "/", .x)))
    c("Developer", "User") %>% purrr::walk(~dir.create(paste0(top_level_chr_vec[3], 
        "/Code/", .x)))
    c("Analysis", "Apps", "Pages", "Scientific Summaries") %>% 
        purrr::walk(~dir.create(paste0(top_level_chr_vec[4], 
            "/", .x)))
}
