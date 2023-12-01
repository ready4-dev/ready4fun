author.ready4fun_manifest <- function(x,
                                      append_1L_lgl = F,
                                      consent_1L_chr = "",
                                      key_1L_chr = Sys.getenv("DATAVERSE_KEY"),
                                      list_generics_1L_lgl = T,
                                      self_serve_1L_lgl = F,
                                      self_serve_fn_ls = NULL) {
  x <- ready4::ratify(x, append_1L_lgl = append_1L_lgl)
  if (!is.null(x$problems_ls)) {
    message("Execution halted - fix issues with manifest before making a new call to author.")
  } else {
    message("Manifest has been validated. Proceeding to package set-up.")
    ready4::author(x$initial_ls)
    write_citation_fl(x)
    x <- ready4::authorData(x)
    x <- ready4::authorClasses(x,
      key_1L_chr = key_1L_chr,
      self_serve_1L_lgl = self_serve_1L_lgl,
      self_serve_fn_ls = self_serve_fn_ls
    )
    x <- ready4::renew(x, append_1L_lgl = append_1L_lgl, type_1L_chr = "fns_dmt", key_1L_chr = key_1L_chr)
    ready4::authorFunctions(x, list_generics_1L_lgl = list_generics_1L_lgl)
    ready4::authorReport(x,
      key_1L_chr = key_1L_chr
    )
    write_fns_dmt_tb(x)
    ready4::write_extra_pkgs_to_actions(path_to_dir_1L_chr = ".github/workflows") # Add to author method once consent has been added to function.
    ready4::write_to_edit_workflow("pkgdown.yaml")
    if (!consent_1L_chr %in% c("Y", "N")) {
      consent_1_1L_chr <- make_prompt(
        prompt_1L_chr = paste0(
          "Do you confirm ('Y') that you want to edit the file ",
          ".github/workflows/R-CMD-check.yaml ?"
        ),
        options_chr = c("Y", "N"), force_from_opts_1L_chr = T
      )
    } else {
      consent_1_1L_chr <- consent_1L_chr
    }
    if (consent_1_1L_chr %in% c("Y")) {
      readLines(".github/workflows/R-CMD-check.yaml") %>%
        stringr::str_replace_all("r-lib/actions/setup-r@master", "r-lib/actions/setup-r@v2") %>%
        stringr::str_replace_all("r-lib/actions/setup-pandoc@master", "r-lib/actions/setup-pandoc@v2") %>%
        writeLines(con = ".github/workflows/R-CMD-check.yaml")
    } else {
      warning("Write request cancelled - no new files have been written.")
    }

    if (!file.exists("pkgdown/extra.css")) {
      if (!consent_1L_chr %in% c("Y", "N")) {
        consent_2_1L_chr <- make_prompt(
          prompt_1L_chr = paste0(
            "Do you confirm ('Y') that you want to edit the file ",
            "pkgdown/extra.css ?"
          ),
          options_chr = c("Y", "N"), force_from_opts_1L_chr = T
        )
      } else {
        consent_2_1L_chr <- consent_1L_chr
      }
      if (consent_2_1L_chr %in% c("Y")) {
        writeLines(c("main table {", "  display: table;", "}"),
          con = "pkgdown/extra.css"
        )
      } else {
        warning("Write request cancelled - no new files have been written.")
      }
    }
  }
  return(x)
}
author.ready4fun_metadata_a <- function(x,
                                        consent_1L_chr = "",
                                        self_serve_1L_lgl = F) {
  rlang::exec(write_pkg_setup_fls, !!!x, consent_1L_chr = consent_1L_chr, self_serve_1L_lgl = self_serve_1L_lgl)
}
