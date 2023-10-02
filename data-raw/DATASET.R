library(lifecycle)
library(ready4)
library(ready4show)
library(generics)
# ready4fun::fns_dmt_tb -> fns_dmt_tb
# piggyback::pb_new_release("ready4-dev/ready4fun",
#                           tag = "0.0.0.9461",
#                           body = "First release indexed with a Zenodo DOI.",
#                           prerelease = T)
fns_dir_1L_chr <- "data-raw/fns"
if (!dir.exists(fns_dir_1L_chr)) {
  dir.create(fns_dir_1L_chr)
}
# MANUAL STEP. Write all functions to R files in the new "fns" directory.
fns_env_ls <- new.env(parent = globalenv())
source(paste0(fns_dir_1L_chr, "/read.R"))
fns_env_ls <- read_fns(fns_dir_1L_chr,
  fns_env = fns_env_ls
)
fns_env_ls$fns_env$write_fn_type_dirs()
rm(read_fns)
badges_lup <- tibble::tibble(
  badge_names_chr = "ready4",
  label_names_chr = c("foundation", "authoring", "description", "modelling", "prediction"),
  colours_chr = c("coral", "maroon", "navy", "indigo", "forestgreen")
) %>%
  dplyr::mutate(badges_chr = purrr::map2_chr(
    label_names_chr, colours_chr,
    ~ badgr::get_badge(
      label = "ready4",
      message = .x,
      color = .y,
      label_color = "black",
      md_link = ifelse(.x %in% c("foundation", "authoring"), "https://www.ready4-dev.com/docs/software/libraries/types/framework/", "https://www.ready4-dev.com/docs/software/libraries/types/module/"),
      logo_path = "https://github.com/ready4-dev/ready4/releases/download/Documentation_0.0/favicon-16x16.png", # "https://raw.githubusercontent.com/ready4-dev/ready4fun/dev/data-raw/favicon-16x16.png",
      browser_preview = F,
      to_clipboard = F
    )
  ))
pkg_desc_ls <- fns_env_ls$fns_env$make_pkg_desc_ls(
  pkg_title_1L_chr = "Author and Document Functions That Implement Transferable Health Economic Model Algorithms" %>% tools::toTitleCase(),
  pkg_desc_1L_chr = "ready4fun is a toolkit for authoring and documenting functions that implement algorithms for models developed with the ready4 framework (https://www.ready4-dev.com/). The toolkit aims to help all developers contributing to ready4 to adopt a common house style in authoring and documenting functions.
                                                   The current version of this software is a development release, which you should only trial if you feel confident you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton1@monash.edu).",
  authors_prsn = c(
    utils::person(
      given = "Matthew", family = "Hamilton",
      email = "matthew.hamilton1@monash.edu",
      role = c("aut", "cre"),
      comment = c(ORCID = "0000-0001-7407-9194")
    ),
    utils::person("Glen", "Wiesner",
      #email = "Glen.Wiesner@vu.edu.au",
      role = c("aut"),
      comment = c(ORCID = "0000-0002-0071-130X")
    ),
    # person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
    # person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
    # person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
    # person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
    utils::person("Orygen", role = c("cph", "fnd")),
    utils::person("Australian Government Research Training Program", role = c("fnd")),
    utils::person("VicHealth", role = c("fnd")),
    utils::person("Victoria University", role = c("fnd"))
  ),
  urls_chr = c(
    "https://ready4-dev.github.io/ready4fun/",
    "https://github.com/ready4-dev/ready4fun",
    "https://www.ready4-dev.com/"
  )
)
pkg_ds_ls_ls <- list(
  fns_env_ls$fns_env$get_rds_from_pkg_dmt(
    fl_nm_1L_chr = "object_type_lup",
    piggyback_to_1L_chr = "ready4-dev/ready4"
  ) %>%
    fns_env_ls$fns_env$make_pkg_ds_ls(
      db_df = .,
      abbreviations_lup = .,
      db_1L_chr = "object_type_lup",
      desc_1L_chr = "A lookup table to identify R object types from an abbreviation that can be used as object name suffices.",
      object_type_lup = .,
      title_1L_chr = "Object abbreviations lookup table",
      url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9"
    ),
  fns_env_ls$fns_env$make_pkg_ds_ls(
    db_df = badges_lup,
    db_1L_chr = "badges_lup",
    desc_1L_chr = "A lookup table to identify the appropriate text to insert in README files to represent different types of ready4 badges.",
    title_1L_chr = "ready4 badges lookup table",
    url_1L_chr = "https://ready4-dev.github.io/ready4/"
  )
)
constructor_r3 <- dplyr::bind_rows(
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Badges lookup table.",
    make_s3_lgl = TRUE,
    name_stub_chr = "badges",
    pt_ls = list(list("tibble")),
    pt_chkr_pfx_ls = list(list("is_")),
    pt_ns_ls = list(list("tibble")),
    vals_ls = list(list(
      badge_names_chr = "character(0)",
      label_names_chr = "character(0)",
      colours_chr = "character(0)",
      badges_chr = "character(0)"
    ))
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Abbreviations lookup table.",
    make_s3_lgl = TRUE,
    name_stub_chr = "abbreviations",
    pt_ls = list(list("tibble")),
    pt_chkr_pfx_ls = list(list("is_")),
    pt_ns_ls = list(list("tibble")),
    vals_ls = list(list(
      short_name_chr = "character(0)",
      long_name_chr = "character(0)",
      plural_lgl = "logical(0)"
    ))
  ),
  ##
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Object types lookup table.",
    make_s3_lgl = TRUE,
    name_stub_chr = "objects",
    pt_ls = list(list("tibble")),
    pt_chkr_pfx_ls = list(list("is_")),
    pt_ns_ls = list(list("tibble")),
    vals_ls = list(list(
      short_name_chr = "character(0)",
      long_name_chr = "character(0)",
      atomic_element_lgl = "logical(0)",
      r3_can_extend_lgl = "logical(0)"
    ))
  ),
  ##
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Function types lookup table.",
    make_s3_lgl = TRUE,
    name_stub_chr = "functions",
    pt_ls = list(list("tibble")),
    pt_chkr_pfx_ls = list(list("is_")),
    pt_ns_ls = list(list("tibble")),
    vals_ls = list(list(
      fn_type_nm_chr = "character(0)",
      fn_type_desc_chr = "character(0)",
      first_arg_desc_chr = "character(0)",
      second_arg_desc_chr = "character(0)",
      is_generic_lgl = "logical(0)",
      is_method_lgl = "logical(0)"
    ))
  ), ##
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Function and function arguments pair.", # S4
    make_s3_lgl = T,
    name_stub_chr = "executor",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(list(
      args_ls = "list()",
      fn = "identity"
    ))
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Package description metadata.", # S4
    make_s3_lgl = T,
    name_stub_chr = "description",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(list(
      Package = "character(0)",
      Title = "character(0)",
      Description = "character(0)",
      License = "logical(0)",
      URL = "character(0)"
    ))
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Package metadata (A).", # S3
    make_s3_lgl = T,
    name_stub_chr = "metadata_a",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(list(
      "ready4fun_description()", "character(0)", "character(0)", "logical(0)", "list()",
      "ready4fun_badges()", "character(0)", "logical(0)", "character(0)", "character(0)",
      "logical(0)", "logical(0)", "character(0)", "character(0)"
    ) %>%
      stats::setNames(c(
        "pkg_desc_ls", "copyright_holders_chr", "gh_repo_1L_chr", "add_gh_site_1L_lgl", "addl_badges_ls",
        "badges_lup", "check_type_1L_chr", "delete_r_dir_cnts_1L_lgl", "dev_pkg_nm_1L_chr", "lifecycle_stage_1L_chr",
        "incr_ver_1L_lgl", "on_cran_1L_lgl", "path_to_pkg_logo_1L_chr", "path_to_pkg_rt_1L_chr"
      )))
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Package metadata (B).",
    make_s3_lgl = T,
    name_stub_chr = "metadata_b",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(
      "ready4fun_abbreviations()", "list()", "list()",
      "ready4fun_executor()", "list()", "character(0)",
      "list()",
      "character(0)", "character(0)", "tibble::tibble()", "tibble::tibble()",
      "character(0)", "logical(0)", "ready4fun_abbreviations()",
      "character(0)", "character(0)",
      "character(0)", "list()",
      "tibble::tibble()", "character(0)",
      "ready4fun_executor()", "character(0)"
    ) %>%
      stats::setNames(c(
        "abbreviations_lup", "addl_pkgs_ls", "build_ignore_ls",
        "cls_fn_ls", "custom_dmt_ls", "dev_pkgs_chr",
        "dss_records_ls",
        "dv_ds_nm_1L_chr", "dv_url_pfx_1L_chr", "fn_types_lup", "fns_dmt_tb",
        "import_from_chr", "inc_pkg_meta_data_1L_lgl", "object_type_lup",
        "path_to_dmt_dir_1L_chr", "piggyback_to_1L_chr",
        "pkg_dmt_dv_dss_chr", "pkg_ds_ls_ls",
        "seed_obj_type_lup", "server_1L_chr",
        "s4_fns_ls", "treat_as_words_chr"
      )) %>%
      list()
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Package set-up manifest.",
    make_s3_lgl = T,
    name_stub_chr = "manifest",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(
      "ready4fun_metadata_a()",
      "ready4fun_metadata_b()"
    ) %>%
      stats::setNames(c("initial_ls", "subsequent_ls")) %>% list()
  ),
  ready4class::make_pt_ready4class_constructor(
    class_desc_chr = "Package datasets metadata.", # S3
    make_s3_lgl = T, # F
    name_stub_chr = "dataset",
    pt_chkr_pfx_ls = list(list("is.")),
    pt_ls = list(list("list")),
    pt_ns_ls = list(list("base")),
    vals_ls = list(
      "data.frame()", "character(0)", "character(0)", "character(0)", "ready4fun_abbreviations()",
      "character(0)", "ready4fun_abbreviations()", "logical(0)", "character(0)", "list()"
    ) %>%
      stats::setNames(c(
        "db_df", "db_1L_chr", "title_1L_chr", "desc_1L_chr", "abbreviations_lup",
        "format_1L_chr", "object_type_lup", "simple_lup_1L_lgl", "url_1L_chr", "vars_ls"
      )) %>% list()
  )
) %>%
  ready4class::ready4class_constructor()
manifest_ls <- pkg_desc_ls %>%
  fns_env_ls$fns_env$make_manifest(
    addl_pkgs_ls = fns_env_ls$fns_env$make_addl_pkgs_ls(
      depends_chr = c("ready4", "generics"),
      imports_chr = c("ready4show", "ready4use"),
      suggests_chr = c("rmarkdown")
    ),
    build_ignore_ls = fns_env_ls$fns_env$make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
    check_type_1L_chr = "standard",
    cls_fn_ls = list(
      fn = ready4class::author.ready4class_constructor,
      args_ls = list(x = constructor_r3)
    ),
    classify_1L_lgl = F, ###
    custom_dmt_ls = fns_env_ls$fns_env$make_custom_dmt_ls(
      # user_manual_fns_chr = c("add_new_cls_pts","make_addl_pkgs_ls","make_build_ignore_ls", "make_pkg_desc_ls", "make_pkg_ds_ls","make_manifest",
      #                         "update_abbr_lup", "update_msng_abbrs", "write_new_abbrs","write_new_fn_types","write_new_obj_types","write_package")
    ),
    copyright_holders_chr = "Orygen",
    dev_pkgs_chr = c(
      "ready4",
      "ready4use",
      "ready4show"
    ),
    inc_pkg_meta_data_1L_lgl = T, ###
    lifecycle_stage_1L_chr = "experimental",
    path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4fun-logo/default.png",
    path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"),
    piggyback_to_1L_chr = "ready4-dev/ready4",
    pkg_ds_ls_ls = pkg_ds_ls_ls,
    ready4_type_1L_chr = "authoring",
    zenodo_badge_1L_chr = "[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5611779.svg)](https://doi.org/10.5281/zenodo.5611779)"
  )
manifest_ls <- fns_env_ls$fns_env$write_package(manifest_ls,
  self_serve_1L_lgl = T
)
ready4::write_extra_pkgs_to_actions() # Add to author method once consent has been added to function.
#readLines(".github/workflows/R-CMD-check.yaml") %>%
  #stringr::str_replace_all("r-lib/actions/setup-r@master", "r-lib/actions/setup-r@v2") %>%
  #stringr::str_replace_all("r-lib/actions/setup-pandoc@master", "r-lib/actions/setup-pandoc@v2") %>%
  # stringr::str_replace_all("- \\{os: windows-latest, r: '3.6'\\}", "#- \\{os: windows-latest, r: '3.6'\\}") %>%
  # stringr::str_replace_all("- \\{os: ubuntu-20.04,   r: 'oldrel', ", "#- \\{os: ubuntu-20.04,   r: 'oldrel', ") %>%
  # purrr::discard_at(2:4) %>%
  # writeLines(con = ".github/workflows/R-CMD-check.yaml")
write_to_edit_workflow("pkgdown.yaml") # In other packages, run for "test-coverage.yaml" as well.
readLines("_pkgdown.yml") %>%
  stringr::str_replace_all("  - text: Model", "  - text: Framework & Model") %>%
  writeLines(con = "_pkgdown.yml")
usethis::use_dev_package("ready4", type = "depends", remote = "ready4-dev/ready4")
usethis::use_dev_package("ready4show", remote = "ready4-dev/ready4show")
devtools::build_vignettes()
# fns_env_ls$fns_env$read_fns(fns_dir_1L_chr,use_env_1L_lgl = F)
