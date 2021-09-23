## Note files to be rewritten cannot be open in RStudio.
## Empty dataverse dataset must be published first.
#
# 1. Set-up workspace.
library(magrittr)
library(lifecycle)
fns_dir_1L_chr <-"data-raw/fns"
if(!dir.exists(fns_dir_1L_chr))
  dir.create(fns_dir_1L_chr)
#
# 2. Add functions.
# 2.1 MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 2.2. Read all undocumented functions in the temporary "fns" directory.
fns_env_ls <- new.env(parent = globalenv())
source(paste0(fns_dir_1L_chr,"/read.R"))
fns_env_ls <- read_fns(fns_dir_1L_chr,
                       fns_env = fns_env_ls)
rm(read_fns)
fns_env_ls$fns_env$write_fn_type_dirs()
#
# 3. Add package metadata
badges_lup <- tibble::tibble(badge_names_chr = "ready4",
                             label_names_chr = c("authoring","description","modelling", "prediction"),
                             colours_chr = c("maroon", "navy","indigo", "forestgreen")) %>%
  dplyr::mutate(badges_chr = purrr::map2_chr(label_names_chr, colours_chr,
                                             ~badgr::get_badge(
                                               label = "ready4",
                                               message = .x,
                                               color = .y,
                                               label_color = "black",
                                               md_link = "https://www.ready4-dev.com/toolkits/",
                                               logo_path = "https://raw.githubusercontent.com/ready4-dev/ready4fun/dev/data-raw/favicon-16x16.png",
                                               browser_preview = F,
                                               to_clipboard = F)))
pkg_desc_ls <- fns_env_ls$fns_env$make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Function Authoring And Documentation Tools For Use With The ready4 Suite",
                                                   pkg_desc_1L_chr = "ready4fun is a collection of functions for authoring code libraries of functions and datasets for use in mental health simulations developed within the ready4 ecosystem. The tools contained in this package automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE. You should only therefore only trial this software if you feel confident you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                                                   authors_prsn = c(utils::person(
                                                     given = "Matthew",family = "Hamilton",
                                                     email = "matthew.hamilton@orygen.org.au",
                                                     role = c("aut","cre"),
                                                     comment = c(ORCID = "0000-0001-7407-9194")),
                                                     utils::person("Glen", "Wiesner",
                                                                   email = "Glen.Wiesner@vu.edu.au",
                                                                   role = c("aut"),
                                                                   comment = c(ORCID = "0000-0002-0071-130X")),
                                                     #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                                                     #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                                                     #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                                                     #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                                                     utils::person("Orygen", role = c("cph", "fnd")),
                                                     utils::person("VicHealth",role = c("fnd")),
                                                     utils::person("Victoria University", role =c("fnd"))),
                                                   urls_chr = c("https://ready4-dev.github.io/ready4fun/",
                                                                "https://github.com/ready4-dev/ready4fun",
                                                                "https://www.ready4-dev.com/"))
pkg_ds_ls_ls <- list(fns_env_ls$fns_env$get_rds_from_dv("object_type_lup") %>%
                       fns_env_ls$fns_env$make_pkg_ds_ls(db_df = .,
                                                         abbreviations_lup = .,
                                                         db_1L_chr = "object_type_lup",
                                                         desc_1L_chr = "A lookup table to identify R object types from an abbreviation that can be used as object name suffices.",
                                                         object_type_lup = .,
                                                         title_1L_chr = "Object abbreviations lookup table",
                                                         url_1L_chr = "https://doi.org/10.7910/DVN/2Y9VF9"),
                     fns_env_ls$fns_env$make_pkg_ds_ls(db_df = badges_lup,
                                                       db_1L_chr = "badges_lup",
                                                       desc_1L_chr = "A lookup table to identify the appropriate text to insert in README files to represent different types of ready4 badges.",
                                                       title_1L_chr = "ready4 badges lookup table",
                                                       url_1L_chr = "https://ready4-dev.github.io/ready4/"))
#
# 4. Specify the new classes to be created
classes_to_make_tb <- dplyr::bind_rows(
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for tibble object lookup table of badges metadata.",
                                                   make_s3_lgl = TRUE,
                                                   name_stub_chr = "badges",
                                                   pt_ls = list(list("tibble")),
                                                   pt_chkr_pfx_ls = list(list("is_")),
                                                   pt_ns_ls = list(list("tibble")),
                                                   vals_ls = list(list(badge_names_chr = "character(0)",
                                                                       label_names_chr = "character(0)",
                                                                       colours_chr = "character(0)",
                                                                       badges_chr = "character(0)"))),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for tibble object lookup table of abbreviations.",
                                                   make_s3_lgl = TRUE,
                                                   name_stub_chr = "abbreviations",
                                                   pt_ls = list(list("tibble")),
                                                   pt_chkr_pfx_ls = list(list("is_")),
                                                   pt_ns_ls = list(list("tibble")),
                                                   vals_ls = list(list(short_name_chr = "character(0)",
                                                                       long_name_chr = "character(0)",
                                                                       plural_lgl = "logical(0)"))),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for list object specifying function arguments and function.",# S4
                                                   make_s3_lgl = T,
                                                   name_stub_chr = "fn_ls",
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")),
                                                   pt_ns_ls = list(list("base")),
                                                   vals_ls = list(list(args_ls = "list()",
                                                                       fn = "identity"))
  ),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for declaring package description file data.",# S4
                                                   make_s3_lgl = T,#F
                                                   name_stub_chr = "pkg_desc",
                                                   #slots_ls = c("Package","Title","Description","Authors", "License", "URL") %>% list(),
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")),#c("character","character","character","list", "logical","character") %>% list(),
                                                   pt_ns_ls = list(list("base")),
                                                   #parent_class_chr = NA_character_
                                                   vals_ls = list(list(Package = "character(0)",
                                                                       Title = "character(0)",
                                                                       Description = "character(0)",
                                                                       License = "logical(0)",
                                                                       URL = "character(0)"))
  ),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for package metadata required for initial package set-up step.",#S3
                                                   make_s3_lgl = T,#FALSE,
                                                   name_stub_chr = "pkg_setup_one",
                                                   # slots_ls = c("pkg_desc_ls","copyright_holders_chr","gh_repo_1L_chr","add_gh_site_1L_lgl", "addl_badges_ls",
                                                   #              "badges_lup", "check_type_1L_chr", "delete_r_dir_cnts_1L_lgl", "dev_pkg_nm_1L_chr", "lifecycle_stage_1L_chr",
                                                   #              "incr_ver_1L_lgl","on_cran_1L_lgl", "path_to_pkg_logo_1L_chr", "path_to_pkg_rt_1L_chr") %>% list(),
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")),# c("list", "character", "character", "character", "list",
                                                   #         "ready4_badges", "character", "logical", "character", "character",
                                                   #         "logical", "logical", "character", "character") %>% list(),
                                                   pt_ns_ls = list(list("base")),
                                                   # parent_class_chr = NA_character_
                                                   vals_ls = list(list("list()", "character(0)", "character(0)", "character(0)", "list()",
                                                                       "ready4_badges()", "character(0)", "logical(0)", "character(0)", "character(0)",
                                                                       "logical(0)", "logical(0)", "character(0)", "character(0)") %>%
                                                                    stats::setNames(c("pkg_desc_ls","copyright_holders_chr","gh_repo_1L_chr","add_gh_site_1L_lgl", "addl_badges_ls",
                                                                                      "badges_lup", "check_type_1L_chr", "delete_r_dir_cnts_1L_lgl", "dev_pkg_nm_1L_chr", "lifecycle_stage_1L_chr",
                                                                                      "incr_ver_1L_lgl","on_cran_1L_lgl", "path_to_pkg_logo_1L_chr", "path_to_pkg_rt_1L_chr")))
  ),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for package metadata required for second package set-up step.", # S3
                                                   make_s3_lgl = T,#F
                                                   name_stub_chr = "pkg_setup_two",
                                                   #slots_ls = c("addl_pkgs_ls","build_ignore_ls","dev_pkgs_chr","pkg_dmt_dv_dss_chr", "user_manual_fns_chr") %>% list(),
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")), #c("list", "list", "character", "character", "character") %>% list(),
                                                   pt_ns_ls = list(list("base")),
                                                   # parent_class_chr = NA_character_
                                                   vals_ls = list("list()", "list()", "character(0)", "character(0)", "character(0)") %>%
                                                     stats::setNames(c("addl_pkgs_ls","build_ignore_ls","dev_pkgs_chr","pkg_dmt_dv_dss_chr", "user_manual_fns_chr")) %>% list(),
  ),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for package metadata required for package set-up.", #S3
                                                   make_s3_lgl = T, #F
                                                   name_stub_chr = "pkg_setup",
                                                   #slots_ls = c("initial_ls","subsequent_ls") %>% list(),
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")), #c("ready4fun_pkg_setup_one", "ready4fun_pkg_setup_two") %>% list(),
                                                   pt_ns_ls = list(list("base")),
                                                   # parent_class_chr = NA_character_
                                                   vals_ls = list("ready4fun_pkg_setup_one()", "ready4fun_pkg_setup_two()") %>%
                                                     stats::setNames(c("initial_ls","subsequent_ls")) %>% list()
  ),
  ready4class::make_pt_ready4class_constructor_tbl(class_desc_chr = "ready4 S3 class for declaring package description file data.",#S3
                                                   make_s3_lgl = T,#F
                                                   name_stub_chr = "pkg_ds",
                                                   # slots_ls = c("db_df","db_1L_chr","title_1L_chr","desc_1L_chr", "abbreviations_lup",
                                                   #              "format_1L_chr", "object_type_lup", "simple_lup_1L_lgl", "url_1L_chr", "vars_ls") %>% list(),
                                                   pt_chkr_pfx_ls = list(list("is.")),
                                                   pt_ls = list(list("list")), # c("data.frame","character","character","character", "ready4fun_abbreviations",
                                                   # "character","ready4fun_abbreviations","logical","character","list") %>% list(),
                                                   pt_ns_ls = list(list("base")),
                                                   # parent_class_chr = NA_character_
                                                   vals_ls = list("data.frame()","character(0)","character(0)","character(0)", "ready4fun_abbreviations()",
                                                                  "character(0)","ready4fun_abbreviations()","logical(0)","character(0)","list()") %>%
                                                     stats::setNames(c("db_df","db_1L_chr","title_1L_chr","desc_1L_chr", "abbreviations_lup",
                                                                       "format_1L_chr", "object_type_lup", "simple_lup_1L_lgl", "url_1L_chr", "vars_ls")) %>% list())) %>%
  ready4class::ready4class_constructor_tbl()
pkg_setup_ls <- pkg_desc_ls %>%
  fns_env_ls$fns_env$make_pkg_setup_ls(addl_pkgs_ls = fns_env_ls$fns_env$make_addl_pkgs_ls(suggests_chr = "rmarkdown"),
                                       badges_lup = badges_lup,
                                       build_ignore_ls = fns_env_ls$fns_env$make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
                                       check_type_1L_chr = "standard",
                                       cls_fn_ls = list(fn = ready4class::write_classes_and_make_lup.ready4class_constructor_tbl,
                                                        args_ls = list(x = classes_to_make_tb)),
                                       classify_1L_lgl = F, ###
                                       copyright_holders_chr = "Orygen",
                                       inc_pkg_meta_data_1L_lgl = T, ###
                                       lifecycle_stage_1L_chr = "experimental",
                                       path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4fun-logo/default.png",
                                       path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"),
                                       pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                                              "https://doi.org/10.7910/DVN/2Y9VF9"),
                                       pkg_ds_ls_ls = pkg_ds_ls_ls,
                                       ready4_type_1L_chr = "authoring",
                                       user_manual_fns_chr = c("add_new_cls_pts",
                                                               "get_dv_fls_urls", "get_from_lup_obj", "get_rds_from_dv",
                                                               "make_addl_pkgs_ls","make_build_ignore_ls", "make_pkg_desc_ls", "make_pkg_ds_ls","make_pkg_setup_ls",
                                                               "update_abbr_lup", "update_msng_abbrs",
                                                               "write_new_abbrs","write_new_fn_types","write_new_obj_types","write_package", "write_ws"))
#
# 5. Add content to and document the package
pkg_setup_ls<- fns_env_ls$fns_env$write_package(pkg_setup_ls,
                                                list_generics_1L_lgl = T,
                                                self_serve_1L_lgl = T)
# pkg_setup_ls <- fns_env_ls$fns_env$write_new_abbrs(pkg_setup_ls,
#                                                    #classes_to_make_tb = classes_to_make_tb,
#                                                    long_name_chr = c("ready4fun R package"),
#                                                    #custom_plural_ls = list(repository = "repositories"),
#                                                    no_plural_chr = c("ready4fun R package"))
# pkg_setup_ls<- fns_env_ls$fns_env$write_new_fn_types(pkg_setup_ls,
#                                                      fn_type_desc_chr = c("Runs an analysis or analyses using specified data and parameters.",
#                                                                           "Searches and retrieves requested data from a specified source."),
#                                       is_generic_lgl = T,
#                                       publish_dv_1L_lgl = T)
# pkg_setup_ls <- fns_env_ls$fns_env$update_msng_abbrs(pkg_setup_ls,
#                                                      are_words_chr = c("cran", "lifecycle", "pdf",
#                                                                        "pdfs","pkgdown",
#                                                                        "R", "rds", "ready4", "url", "urls"),
#                                                      tf_to_singular_chr = c(dest = "dests",
#                                                                             dupl = "dupls",
#                                                                             msg = "msgs"))
# pkg_setup_ls <- fns_env_ls$fns_env$write_new_abbrs(pkg_setup_ls,
#                                                    #classes_to_make_tb = classes_to_make_tb,
#                                                    long_name_chr = c("additional","destination","detail","duplicate","github",
#                                                                      "increment","lookup tables", "message", "repository", "version"),
#                                                    custom_plural_ls = list(repository = "repositories"),
#                                                    no_plural_chr = c("lookup tables"))
##
# Create vignettes
# NOTE TO SELF: NEED TO RENAME export_lgl in tables and initial (not subsequent) functions to something like: inc_in_user_dmt_lgl
# NOTE TO SELF: NEED TO ADD WORKFLOW FOR TRANSITIONING FROM PRIVATE TO PUBLIC REPO TO CLENSE ALL PRIVATE COMMIT HISTORY. Variant of: https://gist.github.com/stephenhardy/5470814
# NOTE TO SELF: IN WORKFOW VIGNETTE, INCLUDE LINK TO: https://thenewstack.io/dont-mess-with-the-master-working-with-branches-in-git-and-github/
# and https://github.com/Kunena/Kunena-Forum/wiki/Create-a-new-branch-with-git-and-manage-branches
# and https://www.thegeekstuff.com/2019/03/git-create-dev-branch-and-merge/
# NOTE TO SELF: In vignette, include: https://docs.github.com/en/github/using-git/setting-your-username-in-git
# (plus user.email)
# Note to self - Ensure gitignore in default package bundle does not include docs/
