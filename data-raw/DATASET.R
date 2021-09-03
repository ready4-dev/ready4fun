## Note files to be rewritten cannot be open in RStudio.
## Dataverse dataset with seed object_type and abbreviations look-ups must be established first.
# 1. Set-up workspace.
library(magrittr)
fns_dir_1L_chr <-"data-raw/fns"
if(!dir.exists(fns_dir_1L_chr))
  dir.create(fns_dir_1L_chr)
#
# 2. Add functions.
# 2.1 MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 2.2 Update code house-style datasets.
# Ensure all abbreviations used in function names, arguments and return values are described in abbreviation look-up tables.
# Make the necessary changes in MAKE_HOUSESTYLE_DV_DSS.R and uncomment and run the following line.
# source("data-raw/MAKE_HOUSESTYLE_DV_DSS.R")
#
# 2.3. Read all undocumented functions in the temporary "fns" directory.
fns_env_ls <- new.env(parent = globalenv())
source(paste0(fns_dir_1L_chr,"/read.R"))
fns_env_ls <- read_fns(fns_dir_1L_chr,
                       fns_env = fns_env_ls)
rm(read_fns)
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
pkg_setup_ls <- pkg_desc_ls %>%
  fns_env_ls$fns_env$make_pkg_setup_ls(addl_pkgs_ls = fns_env_ls$fns_env$make_addl_pkgs_ls(suggests_chr = "rmarkdown"),
                    badges_lup = badges_lup,
                    build_ignore_ls = fns_env_ls$fns_env$make_build_ignore_ls(file_nms_chr = c("initial_setup.R")), #
                    check_type_1L_chr = "standard",
                    copyright_holders_chr = "Orygen",
                    lifecycle_stage_1L_chr = "experimental",
                    path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4fun-logo/default.png",
                    pkg_dmt_dv_dss_chr = c("https://doi.org/10.7910/DVN/HLLXZN",
                                           "https://doi.org/10.7910/DVN/2Y9VF9"),
                    ready4_type_1L_chr = "authoring",
                    user_manual_fns_chr = c("get_dv_fls_urls", "get_from_lup_obj", "get_rds_from_dv",
                                            "make_pkg_desc_ls", "make_pkg_ds_ls","make_pkg_setup_ls",
                                            "update_abbr_lup",
                                           " write_package", "write_ws"))
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
##
# 4. Specify the new classes to be created
name_pfx_1L_chr <- pkg_setup_ls$initial_ls$pkg_desc_ls$Package
classes_to_make_tb <- dplyr::bind_rows(
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "badges",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(badge_names_chr = "character(0)",
                                                                  label_names_chr = "character(0)",
                                                                  colours_chr = "character(0)",
                                                                  badges_chr = "character(0)")),
                                              class_desc_chr = "ready4 S3 class for tibble object lookup table of badges metadata."),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = TRUE,
                                              name_stub_chr = "abbreviations",
                                              pt_ls = list(list("tibble")),
                                              pt_chkr_pfx_ls = list(list("is_")),
                                              pt_ns_ls = list(list("tibble")),
                                              vals_ls = list(list(short_name_chr = "character(0)",
                                                                  long_name_chr = "character(0)",
                                                                  plural_lgl = "logical(0)")),
                                              class_desc_chr = "ready4 S3 class for tibble object lookup table of abbreviations."),
   ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "pkg_desc",
                                              slots_ls = c("Package","Title","Description","Authors", "License", "URL") %>% list(),
                                              pt_ls = c("character","character","character","list", "logical","character") %>% list(),
                                              class_desc_chr= "ready4 S4 class for declaring package description file data.",
                                              parent_class_chr = NA_character_),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "pkg_setup_one",
                                              slots_ls = c("pkg_desc_ls","copyright_holders_chr","gh_repo_1L_chr","add_gh_site_1L_lgl", "addl_badges_ls",
                                                           "badges_lup", "check_type_1L_chr", "delete_r_dir_cnts_1L_lgl", "dev_pkg_nm_1L_chr", "lifecycle_stage_1L_chr",
                                                           "incr_ver_1L_lgl","on_cran_1L_lgl", "path_to_pkg_logo_1L_chr", "path_to_pkg_rt_1L_chr") %>% list(),
                                              pt_ls = c("list", "character", "character", "character", "list",
                                                        "ready4_badges", "character", "logical", "character", "character",
                                                        "logical", "logical", "character", "character") %>% list(),
                                              class_desc_chr= "ready4 S4 class for package metadata required for initial package set-up step.",
                                              parent_class_chr = NA_character_),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "pkg_setup_two",
                                              slots_ls = c("addl_pkgs_ls","build_ignore_ls","dev_pkgs_chr","pkg_dmt_dv_dss_chr", "user_manual_fns_chr") %>% list(),
                                              pt_ls = c("list", "list", "character", "character", "character") %>% list(),
                                              class_desc_chr= "ready4 S4 class for package metadata required for second package set-up step.",
                                              parent_class_chr = NA_character_),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "pkg_setup",
                                              slots_ls = c("initial_ls","subsequent_ls") %>% list(),
                                              pt_ls = c("ready4_pkg_setup_one", "ready4_pkg_setup_two") %>% list(),
                                              class_desc_chr= "ready4 S4 class for package metadata required for package set-up.",
                                              parent_class_chr = NA_character_),
  ready4class::make_pt_ready4_constructor_tbl(make_s3_lgl = FALSE,
                                              name_stub_chr = "pkg_ds",
                                              slots_ls = c("db_df","db_1L_chr","title_1L_chr","desc_1L_chr", "abbreviations_lup",
                                                           "format_1L_chr", "object_type_lup", "simple_lup_1L_lgl", "url_1L_chr", "vars_ls") %>% list(),
                                              pt_ls = c("data.frame","character","character","character", "ready4_abbreviations",
                                                        "character","ready4_abbreviations","logical","character","list") %>% list(),
                                              class_desc_chr= "ready4 S4 class for declaring package description file data.",
                                              parent_class_chr = NA_character_)

) %>%
  ready4class::ready4_constructor_tbl()
pkg_setup_ls <- fns_env_ls$fns_env$validate_pkg_setup(pkg_setup_ls,
                                                      classes_to_make_tb = classes_to_make_tb,
                                                      pkg_ds_ls_ls = pkg_ds_ls_ls)
# pkg_setup_ls <- fns_env_ls$fns_env$write_new_fn_types(pkg_setup_ls,
#                                                       fn_type_desc_chr = "Validates that an object conforms to required criteria.",
#                                                       is_generic_lgl = F,
#                                                       is_method_lgl = F,
#                                                       publish_dv_1L_lgl = T)
# pkg_setup_ls <- fns_env_ls$fns_env$write_new_abbrs(pkg_setup_ls,
#                                                    are_plurals_chr = c("dests"), # rework as list
#                                                    are_words_chr = c("cran", "lifecycle", "pdfs","pkgdown",
#                                                                      "R", "rds", "ready4", "url", "urls"),
#                                                    classes_to_make_tb = classes_to_make_tb,
#                                                    long_name_chr = c("additional","destination","detail","duplicates","github",
#                                                                      "increment","lookup tables", "messages", "repository", "version"),
#                                                    custom_plural_ls = list(repository = "repositories"),
#                                                    no_plural_chr = c("duplicates","lookup tables","messages"),
#                                                    publish_dv_1L_lgl = T)
## Add abbreviations to dv - Pick up here.
## Create classes (using rlang::exec and fn passed as arg.)
# 5. Add content to and document the package
##
fns_env_ls$fns_env$write_package(pkg_desc_ls,
                                 pkg_ds_ls_ls,
                                 pkg_setup_ls,
                                 dv_url_pfx_1L_chr = "https://dataverse.harvard.edu/api/access/datafile/",
                                 path_to_dmt_dir_1L_chr = normalizePath("../../../../../Documentation/Code"),
                                 publish_dv_1L_lgl = T)
# Create vignettes
# NOTE TO SELF: Currently Vignettes are overwritten by this last step. Need to implement more sophisticated workflow.
# NOTE TO SELF: NEED TO RENAME export_lgl in tables and initial (not subsequent) functions to something like: inc_in_user_dmt_lgl
# NOTE TO SELF: NEED TO ADD WORKFLOW FOR TRANSITIONING FROM PRIVATE TO PUBLIC REPO TO CLENSE ALL PRIVATE COMMIT HISTORY. Variant of: https://gist.github.com/stephenhardy/5470814
# NOTE TO SELF: IDEALLY SHOULD ADD OPTION TO INCREMENT PACKAGE VERSION NUMBER WHEN DESCRIPTION FILE GETS UPDATED. See: https://stackoverflow.com/questions/24209336/automating-version-increase-of-r-packages
# NOTE TO SELF: IN WORKFOW VIGNETTE, INCLUDE LINK TO: https://thenewstack.io/dont-mess-with-the-master-working-with-branches-in-git-and-github/
# and https://github.com/Kunena/Kunena-Forum/wiki/Create-a-new-branch-with-git-and-manage-branches
# and https://www.thegeekstuff.com/2019/03/git-create-dev-branch-and-merge/
# NOTE TO SELF: In vignette, include: https://docs.github.com/en/github/using-git/setting-your-username-in-git
# (plus user.email)
# Note to self - Ensure gitignore in default package bundle does not include docs/

