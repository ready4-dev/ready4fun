# NOTE: To install, deprecated rtools is sought. Users with R>4.0 need the following line in Renviron: PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
#
# WS Set-up
# # Add token to R environ
# Ensure gh-pages branch on repo: https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
# install.packages('tinytex')
#  tinytex::install_tinytex()
#  tinytex:::install_yihui_pkgs()
# tinytex::tlmgr_install("makeindex")
# 1. Load magrittr package to that the pipe operator ("%>%") can be used in this script.
library(magrittr)
#
# 2. Create a "fns" sub-directory.
fns_dir_1L_chr <-"data-raw/fns"
if(!dir.exists(fns_dir_1L_chr))
  dir.create(fns_dir_1L_chr)
#
# 3. MANUAL STEP. Write all your functions to R files in the new "fns" directory.
#
# 4. Read all undocumented functions in the temporary "fns" directory.
source(paste0(fns_dir_1L_chr,"/read.R"))
fns_path_chr <- read_fns(fns_dir_1L_chr)
#
# 5. Set-up package structure
badges_lup <- tibble::tibble(badge_names_chr = "ready4",
                             label_names_chr = c("development","modelling", "prediction"),
                            colours_chr = c("maroon", "indigo", "forestgreen")) %>%
  dplyr::mutate(badges_chr = purrr::map2_chr(label_names_chr, colours_chr,
                                             ~badgr::get_badge(
                                               label = "ready4",
                                               message = .x,
                                               color = .y,
                                               label_color = "black",
                                               md_link = "https://ready4-dev.github.io/ready4/index.html",
                                               logo_path = "https://raw.githubusercontent.com/ready4-dev/ready4fun/dev/data-raw/favicon-16x16.png",
                                               browser_preview = F,
                                               to_clipboard = F)))
make_pkg_desc_ls(pkg_title_1L_chr = "Standardised Function Authoring And Documentation Tools For Use With The ready4 Suite",
                 pkg_desc_1L_chr = "ready4fun is a collection of functions for authoring code libraries of functions and datasets for use in mental health simulations developed within the ready4 ecosystem.
  This development version of the ready4fun package has been made available as part of the process of testing and documenting the package. The tools contained in this package automate a number of tasks which MODIFY THE DIRECTORY STRUCTURE OF YOUR LOCAL MACHINE.
  You should only therefore only trial this software if you feel confident you understand what it does and have created a sandpit area in which you can safely undertake testing. If you have any questions, please contact the authors (matthew.hamilton@orygen.org.au).",
                 authors_prsns = c(utils::person(
                   given = "Matthew",family = "Hamilton", email =
                     "matthew.hamilton@orygen.org.au",role = c("aut",
                                                               "cre"),comment = c(ORCID = "0000-0001-7407-9194")
                 ),
                 utils::person("Glen", "Wiesner", email = "Glen.Wiesner@vu.edu.au",
                               role = c("aut"), comment = c(ORCID = "0000-0002-0071-130X")),
                 #person("Alexandra", "Parker", email =  "Alex.Parker@vu.edu.au", role = c("rev"), comment = c(ORCID ="0000-0002-2398-6306")),
                 #person("Cathrine", "Mihalopoulos",email = "cathy.mihalopoulos@deakin.edu.au", role = c("rev"), comment = c(ORCID = "0000-0002-7127-9462")),
                 #person("Jonathan", "Karnon", email ="Jonathan.Karnon@flinders.edu.au", role = c("rev"), comment =c(ORCID = "0000-0003-3220-2099")),
                 #person("Petra","Plencnerova", email = "Petra.Plencnerova@vu.edu.au", role =c("rev"), comment = c(ORCID = "0000-0001-9698-9084")),
                 utils::person("Orygen", role = c("cph", "fnd")),
                 utils::person("VicHealth",role = c("fnd")),
                 utils::person("Victoria University", role =c("fnd"))
                 ),
                 urls_chr = c("https://ready4-dev.github.io/ready4fun/",
                              "https://github.com/ready4-dev/ready4fun",
                              "https://ready4-dev.github.io/ready4/")) %>%
write_pkg_setup_fls(incr_ver_1L_lgl = F,
                    delete_contents_of_R_dir = T,
                    copyright_holders_chr = "Orygen",
                    check_type_1L_chr = "gh",
                    path_to_pkg_logo_1L_chr = "../../../../../Documentation/Images/ready4fun-logo/default.png",
                    github_repo = "ready4-dev/ready4fun",
                    lifecycle_stage_1L_chr = "experimental",
                    badges_lup = badges_lup,
                    addl_badges_ls = list(ready4 = "development"))
## INTERACTIVE INPUT
# 6. Create a lookup table of abbreviations of R object types and their descriptions and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
pkg_dss_tb <- make_obj_lup() %>%
  write_and_doc_ds(db = .,
                   overwrite_1L_lgl = T,
                   db_1L_chr = "object_type_lup",
                   title_1L_chr = "Object abbreviations lookup table",
                   desc_1L_chr = "A lookup table to identify R object types from an abbreviation that can be used as object name suffices.",
                   format_1L_chr = "A tibble",
                   url_1L_chr = "https://ready4-dev.github.io/ready4/",
                   abbreviations_lup = .,
                   object_type_lup = .
  )
#
# 7. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
pkg_dss_tb <- write_abbr_lup(short_name_chr = c("1L","abbr","arg","artl","csv","db","depcy","desc","dev","dir","ds","dmt","dmtd","doc","dvpr","fl","fns","gtr","imp","indef","indefartl","indefL","inp","instl","nm","ns","obj","outp","par","pfx","pkg","phr","pt","reqd","rpl","rt","sfx","std","str","tbl","tbs","tmp","tpl","undmtd","unexp","upd","ws","xls"),
                 long_name_chr = c("length one","abbreviation","argument","article","comma separated variables file","database","dependency","description","development","directory","dataset","documentation","documented","document","developer","file","functions","getter","import","indefinite","indefinite article","indefinite length","input","install","name","namespace","object","output","parameter","prefix","package","phrase","prototype","required","replace","root","suffix","standard","setter","table","tibbles","temporary","template","undocumented","unexported","update","workspace","Excel workbook"),
                 no_plural_chr = c("1L","documentation","documented","temporary","undocumented","unexported"),
                 custom_plural_ls = list(dependency = "dependencies",
                                         directory = "directories",
                                         prefix = c("prefixes"),
                                         suffix = c("suffices","sfcs")),
                 url_1L_chr = "https://ready4-dev.github.io/ready4/",
                 pkg_dss_tb = pkg_dss_tb
                 )
utils::data("abbreviations_lup")
#
# 8. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
pkg_dss_tb <- make_fn_type_lup(fn_type_nm_chr = c("Add", "Assert", "Close", "Force",
                                                    "Get", "Import", "Make", "Read",
                                                    "Remove", "Replace", "Reset", "Rowbind",
                                                    "Transform","Unload", "Update",  "Write"),
                                 fn_type_desc_chr = c("Updates an object by adding data to that object.",
                                                      "Validates that an object conforms to required condition(s). If the object does not meet all required conditions, program execution will be stopped and an error message provided.",
                                                      "Closes specified connections.",
                                                      "Checks if a specified local or global environmental condition is met and if not, updates the specified environment to comply with the condition.",
                                                      "Retrieves a pre-existing data object from memory, local file system or online repository.",
                                                      "Reads a data object in its native format and converts it to an R object.",
                                                      "Creates a new R object.",
                                                      "Reads an R script into memory.",
                                                      "Edits an object, removing a specified element or elements.",
                                                      "Edits an object, replacing a specified element with another specified element.",
                                                      "Edits an object, overwriting the current version with a default version.",
                                                      "Performs custom rowbind operations on table objects.",
                                                      "Edits an object in such a way that core object attributes - e.g. shape, dimensions, elements, type - are altered.",
                                                      "Performs a custom detaching of a package from the search path.",
                                                      "Edits an object, while preserving core object attributes.",
                                                      "Writes a file to a specified local directory."),
                                 first_arg_desc_chr = c("Object to be updated.",
                                                        "Object on which assert validation checks are to be performed.",
                                                        NA_character_,
                                                        NA_character_,
                                                        "Where to look for the required object.",
                                                        NA_character_,
                                                        NA_character_,
                                                        "Path to object.",
                                                        "Object to be updated.",
                                                        "Object to be updated.",
                                                        NA_character_,
                                                        NA_character_,
                                                        "Object to be updated.",
                                                        "Package(s) to be detached from the search path.",
                                                        "Object to be updated.",
                                                        NA_character_),
                                 second_arg_desc_chr = c(NA_character_,
                                                         "Object containing values used for validation tests.",
                                                         NA_character_,
                                                         NA_character_,
                                                         NA_character_,
                                                         NA_character_,
                                                         NA_character_,
                                                         NA_character_,
                                                         "Object to be updated.",
                                                         "Object to be updated.",
                                                         NA_character_,
                                                         NA_character_,
                                                         "Object to be updated.",
                                                         "Package(s) to be detached from the search path.",
                                                         "Object to be updated.",
                                                         NA_character_),
                                 is_generic_lgl = F,
                                 is_method_lgl = F) %>%
write_dmtd_fn_type_lup(url_1L_chr = "https://ready4-dev.github.io/ready4/",
                       pkg_dss_tb = pkg_dss_tb)
utils::data("fn_type_lup_tb")
utils::data("object_type_lup")
pkg_dss_tb <- badges_lup %>%
  write_and_doc_ds(overwrite_1L_lgl = T,
                   db_1L_chr = "badges_lup",
                   title_1L_chr = "ready4 badges lookup table",
                   desc_1L_chr = "A lookup table to identify the appropriate text to insert in README files to represent different types of ready4 badges.",
                   format_1L_chr = "A tibble",
                   url_1L_chr = "https://ready4-dev.github.io/ready4/",
                   abbreviations_lup = abbreviations_lup,
                   object_type_lup = object_type_lup,
                   pkg_dss_tb = pkg_dss_tb)
#
# 9. Create a table of all undocumented functions
fns_dmt_tb <- make_fn_dmt_tbl(fns_path_chr,
                                 fns_dir_chr = fns_dir_1L_chr,
                                 custom_dmt_ls = list(details_ls = NULL,#list(add_indefartls_to_phrases = "TEST DETAILS",close_open_sinks = "ANOTHER TEST"),
                                                      inc_for_main_user_lgl_ls = list(force_true_chr = c("get_from_lup_obj","import_xls_sheets",
                                                                                              "make_dmt_for_all_fns","make_fn_dmt_tbl","make_fn_type_lup","read_fns",
                                                                                              "rowbind_all_tbs_in_r4_obj","write_abbr_lup","write_all_tbs_in_tbs_r4_to_csvs",
                                                                                              "write_and_doc_ds","write_and_doc_fn_fls","write_dmtd_fn_type_lup","write_documented_fns",
                                                                                              "make_lines_for_fn_dmt","write_fn_type_dirs","write_ns_imps_to_desc",
                                                                                              "write_pkg","write_pkg_setup_fls","write_pt_lup_db","write_std_imp",
                                                                                              "write_tb_to_csv","write_to_reset_pkg_files","write_ws"),
                                                                       force_false_chr = NA_character_#c("add_indef_artl_to_item", "get_fn_args_chr")
                                                                       ),
                                                      args_ls_ls = NULL#list(add_indefartls_to_phrases = NA_character_#c(abbreviated_phrase_chr_vec = "TEST_ARG_DESC_1",ignore_phrs_not_in_lup_1L_lgl = "TEST_ARG_DESC_3"))
                                                      ),
                                 append_1L_lgl = T,
                                 fn_type_lup_tb = fn_type_lup_tb,
                                 object_type_lup = object_type_lup,
                                 abbreviations_lup = abbreviations_lup)
pkg_dss_tb <- fns_dmt_tb %>%
  write_and_doc_ds(overwrite_1L_lgl = T,
                   db_1L_chr = "fns_dmt_tb",
                   title_1L_chr = "ready4fun function documentation table",
                   desc_1L_chr = "A table with the summary information on functions included in the ready4fun package.",
                   format_1L_chr = "A tibble",
                   url_1L_chr = "https://ready4-dev.github.io/ready4/",
                   abbreviations_lup = abbreviations_lup,
                   object_type_lup = object_type_lup,
                   pkg_dss_tb = pkg_dss_tb)
# NOTE: To update, make call to update_fns_dmt_tb
#
# 10. Write documented functions to R directory.
## Note files to be rewritten cannot be open in RStudio.
usethis::use_build_ignore("initial_setup.R")
write_and_doc_fn_fls(fns_dmt_tb,
                     r_dir_1L_chr = "R",
                     path_to_dvpr_dmt_dir_1L_chr = "../../../../../Documentation/Code/Developer",
                     path_to_user_dmt_dir_1L_chr = "../../../../../../Documentation/Code/User",
                     dev_pkgs_chr = NA_character_,
                     update_pkgdown_1L_lgl = T)
#

# write_links_for_website(user_manual_url_1L_chr = "https://ready4-dev.github.io/ready4/pdfs/ready4fun_0.0.0.9216.pdf",
#                         developer_manual_url_1L_chr = "https://ready4-dev.github.io/ready4/pdfs/ready4fun_0.0.0.9216_dev.pdf",
#                         project_website_url_1L_chr = "https://ready4-dev.github.io/ready4/")

# 11. Create vignettes
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

