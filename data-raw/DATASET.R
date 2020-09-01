# NOTE: To install, deprecated rtools is sought. Users with R>4.0 need the following line in Renviron: PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
#
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
write_pkg_setup_fls_R(#make_tmpl_vignette_lgl = T, First time script is run this should be uncommented then switched off again.
                      incr_ver_1L_lgl = F)
#
# 6. Create a lookup table of abbreviations of R object types and their descriptions and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
make_obj_lup_tb() %>%
  write_and_doc_ds_R(db = .,
                     overwrite_1L_lgl = T,
                     db_1L_chr = "object_type_lup",
                     title_1L_chr = "Object abbreviations lookup table",
                     desc_1L_chr = "A lookup table to identify R object types from an abbreviation that can be used as object name suffices.",
                     format_1L_chr = "A tibble",
                     url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/",
                     abbreviations_lup = .,
                     object_type_lup = .
                     )
#
# 7. Create a lookup table of abbreviations used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
make_abbr_lup_tb(short_name_chr = c("1L","abbr","arg","artl","csv","db","desc","dev","dir","ds","dmt","dmtd","doc","dvpr","fl","fns","gtr","imp","indef","indefartl","indefL","inp","instl","nm","ns","obj","outp","par","pfx","pkg","phr","pt","reqd","rpl","rt","sfx","std","str","tbl","tbs","tmp","tpl","undmtd","unexp","upd","ws","xls"),
                 long_name_chr = c("length one","abbreviation","argument","article","comma separated variables file","database","description","development","directory","dataset","documentation","documented","document","developer","file","functions","getter","import","indefinite","indefinite article","indefinite length","input","install","name","namespace","object","output","parameter","prefix","package","phrase","prototype","required","replace","root","suffix","standard","setter","table","tibbles","temporary","template","undocumented","unexported","update","workspace","Excel workbook"),
                 no_plural_chr = c("1L","documentation","documented","temporary","undocumented","unexported"),
                 custom_plural_ls = list(directory = "directories",
                                         prefix = c("prefixes"),
                                         suffix = c("suffices","sfcs")),
                 url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/"#,
                 #pkg_nm_chr = pkg_nm_chr
                 )
data("abbreviations_lup")
#
# 8. Create a lookup table of function types used in this package and save it as a package dataset (data gets saved in the data directory, documentation script is created in R directory).
make_fn_type_lup_tb(fn_type_nm_chr = c("Add", "Assert", "Close", "Force",
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
make_and_doc_fn_type_R(url_1L_chr = "https://readyforwhatsnext.github.io/readyforwhatsnext/")
data("fn_type_lup_tb")
data("object_type_lup")
#
# 9. Create a table of all undocumented functions
fns_dmt_tb <- make_fn_dmt_tbl_tb(fns_path_chr,
                                 fns_dir_1L_chr = fns_dir_1L_chr,
                                 custom_dmt_ls = list(details_ls = NULL,#list(add_indefartls_to_phrases_1L_chr = "TEST DETAILS",close_open_sinks = "ANOTHER TEST"),
                                                      export_ls = list(force_true_chr = c("add_plurals_to_abbr_lup_tb","close_open_sinks","force_req_pkg_install","get_from_lup_obj","import_xls_sheets_ls",
                                                                                              "make_abbr_lup_tb","make_all_fns_dmt_tb","make_and_doc_fn_type_R","make_fn_dmt_tbl_tb","read_fns",
                                                                                              "rowbind_all_tbs_in_r4_obj_r4","unload_packages","update_ns_chr","write_all_tbs_in_tbs_r4_to_csvs",
                                                                                              "write_and_doc_ds_R","write_and_doc_fn_fls_R","write_documented_fns","write_fn_dmt","write_fn_type_dirs",
                                                                                              "write_ns_imps_to_desc","write_pkg_R","write_pt_lup_db_R","write_std_imp_R",
                                                                                              "write_tb_to_csv","write_ws"),
                                                                       force_false_chr = NA_character_#c("add_indef_artl_to_item_chr", "get_fn_args_chr")
                                                                       ),
                                                      args_ls_ls = NULL#list(add_indefartls_to_phrases_1L_chr = NA_character_#c(abbreviated_phrase_chr_vec = "TEST_ARG_DESC_1",ignore_phrs_not_in_lup_1L_lgl = "TEST_ARG_DESC_3"))
                                                      ),
                                 append_1L_lgl = T,
                                 fn_type_lup_tb = fn_type_lup_tb,
                                 object_type_lup = object_type_lup,
                                 abbreviations_lup = abbreviations_lup)
# NOTE: To update, make call to update_fns_dmt_tb_tb
#
# 10. Write documented functions to R directory.
## Note files to be rewritten cannot be open in RStudio.
write_and_doc_fn_fls_R(fns_dmt_tb,
                       r_dir_1L_chr = "R")
#
# 11. Update Description file with imported packages.
write_ns_imps_to_desc()
#
# 12. Create vignettes
# usethis::use_vignette("ready4fun")
# devtools::document()
# NOTE TO SELF: Currently Vignettes are overwritten by this last step. Need to implement more sophisticated workflow.
# NOTE TO SELF: NEED TO RENAME export_lgl in tables and initial (not subsequent) functions to something like: inc_in_user_dmt_lgl
# NOTE TO SELF: NEED TO ADD WORKFLOW FOR TRANSITIONING FROM PRIVATE TO PUBLIC REPO TO CLENSE ALL PRIVATE COMMIT HISTORY. Variant of: https://gist.github.com/stephenhardy/5470814
# NOTE TO SELF: IDEALLY SHOULD ADD OPTION TO INCREMENT PACKAGE VERSION NUMBER WHEN DESCRIPTION FILE GETS UPDATED. See: https://stackoverflow.com/questions/24209336/automating-version-increase-of-r-packages
# NOTE TO SELF: IN WORKFOW VIGNETTE, INCLUDE LINK TO: https://thenewstack.io/dont-mess-with-the-master-working-with-branches-in-git-and-github/
# and https://github.com/Kunena/Kunena-Forum/wiki/Create-a-new-branch-with-git-and-manage-branches
# and https://www.thegeekstuff.com/2019/03/git-create-dev-branch-and-merge/
# NOTE TO SELF: In vignette, include: https://docs.github.com/en/github/using-git/setting-your-username-in-git
# (plus user.email)

