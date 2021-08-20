library(ready4use)
pkg_dv_dir_1L_chr <- "data-raw/dataverse"
if(!dir.exists(pkg_dv_dir_1L_chr))
  dir.create(pkg_dv_dir_1L_chr)
make_obj_lup_spine() %>%
  ready4use::write_paired_ds_fls_to_dv(fl_nm_1L_chr = "seed_obj_lup_tb",
                                       desc_1L_chr = "Seed object type lookup table")
make_obj_lup() %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "object_type_lup",
                            desc_1L_chr = "Object type lookup table")
# get_rds_from_dv("object_type_lup") %>%
# update_abbr_lup(short_name_chr = c("1L","abbr","arg","artl","cnt","csv","db","depcy","depnt","desc","dev","dir","ds","dmt","dmtd","doc","dvpr","fl","fns","gtr","imp","indef","indefartl","indefL","inp","instl","nm","ns","obj","outp","par","pfx","pkg","phr","pt","reqd","rpl","rt","sfx","std","str","tbl","tbs","tmp","tpl","undmtd","unexp","upd","ws","xls"),
#                 long_name_chr = c("length one","abbreviation","argument","article","content","comma separated variables file","database","dependency", "dependent","description","development","directory","dataset","documentation","documented","document","developer","file","functions","getter","import","indefinite","indefinite article","indefinite length","input","install","name","namespace","object","output","parameter","prefix","package","phrase","prototype","required","replace","root","suffix","standard","setter","table","tibbles","temporary","template","undocumented","unexported","update","workspace","Excel workbook"),
#                 no_plural_chr = c("1L","documentation","documented","temporary","undocumented","unexported"),
#                 custom_plural_ls = list(dependency = "dependencies",
#                                         directory = "directories",
#                                         prefix = c("prefixes"),
#                                         suffix = c("suffices","sfcs"))) %>%
#   write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
#                             desc_1L_chr = "Abbreviations lookup table")
get_rds_from_dv("abbreviations_lup") %>%
update_abbr_lup(short_name_chr = c("dv"),
                long_name_chr = c("dataverse"),
                no_plural_chr = NA_character_,
                custom_plural_ls = NULL) %>%
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "abbreviations_lup",
                            desc_1L_chr = "Abbreviations lookup table")
make_fn_type_lup(fn_type_nm_chr = c("Add", "Assert", "Close", "Force",
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
  write_paired_ds_fls_to_dv(fl_nm_1L_chr = "fn_type_lup_tb",
                            desc_1L_chr = "Function type lookup table")
# ADDITIONAL NOTES
##
# To install, deprecated rtools is sought. Users with R>4.0 need the following line in Renviron: PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
#
# WS Set-up
# # Add token to R environ
# Ensure gh-pages branch on repo: https://sahirbhatnagar.com/blog/2020/03/03/creating-a-website-for-your-r-package/
# install.packages('tinytex')
#  tinytex::install_tinytex()
#  tinytex:::install_yihui_pkgs()
# tinytex::tlmgr_install("makeindex")
