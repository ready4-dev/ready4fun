library(ready4)
library(ready4use)
X <- Ready4useRepos(gh_repo_1L_chr = "ready4-dev/ready4",
                    gh_tag_1L_chr = "Documentation_0.0")
Y <- ingest(X)
# Only needed occationally....
Z <- Ready4useRepos(dv_nm_1L_chr = "ready4fw",
                    dv_server_1L_chr = "dataverse.harvard.edu",
                    dv_ds_nm_1L_chr = "https://doi.org/10.7910/DVN/RIQTKK") %>%
  ingest()
abbreviations_lup <- Y@b_Ready4useIngest@objects_ls$abbreviations_lup %>%
  dplyr::filter(!long_name_chr %>% startsWith("ready4 S4 collection of")) %>%
  dplyr::mutate(long_name_chr = dplyr::case_when(short_name_chr == "r3" ~ "ready4 submodule",
                                                 short_name_chr == "r4" ~ "ready4 module",
                                                 T ~ long_name_chr)) %>%
  dplyr::mutate(long_name_chr = long_name_chr %>% purrr::map_chr(~stringr::str_replace_all(.x,"ready4 S3","ready4 submodule"))) %>%
  dplyr::mutate(long_name_chr = long_name_chr %>% purrr::map_chr(~stringr::str_replace_all(.x,"ready4 S4","ready4 module")))
object_type_lup <- Y@b_Ready4useIngest@objects_ls$object_type_lup %>%
  dplyr::filter(!long_name_chr %>% startsWith("ready4 S4 collection of")) %>%
  dplyr::mutate(long_name_chr = dplyr::case_when(short_name_chr == "r3" ~ "ready4 submodule",
                                                 short_name_chr == "r4" ~ "ready4 module",
                                                 T ~ long_name_chr)) %>%
  dplyr::mutate(long_name_chr = long_name_chr %>% purrr::map_chr(~stringr::str_replace_all(.x,"ready4 S3","ready4 submodule"))) %>%
  dplyr::mutate(long_name_chr = long_name_chr %>% purrr::map_chr(~stringr::str_replace_all(.x,"ready4 S4","ready4 module")))
#seed_obj_type_tb <- Y@b_Ready4useIngest@objects_ls$seed_obj_type_tb
seed_obj_type_lup <- Y@b_Ready4useIngest@objects_ls$seed_obj_type_lup %>%
  dplyr::mutate(long_name_chr = dplyr::case_when(short_name_chr == "r3" ~ "ready4 submodule",
                                                 short_name_chr == "r4" ~ "ready4 module",
                                                 T ~ long_name_chr))
# fn_types_lup <- Y@b_Ready4useIngest@objects_ls$fn_types_lup
# fn_types_lup$first_arg_desc_chr <- fn_types_lup$second_arg_desc_chr <- NA_character_
# fn_types_lup <- fn_types_lup %>%
#   dplyr::arrange(fn_type_nm_chr) %>%
#   dplyr::mutate(fn_type_desc_chr = fn_type_desc_chr %>% paste0(".")) %>%
#   dplyr::filter(fn_type_nm_chr != "Extract") %>%
#   dplyr::filter(!fn_type_nm_chr %in% (fn_types_lup %>% dplyr::filter(!is_generic_lgl & stringr::str_detect(fn_type_nm_chr, pattern = ' ')) %>% dplyr::pull(fn_type_nm_chr)))
  # dplyr::mutate(fn_type_desc_chr = dplyr::case_when(fn_type_nm_chr == "Add" ~ "Updates an object by adding new values to new or empty fields",
  #                                                   fn_type_nm_chr == "Extract" ~ "Extracts data from a field (Deprecated naming convention - use get instead)",
  #                                                   fn_type_nm_chr == "Format" ~ "Modifies the format of an object",
  #                                                   fn_type_nm_chr == "Get" ~ "Extracts data from an object",
  #                                                   fn_type_nm_chr == "Knit" ~ "Knits an RMD or Rmarkdown file",
  #                                                   fn_type_nm_chr == "Launch" ~ "Launches an R Shiny app",
  #                                                   fn_type_nm_chr == "Make Dataverse Import Lookup Table" ~ "Makes a Dataverse import lookup table",
  #                                                   fn_type_nm_chr == "Plot" ~ "Plots data",
  #                                                   fn_type_nm_chr == "Predict" ~ "Applies a model to make predictions",
  #                                                   fn_type_nm_chr == "Print" ~ "Prints output to console",
  #                                                   fn_type_nm_chr == "Rename" ~ "Renames elements of an object based on a pre-specified schema",
  #                                                   T ~ fn_type_desc_chr))
#fn_types_lup$fn_type_desc_chr <- sub("[.]$", "", fn_types_lup$fn_type_desc_chr)
Y <- renewSlot(Y,
               new_val_xx = Ready4useIngest(objects_ls = list(abbreviations_lup = abbreviations_lup,
                                                              object_type_lup = object_type_lup,
                                                              seed_obj_type_lup = seed_obj_type_lup)),
               slot_nm_1L_chr = "b_Ready4useIngest")
Y <- share(Y,
           type_1L_chr = "prefer_gh")

## Note files to be rewritten cannot be open in RStudio.

## Empty dataverse dataset must be published first.
#
# manifest_ls <- fns_env_ls$fns_env$write_new_abbrs(manifest_ls,
#                                                    #classes_to_make_tb = classes_to_make_tb,
#                                                    long_name_chr = c("ready4fun R package"),
#                                                    #custom_plural_ls = list(repository = "repositories"),
#                                                    no_plural_chr = c("ready4fun R package"))
# manifest_ls <- fns_env_ls$fns_env$write_new_fn_types(manifest_ls,
#                                                      fn_type_desc_chr = c("Executes an algorithm for solving forward problems through simulation or prediction."),
#                                                      is_generic_lgl = T,
#                                                      publish_dv_1L_lgl = T)
manifest_ls <- fns_env_ls$fns_env$update_msng_abbrs(manifest_ls,
  are_words_chr = c("zenodo")
)
manifest_ls <- fns_env_ls$fns_env$write_new_words_vec(manifest_ls)
manifest_ls <- fns_env_ls$fns_env$update_pkg_setup_msgs(manifest_ls,
  list_element_1L_chr = "missing_abbrs_chr"
)
# manifest_ls <- fns_env_ls$fns_env$update_msng_abbrs(manifest_ls,
#                                                      are_words_chr = c("cran", "lifecycle", "pdf",
#                                                                        "pdfs","pkgdown",
#                                                                        "R", "rds", "ready4", "url", "urls"),
#                                                      tf_to_singular_chr = c(dest = "dests",
#                                                                             dupl = "dupls",
#                                                                             msg = "msgs"))
# manifest_ls <- fns_env_ls$fns_env$write_new_abbrs(manifest_ls,
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
