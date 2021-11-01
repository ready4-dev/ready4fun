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
                                                     are_words_chr = c("zenodo"))
manifest_ls <- fns_env_ls$fns_env$write_new_words_vec(manifest_ls)
manifest_ls <- fns_env_ls$fns_env$update_pkg_setup_msgs(manifest_ls,
                                                        list_element_1L_chr = "missing_abbrs_chr")
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
