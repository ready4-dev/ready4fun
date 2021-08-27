## Note files to be rewritten cannot be open in RStudio.
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
#fns_env <- new.env(parent = baseenv())
ee <- new.env(parent = globalenv())
source(paste0(fns_dir_1L_chr,"/read.R"))
fns_env_ls <- read_fns(fns_dir_1L_chr,
                       fns_env = ee)
rm(read_fns)
#
# 5. Create code house-style datasets.
# Every time the datasets that describe key features of the code house-style to be used require creation
# or updating, make the necessary changes in MAKE_HOUSESTYLE_DV_DSS.R and uncomment and run the following line.
# source("data-raw/MAKE_HOUSESTYLE_DV_DSS.R")
# 6. Set-up package structure
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
###
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
                    ready4_type_1L_chr = "authoring",
                    user_manual_fns_chr = c("get_from_lup_obj","get_rds_from_dv",
                                            "make_dmt_for_all_fns",
                                            "make_fn_type_lup", "make_lines_for_fn_dmt",
                                            "write_abbr_lup", "write_and_doc_ds",
                                            "write_and_doc_fn_fls","write_dmtd_fn_type_lup",
                                            "write_documented_fns", "write_fn_type_dirs",
                                            "write_links_for_website", "write_pkg_setup_fls",
                                            "write_pt_lup_db", "write_ws"))
pkg_ds_ls_ls <- list(fns_env_ls$fns_env$get_rds_from_dv("object_type_lup") %>% # NB: PROBLEM WITH PKG DESC FILE
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
####
rlang::exec(fns_env_ls$fns_env$write_pkg_setup_fls, !!!pkg_setup_ls$initial_ls)
dss_records_ls <- fns_env_ls$fns_env$write_pkg_dss(pkg_ds_ls_ls,
                                fns_to_incl_chr = pkg_setup_ls$user_manual_fns_chr,
                                pkg_url_1L_chr = pkg_desc_ls$URL %>%
                                  strsplit(",") %>%
                                  unlist() %>%
                                  purrr::pluck(1))
fns_env_ls$fns_env$add_build_ignore(pkg_setup_ls$subsequent_ls$build_ignore_ls)
fns_env_ls$fns_env$add_addl_pkgs(pkg_setup_ls$subsequent_ls$addl_pkgs_ls)
## Add path to dmt dir and create user and dvpr subdirs
fns_env_ls$fns_env$write_and_doc_fn_fls(fns_dmt_tb = dss_records_ls$fns_dmt_tb,
                     dev_pkgs_chr = pkg_setup_ls$subsequent_ls$dev_pkgs_chr,
                     path_to_dmt_dir_1L_chr =  "../../../../../Documentation/Code",
                     r_dir_1L_chr = paste0(pkg_setup_ls$initial_ls$path_to_pkg_rt_1L_chr,"/R"),
                     update_pkgdown_1L_lgl = T)
## Add manuals to DV
project_url_1L_chr <- pkg_desc_ls$URL %>%
  strsplit(",") %>%
  unlist() %>%
  purrr::pluck(3)
if(is.null(project_url_1L_chr))
  project_url_1L_chr <-  NA_character_
write_links_for_website(user_manual_url_1L_chr = "https://github.com/ready4-dev/ready4fun/releases/download/v0.0.0.9289/ready4fun_user_0.0.0.9289.pdf",
                        developer_manual_url_1L_chr = "https://github.com/ready4-dev/ready4fun/releases/download/v0.0.0.9289/ready4fun_developer_0.0.0.9289.pdf",
                        project_website_url_1L_chr = project_url_1L_chr)

# 12. Create vignettes
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

