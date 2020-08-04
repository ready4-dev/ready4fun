reset_pkg_files_R <- function(package_chr,
                              package_dir = getwd(),
                              description_ls = NULL,
                              keep_version_lgl = T){
  devtools::load_all()
  if(keep_version_lgl){
    desc_ls <- packageDescription(package_chr)
    description_ls$Version <- desc_ls$Version
  }
  usethis::use_description(fields = description_ls)
  file.remove(paste0(package_dir,"/NAMESPACE"))
  if(file.exists(paste0(package_dir,"/R/db_pt_lup.R")))
    file.remove(paste0(package_dir,"/R/db_pt_lup.R"))
  devtools::document()
  devtools::load_all()
}
