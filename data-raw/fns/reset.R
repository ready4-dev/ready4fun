write_to_reset_pkg_files <- function(package_1L_chr,
                              package_dir_1L_chr = getwd(),
                              description_ls = NULL,
                              keep_version_lgl = T){
  devtools::load_all()
  if(keep_version_lgl){
    desc_ls <- packageDescription(package_1L_chr)
    description_ls$Version <- desc_ls$Version
  }
  usethis::use_description(fields = description_ls)
  file.remove(paste0(package_dir_1L_chr,"/NAMESPACE"))
  if(file.exists(paste0(package_dir_1L_chr,"/R/db_pt_lup.R")))
    file.remove(paste0(package_dir_1L_chr,"/R/db_pt_lup.R"))
  devtools::document()
  devtools::load_all()
}
