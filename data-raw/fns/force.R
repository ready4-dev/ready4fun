force_instl_of_reqd_pkg <- function(package_nm_1L_chr) {
  if (!package_nm_1L_chr %in% utils::installed.packages()) {
    utils::install.packages(package_nm_1L_chr,
      Sys.getenv("R_LIBS_USER"),
      repos = "https://cran.ms.unimelb.edu.au/"
    )
  }
  library(package_nm_1L_chr,
    character.only = TRUE
  )
}
