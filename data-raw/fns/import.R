import_xls_sheets_ls <- function(range_chr,
                                   sheet_names_chr_vec,
                                   path_chr){
  tb_ls <-purrr::map(sheet_names_chr_vec,
                     ~  readxl::read_excel(path = path_chr,
                                           sheet = .x,
                                           range = range_chr,
                                           col_names = TRUE)) %>%
    stats::setNames(sheet_names_chr_vec)
  return(tb_ls)
}


