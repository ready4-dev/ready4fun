import_xls_sheets <- function(range_1L_chr,
                              sheet_names_chr,
                              path_1L_chr) {
  tb_ls <- purrr::map(
    sheet_names_chr,
    ~ readxl::read_excel(
      path = path_1L_chr,
      sheet = .x,
      range = range_1L_chr,
      col_names = TRUE
    )
  ) %>%
    stats::setNames(sheet_names_chr)
  return(tb_ls)
}
