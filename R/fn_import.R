#' Import Excel workbook sheets
#' @description import_xls_sheets() is an Import function that reads a data object in its native format and converts it to an R object. Specifically, this function implements an algorithm to import excel workbook sheets.The function returns a tibble list list of tibbles.
#' @param range_1L_chr Range (a character vector of length one)
#' @param sheet_names_chr Sheet names (a character vector)
#' @param path_1L_chr Path (a character vector of length one)
#' @return Tibble list (a list of tibbles)
#' @rdname import_xls_sheets
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom stats setNames
import_xls_sheets <- function (range_1L_chr, sheet_names_chr, path_1L_chr) 
{
    tb_ls <- purrr::map(sheet_names_chr, ~readxl::read_excel(path = path_1L_chr, 
        sheet = .x, range = range_1L_chr, col_names = TRUE)) %>% 
        stats::setNames(sheet_names_chr)
    return(tb_ls)
}
