#' Import Excel workbook sheets
#' @description import_xls_sheets_ls() is an Import function that reads a data object in its native format and converts it to an R object. Specifically, this function implements an algorithm to an import Excel workbook sheets. The function returns a tibble list (a list of tibbles).
#' @param range_chr Range (a character vector of length 1)
#' @param sheet_names_chr_vec Sheet names (a character vector)
#' @param path_chr Path (a character vector of length 1)
#' @return Tibble list (a list of tibbles)
#' @rdname import_xls_sheets_ls
#' @export 
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom stats setNames
import_xls_sheets_ls <- function (range_chr, sheet_names_chr_vec, path_chr) 
{
    tb_ls <- purrr::map(sheet_names_chr_vec, ~readxl::read_excel(path = path_chr, 
        sheet = .x, range = range_chr, col_names = TRUE)) %>% 
        stats::setNames(sheet_names_chr_vec)
    return(tb_ls)
}
