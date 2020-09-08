#' Rowbind all tibbles in readyforwhatsnext S4 object
#' @description rowbind_all_tbs_in_r4_obj() is a Rowbind function that performs custom rowbind operations on table objects. Specifically, this function implements an algorithm to rowbind all tibbles in readyforwhatsnext s4 object.The function returns a tibbles (a readyforwhatsnext s4).
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param second_tbs_r4 Second tibbles (a readyforwhatsnext S4)
#' @param r4_name_1L_chr Readyforwhatsnext S4 name (a character vector of length one)
#' @return Tibbles (a readyforwhatsnext S4)
#' @rdname rowbind_all_tbs_in_r4_obj
#' @export 
#' @importFrom purrr reduce
#' @importFrom methods getSlots
rowbind_all_tbs_in_r4_obj <- function (tbs_r4, second_tbs_r4, r4_name_1L_chr) 
{
    tbs_r4 <- purrr::reduce(methods::getSlots(r4_name_1L_chr) %>% 
        names(), .init = tbs_r4, ~rowbind_tbs_in_r4_obj(tbs_r4 = .x, 
        slot_nm_1L_chr = .y, second_tbs_r4 = second_tbs_r4, r4_name_1L_chr = r4_name_1L_chr))
    return(tbs_r4)
}
#' Rowbind tibbles in readyforwhatsnext S4 object
#' @description rowbind_tbs_in_r4_obj() is a Rowbind function that performs custom rowbind operations on table objects. Specifically, this function implements an algorithm to rowbind tibbles in readyforwhatsnext s4 object.The function returns a tibbles (a readyforwhatsnext s4).
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param slot_nm_1L_chr Slot name (a character vector of length one)
#' @param second_tbs_r4 Second tibbles (a readyforwhatsnext S4)
#' @param r4_name_1L_chr Readyforwhatsnext S4 name (a character vector of length one)
#' @return Tibbles (a readyforwhatsnext S4)
#' @rdname rowbind_tbs_in_r4_obj
#' @export 
#' @importFrom tibble is_tibble
#' @importFrom methods slot
rowbind_tbs_in_r4_obj <- function (tbs_r4, slot_nm_1L_chr, second_tbs_r4, r4_name_1L_chr) 
{
    if (tibble::is_tibble(methods::slot(tbs_r4, slot_nm_1L_chr))) {
        slot(tbs_r4, slot_nm_1L_chr) <- rbind(methods::slot(tbs_r4, 
            slot_nm_1L_chr), methods::slot(second_tbs_r4, slot_nm_1L_chr))
    }
    return(tbs_r4)
}
