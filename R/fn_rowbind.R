#' Rowbind all tibbles in readyforwhatsnext S4 object
#' @description rowbind_all_tbs_in_r4_obj_r4() is a Rowbind function that performs custom rowbind operations on table objects. Specifically, this function implements an algorithm to rowbind all a tibbles in a readyforwhatsnext S4 object.The function returns a tibbles (a readyforwhatsnext s4).
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param second_tbs_r4 Second tibbles (a readyforwhatsnext S4)
#' @param r4_name_chr Readyforwhatsnext S4 name (a character vector of length 1)
#' @return Tibbles (a readyforwhatsnext S4)
#' @rdname rowbind_all_tbs_in_r4_obj_r4
#' @export 
#' @importFrom purrr reduce
#' @importFrom methods getSlots
rowbind_all_tbs_in_r4_obj_r4 <- function (tbs_r4, second_tbs_r4, r4_name_chr) 
{
    tbs_r4 <- purrr::reduce(methods::getSlots(r4_name_chr) %>% 
        names(), .init = tbs_r4, ~rowbind_tbs_in_r4_obj_r4(tbs_r4 = .x, 
        slot_nm_chr = .y, second_tbs_r4 = second_tbs_r4, r4_name_chr = r4_name_chr))
    return(tbs_r4)
}
#' Rowbind tibbles in readyforwhatsnext S4 object
#' @description rowbind_tbs_in_r4_obj_r4() is a Rowbind function that performs custom rowbind operations on table objects. Specifically, this function implements an algorithm to rowbind a tibbles in a readyforwhatsnext S4 object.The function returns a tibbles (a readyforwhatsnext s4).
#' @param tbs_r4 Tibbles (a readyforwhatsnext S4)
#' @param slot_nm_chr Slot name (a character vector of length 1)
#' @param second_tbs_r4 Second tibbles (a readyforwhatsnext S4)
#' @param r4_name_chr Readyforwhatsnext S4 name (a character vector of length 1)
#' @return Tibbles (a readyforwhatsnext S4)
#' @rdname rowbind_tbs_in_r4_obj_r4
#' @export 
#' @importFrom tibble is_tibble
#' @importFrom methods slot
#' @keywords internal
rowbind_tbs_in_r4_obj_r4 <- function (tbs_r4, slot_nm_chr, second_tbs_r4, r4_name_chr) 
{
    if (tibble::is_tibble(methods::slot(tbs_r4, slot_nm_chr))) {
        slot(tbs_r4, slot_nm_chr) <- rbind(methods::slot(tbs_r4, 
            slot_nm_chr), methods::slot(second_tbs_r4, slot_nm_chr))
    }
    return(tbs_r4)
}
