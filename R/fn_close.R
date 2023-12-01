#' Close open sinks
#' @description close_open_sinks() is a Close function that closes specified connections. Specifically, this function implements an algorithm to close open sinks. The function is called for its side effects and does not return a value.

#' @return No return value, called for side effects.
#' @rdname close_open_sinks
#' @export 
#' @importFrom purrr walk
#' @keywords internal
close_open_sinks <- function () 
{
    purrr::walk(1:length(sink.number()), ~sink(NULL))
}
