close_open_sinks <- function() {
  purrr::walk(
    1:length(sink.number()),
    ~ sink(NULL)
  )
}
