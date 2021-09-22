author <- function(x,
                    ...){
  UseMethod("author",x)
}
# methods::setGeneric("author")
# extract <- function(x,
#                 ...){
#   UseMethod("extract",x)
# }
make <- function(x,
                 ...){
  UseMethod("make",x)
}
update <- function(x,
                  ...){
  UseMethod("update",x)
}
validate <- function(x,
                     ...){
  UseMethod("validate",x)
}
