#' glue_message
#' @importFrom glue glue
#' @param string A string
#' @export glue_message
glue_message <- function(string, ...) message(glue(string, ...))
