#' Connect Server to Github
#'
#' Function to connect to github via personal access token, explicitly
#' provided or through use of environmental variable \code{GITHUB_PAT}.
#'
#' @param personal_token An optional personal access token.
#' If not provided, will default to system var GITHUB_PAT
#'
#' @return Boolean
#'
#' @importFrom stats runif
#' @import httr
#' @export
connect_github <- function(personal_token=NULL){
  if(is.null(personal_token)){
    personal_token <- Sys.getenv("GITHUB_PAT")
    if(personal_token == "")
      return(FALSE)
  }
  api <- 'https://api.github.com/user/keys?access_token='
  pk <- readLines('~/.ssh/authorized_keys')
  pknam <- paste0('server_', Sys.Date(), '_', ceiling(runif(1, 1, 1000)))
  url <- paste0(api, personal_token)
  r <- POST(url, body = list(title = pknam, key = pk), encode = 'json')

  if(http_error(r))
    return(FALSE)
  return(TRUE)
}
