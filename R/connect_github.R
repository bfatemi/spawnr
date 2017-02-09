#' Connect Server to Github
#'
#' Function to connect to github via personal access token, explicitly
#' provided or through use of environmental variable \code{GITHUB_PAT}.
#'
#' @param ptoken An optional personal access token.
#' If not provided, will default to system var GITHUB_PAT
#'
#' @return Boolean
#'
#' @importFrom stats runif
#' @import httr
#' @export
connect_github <- function(ptoken=NULL){
  if(is.null(ptoken))
    ptoken <- "6d0fb3a8768dc35bf91b6c9fc0a1a44e99816bf7"#runlock::gh_token()

  pubkey <- get_pubkey(choice = 1)

  # Make github connection and place public key on gh account
  api <- 'https://api.github.com/user/keys?access_token='
  pknam <- paste0('server_', Sys.Date(), '_', ceiling(runif(1, 1, 1000)))
  url <- paste0(api, ptoken)
  r <- httr::POST(url, body = list(title = pknam, key = pubkey), encode = 'json')

  if(httr::status_code(r) == 422){
    warning("key already in use")
    return(TRUE)
  }
  stop_for_status(r)
  return(TRUE)
}

