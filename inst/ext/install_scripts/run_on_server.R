library(httr)

connect2github <- function(personal_token=NULL){
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

connect2github()



