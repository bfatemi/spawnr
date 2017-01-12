library(httr)
url <- 'https://api.github.com/user/keys?access_token='
ptoke <- '084369bccf2977ee0041c62da91218ed2fbb9cf9'
pk <- readLines('~/.ssh/authorized_keys')
print(pk)
pknam <- paste0('server_', Sys.Date(), '_', ceiling(runif(1, 1, 1000)))
POST(paste0(url, ptoke), body = list(title = pknam, key = pk), encode = 'json')
