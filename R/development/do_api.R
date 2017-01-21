library(jsonlite)
library(httr)
library(data.table)
library(stringr)

float1 <-"45.55.115.231"
float2 <-"45.55.115.232"
float3 <-"45.55.115.233"


# #### NEXT LINE WORKS
# URL_auth <- "https://cloud.digitalocean.com/v1/oauth/authorize"
# URL_token <- "https://cloud.digitalocean.com/v1/oauth/token?grant_type=authorization_code"
#
#
# ## First request the authorization code
# ##
# add_params <- list(redirect_uri=callback_URL,
#                    response_type="code",
#                    client_id="b21b8a4ec482d79ea76b6979d28d789f496af015b463b17be3cbcd30b4d237c0")
#
# ll <- parse_url(URL_auth)
# ll$query <- add_params
#
# auth_code <- oauth_listener(build_url(ll))


## Now use the auth code to receive the access token
##
# lla <- parse_url(URL_token)
# lla$query <- c(lla$query, auth_code, do_app, list(redirect_uri=callback_URL))
#
# oa_eoauth_endpoint(NULL, authorize = URL_auth, URL_token)
# oauth_listener(build_url(lla))
# oauth_exchanger(build_url(lla))
#
# POST "https://cloud.digitalocean.com/v1/oauth/token?grant_type=authorization_code
#   &code=f252c4bd6b1b4d249b7
#   &client_id=4c413ac36ac22268
#   &client_secret=b05a2ad77b24f3
#   &redirect_uri=https://example.com/callback"
# code <- oauth_listener("?client_id=b21b8a4ec482d79ea76b6979d28d789f496af015b463b17be3cbcd30b4d237c0&redirect_uri=http://localhost:1410&response_type=code")



url <- "https://api.digitalocean.com/v2"
GET(url = url, httr::add_headers(code = as.character(code)))
api <- "/[REPLACE_WITH_IP]/actions"

## construct api call
##
tmp <- str_replace(paste0(url, api), "\\[REPLACE_WITH_IP\\]", float1)
ll_url <- parse_url(tmp)


h1 <- content_type("application/json")






# do_token <- function(){
#   callback_URL <- "http://localhost:1410"
#
#   do_app <- oauth_app("spawnr",
#                       key = "b21b8a4ec482d79ea76b6979d28d789f496af015b463b17be3cbcd30b4d237c0",
#                       secret = "70e1f09242d7c53ed6b743100f47714495c75e0ac8b6844e9ee6c8d20525176b")
#
#   do_oauth_ep <- oauth_endpoint(request = NULL,
#                                 "https://cloud.digitalocean.com/v1/oauth/authorize",
#                                 "https://cloud.digitalocean.com/v1/oauth/token")
#
#   do_oaheads <- httr::add_headers(
#     redirect_uri=callback_URL,
#     response_type="code",
#     grant_type="authorization_code"
#   )
#
#   token <- oauth2.0_token(do_oauth_ep, do_app)
#   return(token)
# }
#
# do_api <- function(url = NULL, token=NULL){
#   base <- "https://api.digitalocean.com/v2"
#   nurl <- paste0(base, url)
#   r <- GET(nurl, token)
#   if(http_error(r))
#     stop("http error code received: ", r$status_code)
#   parsed <- fromJSON(content(r, "text"), simplifyVector = FALSE)
#   return(parsed)
# }
#
#
#
# do_droplets <- function(){
#   drop_url <- "/droplets"
# }
#
# do_floating_ip <- function(){
#   float_url <- "/floating_ips"
# }



