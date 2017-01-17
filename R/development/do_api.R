


library(httr)
library(data.table)
library(stringr)

float1 <-"45.55.115.231"
float2 <-"45.55.115.232"
float3 <-"45.55.115.233"

do_token <- "bce90a45df77f6dddc5f442ceda3071c85b6c54270d3356221716ffc81e7f00b"

#### NEXT LINE WORKS
oauth_listener("https://cloud.digitalocean.com/v1/oauth/authorize?client_id=b21b8a4ec482d79ea76b6979d28d789f496af015b463b17be3cbcd30b4d237c0&redirect_uri=http://localhost:1410&response_type=code")



url <- "https://api.digitalocean.com/v2/floating_ips"
GET(url = )
api <- "/[REPLACE_WITH_IP]/actions"

## construct api call
##
tmp <- str_replace(paste0(url, api), "\\[REPLACE_WITH_IP\\]", float1)
ll_url <- parse_url(tmp)


h1 <- content_type("application/json")

do_app <- oauth_app("spawnr", key = "b21b8a4ec482d79ea76b6979d28d789f496af015b463b17be3cbcd30b4d237c0", secret = "70e1f09242d7c53ed6b743100f47714495c75e0ac8b6844e9ee6c8d20525176b")

do_oauth_ep <- oauth_endpoint(request = "https://cloud.digitalocean.com/v1",
                              authorize = "https://cloud.digitalocean.com/v1/oauth/authorize",
                              NULL)


# httr::add_headers(
#   bearer = do_token,
#   redirect_uri="http://localhost:1410",
#   response_type="code"
# )




oauth2.0_token(do_oauth_ep, do_app, cache = FALSE)
chk <- ?client_id= &

oauth_
oauth_header(do_token)
oauth1.0_token(do_oauth_ep, do_app)
list(redirect_uri="http://localhost:1410", response_type="code")

/?code=16e2c23030157e33faae747a5a34ecc3664600f9dfdab468d6f47a71b74def10&state=LJSdZsWzo2
GET(chk)

h2 <- oauth2.0_token()
list(type = "")
add_headers()


curl -X POST
-H "Content-Type: application/json"
-H "Authorization: Bearer b7d03a6947b217efb6f3ec3bd3504582"
-d '{"type":"assign","droplet_id":8219222}'

