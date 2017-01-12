pkgs <- c('httr', 'data.table', 'devtools')
rep <- 'http://cran.rstudio.com/'
do.call(install.packages, list(pkgs, .Library, repos=rep))
