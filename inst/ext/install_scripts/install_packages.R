pkgs <- c('data.table')
rep <- 'http://cran.rstudio.com/'
do.call(install.packages, list(pkgs, .Library, repos=rep))
