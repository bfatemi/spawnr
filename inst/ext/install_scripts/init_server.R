##
## This script will be called by cloud-init file to initialize the R environment
## on this newly launched server.
##
## Notes:
##    - Only linux. Need to implement file writing for windows
##    - This script is written to the server by cloud-init directive, then is called
##      using Rscript --vanilla by a bash script that was also written by cloud-init
##    - This script will install all dependancies (packages) that are required for successive lines
##
##
##********** INSTALL PACKAGES **********##
##
## default packages
tryCatch({
  pkgs <- c('data.table')
  rep <- 'http://cran.rstudio.com/'
  do.call(install.packages, list(pkgs, .Library, repos=rep))
}, error = function(c){
  stop("\n\nError installing default packages... ", c$message)
})
##
## Install my github packages
##   - Note that spawnr installs a copy of itself on the server
tryCatch({
  pkgs.gh <- c('bfatemi/ninjar', 'bfatemi/easydata', 'bfatemi/spawnr')
  lapply(pkgs.gh, devtools::install_github)
}, error = function(c){
  stop("\n\nError installing github packages... ", c$message)
})
##
##********** LOAD PACKAGES THEN CONTINUE **********##
##
library(data.table)
library(devtools)
library(httr)
library(stringr)
library(spawnr)
bres <- TRUE
##
## Set default packages
##
pkgs <- c("data.table", "stringr")
tryCatch({
  bres <<- set_default_pkgs(pkgs)
}, error = function(c){
  stop("set_default_pkgs failed (did not return boolean as expected)...Here is the error: ", c$message)
})
if(bres == FALSE)
  stop("set_default_pkgs ran with no errors but returned FALSE. Stopping execution")
##
##
## Add public key to github
##
tryCatch({
  bres <<- connect_github()
}, error = function(c){
  stop("connect_github failed (did not return boolean as expected)... Here is the error: ", c$message)
})
if(bres == FALSE)
  stop("connect_github ran with no errors but returned FALSE. Stopping execution")
##
## NEXT??
##

