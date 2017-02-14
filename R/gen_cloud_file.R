#' Generate Cloud-Config for Parallel R
#'
#' Functions to generate the cloud-config file that is used to launch single/multi cluster R nodes in the cloud.
#'
#' @describeIn gen_cloud_config for complete RStudio server environment and opencpu server.
#' @export
cc_r <- function() {
    cc <- CLOUDCONFIG$new()
    cc$WRITE_FILES <- WRITE_FILES$new(system.file("ext", "copy_to_server", "only_r.sh", package = "spawnr"))
    cc$POWER_STATE <- NULL
    cc$RUNCMD$add_cmd("chmod +x /only_r.sh")
    cc$RUNCMD$add_cmd("sudo /only_r.sh")
    fpath <- cc$make_file()
    return(fpath)
}

#' @describeIn gen_cloud_config for complete RStudio server environment and opencpu server.
#' @export
cc_rstudio_server <- function() {
    cc <- CLOUDCONFIG$new()
    pat <- runlock::gh_token()
    txt <- paste0("echo 'GITHUB_PAT=", pat, "' >> /etc/environment")
    do.call(cc$BOOTCMD$add, list(txt))
    cc$BOOTCMD$add("echo 'R_PROFILE=/usr/lib/R/etc/.Rprofile' >> /etc/environment")
    cc$WRITE_FILES <- WRITE_FILES$new(system.file("ext", "copy_to_server", "setup_rstudio_ocpu.sh", package = "spawnr"))
    fpath <- system.file("ext", "install_scripts", "init_server.R", package = "spawnr")
    cc$WRITE_FILES$add_server_file(fpath)
    cc$PACKAGES$add("apache2")
    cc$PACKAGES$add("build-essential")
    cc$PACKAGES$add("libxml2-dev")
    cc$PACKAGES$add("libcurl4-openssl-dev")
    cc$PACKAGES$add("libprotobuf-dev")
    cc$PACKAGES$add("protobuf-compiler")
    cc$RUNCMD$add_cmd("echo \"deb http://cran.rstudio.com/bin/linux/ubuntu xenial/\" | sudo tee -a /etc/apt/sources.list")
    cc$RUNCMD$add_cmd("gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9")
    cc$RUNCMD$add_cmd("gpg -a --export E084DAB9 | sudo apt-key add -")
    cc$RUNCMD$add_cmd("sudo add-apt-repository -y ppa:opencpu/opencpu-1.6")
    cc$RUNCMD$add_cmd("sudo apt-get update -y")
    cc$RUNCMD$add_cmd("sudo apt-get install -y r-base r-base-dev libssl-dev libcurl4-gnutls-dev libssh2-1-dev")
    cc$RUNCMD$add_cmd("chmod +x /setup_rstudio_ocpu.sh")
    cc$RUNCMD$add_cmd("sudo /setup_rstudio_ocpu.sh")
    cc$RUNCMD$add_cmd("Rscript --vanilla /init_server.R > R_init_server_log")
    fpath <- cc$make_file()
    return(fpath)
}
