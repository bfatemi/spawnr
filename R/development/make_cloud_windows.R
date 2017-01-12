make_cloud_init <- function(pubkey=NULL, username="ruser"){
  if(is.null(pubkey)){
    path <- paste0(Sys.getenv("HOMEDRIVE"), Sys.getenv("HOMEPATH"), "\\.ssh\\id_rsa.pub")
    tryCatch({
      pubkey <- readLines(path)
    }, error=function(c){
      stop("Issue reading public key. Check path and if wrong, provide pubkey via arg:\n", path)
    })
  }

  ## Construct cloud init file
  ##
  tmp <- readLines("inst/ext/template.yml")
  tb <- "  "

  # username
  ind <- which(str_detect(tmp, paste0(tb, "- name:")))
  tmp[ind] <- paste0(tmp[ind], " ", username)

  # public key
  ind <- which(str_detect(tmp, "ssh-authorized-keys:"))
  tmp2 <- c(tmp[1:(ind-1)],
            c(tmp[ind], paste0(tb, tb,tb, "- ", pubkey)),
            tmp[(ind+1):length(tmp)])

  # packages
  packs <- c("apache2",
             "build-essential",
             "libxml2-dev",
             "libcurl4-openssl-dev")
  ind <- which(str_detect(tmp2, "packages:"))
  tmp3 <- c(tmp2[1:(ind-1)],
            c("packages:", paste0(paste0(tb, "- ", collapse = ""), packs)),
            tmp2[(ind+1):length(tmp2)])

  # write files
  ind <- which(str_detect(tmp3, "write_files:"))
  ind.block <- c(ind, ind+1, ind+2)

  dpath <- "inst/ext/install_scripts/"
  fname_a <- "silent-install-ocpu.sh"
  fname_b <- "inst_standard_packs.R"
  fname_c <- "post_pubkey.R"
  file.a <- readLines(paste0(dpath, fname_a))
  file.b <- readLines(paste0(dpath, fname_b))
  file.c <- readLines(paste0(dpath, fname_c))

  k <- paste0(tb, tb, tb, collapse = "")

  block_a <- c(paste0(tmp3[ind + 1], " /", fname_a), c(tmp3[ind + 2], paste0(k, file.a)))
  block_b <- c(paste0(tmp3[ind + 1], " /", fname_b), c(tmp3[ind + 2], paste0(k, file.b)))
  block_c <- c(paste0(tmp3[ind + 1], " /", fname_c), c(tmp3[ind + 2], paste0(k, file.c)))

  all_blocks <- c(tmp3[ind], block_a, block_b, block_c)
  tmp4 <- c(tmp3[1:(ind-1)], all_blocks, tmp3[(ind.block[length(ind.block)] + 1):length(tmp3)])

  # Ensure there is a final new line
  if(tmp4[length(tmp4)] != "")
    tmp4 <- c(tmp4, "")
  cloud_file <- c(tmp4[-length(tmp4)],
                  paste0(tb, "- Rscript --vanilla /inst_standard_packs.R > ins_standard_packs_log"),
                  paste0(tb, "- Rscript --vanilla /post_pubkey.R > post_pubkey_log"), "\n")
  writeLines(cloud_file)
}

