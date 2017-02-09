new_lines <- c(
  '#!/bin/bash',
  'sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list',
  'gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9',
  'gpg -a --export E084DAB9 | sudo apt-key add -',
  'sudo apt-get update -y',
  'sudo apt-get install -y r-base-dev'
)
writeLines(new_lines, "inst/ext/copy_to_server/only_r.sh")
