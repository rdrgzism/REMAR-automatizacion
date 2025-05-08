# install.R

# Install remotes if not available
if(!requireNamespace("remotes", quietly = TRUE)){
  install.packages("remotes")
}

# Install cmdstanr from GitHub
remotes::install_github("stan-dev/cmdstanr", dependencies = TRUE)

# Load library
library(cmdstanr)

# Install CmdStan if needed
if(is.null(cmdstanr::cmdstan_path())){
  cat("Installing CmdStan...\n")
  cmdstanr::install_cmdstan()
} else{
  cat("CmdStan already installed at:", cmdstanr::cmdstan_path(), "\n")
}

# Verify installation
check_cmdstan_toolchain(fix = TRUE, quiet = TRUE)
