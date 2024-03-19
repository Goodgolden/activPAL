
## first time setup ------------------------------------------------------------

## For first time user, you need to install several packages
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"',
           con = "~/.Renviron")

install.packages("devtools", dependencies = TRUE)
install.packages("Rcpp", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)


## now load the devtools and install activPAL_v0.3.0
library(devtools)
devtools::install_github("Goodgolden/activPAL_v0.3.0")
## Here are the original repositiory for activPAL
## install_github("PALkitchen/activPAL")

'force = TRUE'
