
## first time setup ------------------------------------------------------------

## now load the devtools and install activPAL_v0.3.0

install.packages("devtools", dependencies = TRUE)
library(devtools)
devtools::install_github("Goodgolden/activPAL_v0.3.0", force = TRUE)
## Here are the original repositiory for activPAL
## install_github("PALkitchen/activPAL")

## For first time user, you need to install several packages
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"',
           con = "~/.Renviron")

install_version("devtools",
                version = "2.4.5",
                upgrade = "never",
                dependencies = TRUE)
install_version("dplyr",
                version = "1.1.1",
                upgrade = "never",
                dependencies = TRUE)
install_version("ggplot2",
                version = "3.4.2",
                upgrade = "never",
                dependencies = TRUE)
install_version("gridExtra",
                version = "2.3",
                upgrade = "never",
                dependencies = TRUE)
install_version("lubridate",
                version = "1.9.2",
                upgrade = "never",
                dependencies = TRUE)
install_version("magrittr",
                version = "2.0.3",
                upgrade = "never",
                dependencies = TRUE)
install_version("methods",
                version = "4.2.2",
                upgrade = "never",
                dependencies = TRUE)
install_version("png",
                version = "0.1-8",
                upgrade = "never",
                dependencies = TRUE)
install_version("Rcpp",
                version = "1.0.10",
                upgrade = "never",
                dependencies = TRUE)
install_version("readr",
                version = "2.1.4",
                upgrade = "never",
                dependencies = TRUE)
install_version("tidyr",
                version = "1.3.0",
                upgrade = "never",
                dependencies = TRUE)



'force = TRUE'
