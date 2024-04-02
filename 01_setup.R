
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"',
           con = "~/.Renviron")

## Now load the devtools and install activPAL_v0.3.0


install.packages("devtools", dependencies = TRUE)

devtools::install_github("Goodgolden/activPAL_v0.3.0",
                         dependencies = TRUE,
                         upgrade = "always",
                         force = TRUE)

'force = TRUE'








