# Working Log for Randy

1. Getting the fork v0.3.0 activPAL//

2. Down-grading ggplot2 to lower than 3.4.0 to avoid the warning//

3. Adding the code files into `01_utils.R` and `02_Rcpp.R`
  - 3.1 `01_utils.R` is for the functions that are used in the main code
  - 3.2 `02_Rcpp.R` is for the Rcpp functions
  - 3.3 several function in `02_Rcpp.R` is over-writting the package code
  - 3.4 deletion of the `walk.test.R` file
    - `activpal.stepping.process.file()`
    - `activpal.remove.longer.bouts()`
    - `activpal.stepping.test.file()`
    - `activpal.stepping.test.day()`

4. Adding new functions for Macbook
  - 4.1 `~.macbook` and `~.windows` functions are added into `01_utils.R`
  Here are functions modified:
    - `activpal.process.folder()`
    - `activpal.process.file()`
    - `prepare.ex.times()`
    - `make.index.file()`
    - `individual.chart.overlay()`
    - `apSummary()`
  - 4.2 change the directory style for the folder **need to talk with Kat**
  - 4.3 add new test for the new functions to work

5. Update the DESCRIPTION for different versions of packages
  - 5.1 also write the install.package with specific version in 01_setup.Rmd
  - 5.2 change the directory into Randy's github repository

6. Update the documents with new NAMESPACE and README.md
  - 6.1 something need to learn about Rcpp

```
useDynLib(activPAL)
exportPattern("ˆ[[:alpha:]]+")
importFrom(Rcpp, evalCpp)
```

7. Meet up with Kat on Monday to check the work on PC
  - 7.1 probably need to add **.R** files, if not familiar with Rmarkdown 
