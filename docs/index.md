# Prerequisite
It is recommended that you have the latest version of the R environment installed.
You need to install devtools and load it into the R environment, and then you can install circlizePlus from Github.
```R
if (!requireNamespace("devtools", quietly=TRUE))
    install.packages("devtools")
install_github("TianzeLab/circlizePlus")
```

Every time you reboot the R environment, you need to load circlizePlus again.

`library(circlizePlus)`