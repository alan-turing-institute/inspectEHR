# Install this package and its dependencies for C.I. testing
install.packages("remotes")
remotes::install_github("DocEd/commonEHR")
remotes::install_github("DocEd/wranglEHR")
remotes::install_local(".", dependencies=TRUE)
