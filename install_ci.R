# Install this package and its dependencies for C.I. testing
install.packages("remotes")
remotes::install_github("DocEd/commonEHR", force=FALSE)
remotes::install_github("DocEd/wranglEHR", force=FALSE)
remotes::install_local(".", dependencies=TRUE, force=FALSE)
