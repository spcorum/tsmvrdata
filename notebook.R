library(devtools)
library(roxygen2)
library(usethis)
library(available)
library(covr)
library(spelling)
library(styler)
library(Rdpack)

# One-time setup tasks ------------------------------------------------

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Masters")

pkg_name = "tsmvrextras"

available(pkg_name)
create_package(pkg_name)
use_git()
browse_github_pat(description = "R:tsmvrextras")
token = ''
use_github(protocol = "https", auth_token = token)
usethis::use_github_links()

options( usethis.description = list
    (
        'Title' = 'Extra Features for tsmvr',
        'Version' = '0.0.0.0000',
        'Authors@R' = 'person("Sean", "Corum",
                       email = "spcorum@gmail.com",
                       role = c("aut", "cre"))',
        'Description' = 'This package contains extra features and
                         functions for the Truly Sparse Multivariate
                         Regression (tsmvr) package. These features
                         include (1)  functions for creating and
                         evaluate sparse regression datasets that work
                         will with tsmvr, (2) real datasets pre-processed
                         to work with tsmvr (pre-processing functions
                         included), and (3) functions for testing a
                         pipeline of (a) running the algorithm on
                         synthetic data and then evaluating the results
                         or (b) running the  on real data and evaluating
                         the results.',
        'Language' = 'en-US'
    ),
    usethis.full_name = "Sean Corum"
)

usethis::use_description()
usethis::use_gpl3_license()
# Add to DESCRIPTION --> RdMacros: Rdpack

# Packages to import, depend, include, or suggest ----------------------

usethis::use_dev_package('tsmvr', type = 'Imports')
usethis::use_package('ggplot2', type = 'Imports')
usethis::use_package('AUC', type = 'Imports')
usethis::use_package('MASS', type = 'Imports')
usethis::use_package('igraph', type = 'Imports')
usethis::use_package('pROC', type = 'Imports')
usethis::use_package('Rdpack', type = 'Imports')
usethis::use_package('ggplot2', type = 'Imports')
usethis::use_package('pracma', type = 'Imports')
usethis::use_package('stats', type = 'Imports')
usethis::use_package('Rdpack', type = 'Imports')
usethis::use_package('expm', type = 'Imports')
usethis::use_package('reshape', type = 'Imports')
usethis::use_package('matrixcalc', type = 'Imports')

# Development testing and integration ---------------------------------

usethis::use_readme_rmd()

usethis::use_travis()
usethis::use_appveyor()
usethis::use_coverage(type = c("codecov"))

usethis::use_testthat()
usethis::use_spell_check()
# usethis::use_coverage() ??

# Setup package data --------------------------------------------------

usethis::use_data_raw()
# Add data creation scripts in 'data-raw/'
# Use `usethis::use_data()` to add data to package


# Function development cycle ------------------------------------------

usethis::use_r('rbern')
roxygen2::roxygenize()
devtools::spell_check()
# Restart R: Cmd+Shift+F10
# Build and reload: Cmd+Shift+B
usethis::use_test('rbern')
devtools::test_file('R/rbern.R')
devtools::test()
devtools::check(args = "--as-cran")
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Commit at command line
usethis::use_tidy_style()

usethis::use_r('mvrnorm')
roxygen2::roxygenize()
devtools::spell_check()
# Restart R: Cmd+Shift+F10
# Build and reload: Cmd+Shift+B
usethis::use_test('mvrnorm')
devtools::test_file('R/mvrnorm.R')
devtools::test()
devtools::check(args = "--as-cran")
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Commit at command line
usethis::use_tidy_style()

usethis::use_r('mvrnorm')
roxygen2::roxygenize()
devtools::spell_check()
# Restart R: Cmd+Shift+F10
# Build and reload: Cmd+Shift+B
usethis::use_test('covar_ar1')
devtools::test_file('R/covar_ar1.R')
devtools::test()
devtools::check(args = "--as-cran")
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Commit at command line
usethis::use_tidy_style()

usethis::use_r('covar_fgn')
roxygen2::roxygenize()
devtools::spell_check()
# Restart R: Cmd+Shift+F10
# Build and reload: Cmd+Shift+B
usethis::use_test('covar_fgn')
devtools::test_file('R/covar_fgn.R')
devtools::test()
devtools::check(args = "--as-cran")
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Commit at command line
usethis::use_tidy_style()

usethis::use_r('precision_sfn')
roxygen2::roxygenize()
devtools::spell_check()
# Restart R: Cmd+Shift+F10
# Build and reload: Cmd+Shift+B
usethis::use_test('precision_sfn')
devtools::test_file('R/precision_sfn.R')
devtools::test()
devtools::check(args = "--as-cran")
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Commit at command line
usethis::use_tidy_style()

usethis::use_r('covariance_matrix')
usethis::use_r('regressor_matrix')
usethis::use_r('make_data')
usethis::use_r('model_error')
usethis::use_r('evaluate_model')
usethis::use_r('error_curve')
usethis::use_r('plot_error_curve')



# Commit cycle --------------------------------------------------------

# Restart: Cmd+Shift+F10
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Check: Cmd+Shift+E
# Optional: to update development version ..
usethis::use_dev_version()
# Add, commit, and push in command line


# Development version release cycle -----------------------------------

# Restart: Cmd+Shift+F10
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Check: Cmd+Shift+E
# Update development version
usethis::use_version('dev')
# Knit README.Rmd
# Add, commit, and push in command line


# Version release cycle -------------------------------------------

# Restart: Cmd+Shift+F10
# Document Cmd+Shift+D
roxygen2::roxygenize()
# Check: Cmd+Shift+E
# Update version ..
wch = 'pathc'
wch = 'minor'
wch = 'minor'
usethis::use_version(which=wch)
# Knit README.Rmd
# Add, commit, and push in command line

# which = A string specifying which level to increment, one of: 
# "major", "minor", "patch", "dev". If NULL, user can choose 
# interactively.


# Tasks ---------------------------------------------------------------
# 
# 0. tsmvrextras: test
# 1. tsmvrextras: find, preprocess, and add real data (4-9 more)
# 2. tsmvr: re-constuct tsmvr in full development environment
# 3. tsmvr: test
# 4. tsmvrextras: test
# 5. tsmvr / tsmvrextras: vingette(s)
# 6. Paper: write
# 7. Paper: experiments
# 8. Meet with advisor
# 9. Revise