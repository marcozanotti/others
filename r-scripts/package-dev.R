# Package Development
library(devtools)
library(usethis)
library(tidyverse)
library(fs)

# Create package ---
# 1) create a brand new directory
# 2) create a package on that existing dir with RStudio or
usethis::create_package("../dir_name")

# Setup git & github ---
# 1) create a brand new repo on github with the exact same name
usethis::use_git()
usethis::use_github()

# Setup package ---
usethis::use_roxygen_md()
usethis::use_mit_license("Marco Zanotti")
usethis::use_readme_rmd()
usethis::use_vignette("getting-started", title = "Getting Started with Pkg Name")

usethis::use_pipe()
usethis::use_tidy_eval()
usethis::use_tibble()

# Upate DESCRIPTION ---
# 1) authors
Author@R: c(
  person("Lorenzo", "Mazzucchelli", email = "lorenzo.mazzucchelli@unimi.it", role = "aut"),
  person("Marco", "Zanotti", email = "zanotti.marco17@gmail.com", role = "cre"))
# 2) remove Maintainer
# 3) add URL
URL: https://github.com/marcozanotti/dispositionEffect
# 4) update title
# 5) update description
# 6) update version to a dev version 0.0.0.9000
usethis::use_github_links()

# global-vars.R
globalVariables(
	names = c(".")
)

# Setup CI/CD
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
usethis::use_github_actions_badge()

# Setup unit testing
usethis::use_testthat()
usethis::use_coverage("codecov")

# Setup website
usethis::use_pkgdown()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()


# Git controls
git config --global user.name "Marco Zanotti"
git config --global user.email zanottimarco17@gmail.com
git pull origin main # pull locally remote changes
git pull origin branch_name # create locally a new remote branch
git push --set-upstream origin branch_name # push remotely a locally created branch
git branch -d branch_name # delete branch
git remote prune origin branch_name # locally removes branches that have been removed remotely
