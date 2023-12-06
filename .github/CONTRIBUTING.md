# Contributing to restrictedROC

This outlines how to propose a change to restrictedROC.
For a detailed discussion on contributing to this and other tidyverse packages, please see the [development contributing guide](https://rstd.io/tidy-contrib) and our [code review principles](https://code-review.tidyverse.org/).

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See our guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("ggrlab/restrictedROC", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header). Follow the style described in <https://style.tidyverse.org/news.html>.

```r
# Use pre-commits to check your code before committing it
remotes::install_github("lorenzwalthert/precommit")
precommit::install_precommit()
precommit::use_precommit()

# Increase the versions, manually or by using usethis::use_version()
usethis::use_version("minor")
usethis::use_version("dev")

# Add new functionality and document it
# During development, have a clean R environment and run devtools::load_all() to load the current status of the package
devtools::load_all()

# After adding new functionality, run devtools::check() to update the documentation
devtools::check()
devtools::document()

# Vignettes are a great way to document your package
# Add a new vignette by running
usethis::use_vignette("vignette_name")
# Change the vignette in vignettes/vignette_name.Rmd
# Build the vignette by running
devtools::build_vignettes() # This also installs the package

# Articles
# Instead of a vignette, you can create an article, which is a term used by
# pkgdown for a vignette-like .Rmd document that is not shipped with the package,
# but that appears only in the website.
usethis::use_article("article_name")


devtools::build_readme() # This updates the README.md file from the README.Rmd


# Further arguments of devtools::build_site() are forwarded to pkgdown::build_site():
# https://pkgdown.r-lib.org/reference/build_site.html
devtools::build_site()
devtools::build_site(devel = TRUE, lazy = TRUE) # Use this for faster iteration during development

```

- Disable pre-commit for a single commit: ``git commit . -m 'quick fix' --no-verify``

### Code style

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

## Code of Conduct

Please note that the restrictedROC project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this
project you agree to abide by its terms.




## How was this package created?

```r
# for VSCode
install.packages("languageserver")
install.packages("devtools")
usethis::create_tidy_package("/home/gugl/clonedgit/ggrlab/restrictedROC")
usethis::proj_activate("/home/gugl/clonedgit/ggrlab/restrictedROC")
usethis::use_tidy_style(strict = TRUE)
usethis::use_git()
```


``usethis`` tells you to envoke further github-related commands. 
There is two ways to continue: 
1. Create a personal access token (PAT) and use it to authenticate with github
2. Manually push the package to github 

Pushing manually works fine, but some advanced ``usethis`` commands won't work properly, 
therefore I will continue with the PAT.

```r
usethis::create_github_token()
gitcreds::gitcreds_set() # Then enter the freshly generated token
usethis::use_github(
    organisation = "ggrlab",
    private = FALSE,
    visibility = "public"
)
```
WARNING!!!
If an error occurs because e.g. the repository exists already at github, use the following instead of 
``usethis::use_github()``:
```bash
git branch -M main_devel
git remote add origin git@github.com:ggrlab/restrictedROC.git
git checkout main
git merge main_devel --allow-unrelated-histories
git push -u origin main
git branch --delete main_devel
```

```r
usethis::use_tidy_github()
usethis::use_tidy_github_actions()
# overwrite tidy's default "check-full" with "check-standard"
# to not run so many checks
usethis::use_github_action("check-standard")
usethis::use_tidy_github_labels()
usethis::use_pkgdown_github_pages()
```


Additional information: 
```r
usethis::use_author(
    given = "Gunther",
    family = "Glehr",
    email = "gunthergl@gmx.net",
    role = c("aut", "cre"),
    comment = c("ORCID" = "0000-0002-1495-9162")
)
usethis::use_lifecycle()
usethis::use_news_md()
lintr::use_lintr(type = "tidyverse")
# Change manually to:
# linters: linters_with_defaults(line_length_linter = line_length_linter(120),indentation_linter = indentation_linter(4)) # see vignette("lintr")
# encoding: "UTF-8"
```
precommit is a wonderful tool to check your code before committing it. 

```r
# https://lorenzwalthert.github.io/precommit/articles/precommit.html
# install.packages("precommit")
# bash::$ conda deactivate
# bash::$ pip3 install pre-commit
precommit::install_precommit()
precommit::use_precommit()
```
Before committing: ``pre-commit install --hook-type pre-push``, then commit. 

Used packages:
```r
usethis::use_package("ggplot2")
usethis::use_package("patchwork")
usethis::use_package("pROC")
usethis::use_package("ggpubr")
usethis::use_package("stats")
usethis::use_package("tibble")
usethis::use_package("dplyr")
usethis::use_package("labeling")
usethis::use_package("tidyr")
usethis::use_package("statmod")
usethis::use_package("future.apply")
usethis::use_package("readxl", type = "Suggests")
usethis::use_package("h2o", type = "Suggests")
usethis::use_package("data.table", type = "Suggests")
precommit::snippet_generate("additional-deps-roxygenize")
```

