# Getting help with restrictedROC

Thanks for using restrictedROC!
Before filing an issue, there are a few places to explore and pieces to put together to make the process as smooth as possible.

## Make a reprex

Start by making a minimal **repr**oducible **ex**ample using the  [reprex](https://reprex.tidyverse.org/) package. 
If you haven't heard of or used reprex before, you're in for a treat! 
Seriously, reprex will make all of your R-question-asking endeavors easier (which is a pretty insane ROI for the five to ten minutes it'll take you to learn what it's all about). 
For additional reprex pointers, check out the [Get help!](https://www.tidyverse.org/help/) section of the tidyverse site.

## Where to ask?

Armed with your reprex, the next step is to figure out [where to ask](https://www.tidyverse.org/help/#where-to-ask). 

*   If it's a question: start with [community.rstudio.com](https://community.rstudio.com/), and/or StackOverflow. There are more people there to answer questions.  

*   If it's a bug: you're in the right place, [file an issue](https://github.com/ggrlab/restrictedROC/issues/new).  
  
*   If you're not sure: let the community help you figure it out! 
    If your problem _is_ a bug or a feature request, you can easily return here and report it. 

Before opening a new issue, be sure to [search issues and pull requests](https://github.com/ggrlab/restrictedROC/issues) to make sure the bug hasn't been reported and/or already fixed in the development version. 
By default, the search will be pre-populated with `is:issue is:open`. 
You can [edit the qualifiers](https://help.github.com/articles/searching-issues-and-pull-requests/)  (e.g. `is:pr`, `is:closed`) as needed. 
For example, you'd simply remove `is:open` to search _all_ issues in the repo, open or closed.

## What happens next?

To be as efficient as possible, development of tidyverse packages tends to be very bursty, so you shouldn't worry if you don't get an immediate response.
Typically we don't look at a repo until a sufficient quantity of issues accumulates, then there’s a burst of intense activity as we focus our efforts. 
That makes development more efficient because it avoids expensive context switching between problems, at the cost of taking longer to get back to you. 
This process makes a good reprex particularly important because it might be multiple months between your initial report and when we start working on it. 
If we can’t reproduce the bug, we can’t fix it!

# Specific problems and solutions

## ``devtools::build_vignettes`` fails

```bash
--- re-building 'first_function.Rmd' using rmarkdown
Error: processing vignette 'first_function.Rmd' failed with diagnostics:
unused argument (resolve_symlink = FALSE)
--- failed re-building 'first_function.Rmd'

SUMMARY: processing the following file failed:
  'first_function.Rmd'
```

Solution: 

```r
install.packages("xfun")
```


## Vignette xfun::isFalse() will be deprecated

What:    ``devtools::check()`` throws the following error:

```bash
> checking re-building of vignette outputs ... ERROR
  Error(s) in re-building vignettes:
    ...
  --- re-building 'first_function.Rmd' using rmarkdown
  Quitting from lines 11-15 (first_function.Rmd) 
  Error: processing vignette 'first_function.Rmd' failed with diagnostics:
  The function xfun::isFALSE() will be deprecated in the future. Please consider using base::isFALSE(x) or identical(x, FALSE) instead.
  --- failed re-building 'first_function.Rmd'
  
  SUMMARY: processing the following file failed:
    'first_function.Rmd'
  
  Error: Vignette re-building failed.
  Execution halted
```
    
Solution: See https://stackoverflow.com/questions/76081732/problem-when-converting-r-file-to-r-markdown

```r
install.packages("knitr")
```
