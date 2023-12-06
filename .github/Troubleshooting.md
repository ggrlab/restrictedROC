# Troubleshooting restrictedROC

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
