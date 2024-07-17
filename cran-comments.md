## Reason for back-to-back releases

After we released ipumsr 0.8.0, a user notified us via GitHub of an error when 
checking our package with the `--no-build-vignettes` option (see
[ipumsr issue #80](https://github.com/ipums/ipumsr/issues/80)). This same issue 
seems to be affecting our oldrel-macos 
[CRAN checks](https://cran.rstudio.org/web/checks/check_results_ipumsr.html), so 
we want to release a patch to CRAN to fix the error.

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

We checked 1 reverse dependencies, comparing R CMD check results across CRAN and dev versions of this package.

 * We saw 0 new problems
 * We failed to check 0 packages
