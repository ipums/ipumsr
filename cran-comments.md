## Purpose of release
This release adds functions to the interact with the new IPUMS API.

## Test environments
* local Windows, R 4.2.0
* win builder release and devel
* Windows: R 3.6 and release via GitHub Actions
* Linux: R 3.5, oldrel, and release via GitHub Actions
* MacOS: R release and devel via GitHub Actions

## R CMD check results

0 errors | 0 warnings | 0 notes

## Note from devtools::check_rhub()

Got the following Note when checking on rhub:

N  checking for detritus in the temp directory
   Found the following files/directories:
     'lastMiKTeXException'

It seems that this might be a bug or crash in miktex, based on this GitHub 
issue: https://github.com/r-hub/rhub/issues/503

## Reverse dependencies
1 package lists ipumsr in the 'Suggests' field, but does not actually use any
functionality from this package.
