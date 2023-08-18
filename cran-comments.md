## Purpose of release

This patch addresses errors in automated CRAN package checks for the
r-devel-linux-x86_64-debian-clang platform.

The official CRAN check results indicated a segmentation fault during package
unit testing. We were not able to reproduce the precise error, but identified 
ancillary issues that may be related. We have updated our tests to address
these errors and confirmed that R CMD check passes on R-hub's debian-clang-devel
platform (Debian clang version 14.0.6).

CRAN check results also indicate unknown test failures for 
r-devel-windows-x86_64 and r-release-windows-x86_64 platforms. We are not able
to reproduce these issues locally. Checks on similar platforms via R-hub are 
also successful.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

1 package (Ecfun) lists ipumsr in the 'Suggests' field. This package
uses ipumsr in one of its vignettes, but the code used in that
vignette is not impacted by this release. We did not detect any problems
running this vignette with the current version of ipumsr.
