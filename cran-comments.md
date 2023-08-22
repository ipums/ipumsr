## Purpose of release

This patch addresses errors in automated CRAN package checks for the following
platforms:

- r-devel-linux-x86_64-debian-gcc
- r-devel-linux-x86_64-fedora-clang
- r-devel-windows-x86_64
- r-release-windows-x86_64

The official CRAN check results indicated unidentified failures during
unit testing on these platforms. We recently submitted a patch (v0.6.1) in
an attempt to address these failures, which was unsuccessful.

Since that submission, we have made more progress identifying the source of the
errors and have updated our unit test code accordingly. This version passes 
R CMD check on the following platforms:

- debian-clang-devel (checked via R-hub)
- debian-gcc-devel (checked via R-hub)
- ubuntu-gcc-release (checked via R-hub)
- fedora-clang-devel (checked via R-hub)
- windows-x86_64-devel (checked via R-hub and win-builder)
- windows-x86_64-release (checked via win-builder)

Thanks for your patience given multiple releases in a short time frame.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

1 package (Ecfun) lists ipumsr in the 'Suggests' field. This package
uses ipumsr in one of its vignettes, but the code used in that
vignette is not impacted by this release. We did not detect any problems
running this vignette with the current version of ipumsr.
