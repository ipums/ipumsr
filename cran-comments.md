## Purpose of release

This patch addresses errors in automated CRAN package checks for the following
platforms:

- r-devel-linux-x86_64-fedora-clang
- r-devel-linux-x86_64-fedora-gcc

We recently submitted two patches (v0.6.1 and v0.6.2) in
an attempt to address similar check errors on other platforms. These patches
were successful in resolving the errors occurring on these platforms, but did
not resolve the issues on fedora. 

While we have localized the source of the test failure that is resulting 
in check errors on the fedora platforms, we are unable
to reproduce this failure locally or on any other similar platforms.

The test failure involved non-critical features that were slated for 
possible deprecation. Therefore, we have proceeded with the deprecation 
process and updated our tests accordingly in an effort to resolve the issue.

We can confirm that the version being submitted passes R CMD check locally on
Windows and MacOS, as well as on the following remote platforms:

- debian-clang-devel (checked via R-hub)
- ubuntu-gcc-release (checked via R-hub)
- fedora-clang-devel (checked via R-hub)
- fedora-gcc-devel (checked via R-hub)
- windows-x86_64-devel (checked via R-hub and win-builder)
- windows-x86_64-release (checked via win-builder)

Thanks again for your patience as we attempt to resolve these issues.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

1 package (Ecfun) lists ipumsr in the 'Suggests' field. This package
uses ipumsr in one of its vignettes, but the code used in that
vignette is not impacted by this release. We did not detect any problems
running this vignette with the current version of ipumsr.
