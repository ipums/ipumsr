# Contributing
Thank you for considering improving this project! By participating, you
agree to abide by the [code of conduct](https://tech.popdata.org/ipumsr/CODE_OF_CONDUCT.html).

# Issues (Reporting a problem or suggestion)
If you've experience a problem with the package, or have a suggestion for it, 
please post it on the [issues tab](https://github.com/ipums/ipumsr/issues).
This space is meant for questions directly related to the R package, so questions
related to your specific extract may be better answered via email to ipums@umn.edu
(but don't worry about making a mistake, we know it is tough to tell the difference). 

Since our extracts are such large files, posting minimal reproducible examples
may be difficult. Therefore, it will be most helpful if you can provide as 
much detail about your problem as possible including the code and error message,
the project the extract is from, the variables you have selected, file type, etc. 
We'll do our best to answer your question.

# Pull Requests (Making changes to the package)
We appreciate pull requests that follow these guidelines:

1) Make sure that tests pass (and add new ones if possible). 

2) Do your best to conform to the code style of the package, currently
based on the [tidyverse style guide](http://style.tidyverse.org/). See the
[styler](https://styler.r-lib.org/) package to easily catch stylistic errors.

3) Please add you name and affiliation to the NOTICE.txt file.

4) Summarize your changes in the NEWS.md file.

## Basics of Pull Requests
If you've never worked on an R package before, the book 
[R Packages by Hadley Wickham](https://r-pkgs.had.co.nz) is a great 
resource for learning the mechanics of building an R package and contributing 
to R packages on github. Additionally, here's a 
[great primer on git and github specifically](http://happygitwithr.com/).

In the meantime, here's a quick step-by-step guide on 
contributing to this project using RStudio:

1) If don't already have RStudio and Git installed, you can download them [here](https://www.rstudio.com/products/rstudio/download/) and [here](https://git-scm.com/downloads).

2) Fork this repo (top right corner button on the github website).

3) Clone the repo from RStudio's toolbar: 
`File > New Project > From Verson Control > https://github.com/*YOUR_USER_NAME*/ipumsr/`.

4) Make changes to your local copy. 

5) Commit your changes and push them to the github webiste using RStudio's Git pane 
(push using the green up arrow). 

6) [Submit a pull request](https://github.com/ipums/ipumsr/compare/), selecting 
the "compare across forks" option. Please include a short message summarizing your
changes.
