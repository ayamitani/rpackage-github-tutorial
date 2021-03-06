## How to write your own R package and upload it to GitHub

Slides are available at https://ayamitani.github.io/rpackage-github-tutorial/

### Abstract
R package development is an important skill to have as reproducibility and accessibility of results from scientific research are highly valued.
For this week's seminar, I will show the basic steps to develop an R package using the devtools package. 
I will also present on how to make your package available for download from your GitHub account. 
If you want to follow along, please make sure you have the following three things set up on your computer.
1. Download R Studio (https://rstudio.com/)
2. Download these packages
```
install.packages(c("devtools", "roxygen2", "usethis", "here", "available"))
```
3. Download Git (https://git-scm.com/)
4. Create a GitHub account (https://github.com/) 
If this is your first time creating a GitHub account, here are a few tips on choosing your username. 
https://happygitwithr.com/github-acct.html
5. Download Rtools package (https://cran.r-project.org/bin/windows/Rtools/)
Not necessary if you stick to base R and add-on packages on CRAN
Good idea to install it eventually
