# First Submission to CRAN, version 0.1.0

The first submission of the gompertztrunc package

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  
  Maintainer: 'Maria Osborne <mariaosborne@berkeley.edu>'
  New submission

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✓ | 0 warnings ✓ | 2 notes x

## Explanation of Check Results

The 'lastMiKTeXException' note only occurs when build is checked on windows-x86_64-devel. As noted in [rhub issue 503](https://github.com/r-hub/rhub/issues/503), it can likely be ignored.


