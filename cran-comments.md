# Second Submission to CRAN, version 0.1.0
The following changes have been made in response to CRAN's comments:

- Reworded the description so that it does not start with "Functions for"

- Write less information to the console by changing `print()` statements to `warning()` or making sure 
they are encompassed by an `if verbose()` block. Also, `diagnostic_plot()` and `diagnostic_plot_hazard()` 
functions were altered to produce fewer internal warnings and/or to not write internal ggplot warnings to the console.

The suggestion to add references was optional; we have not done so.

### test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD Check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Maria Osborne <mariaosborne@berkeley.edu>'
  
  New submission

> On fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✓ | 0 warnings ✓ | 2 notes x

### Notes
* The note about HTML validation only occurs on fedora-clang-devel and does not seem critical. The HTML version of the manual is able to be validated on windows-x86_64-devel.



# Reply from CRAN for first submission
- Please do not start the description with "This package", package name, title or
"Functions for".

- If there are references describing the methods in your package, please 
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for 
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

- You write information messages to the console that cannot be easily 
suppressed. It is more R like to generate objects that can be used to 
extract the information a user is interested in, and then print() that 
object.
Instead of print() rather use message()/warning()  or if(verbose)cat(..) 
(or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)


# First Submission to CRAN, version 0.1.0

The first submission of the `gompertztrunc` package

### Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

### R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  
  Maintainer: 'Maria Osborne <mariaosborne@berkeley.edu>'
  New submission

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✓ | 0 warnings ✓ | 2 notes x

Notes:
* The 'lastMiKTeXException' note only occurs when build is checked on windows-x86_64-devel. After searching online, it seems that this note is possibly due to a MiKTeX bug during checking and can probably be ignored.

