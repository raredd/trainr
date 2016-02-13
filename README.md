Better practices for statistical comupting in r

20 May 2015

View the [slides](http://htmlpreview.github.io/?https://github.com/raredd/trainr/blob/master/inst/pres/pres.html#1)

- Use the arrow keys or click to the left/right of the slides to navigate.

- Press "o" to see an overview of the slides, "w" to toggle widescreen, "f" for fullscreen

- Press "p" to show presenter notes, "h" to enable code highlight mode


[Download a zip](https://github.com/raredd/trainr/archive/master.zip) file of this repo or clone to your computer:

```shell
mkdir trainr
cd trainr
git clone https://github.com/raredd/trainr.git
```

This repo includes an r package. For information on building r packages, see Hadley's [intro](http://r-pkgs.had.co.nz/intro.html).

To get started, you should download and install [rstudio](http://www.rstudio.com/products/rstudio/download/) and also a few r packages.

```r
install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
```

(I have just learned that rstudio also takes care of the compiler and command line tools you will need to build r packages, but if you are *not* using rstudio, you will need to install either [Rtools](http://cran.r-project.org/bin/windows/Rtools) for windows machines, [xcode + command line tools](http://developer.apple.com/downloads) for macintosh (you will also need to sign up for a [free] apple id), or install the development tools package, `r-base-dev`, for [linux distros](http://cran.r-project.org/bin/linux/).)

After installing rstudio and the four packages, run the following in r to see if you are ready to build packages (this function should return `TRUE`):

```r
devtools::has_devel()
```

Finally, open `trainr.Rproj` to get started.