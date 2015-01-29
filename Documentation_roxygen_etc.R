

git push -f origin master

library("roxygen2")
roxygenise()

library("devtools")
#dev_example("spca")

package_version("knitr")
packageVersion("knitr")
missing_s3()


roxygen2 
devtools::missing_s3()
roxygen2 v 4.0.2
devtools 1.6.1
Error in roxygen2::is_s3_method(x, envir = loaded$env) : 
  unused argument (envir = loaded$env)


devtools::release(check = FALSE)
11devtools::build_win()
document(".")

devtools::check(".", cleanup = FALSE)#, args = "--no-manual")

devtools::build(args = "--no-manual")

#========  TRAVIS ===================================
devtools::use_travis()

http://docs.travis-ci.com/user/getting-started/

Step four: Trigger Your First Build With a Git Push #

Once GitHub hook is set up, push your commit that adds .travis.yml to your repository. That should add a build into one of the queues on Travis CI and your build will start as soon as one worker for your language is available.

To start a build, perform one of the following:
  
  Commit and push something to your repository
# Go to your repository's settings page, click on "Webhooks & Services" on the left menu, choose "Travis CI" in the "Services", and use the "Test service" button.
# Please note that you cannot trigger your first build using Test Hook button. It has to be triggered by a push to your repository.

to push commits with overwritten changes, use shell, push -f
#========  END TRAVIS ===================================

devtools::check_doc(pkg = ".")
getwd()
setwd("..")
setwd("spca")
system("R CMD Rd2pdf spca") #

tools::compactPDF("\\inst", gs_cmd = "C:\\Program Files\\gs\\gs9.06\\bin\\gswin64c.exe")
Sys.which(Sys.getenv("R_QPDF", "qpdf"))
Sys.getenv("R_GSCMD", "")
f = "Vignettes\\spca_Vignettes.Rnw"
vi_r = knitr::purl(f, "vignettes\\spca_Vignettes.R")
vi_r

devtools::use_package("formatR", "Suggests")
devtools::use_package("ggplot2", "Suggests")
devtools::use_package("reshape2", "Suggests")
spca:::plotcor(bsbl_avg)


remove.packages(pkgs, lib)


devtools::build_vignettes()
devtools::clean_vignettes()

rmarkdown::render("vignettes\\Advanced_Example.Rmd", clean = FALSE)#, encoding = encoding)
rmarkdown::render("README.Rmd", clean = FALSE)#, encoding = encoding)

rmarkdown::render("..\\spca2\\readme.Rmd", clean = FALSE)#, encoding = encoding)

## missing devtools::use_readme_rmd()

##D:\\Dropbox\\Papers\\Rotate PC\\R_SPCA\\spca\\man\\
utils:::.getHelpFile("qplot")
help_console(optim, "html", lines = 1:25, before = "<blockquote>", after = "</blockquote>")
help(help)
help(spin, help_type = "text")

Stangle(f)

vignette('printr', package = 'printr')
library(printr)
library("devtools")
# 

run_examples(".", run = FALSE)#, fresh = TRUE)

getwd()

options(device = "windows")
options(device ="RStudioGD")

#spcabb = function(x) print("stoocazzone")

ftype(compare)
ftype(compare.spca)



rd.file = system.file("examples", "reformat_code_demo.Rd", package = "Rd2roxygen")
file.copy(rd.file, tempdir())
fmt.file = file.path(tempdir(), "reformat_code_demo.Rd")

file.show(fmt.file)  ## show the raw Rd

reformat_code(fmt.file)
file.show(fmt.file)  ## the formatted Rd


getwd()
library("roxygen2", lib.loc="~/R/win-library/3.2")
# Rd2roxy just one file

aa = create_roxygen(parse_file("man\\spcabe.rd"))
paste(aa)
length(aa)
aa[1:4]
cat(aa, file = "tmp\\spcabe_onlyroxy.txt", sep = "\n")

# THIS CONVERTS RD TO ROXYGEN FORMAT
# rd2roxy

getwd()
od = setwd("..")
#Rd2roxygen(file.path("."), nomatch, usage = TRUE)

library("Rd2roxygen", lib.loc="~/R/win-library/3.2")
Rd2roxygen("spca")
rab(".")

library(spca)
#### DEV TOOLS
this is from devtools
library("devtools")


devtools::build_win()

check_doc(pkg = ".")


build_vignettes(pkg = ".")
clean_vignettes(pkg = ".")
  ##this checks like cran

check(".")


packageVersion("spca")
=== printr HELP TO LATEX

install.packages(
  'printr',
  type = 'source',
  repos = c('http://yihui.name/xran')
)

vignette('printr', package = 'printr')

##################################
THIS FOR TIDYING UP OUTPUT

library(formatR)
## evaluate simple code as a character vector
tidy_eval(text = c("a<-1+1;a", "matrix(rnorm(10),5)"))

## evaluate a file
tidy_eval(file.path(system.file(package = "stats"), "demo", "nlm.R"))


library(formatR)

## a messy R script
messy = system.file("format", "messy.R", package = "formatR")
tidy_source(messy)

## use the 'text' argument
src = readLines(messy)

## source code
cat(src, sep = "\n")

## the formatted version
tidy_source(text = src)

## preserve blank lines
tidy_source(text = src, blank = TRUE)

## indent with 2 spaces
tidy_source(text = src, indent = 2)

## discard comments!
tidy_source(text = src, comment = FALSE)

## wanna see the gory truth??
tidy_source(text = src, output = FALSE)$text.mask

#######

THIS IS FOR GETTING HELP FILES IN KNITR 

install.packages(
  'printr',
  type = 'source',
  repos = c('http://yihui.name/xran')#, 'http://cran.rstudio.com')
)

THIS IS FOR HELP TO LATEX
Rd <- file.path("man/spcabe.Rd")

outfile <- file.path(filetext = "spcabe.TeX", package = "spca")


Rd2latex(Rd, outfile)

WORKS BUT DON'T KNOW WHICH PACKAGE UNDERSTANDS IT'

###########################

file.show("pkgDemo/R/foo.R")  # what happened to foo.R and bar.R?



"R_HOME"

.First <- function(){
  library(spca)
  cat("\nWelcome at", date(), "\n") 
}

options()$defaultPackages

file.edit("~/Desktop//.Rprofile")





## a demo package
pkg = system.file("examples", "pkgDemo", package = "Rd2roxygen")
file.copy(pkg, tempdir(), recursive = TRUE)  # copy to temp dir first



od = setwd(tempdir())

## take a look at original R scripts
file.show("pkgDemo/R/foo.R")

#options(roxygen.comment = "##' ")

## convert Rd's under man to roxygen comments
Rd2roxygen(file.path(tempdir(), "pkgDemo"))

file.show("pkgDemo/R/foo.R")  # what happened to foo.R and bar.R?

This don\' t ' know
rd.file = system.file("examples", "parse_and_save.Rd", package = "Rd2roxygen")
options(roxygen.comment = "##' ")
create_roxygen(parse_file(rd.file))

setwd(od)  # restore working directory
tempdir()

########################################################
b.pca = pca(baseball)
indos = 1:6
indoc = 7:13
indds = 14:16
colnames(baseball)[indos]
colnames(baseball)[indoc]
colnames(baseball)[indds]

b.pca$loa
usg("pca")
A = spca:::make.cont(b.pca)

sum(abs(A[indos,1]))
sum(abs(A[indoc,1]))
sum(abs(A[indds,1]))

sum(abs(A[indos,2]))
sum(abs(A[indoc,2]))
sum(abs(A[indds,2]))