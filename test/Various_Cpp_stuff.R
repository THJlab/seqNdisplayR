
## After changes

setwd("/Users/au103725/Dropbox/Lab_stuff/Bioinformatics/R_packages/bwimport")
# Clean any old builds
remove.packages("bwimport")

# Rebuild Rcpp glue (safe even if unchanged)
Rcpp::compileAttributes()

# Install locally (no multiarch speeds it up)
system("R CMD INSTALL --preclean --no-multiarch .")



library(bwimport)

.bw.file = "/Users/au103725/Dropbox/Lab_stuff/Bioinformatics/R_packages/seqNdisplayR/test/siRRP40_xPAP_in_batch3_plus.bw"
.bw.file = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_3pseq/siGFP_noPAP_in_batch1_plus.bw"

.chrom = "chr12"
.chrom.start=6531808
.chrom.end=6541078

x <- bw_import(.bw.file, .chrom, .chrom.start, .chrom.end)


library(bwimport)
bw_URL = "http://genome-ftp.mbg.au.dk/public/THJ/seqNdisplayR/examples/tracks/HeLa_3pseq/siGFP_noPAP_in_batch1_plus.bw"

chrom = "chr12"
chrom_start = 6531808
chrom_end = 6541078

vals = bw_import(bw_URL, chrom, chrom_start, chrom_end)

"""
if it works

In your terminal, from the root of the package:
git add src/*.h src/*.c src/libBigWig.a
git commit -m "Add full libBigWig source and headers for portable builds"

git push origin main

remove.packages("bwimport")
remotes::install_github("slaish/bwimport")

git status
git add . 

# or e.g. git add README.md

git commit -m 'Update README with new info'
git push origin main
"""

## seqNdisplayR testing

setwd("/Users/au103725/Dropbox/Lab_stuff/Bioinformatics/R_packages/seqNdisplayR")
remove.packages("seqNdisplayR")
# bash
R CMD build .
R CMD INSTALL seqNdisplayR_1.1.0.tar.gz

library(seqNdisplayR)
packageVersion("seqNdisplayR")


remove.packages("seqNdisplayR")
remotes::install_github("slaish/seqNdisplayR")
library(seqNdisplayR)
packageVersion("seqNdisplayR")

roxygen2::roxygenise("/Users/au103725/Dropbox/Lab_stuff/Bioinformatics/R_packages/seqNdisplayR")

devtools::clean_dll()
devtools::document()
devtools::build()
devtools::install()



