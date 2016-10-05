## ============================================================================ ##
## Run this file to render both a report with code and a report without code.
## ============================================================================ ##

rmarkdown::render("analysis-datasharing.Rmd",
                  output_file = "analysis-datasharing.html", # this is relative to the markdown file
                  params = list(showcode=FALSE))

rmarkdown::render("analysis-datasharing.Rmd",
                  output_file = "analysis-datasharing-withcode.html",  # this is relative to the markdown file
                  params = list(showcode=TRUE))
