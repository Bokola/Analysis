# officedown for word
# The key features of package {officedown} are :
#   
# Compatibility with the functions of the package officer for the production of
# "runs" and "blocks" of content (text formatting, landscape mode, tables of
# contents, etc.). Ability to use the table styles and list styles defined in the
# "reference_docx" which serves as a template for the pandoc document.
# The replacement of captions (tables, figures and standard identifiers)
# by captions containing a Word bookmark that can be used for cross-referencing.
# Also the replacement of cross-references by cross-references using fields
# calculated by Word. The syntax conforms to the bookdown cross-reference definition.
# Full support for flextable output, including with outputs containing images and links.

# demo
browseURL("https://ardata-fr.github.io/officeverse/officedown-for-word.html")
ipk <- function(pkg) {
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if (length(new.pkg))
    install.packages(new.pkg, dep = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <-
  c("tidyverse",
    "magrittr",
    "officer",
    "officedown",
    "flextable",
    "rvg",
    'mschart',
    'uuid')
ipk(list.pkg)

home <- rstudioapi::getActiveDocumentContext()$path %>% dirname()
dir <- system.file(package = "officedown", "examples", "bookdown")
file.copy(dir, home, recursive = T, overwrite = T)
rmarkdown::render_site(file.path(home, "bookdown"))
browseURL(file.path(home, "bookdown", "_book", "bookdown.docx"))

# the default values are the following
list(
  style = "Table", layout = "autofit", width = 1,
  caption = list(
    style = "Table Caption", pre = "Table ", sep = ": "),
  conditional = list(
    first_row = TRUE, first_column = FALSE, last_row = FALSE,
    last_column = FALSE, no_hband = FALSE, no_vband = TRUE
  )
)

# the values in YAML format are
`output:
  officedown::rdocx_document:
  tables:
  style: Table
layout: autofit
width: 1.0
caption:
  style: Table Caption
pre: 'Table '
sep: ': '
conditional:
  first_row: true
first_column: false
last_row: false
last_column: false
no_hband: false
no_vband: true`

# You can change the default page dimensions of your document by setting the
# values for page_size and page_margins.

# officedown yaml header
output: 
  officedown::rdocx_document:
  page_size:
  width: 8.3
height: 11.7
orient: "portrait"
page_margins:
  bottom: 1
top: 1
right: 1.25
left: 1.25
header: 0.5
footer: 0.5
gutter: 0.5
# Caption label for figures and tables

# You can change the default values for captions (Word style to use, prefix, separator).
# 
# The values to be configured for the tables are the following:
  
  tables:
  caption:
  style: Table Caption
pre: 'Table '
sep: ': '
# These values can also be altered via the function knitr::opts_chunk$set().

knitr::opts_chunk$set(
  tab.cap.style = "Table Caption",
  tab.cap.pre = "Table ",
  tab.cap.sep = ": ",
)
# The values to be configured for the figures are the following:
  
  plots:
  caption:
  style: Image Caption
pre: 'Figure '
sep: ': '
# These values can also be altered via the function knitr::opts_chunk$set().


knitr::opts_chunk$set(
  fig.cap.style = "Image Caption",
  fig.cap.pre = "Figure ",
  fig.cap.sep = ": ",
)