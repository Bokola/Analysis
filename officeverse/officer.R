# officeverse: 
# {officer}: produce content made mostly of tables, graphs, table of contents
# {officedown}: for formatting rmarkdown output, if you write a lot of text
# {flextable}: create tables for reporting
# {mschart:} with officer produces native office charts in powerpoint / word doc
# {rvg}: brings an API to produce vector graphics embedded in powerpoint / excel 
#   workbooks
browseURL("https://ardata-fr.github.io/officeverse/officer-for-word.html")
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

home <- rstudioapi::getActiveDocumentContext()$path
home <- dirname(home) # get home directory

# {officer} allows you send a set of r generated content to word
doc_1 <- read_docx() %>%
  body_add_par("Hello officer", style = "heading 1") %>%
  body_add_par("", style = "Normal") %>%
  body_add_table(airquality, style = "table_template")

# template usage with {officer}
template <- list.files(home, pattern = ".docx") %>% subset(grepl("templ",.))

doc_2 <- read_docx(path = file.path(home, template)) %>%
  body_add_par("A simple table with officer", style = "Normal") %>%
  body_add_table(head(mtcars))
print(doc_2, target = file.path(home, "example_template.docx"))
# add ggplot objects
gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + geom_point()
doc_2 <- doc_2 %>%
  body_add_gg (value = gg)
print(doc_2, target = file.path(home, "example_template.docx"))
# insert page break with body_add_break()
ft <- flextable(head(iris, n = 10))
ft <- set_table_properties(ft, layout = "autofit")
read_docx() %>%
  body_add_par(value = "iris data", style = "heading 2") %>%
  # add flextable
  body_add_flextable(value = ft) %>%
  body_add_break() %>%
  body_add_par(value = "plot examples", style = 'heading 2') %>%
  body_add_gg(value = gg, style = "centered") %>%
  print(target = file.path(home, "example_template.docx"))
# external documents - body_add_docx()
read_docx() %>% 
  body_add_par(value = "An external document", style = "heading 1") %>% 
  body_add_docx(src = "reports/example_break.docx") %>% 
  print(target = "reports/example_add_docx.docx")

# It is necessary to generate smaller documents and to design a main
# script that inserts the different documents into a main Word document.
# The following script illustrates the strategy:
library(uuid)

ft <- flextable(iris)
ft <- set_table_properties(ft, layout = "autofit")

gg_plot <- ggplot(data = iris ) +
  geom_point(mapping = aes(Sepal.Length, Petal.Length))

tmpdir <- tempfile()
dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
tempfiles <- file.path(tmpdir, paste0(UUIDgenerate(n = 10), ".docx") )

for(i in seq_along(tempfiles)) {
  doc <- read_docx()
  doc <- body_add_par(doc, value = "", style = "Normal")
  doc <- body_add_gg(doc, value = gg_plot, style = "centered")
  doc <- body_add_par(doc, value = "", style = "Normal")
  doc <- body_add_flextable(doc, value = ft)
  temp_file <- tempfile(fileext = ".docx")
  print(doc, target = tempfiles[i])
}

# tempfiles contains all generated docx paths

main_doc <- read_docx()
for(tempfile in tempfiles){
  main_doc <- body_add_docx(main_doc, src = tempfile)
}
print(main_doc, target = file.path(home, "example_huge.docx"))

# add section with 

# To format your content in a section, you should use the body_end_block_section
# function. First you need to define the section with the block_section function,
# which takes an object returned by the prop_section function. It is prop_section()
# that allows you to define the properties of your section.
library(ggplot2)

gg <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + 
  geom_point()

doc_section_1 <- read_docx()
doc_section_1 <- body_add_gg(
  x = doc_section_1, value = gg, 
  width = 9, height = 6,
  style = "centered")
# add section with prop_section
ps <- prop_section(
  page_size = page_size(orient = "landscape"),
  type = "oddPage")

doc_section_1 <- body_end_block_section(
  x = doc_section_1, 
  value = block_section(property = ps))
# That's it, let's add the graphic again to see it display at the end of the
# document in the default section:
doc_section_1 <- body_add_gg(
  x = doc_section_1, value = gg,
  width = 6.29, height = 9.72,
  style = "centered"
)

print(doc_section_1, target = file.path(home, "example_landscape_gg.docx"))
# Section properties are defined with function prop_section with arguments:
#   
#   page_size: page dimensions defined with function page_size().
# page_margins: page margins defined with function page_margins().
# type : section type ("continuous", "evenPage", "oddPage", .).
# section_columns: section columns defined with function section_columns()

# How to manage sections

# The body_end_block_section function is usually used twice. The first time to
# close the previous section (and thus start the new one) and then another
# section to close the second one
doc_section_2 <- read_docx() %>% 
  body_add_par("This is a dummy text. It is in a continuous section") %>% 
  body_end_block_section(block_section(prop_section(type = "continuous"))) %>% 
  body_add_gg(value = gg, width = 7, height = 5, style = "centered") %>% 
  body_end_block_section(block_section(property = ps))

print(doc_section_2, target = file.path(home,"example_landscape_gg2.docx"))
# Note that if you add a section break at the end of the document with a different
# orientation than the default, it generates a last page that is empty. This is a
# behavior of Word and there is only one solution: using a template where the
# default orientation is the same as the last section break. For example,
# a default landscape orientation if you insert a landscape oriented
# section at the end of the document.
landscape_one_column <- block_section(
  prop_section(
    page_size = page_size(orient = "landscape"), type = "continuous"
  )
)
landscape_two_columns <- block_section(
  prop_section(
    page_size = page_size(orient = "landscape"), type = "continuous",
    section_columns = section_columns(widths = c(4, 4))
  )
)

doc_section_3 <- read_docx() %>%
  body_add_table(value = head(mtcars), style = "table_template") %>%
  body_end_block_section(value = landscape_one_column) %>% 
  body_add_par(value = paste(rep(letters, 60), collapse = " ")) %>%
  body_end_block_section(value = landscape_two_columns)

print(doc_section_3, target = file.path(home,"example_complex_section.docx"))

# In addition to the generic function body_end_block_section, some utility
# functions are available to be used as shortcuts:
#   
#   body_end_section_landscape()
# body_end_section_portrait()
# body_end_section_columns()
# body_end_section_columns_landscape()
# body_end_section_continuous()

# remove content with body_remove()
# The function body_remove() lets you remove content from a Word document. 
# This function is often to be used with a cursor_* function.
my_doc <- read_docx(path = file.path(home, "example_complex_section.docx"))

my_doc <- body_remove(my_doc) %>% cursor_end()
my_doc <- body_remove(my_doc) %>% cursor_end()
my_doc <- body_remove(my_doc) %>% cursor_end()

print(my_doc, target = file.path(home, "example_remove.docx"))
