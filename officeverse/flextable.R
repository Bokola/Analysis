# flextable
browseURL("https://ardata-fr.github.io/flextable-book/")
browseURL("https://stackoverflow.com/questions/70897954/flextable-and-gtsummary-title-font-is-different-from-body-font-with-save-as-doc")
ipk <- function(pkg) {
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if (length(new.pkg))
    install.packages(new.pkg, dep = T, repos = "https://cran.us.r-project.org")
  sapply(pkg, require, character.only = T)
}
list.pkg <-
  c(
    "tidyverse",
    "magrittr",
    "officer",
    "officedown",
    "flextable",
    "rvg",
    'mschart',
    'uuid'
  )
ipk(list.pkg)


# Building s flextable object ---------------------------------------------


home <- rstudioapi::getActiveDocumentContext()$path %>% dirname()
f1 <- flextable(airquality[sample.int(10), ])
f1 <- add_header_row(f1,
                     colwidths = c(4, 2),
                     # number of cols to merge in the row for each label
                     values = c("Air quality", "Time")) # values to add as a character vector
# add theme with theme_
f1 <- theme_zebra(f1) # zebra fills - choice 2
f1 <-
  theme_booktabs(f1) # lines for headers and end of table - choice 1

# set flextable defaults
set_flextable_defaults(
  font.size = 10,
  theme_fun = theme_booktabs,
  padding = 6,
  background.color = "#EFEFEF"
)
flextable(as.data.frame(WorldPhones))
class(WorldPhones)
# Optional argument col_keys is used to only display a subset of columns.
ft <- flextable(head(mtcars),
                col_keys = c("am", "carb", "gear", "mpg", "drat"))
ft
# Many sugar functions can be used to format flextables: bg(),
# fontsize(), italic(), bold(), color(), padding()
ft <- italic(ft, j = 3) # col 3
ft <- color(ft, ~ drat > 3.5, ~ drat, color = "red")
ft <- bold(ft, ~ drat > 3.5,  c('mpg', 'drat'), bold = T)
# Table layout can be modified. One can add or change header/footer rows,
# change cells height and width and merge cells.

# add header and align
ft <- add_header_lines(ft, values = "My flextable")
ft <- align(ft, i = 1, part = "header", align = "center")
# add header row - merges columns
ft <- add_header_row(
  x = ft,
  values = c("some names", "other names"),
  colwidths = c(3, 2)
)
# Functions theme_ are sugar functions whose role is to apply a set of formatting
# instructions to a flextable.
ft <- theme_booktabs(ft)
# Selectors - Many flextable functions have selectors i and j
dat <- head(ggplot2::diamonds, n = 10)
qflextable(dat) %>%
  color( ~ price < 330, color = "orange", ~ price + x + y + z) %>%
  color( ~ carat > .24, ~ cut, color = "red")
# selectors as formulas
ft <- qflextable(dat)

color(ft,
      i = ~ cut %in% "Premium",
      j = ~ x + y,
      color = "red")
# selectors as character vector
ft <- qflextable(dat) %>%
  color(j = c("x", "y"),
        color = "orange",
        part = "all") %>%
  bold(j = c("price", "x"), bold = T)
# selectors as integer vector
# Each element is the row number or col_key number:
color(ft,
      i = 1:3,
      j = 1:3,
      color = "orange")
# as logical vector
color(ft, i = rep(c(TRUE, FALSE), 5), color = "orange")

# selectors and flextable parts
# everal operations (bold, color, padding, compose) accept part="all"
border <- fp_border()
# add vertical line with vline()
vline(ft,
      j = c('clarity', 'price'),
      border = border,
      part = "all")
# change color only on the first row of the header part, use part="header",
# i = 1 as selector.
color(ft, i = 1, color = 'red', part = "header")
# change background wiht bg()
bg(
  ft,
  i = ~ price < 335,
  j = c('x', 'y', 'z'),
  bg = "orange",
  part = 'body'
)
# set captions with set_caption
ft <- set_caption(
  ft,
  "Diamonds data",
  style = "Table Caption",
  autonum = run_autonum(seq_id = "tab", bkm = "diamonds")
)
# add footer
ft <- add_footer_lines(x = ft,
                 values = paste0('created on ', format(Sys.Date(), '%B %d, %Y')))

# Rendering flextable with {officer} --------------------------------------
read_docx() %>%
  body_add_par("Diamonds table") %>%
  body_add_flextable(value = ft) %>%
  print(target = file.path(home, "flextable_to_word.docx"))
