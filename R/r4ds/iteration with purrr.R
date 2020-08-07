library(tidyverse)
df = tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

output = vector("double", ncol(df))

for (i in seq_along(df)) {
  output[[i]] = median(df[[i]])
}
output
# write for loops for 
# a) mean of every column in mtcars
mm = vector("double", ncol(mtcars))
for (i in seq_along(mtcars)) {
  mm[[i]] = mean(mtcars[[i]])
}
mm

# b) types in flights data

type = tibble::tibble(var = vector("character", ncol(nycflights13::flights)), type = vector("character", ncol(nycflights13::flights)))
for (i in seq_along(nycflights13::flights)) {
 type$var[[i]] =  names(nycflights13::flights)[[i]]
 type$type[[i]] = typeof(nycflights13::flights[[i]]) 
}
type

#  10 random normals

mean = c(-10, 0, 10, 100)
out = vector("list", length(mean))
for (i  in seq_along(out)) {
  out[[i]] = rnorm(10, mean = mean[[i]])
  
}
do.call(cbind, out) 

out = ""
for (x in letters) {
  out <- stringr::str_c(out, x)
}
out
# or
out = str_c(letters, collapse = "")

x <- runif(100)
out <- vector("numeric", length(x))
out[1] <- x[1]
for (i in 2:length(x)) {
  out[i] <- out[i - 1] + x[i]
}
# or
cumsum(x)

# a loop that prints 'Alice and the Camel'

animal = c("Alice the Camel has one hump", "Ruby the Rabbit has two ears", "Sally the Sloth has three toes", "Felix the fox has four legs", "Lilly the Ladybug has five spots", "Andy the Ant has six legs", "Larry the Lizard has seven stripes", "Sammy the Spider has eight legs")
out = vector("character", length(animal))

for(i in seq_along(animal)) {
  out[[i]] = str_split(animal[[i]], " ", 2)[[1]] [1]
}


for(i in seq_along(animal)) {
 paste0(animal[[i]]) %>%
    rep(3) %>%
   writeLines() %>%
    paste0("So go, ", as.character(str_split(animal[[i]], " ", 2)[[1]] [1]), " go!", "\n") %>%
    writeLines()
}


# reading in multiple files into 1

home = ifelse(Sys.info()[["sysname"]] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE")) %>% gsub("\\\\", "/",.)

data = file.path(home, "Downloads")

files = dir(data, pattern = "\\.csv$", full.names = T) %>%
  subset(., grepl("\\bupd\\b", .))

file = vector("list")#, length(files))

for(i in seq_along(files)) {
  file[[i]] = readr::read_csv(files[[i]])
}

file = dplyr::bind_rows(file)

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) {
    factor(x, labels = c("auto", "manual"))
  }
)
for (var in names(trans)) {
  mtcars[[var]] <- trans[[var]](mtcars[[var]])
}
# functions - passing function to a function

col_summ = function(df, fun) {
  out = vector("double", length(df))
    for(i in seq_along(df)) {
      if(is.numeric(df[[i]])){
        
      out[i] = fun(df[[i]])
      }
    }
  out
}
col_summ(mtcars, median)

# The Map Functions
