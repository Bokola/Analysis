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
 type$type[[i]] = typeof(nycflights13::flights[[1]]) 
}
type

#  10 random normals

mean = c(-10, 0, 10, 100)
out = vector("list", length(mean))
for (i  in seq_along(out)) {
  out[[i]] = rnorm(10, mean = mean[[i]])
  
}
out 
