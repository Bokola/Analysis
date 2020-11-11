ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
  suppressPackageStartupMessages(sapply(pkg, library, character.only = TRUE))
}

list.of.pkgs = c('tidyverse', 'magrittr', 'nycflights13')
ipk(list.of.pkgs)


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

animal = c("Alice the Camel has one hump", "Ruby the Rabbit has two ears", "Sally the Sloth has three toes", 
           "Felix the fox has four legs", "Lilly the Ladybug has five spots", "Andy the Ant has six legs",
           "Larry the Lizard has seven stripes", "Sammy the Spider has eight legs")
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

add_to_vector <- function(n) {
  output <- vector("integer", 0)
  for (i in seq_len(n)) {
    output <- c(output, i)
  }
  output
}

add_to_vector(100)
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

u = c(10,-10,100, 0)
map(u, rnorm, n=10)
map_dbl(mtcars, mean)
map_lgl(mtcars, is.numeric)
map_chr(mtcars, class)

z = list(x = 1:2, y = 7:10)
map_int(z, length)

# shortcuts
models = mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .))

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

# or simply

models %>%
  map(summary) %>%
  map_dbl("r.squared")

# you can use an integer to select elements by position

x = list(list(1,2,3), list(4,5,6), list(7,8,9))
x %>% map_dbl(2)

# Dealing with failure: safely(), possibly(), quietly()
x = list(1, 10, "a")
y = x %>% map(safely(log))
str(y)

y = y %>% transpose()
str(y)

is_ok = y$error %>% map_lgl(is.null)
x[!is_ok]

y$result[is_ok] %>% flatten_dbl()

x <- list(1, 10, "a")
x %>% map_dbl(possibly(log, NA_real_))

x <- list(1, -1)
x %>% map(quietly(log)) %>% str()

# Mapping over Multiple Arguments
mu = list(5, 10, -3)
sigma = list(1, 5, 10)
seq_along(mu) %>%
  map(~rnorm(5, mu[[.]], sigma[[.]])) %>%
  str()
# or precisely

map2(mu, sigma,rnorm, n = 5) %>% str()

# for more a list of arguments - pmap()

n = list(1,3,5)
args1 = list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>%
  str()
# Invoking different functions

f = c("runif", "rnorm", "rpois")
params = list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)
invoke_map(f, params, n = 5) %>% str()

# Walk - function side effects other than the result

x = list(1, "a", 3)
x %>% walk(print)

# pwalk and walk2 
library(ggplot2)
 plots = mtcars %>%
   split(.$cyl) %>%
   map(~ggplot(., aes(mpg, wt)) + geom_point())
 paths = stringr::str_c(names(plots), ".pdf")
 
 pwalk(list(paths, plots), ggsave, path = file.path(home, "Downloads"))
 
 # Reduce and Accumulate
 
 dfs <- list(
   age = tibble(name = "John", age = 30),
   sex = tibble(name = c("John", "Mary"), sex = c("M", "F")),
   trt = tibble(name = "Mary", treatment = "A")
 )
dfs %>% reduce(full_join)

vs <- list(
  c(1, 3, 5, 6, 10),
  c(1, 2, 3, 7, 8, 10),
  c(1, 2, 3, 4, 8, 9, 10)
)
vs %>%
  reduce(intersect)



# mapping over two vectors ------------------------------------------------

mu = list(5, 10, -3) ; sigma = list(1, 5, 10)
map2(mu, sigma, rnorm, n = 5) %>% str()

# pmap for more than 2 arguments ------------------------------------------

n = list(1, 3, 5)
args1 = list(n, mu, sigma)
args1 %>%
  pmap(rnorm) %>% str()



# predicate functions -----------------------------------------------------
# keep & discard
iris %>%
  keep(is.factor) %>% str()

iris %>%
  discard(is.factor) %>% str()
# some & every
x = list(1:5, letters, list(10))
x %>% some(is_character)
x %>% every(is_vector)

# detect & detect_index

x = sample(10)
x %>% detect(~ . > 5)
x %>% detect_index(~ . > 5)

# head_while & tail_while

x %>% head_while(~ . > 5)
x %>% tail_while(~ . > 5)

# invoking different functions --------------------------------------------

f = c("runif", "rnorm", "rpois")
param = list(
  list(min = -1, max = 1),
  list(sd = 5),
  list(lambda = 10)
)

invoke_map(f, param, n = 5) %>% str()
# with a tibble

sim <- tribble(
  ~f, ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 5),
  "rpois", list(lambda = 10)
)
sim %>%
  mutate(sim = invoke_map(f, params, n = 10))