# Inside `tidyr` package
# Data is a data frame

## packages
ipk <- function(pkg) {
  new.pkg <- list.of.pkg[!(list.of.pkg %in% installed.packages()[,"Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
list.of.pkg <- c("tidyverse", "reprex")
ipk(list.of.pkg)

##1.  `complete` - complete a df with missing combinations of data
#complete(data,...,fill = list())

df <- tibble(group = c(1:2, 1), 
             item_id = c(1:2, 2), 
             item_name = c("a","b","b"),
             value1 = 1:3,
             value2 = 4:6
              )
df %>% complete(group, nesting(item_id, item_name))
# You can choose to fill in missing values
df %>%complete(group, nesting(item_id, item_name), fill = list(value1 = 0))

##2. `drop_na(data, ...)` -- drops rows containing missing values
df <- tibble(x = c(1,2, NA), y = c("a", NA, "b"))
df %>% drop_na()
df %>% drop_na(-x)

##3. `expand(data,...)`
# useful in conjunction with `left_join` if you want to convert implicit missing values to explicit missing values. Can be used with `anti_join()` to figure out which combinations are missing

# all possible combs of vs & cyl, even those missing in data

expand(mtcars, vs, cyl)

# only combinations of vs & cyl that appear in data
expand(mtcars, nesting(vs, cyl))

# implicit missings -------
df <- tibble(
  year = c(2010, 2010, 2010, 2010, 2012, 2012, 2012),
  qtr = c( 1, 2, 3, 4, 1, 2, 3),
  return = rnorm(7)
)
df %>% expand(year, qtr)
df %>% expand(year = 2010:2012, qtr)
df %>% expand(year = full_seq(year, 1), qtr)
df %>% complete(year = full_seq(year, 1), qtr)

# Nesting ----
# Each person was given one of two treatments, repeated three times
# But some of the replications haven't happened yet, so we have
# incomplete data:
experiment <- tibble(
  name = rep(c("Alex", "Robert", "Sam"), c(3, 2, 1)),
  trt = rep(c("a", "b", "a"), c(3, 2, 1)),
  rep = c(1, 2, 3, 1, 2, 1),
  measurment_1 = runif(6),
  measurment_2 = runif(6)
)
# We can figure out the complete set of data with expand()
# Each person only gets one treatment, so we nest name and trt together:
all <- experiment %>% expand(nesting(name, trt), rep)
all
# we can use anti_join to figure out which obs are missing 
all %>% anti_join(experiment)
# and use right_join to add in the appropriate missing values to the original data
experiment %>% right_join(all)
# or use the complete() short-hand
experiment %>% complete(nesting(name, trt), rep)

## 4.`extract` -- extract col to multiple columns
df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")
# if no match, NA:
df %>% extract(x, c("A", "B"), "([a-d]+)-([a-d]+)")

## 5. `fill` -- fill in missing values usinf prevous entry - fill(data, ..., direction = c("down", "up"))
df <- data.frame(Month = 1:12, Year = c(2000,2001, rep(NA, 10)))
df %>% fill(Year)

## 6. `full_seq` -- create full seq of values in a vector
full_seq(c(1, 2, 4, 6, 10), 1)

##7. `gather` -- gather cols into key-value pairs
stocks <- tibble(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)

# get first obs of each species in iris data -- base R
mini_iris <- iris[c(1,51,101),]
# gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
gather(mini_iris, key = flower_att, value = measurement, Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
# same result - less verbose
gather(mini_iris, key = flower_att, value = measurement, -Species)
# dplyr and piping
mini_iris <- 
  iris %>%
  group_by(Species) %>%
  slice(1)
mini_iris %>% gather(key = flower_att, value = measurement, -Species)

##7. `nest` -- Nest repeated values in a list-variable
as_tibble(iris) %>% nest(-Species)
as_tibble(chickwts) %>% nest(weight)
if(!require(gapminder)) {
  install.packages("gapminder"); require(gapminder)
  gapminder %>%
    group_by(country, continent) %>%
    nest()
  gapminder %>% 
    nest(-country, -continent)
}

## 8. `replace_na` -- replace missing values
df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"), z = list(1:5, NULL, 10:20))
df %>% replace_na(list(x = 0, y = "unknown"))
df %>% mutate(x = replace_na(x, 0))

# NULL is the list-col equivalent of NAs
df %>% replace_na(list(z = list(5)))
df$x %>% replace_na(0)
df$y %>% replace_na("Unknown")

##9. `separate` -- separate one col into multiple cols
df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x, into = c("A", "B"))
# if you just want the second variable
df %>% separate(x, c(NA, "B"))
# if every row doesn't split into the same number of pieces, use the extra and fil arguments to control what happens 
df <- data.frame(x = c("a", "ab", "abc", NA))
df %>% separate(x, c("a", "b"))
# same behaviour but no warnings
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
# if you only want to split specified number of times use extra = "merge"
df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value"), ": ", extra = "merge") 

## 10. `separate_rows` -- separate a collapsed column into multiple rows
df <- data.frame(
  x = 1:3,
  y = c("a", "d,e,f", "g,h"),
  z = c("1", "2,3,4", "5,6"),
  stringsAsFactors = FALSE
)
separate_rows(df, y,z, convert = T)

##11. `spread` -- spread a key value pair accross multiple columns
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
stocksm <- stocks %>% gather(stock, price, -time)
stocksm %>% spread(stock, price)
stocksm %>% spread(time, price)
# spread and gather are complements
df <- data.frame(x = c("a", "b"), y = 3:4, z = 5:6)
df %>% spread(x,y) %>% gather(x, y, a:b, na.rm = T)

## 12.`uncount` -- uncount a data frame, duplicating rows according to a weighting variable
df <- tibble::tibble(x = c("a", "b"), n = c(1, 2))
uncount(df, n)
uncount(df, n, .id = "id")
# you can also use constants 
uncount(df, 2)
# or expressions
uncount(df, 2/n)

## 13. `unite` -- unite multiple columns into one - complements `separate`
unite_(mtcars, "vs_am", c("vs", "am")) %>%
  separate(vs_am, c("vs", "am"))

## 14. `unnest` -- unnest a list column
df <- tibble(
  x = 1:3,
  y = c("a", "d,e,f", "g,h")
)
df %>% transform(y = strsplit(y, ",")) %>%
  unnest(y)
# or just 
df %>% unnest(y = strsplit(y, ","))
# It also works if you have a column that contains other data frames!
df <- tibble(
  x = 1:2,
  y = list(
    tibble(z = 1),
    tibble(z = 3:4)
  )
)
df %>% unnest(y)
# You can also unnest multiple columns simultaneously
df <- tibble(
  a = list(c("a", "b"), "c"),
  b = list(1:2, 3),
  c = c(11, 22)
)
df %>% unnest(a, b)
# If you omit the column names, it'll unnest all list-cols
df %>% unnest()
# You can also choose to preserve one or more list-cols
df %>% unnest(a, .preserve = b)
# Nest and unnest are inverses
df <- data.frame(x = c(1, 1, 2), y = 3:1)
df %>% nest(y)
df %>% nest(y) %>% unnest()
# If you have a named list-column, you may want to supply .id
df <- tibble(
  x = 1:2,
  y = list(a = 1, b = 3:4)
)
unnest(df, .id = "name")
