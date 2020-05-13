
# packages ----------------------------------------------------------------

ipk = function(pkg){
  new.pkg = list.of.pkg[!(list.of.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

list.of.pkg = c('tidyverse', 'ggstance', "repr", "kknn", "gridExtra", "GGally", "hexbin", 'cowplot', 'ggstance', 'lvplot', 'viridis')
ipk(list.of.pkg)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  geom_point() +
  geom_smooth()

ggplot(data = mpg, mapping = aes(y = hwy, x = displ)) +
  geom_point() +
  geom_smooth(se = F)

ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point() +
  geom_smooth(aes(group = drv ), se = F)

ggplot(diamonds) +
  geom_bar(aes(x = cut))

ggplot(diamonds) +
  stat_count(aes(x = cut))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )
ggplot(data = diamonds) +
  geom_pointrange(
    mapping = aes(x = cut, y = depth),
    stat = "summary",
    fun.ymin = min,
    fun.ymax = max,
    fun.y = median
  )

ggplot(diamonds) +
  geom_bar(
    mapping = aes(x = cut, fill = clarity), position = 'dodge'
  )

ggplot(
  data = diamonds,
  mapping = aes(x = cut, color = clarity)
) +
  geom_bar(fill = NA, position = "identity")

ggplot(
  data = diamonds,
  mapping = aes(x = cut, fill = clarity)
) +
  geom_bar(alpha = 1/5, position = "identity") # a good one


# exploratory analysis ----------------------------------------------------



diamonds %>%
  filter(carat <3) %>%
  ggplot(mapping = aes(x = carat,color = cut)) +
  geom_freqpoly(binwidth = 0.1)

diamonds %>%
  filter(carat <3) %>%
  ggplot(mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) 
  

ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
# some outliers at y <2, y>20

diamonds %>%
  ggplot(mapping = aes(x = x)) +
  geom_histogram(binwidth = 0.01)

diamonds %>%
  ggplot(mapping = aes(x = x)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 20))
# some outliers at x = 0

diamonds %>%
  ggplot(mapping = aes(x = z)) +
  geom_histogram(binwidth = 0.5)

diamonds %>%
  ggplot(mapping = aes(x = z)) +
  geom_histogram(binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 20))

# outliers at z = 0, z > 7




plot_hist = function(df, numcols, bins = 10){
  options(repr.plot.height = 3, repr.plot.width = 4)
  for(col in numcols){
   # if(is.numeric(col)){
      bw = (max(df[,col], na.rm = T) - min(df[,col], na.rm = T))/ (bins+1)
      p = ggplot(df, aes_string(col)) +
        geom_histogram(alpha = 0.6, binwidth  = 0.5) +
        # coord_cartesian(ylim = c(0,50)) +
        theme_minimal_hgrid(12, rel_small = 1) +
        ggtitle('Histogram of \n ', col)
      print(p)
      
    }
  # }
}

numcols = c('x','y', 'z', 'price')
plot_hist(diamonds, numcols) 

ggplot(diamonds, aes(x = price, y = ..density..)) +
  geom_freqpoly(aes(color = cut), binwidth = 500)


# reorder string arguments within aesthetics ------------------------------



plot_box = function(df, numcols, col_x = 'cut'){
  for(col in numcols){
    # if(is.numeric(col)){
      p = ggplot(df, aes(reorder(!!sym(col_x), !!sym(col), FUN = median), !!sym(col))) + # !!sym() allows the use of string arguments thus
        # we dont need to use aes_string()
        geom_boxplot() +
        theme_minimal_hgrid(12, rel_small = 1) +
        ggtitle(paste("Boxplot of ", col, "\n vs.", col_x))
      print(p)
    }
  # }
  
}
numcols = c('price')
plot_box(diamonds, numcols)

#  good cut diamonds are cheaper on average

a = nycflights13::flights %>%
  mutate(cancelled = is.na(dep_time),
         sched_hour = sched_dep_time %/% 100,
         sched_min = sched_dep_time %% 100,
         sched_dep_time = sched_hour + sched_min / 60) 

# boxploth with ggstance --------------------------------------------------


ggplot(a, aes(cancelled, sched_dep_time)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal_vgrid(12, rel_small = 1)

ggplot(a, aes(cancelled, sched_dep_time)) +
  ggstance::geom_boxploth() +
  theme_minimal_hgrid(12, rel_small = 1)

# heatmap - 2 ordered categorical variables -------------------------------

diamonds %>%
  count(color, cut) %>%
  arrange(n) %>%
  group_by(color) %>%
  # slice(which.min(n))
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n)) 


# proportions to show distinction between color and cut -------------------



diamonds %>%
  count(color, cut) %>%
  arrange(n) %>%
  group_by(color) %>%
  mutate(prop = n / sum(n)) %>% 
  # slice(which.min(n))
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = prop)) +
  scale_fill_viridis(limits = c(0,1))

 nycflights13::flights %>%
   group_by(dest, month) %>%
   filter(dep_delay > 0) %>%
   summarise(mean = mean(dep_delay, na.rm = T)) %>%
   ggplot(mapping = aes(x = month, y = dest)) + 
   geom_tile(mapping = aes(fill = mean)) +
   labs(x = 'Month', y = 'Destination', fill = 'Departure Delay')


# parsing dates and tidying with readr ------------------------------------

  
 
parse_date('January 1, 2010', format = '%B %d, %Y')

table4a %>%
  tidyr::pivot_longer(-country, names_to = 'year', values_to = 'cases')
table4a %>%
  gather(`1999`, `2000`, key = 'year', value = 'cases')

table2 %>%
  spread(key = type, value = count)
table2 %>%
  pivot_wider(names_from = type, values_from = count)

stocks = tibble(
  year = c(2015, 2015, 2016, 2016),
  half = c(1, 2, 1, 2),
  return = c(1.88, 0.59, 0.92, 0.17)
) %>% spread(year, return) %>%
  gather(`2015`, `2016`, key = 'year', value = 'return')


# separating and pull -----------------------------------------------------

table3 %>%
  separate(rate, into = c('cases', 'population'), sep = '/')

table5 %>%
  unite(new, century, year, sep = '')


# cleaning who tb data ----------------------------------------------------

who = tidyr::who

# first gather the colums we cant make much meaning of 

who1 = who %>%
  gather(new_sp_m014:newrel_f65, key = 'key', value = 'cases', na.rm = T)

who1
# count key to get some insight

who1 %>%
  count(key)

#replace newrel with new_rel

who2 = who1 %>%
  mutate(key = stringr::str_replace(key, 'newrel', 'new_rel'))

# separate key into new, type and sexage

who3 = who2 %>%
  separate(key, into = c('new', 'type', 'sexage'), sep = '_')

who3 %>%
  count(new)

# we can drop the new var as it is consistent in the data. 
# we also drop iso2 and iso3 as they are redundant

who4 = who3 %>%
  select(-new, -iso2, -iso3)

# next we separate sexage into sex and age
who5 = who4 %>%
  separate(sexage, c('sex', 'age'), sep = 1)

cat('\tthe whole dataset is now tidy!') 


# strings with stringr ----------------------------------------------------

x = c('app le1', 'banana ', ' pear')
str_length(x) #length

str_sub(x, 1,3) #subset
str_view(x, 'an') # find matches
str_view(c('abc','a.c', 'bef'), 'a\\.c') #explicit match

str_view(x, '\\s') # find space
str_view(x, '\\d') # find digit
# [abc] matches a.b, or c
# [^abc] mattches anything except a, b, or c
str_view(c("grey", "gray"), "gr(e|a)y") # using alternation


# Repetition --------------------------------------------------------------

# ? - 0 or 1 occurrebce of preceding element. can be used to imply optional
# + - 1 or more ""
# * 0 or  more ""
# {n} preceding element matched exactly n times
# {m,} preceding element matched m or more times
# {m, n} preceding element matched between m and n times


y = c('ab')


# grouping and backreferences ---------------------------------------------

#  brackets can be used for grouping referred to by backreferences like \1,\2, etc
str_view(fruit, '(..)\\1', match = T) # finds repeated pair of letters
str_view(fruit, '(.)\1\1') # matches anything

str_view(fruit, '(.)(.)\\2\\1') # finds repeated pair both forward and backwards
str_view(fruit, '(..)\1')
# grep('^(.)((.*\\1$) | \\1?$)', fruit, v = T)
str_subset('abba','^(.)((.*\\1$) | \\1?$)' )

# How many common words start with t?
  sum(str_detect(words, "^t"))

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))

# Find all words containing at least one vowel, and negate
no_vowels_1 <- !str_detect(words, "[aeiou]")
# Find all words consisting only of consonants (non-vowels)
no_vowels_2 <- str_detect(words, "^[^aeiou]+$")
identical(no_vowels_1, no_vowels_2)

#str_detect() == grep(), str_count() == grepl()


# str_detect exercise -----------------------------------------------------

#  all words start/end with x
grep('^x|x$', words, ignore.case = T, v = T)
df = tibble(
  word = words,
  i = seq_along(word)
)
str_extract('^x|x$', words)

df %>%
  filter(str_detect(words, '^x|x$'))
# all words starting with a vowel and ending with a consonant
df %>%
  filter(grepl('^[aeiou]', word)) %>%
  filter(grepl('[^aeiou]$', word))

# extract matches ---------------------------------------------------------

fields <- c("Name: Hadley", "Country: NZ", "Age: 35")
str_split(fields, ':', simplify = T)

phone <- regex("
\\(? # optional opening parens
(\\d{3}) # area code
[)- ]? # optional closing parens, dash, or space
(\\d{3}) # another three numbers
[ -]? # optional space or dash
(\\d{3}) # three more numbers
", comments = TRUE)
str_match("514-791-8141", phone)
