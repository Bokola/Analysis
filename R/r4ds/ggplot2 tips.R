
# packages ----------------------------------------------------------------

ipk = function(pkg){
  new.pkg = list.of.pkg[!(list.of.pkg %in% .packages(all.available = T))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}

list.of.pkg = c('tidyverse', 'ggstance', "repr", "kknn", "gridExtra", "GGally", "hexbin", 'cowplot')
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
