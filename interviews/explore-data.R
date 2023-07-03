
# data exploration --------------------------------------------------------

ipk <- function(pkg){
  for(i in seq(length(pkg))){
    if(!require(pkg[[i]], character.only = T)) install.packages(pkg[[i]], dep = T)
  }
}
list.pkg <- c("Hmisc",
              "tidyverse",
              "magrittr",
              "here",
              "GGally",
              "bookdown",
              "hrbrthemes",
              "viridis")
ipk(list.pkg)

dat <- mtcars
#  get numeric columns
num <- names(dat[,sapply(dat, is.numeric)])
nu.dp <- dat %>% select(where(is.numeric)) %>% names(.)
# count nas

count_na <- function(x){
  sum(is.na(x))
}
sapply(dat, count_na) #%>% as.data.frame() 
# correlation matrix

GGally::ggpairs(dat)

# correlation
out <- cor(dat, method = "pearson") %>% as.data.frame()

# correlation with p-values
out <- Hmisc::rcorr(as.matrix(dat))

# plots

# scatter - geom_point
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, alpha=Species)) + 
  geom_point(size=6, color="#69b3a2") +
  theme_minimal()

# bar - geom_bar
# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)
# grouped bars
ggplot(data, aes(fill = condition, x = specie, y = value)) +
  geom_bar(stat = "identity", position = "dodge")


# stacked
ggplot(data, aes(fill = condition, x = specie, y = value)) +
  geom_bar(stat = "identity", position = "stack")

ggplot(data, aes(fill=condition, y=value, x=condition)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~specie) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")

# pie-chart

data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# compute the position of labels
data <- data %>%
  arrange(desc(group)) %>%
  mutate(prop = value / sum(value) + 100) %>%
  mutate(ypos = cumsum(prop) - 0.5*prop)


ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() + 
  theme(legend.position="none") +
  
  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
