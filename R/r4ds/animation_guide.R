library(tidyverse)
library(ggthemes)
library(animation)
library(cowplot)
library(ggpub)
library(readxl)
library(geonames)


gif_file <- "~/Dropbox/infographics/general/south_america_infant_mortality.gif"
x_space <- 0.16
speed <- 0.12
start_year <- 1985

europe <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador", "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela")

europe <- sort(europe, decreasing = TRUE)

mortality <- read_csv("~/Downloads/child-mortality.csv") %>% 
  filter(Entity %in% europe) 
mortality %>% 
  select(Entity) %>% 
  distinct()


tweet_list <- list()
track_tweets <- 1

max(mortality$Year)
names(mortality)[4] <- "Mortality"

max_value <- max(mortality$Mortality)

breaks = seq(0,60, 10)
labels <- str_c(breaks, "%")
setwd("~/empty/")
file.remove(dir("~/empty/"))
tracker <- 1

for (yy in 1900:2015){
  yy_mortality <- mortality %>%
    filter(Year == yy) 
  
  
  
  
  gg <- yy_mortality %>%
    rename(Country = Entity) %>% 
    mutate(Country = factor(Country, levels = europe)) %>%
    ggplot() +
    geom_bar(aes(Country, Mortality), fill = "skyblue",  stat = "identity") +
    coord_flip(clip= "off") +
    theme_minimal(base_size = 6)+
    labs(x = NULL, y = NULL) +
    labs(caption = "Data source: Our World in Data", title = "The decline of infant mortality in South America")+
    labs(subtitle = "Proportion of infants dying before the age of 5")+
    geom_text(x = 13, y = max_value * 0.93, label = yy, size = 3.5) +
    theme(panel.grid.major.y = element_blank())+
    theme(panel.grid.minor.y = element_blank())+
    theme(panel.grid.minor.x = element_blank())+
    # geom_text(data = fuel_pc, aes(x = Fuel, y = fuel,  label = Percentage), hjust = 0)+
    # geom_text(data = yy_rank, aes(x = Rank, y = -0.01 * max_value, label = Fuel), hjust = 1) +
    scale_y_continuous(limits = c(0, max_value), labels = labels, breaks = breaks, expand = c(0,0)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
    labs(fill = NULL) +
    theme(legend.position = "none")+
    theme(plot.title = element_text(face = "bold"))+
    theme(plot.margin=grid::unit(c(1,1,1,1), "mm"))
  gg
  gg_file <- str_c(str_pad(tracker, width = 3, pad = 0), ".png")
  
  ggsave(gg_file, gg, width = 8, height = 8 * 0.8, units = "cm")
  tracker <- tracker + 1
}

for (i in 1:10) {
  tracker <- tracker + 1
  gg_file <- str_c(str_pad(tracker, width = 3, pad = 0), ".png")
  
  ggsave(gg_file, gg, width = 8, height = 8 * 0.8, units = "cm")
}


# system(str_glue("convert -resize 80% -delay {100 * speed} -loop 0 *.png animation.gif"))
system(str_glue("convert -delay {100 * speed} -loop 0 *.png animation.gif"))


file.rename("animation.gif", gif_file)