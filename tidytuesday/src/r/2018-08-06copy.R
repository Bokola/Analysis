# Packages ----
rm(list = ls(all = T))
ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% installed.packages()[,'Package'])]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  sapply(pkg, require, character.only = T)
}
list.of.pkgs = c('tidyverse', 'magrittr','tm','SnowballC','RColorBrewer','patchwork','cowplot','gridExtra')
ipk(list.of.pkgs)
if(!'patchwork' %in% .packages(all.available = TRUE)){
  devtools::install_github("thomasp85/patchwork")
}

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))
project_dir = path.expand(file.path(home_dir
                                    ,'Analysis'
                                    ,'tidytuesday'
)) %>% gsub( '\\\\', '/',.)
data_dir = path.expand(file.path(project_dir
                                 ,'data'
                                 ,'2019'
                                 ,'2019-08-06')) %>% gsub( '\\\\', '/',.)
# library(tidyverse)
# library(tm)
# library(SnowballC)
# library(RColorBrewer)
# library(patchwork)
# library(cowplot)

# Read in file ----
bob_ross <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# Clean up the episode information and convert titles ----
bob_ross <- bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer) %>% 
  mutate(title = str_to_title(title, locale = "en"))

# Most frequently used words in titles ----

titles <- Corpus(VectorSource(bob_ross$title)) %>% 
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords("english")) %>% 
  tm_map(stripWhitespace) %>% 
  tm_map(stemDocument) %>% 
  TermDocumentMatrix() %>% 
  as.data.frame.matrix(.)

titles.word.freqs <- tibble(word = names(sort(rowSums(titles), decreasing = TRUE)),
                            freq = sort(rowSums(titles), decreasing = TRUE))

# Barplot of the top 15 words in titles ----

getPalette <- colorRampPalette(brewer.pal(9, "YlGnBu"))

plot1 <- 
  ggplot() +
  theme_light() +
  theme(title = element_text(colour = "white"),
        text = element_text(family = "Avant Garde"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(colour = "white"),
        axis.text.y = element_text(margin = margin(0, -5, 0, 20)),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_line(size = 0.5), 
        plot.background = element_rect(fill = "grey40"), 
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(15, 0, 25, 0))) +
  ggtitle(label = "15 most used words in titles") +
  geom_bar(data = titles.word.freqs[1:15, ],
           aes(x = reorder(word, freq), y = freq, fill = factor(freq)),
           stat = "identity",
           show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = getPalette(15))
# Elements appearing in at least 10 paintings ----

total.elements <- bob_ross %>% 
  select(-title, -contains("frame")) %>% 
  gather(-(season:episode), key = "element", value = "count") %>% 
  mutate(element = case_when(.$element %in% c("lake", "lakes") ~ "lake",
                             TRUE ~ as.character(.$element))) %>% 
  mutate(element = case_when(.$element %in% c("mountain", "mountains") ~ "mountain",
                             TRUE ~ as.character(.$element))) %>% 
  mutate(element = case_when(.$element %in% c("tree", "trees") ~ "tree",
                             TRUE ~ as.character(.$element))) 

top.elements <- total.elements %>% 
  group_by(element) %>% 
  summarise_all(sum) %>% 
  filter(count >= 10) %>% 
  arrange(desc(count))

elements <- total.elements %>% 
  filter(element %in% top.elements$element) %>% 
  mutate(element = gsub("_", " ", element)) %>% 
  group_by(season, element) %>% 
  summarise_all(sum) %>% 
  select(-episode) %>% 
  #mutate(element = factor(element, levels = rev(top.elements$element))) %>% 
  mutate(range = cut(count,
                     breaks = c(-1, 0, 5, 10, 15, 20, 25, 30),
                     labels = c("0", "0-5", "5-10", "10-15", "15-20", "20-25", ">25")))
plot2 <- 
  ggplot() +
  theme_light() +
  theme(title = element_text(colour = "white"),
        text = element_text(family = "Avant Garde"),
        axis.title.x = element_text(colour = "white", face = "bold"),
        axis.title.y = element_blank(),
        axis.text = element_text(colour = "white"),
        axis.text.y = element_text(margin = margin(0, -5, 0, 20), size = 8),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey40"),
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid = element_line(size = 0.5), 
        plot.margin = margin(0, 5, 8, 5),
        plot.background = element_rect(fill = "grey40"), 
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(5, 0, 10, 0)),
        legend.position = "right", 
        legend.direction = "vertical",
        legend.margin = margin(unit(0, "cm")),
        legend.background = element_rect(fill = "grey40"),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.text = element_text(colour = "white", size = 8),
        legend.spacing.y = unit(0.5, "cm"),
        legend.title = element_blank()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  ggtitle(label = "Elements appearing at least 10 times overall") +
  geom_tile(data = elements, mapping = aes(x = season, y = element, fill = factor(range)),
            colour = "white", size = 0.25) +
  scale_fill_manual(values = c("grey90", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")) +
  scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30), labels = c(1, 5, 10, 15, 20, 25, 30))

# Assemble plots ----

header <- 
  ggplot() +
  ggtitle(label = "The Paintings of Bob Ross") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey40"), 
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold",
                                  margin = margin(15, 0, 25, 0)),
        title = element_text(colour = "white"),
        text = element_text(family = "Palatino"))


png(path.expand(file.path(project_dir, "plots","2019-08-06.png")),
    width = 1920, height = 1080, res = 144)
grid.arrange(header, plot1, plot2,
             ncol = 2, nrow = 2,
             layout_matrix = rbind(
               c(1, 1),
               c(2, 3)),
             heights = c(0.5, 4.5),
             widths = c(2, 2))
dev.off()
