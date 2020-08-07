#	 
#	<copyright file="week_32.R" company="bokola">
#	 
#		Copyright (c) 2019 All rights reserved
#	 
#		This source is owned and managed by KEMRI Wellcome Trust.
#		Permission must be sought from the above mentioned entity for use, distribution or modification
#		If you are not a KEMRI employee, any violation of the above can constitute malice and criminal intent
#		All other rights reserved.
#	 
#	</copyright>
#	 
#	<author>Basil Okola</author>
#	<email>okolabasilowiti@gmail.com</email>
#	<date>2019-08-07</date>
#	<summary>
#	 
#		Point of entry and exit
#	 
#	</summary>
#	 

# Clear memory

rm(list = ls(all = T))

# Display message:

cat("----------------------------------------\n\n")
cat("Welcome ", Sys.getenv("USERNAME"), "!\n\n", sep = "")
cat("The date today is", format(Sys.Date(), "%d %B %Y."), "\n")-
  cat("Reporting session started at ", format(Sys.time(), "%I:%M %p"), "\n\n", sep = "")

# Set project path and load packages
cat("Installing packages", '\n')

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  if('tidyr' %in% list.of.pkgs){
    devtools::install_github("tidyverse/tidyr",force = T)
  }
  if(!'patchwork' %in% .packages(all.available = TRUE)){
    devtools::install_github("thomasp85/patchwork")
  }
  if(!'ReporteRsjars' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRsjars')
  }
  if(!'ReporteRs' %in% .packages(all.available = TRUE)){
    devtools::install_github('davidgohel/ReporteRs')
  }
 
  sapply(pkg, require, character.only = T)
}
list.of.pkgs = c('devtools', 'Rcpp', 'digest', 'backports', 'tidyverse',  'tidyr', 'dplyr', 'ggplot2', 'magrittr','tm','SnowballC','RColorBrewer','patchwork','cowplot','gridExtra','ReporteRs')
ipk(list.of.pkgs)

cat('setting project directories', '\n')

home_dir =ifelse(Sys.info()['sysname'] == 'Windows', Sys.getenv('USERPROFILE'), Sys.getenv('HOME'))

project_dir = path.expand(file.path(home_dir
                                   ,'Analysis'
                                   ,'tidytuesday'
                                   )) %>% gsub( '\\\\', '/',.)
data_dir = path.expand(file.path(project_dir
                                   ,'data'
                                   ,'2019'
                                  ,'2019-08-06')) %>% gsub( '\\\\', '/',.)

# load data
bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")
bob_ross = bob_ross %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer)

bob_ross_long = bob_ross %>%
  pivot_longer(cols =  ends_with('frame'), names_to = 'frame', values_to = 'count') %>%
  filter(count > 0)
bob_ross_objects = bob_ross %>%
  pivot_longer(cols = c("barn","beach","boat","bridge","building","bushes","cabin","cactus","cirrus","cliff","clouds","conifer","cumulus","deciduous","dock","farm","fence","fire","flowers","fog", "framed","grass","hills","lake","lakes","lighthouse","mill","moon","mountain","mountains","night","ocean","palm_trees","path","person","portrait","river","rocks","snow","snowy_mountain","structure","sun", "tree","trees","waterfall","waves","windmill"), names_to = 'objects', values_to = 'count') %>%
  filter(count >0)
bob_ross_objects = bob_ross_objects %>%
  mutate(., objects = ifelse(grepl('lake', objects), "lake" , objects)) %>%
  mutate(., objects = ifelse(grepl('mount', objects), 'mountain', objects)) %>%
  mutate(., objects = ifelse(grepl('tree', objects), 'tree', objects)) %>%
  group_by(objects) %>%
  summarise(total = dplyr::n()) %>%
  arrange(desc(total))

getPalette <- colorRampPalette(brewer.pal(9, "Blues"))
plot1 <- 
  ggplot(bob_ross_objects) +
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
        plot.background = element_rect(fill = "grey20"), 
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
                                  margin = margin(15, 0, 25, 0))) +
  ggtitle(label = "30 most drawn objects in Bob Ross paintings") +
  geom_bar(data = bob_ross_objects[1:30, ],
           aes(x = reorder(objects, total), y = total, fill = factor(total)),
           stat = "identity",
           show.legend = FALSE) +
  coord_flip() +
  scale_fill_manual(values = getPalette(30)) +
  labs(caption = "Data:https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv") 

# top_objects <- bob_ross_objects %>% 
#   group_by(objects) %>% 
#   summarise_all(sum) %>% 
#   filter(total >= 10) %>% 
#   arrange(desc(total))
# 
# objects <- bob_ross_objects %>% 
#   filter(objects %in% top_objects$element) %>% 
#   mutate(element = gsub("_", " ", element)) %>% 
#   group_by(season, element) %>% 
#   summarise_all(sum) %>% 
#   select(-episode) %>% 
#   #mutate(element = factor(element, levels = rev(top.elements$element))) %>% 
#   mutate(range = cut(count,
#                      breaks = c(-1, 0, 5, 10, 15, 20, 25, 30),
#                      labels = c("0", "0-5", "5-10", "10-15", "15-20", "20-25", ">25")))
# plot2 <- 
#   ggplot() +
#   theme_light() +
#   theme(title = element_text(colour = "white"),
#         text = element_text(family = "Avant Garde"),
#         axis.title.x = element_text(colour = "white", face = "bold"),
#         axis.title.y = element_blank(),
#         axis.text = element_text(colour = "white"),
#         axis.text.y = element_text(margin = margin(0, -5, 0, 20), size = 8),
#         axis.ticks = element_blank(),
#         panel.background = element_rect(fill = "grey40"),
#         panel.border = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         panel.grid.major.y = element_blank(),
#         panel.grid = element_line(size = 0.5), 
#         plot.margin = margin(0, 5, 8, 5),
#         plot.background = element_rect(fill = "grey40"), 
#         plot.title = element_text(hjust = 0.5, size = 15, face = "bold",
#                                   margin = margin(5, 0, 10, 0)),
#         legend.position = "right", 
#         legend.direction = "vertical",
#         legend.margin = margin(unit(0, "cm")),
#         legend.background = element_rect(fill = "grey40"),
#         legend.key.height = unit(0.4, "cm"),
#         legend.key.width = unit(0.4, "cm"),
#         legend.text = element_text(colour = "white", size = 8),
#         legend.spacing.y = unit(0.5, "cm"),
#         legend.title = element_blank()) +
#   guides(fill = guide_legend(reverse = TRUE)) +
#   ggtitle(label = "Elements appearing at least 10 times overall") +
#   geom_tile(data = elements, mapping = aes(x = season, y = element, fill = factor(range)),
#             colour = "white", size = 0.25) +
#   scale_fill_manual(values = c("grey90", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")) +
#   scale_x_continuous(breaks = c(1, 5, 10, 15, 20, 25, 30), labels = c(1, 5, 10, 15, 20, 25, 30))

# Assemble plots ----

header <- 
  ggplot() +
  ggtitle(label = "Bob Ross - painting by the numbers") +
  theme_void() +
  theme(plot.background = element_rect(fill = "grey40"), 
        plot.title = element_text(hjust = 0.5, size = 25, face = "bold",
                                  margin = margin(15, 0, 25, 0)
                                  ),
        title = element_text(colour = "white"),
        text = element_text(family = "Avant Garde"))


png(path.expand(file.path(project_dir, "plots","bob_ross_objects.png")),
    width = 1920, height = 1080, res = 144)
grid.arrange(plot1
             # nrow = 2,
             # layout_matrix = rbind(
             #   c(1, 1),
             #   c(2, 3)),
             # heights = c(0.5, 4.5),
             # widths = c(2, 2)
             )
             
dev.off()
  


  
  
