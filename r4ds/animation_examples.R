p <- ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars)))

p + geom_text()
# Avoid overlaps
p + geom_text(check_overlap = TRUE)
# Labels with background
p + geom_label()
# Change size of the label
p + geom_text(size = 10)

# Set aesthetics to fixed value
p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
p + geom_point() + geom_text(vjust = 0, nudge_y = 0.5)
p + geom_point() + geom_text(angle = 45)
## Not run: 
# Doesn't work on all systems
p + geom_text(family = "Times New Roman")

## End(Not run)

# Add aesthetic mappings
p + geom_text(aes(colour = factor(cyl)))
p + geom_text(aes(colour = factor(cyl))) +
  scale_colour_discrete(l = 40)
p + geom_label(aes(fill = factor(cyl)), colour = "white", fontface = "bold")

p + geom_text(aes(size = wt))
# Scale height of text, rather than sqrt(height)
p + geom_text(aes(size = wt)) + scale_radius(range = c(3,6))

# You can display expressions by setting parse = TRUE.  The
# details of the display are described in ?plotmath, but note that
# geom_text uses strings, not expressions.
p + geom_text(aes(label = paste(wt, "^(", cyl, ")", sep = "")),
              parse = TRUE)

# Add a text annotation
p +
  geom_text() +
  annotate("text", label = "plot mpg vs. wt", x = 2, y = 15, size = 8, colour = "red")

library(tidyverse)
library(gganimate)

gdp_tidy <- readr::read_csv("https://raw.githubusercontent.com/amrrs/animated_bar_charts_in_R/master/data/gdp_tidy.csv")


gdp_formatted <- gdp_tidy %>%
  group_by(year) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-value),
         Value_rel = value/value[rank==1],
         Value_lbl = paste0(" ",round(value/1e9))) %>%
  group_by(country_name) %>% 
  filter(rank <=10) %>%
  ungroup()

# Animation

#gdp_formatted <- readr::read_csv("https://raw.githubusercontent.com/amrrs/animated_bar_charts_in_R/master/data/gdp_tidy.csv")
anim <- ggplot(gdp_formatted, aes(rank, group = country_name, 
                                  fill = as.factor(country_name), color = as.factor(country_name))) +
  geom_tile(aes(y = value/2,
                height = value,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(country_name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=value,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  #scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'GDP per Year : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data") 

# For GIF

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif")) 

# For MP4

animate(anim, 200, fps = 20,  width = 1200, height = 1000, 
        renderer = ffmpeg_renderer()) -> for_mp4

anim_save("animation.mp4", animation = for_mp4 )

#===========================================================

turnout <- tibble(year = c(2002, 2002, 2002, 2002, 2002, 2002, 2002, 
                           2002, 2002, 2002, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 2005, 
                           2005, 2005, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 2009, 
                           2009, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 2013, 
                           2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017), 
                  group = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                                      1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L, 4L, 
                                      5L, 6L, 7L, 8L, 9L, 10L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
                                      9L, 10L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L), 
                                    .Label = c("18-20", "21-24", "25-29", "30-34", "35-39", 
                                               "40-44", "45-49", "50-59", "60-69", "> 69"), 
                                    class = "factor"), 
                  percent = c(69.9, 67.7, 71.6, 76.2, 79.2, 
                              79.6, 80.6, 83.4, 85.7, 77.7, 69.6, 66, 69.5, 73.9, 77.9, 
                              79.2, 79.7, 81.8, 84.2, 75.8, 62.5, 58.6, 60.6, 64.5, 68.5, 
                              71.9, 72.6, 74.1, 79.2, 72, 63.7, 59.6, 61.6, 64.8, 68.1, 
                              71.8, 74, 74.7, 78.7, 73.7, 69.9, 67, 68.6, 72, 74.4, 76.3, 
                              78.8, 79.4, 81, 75.8))
wanim <- ggplot(turnout, aes(group, percent)) +
  geom_bar(stat = "identity") +
  geom_line(aes(x = as.numeric(factor(group)), col = factor(year)), size = 1) +
  scale_colour_discrete("year") +
  theme_minimal() +
  scale_x_discrete("age group") +
  transition_time(year) +
  labs(title="{if(frame_time <= 2017) round(frame_time) else 2017}") +
  shadow_mark(exclude_layer = 1)

animate(wanim, nframes = 150, fps = 10, width = 600, height = 600,
        renderer = gifski_renderer(loop = F))
