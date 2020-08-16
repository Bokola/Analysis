# A bad chart 5 ways in ggplot2

ipk = function(pkg){
  new.pkg = list.of.pkgs[!(list.of.pkgs %in% .packages(all.available = TRUE))]
  
  if(length(new.pkg)) install.packages(new.pkg, dependencies = T)
  if('d3treeR' %in% list.of.pkgs){
    remotes::install_github("d3treeR/d3treeR")
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
  if(!"thematic" %in% .packages(all.available = TRUE)) {
    remotes::install_github("rstudio/thematic")
  }
  
  if(!'INLA' %in% .packages(all.available = TRUE)){
    install.packages("INLA", repos = "https://inla.r-inla-download.org/R/stable", dep = TRUE)
  }
  
  sapply(pkg, require, character.only = T)
}

list.of.pkgs = c("rvest", "tidyverse")
ipk(list.of.pkgs)

home = ifelse(Sys.info()[["sysname"]] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\", "/",.)

# data

url <- "https://en.wikipedia.org/wiki/List_of_most-streamed_songs_on_Spotify"

df <- url %>% 
  read_html() %>% 
  html_table(fill = TRUE) %>% 
  .[[1]] %>% 
  set_names(nm = c("rank", "song_name", "artist", "album", "streams", "date_published")) %>% 
  slice(1:100) %>% 
  mutate(num_rank = parse_number(rank),
         streams_comma = streams,
         streams = parse_number(streams)/1000,
         streams_text = if_else(
           num_rank == 1,
           paste(round(streams, digits = 2), "billion streams"),
           as.character(round(streams, digits = 2))
         ),
         lab_text = glue::glue("{rank}. {song_name} by {artist}"),
  ) %>% 
  as_tibble()

df %>% glimpse()

# Chart 1: Font-height bars

font_height_bars = df %>%
  filter(num_rank <=10) %>%
  ggplot(aes(y = fct_reorder(lab_text, streams), x = streams)) +
  geom_col(fill = "#7dc8c6", width = 0.3) +
  theme(text = element_text(family = "Nunito Bold", face = "bold", size = 14),
        axis.text = element_text(face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "lightgrey")) +
        labs(x = "\nNumber of streams in billions", y = "") +
  scale_x_continuous(limits = c(0, 2.700), expand = c(0, 0),
                     breaks = scales::breaks_pretty(n = 10))
ggsave(file.path(home,"Analysis", "R","r4ds", "font_height_bars.png"), font_height_bars, dpi = 300,
       height = 6, width = 14, units = "in")

# Bars with invisible gridlines

ivis_gridline = df %>%
  filter(num_rank <=10) %>%
  ggplot(aes(x = streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = "#3686d3", width = .9) +
  geom_vline(data = data.frame(x = seq(0, 2.6, .2)),
             aes(xintercept = x), color = "white", size = 0.5) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito Bold", face = "bold", size = 14),
        axis.text = element_text(face = "bold"),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
    
  ) +
  labs(x = "\nNumber of streams in billions", y = "") +
  scale_x_continuous(limits = c(0, 2.7), expand = c(0, 0),
                     breaks = scales::breaks_pretty(n = 10))
  

ggsave(file.path(home,"Analysis", "R","r4ds", "ivis_gridline.png"), ivis_gridline, dpi = 300,
       height = 6, width = 14, units = "in")

# Direct labels

direct_label <- df %>% 
  filter(num_rank <=10) %>% 
  ggplot(aes(x = streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = "#303844", width = .9) +
  geom_text(aes(y = fct_reorder(lab_text, streams), x = streams, label = streams_text),
            color = "white", hjust = 1, fontface = "bold", position = position_nudge(x = -.020)) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito Bold", face = "bold", size = 16),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "",
       x = "") +
  scale_x_continuous(limits = c(0,2.7), expand = c(0, 0),
                     breaks = scales::breaks_pretty(n = 10))

ggsave(file.path(home,"Analysis", "R","r4ds","direct_label.png"), direct_label, dpi = 300,
       height = 6, width = 14, units = "in")

# labels above

label_above <- df %>% 
  filter(num_rank <=10) %>% 
  ggplot(aes(x = streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = "#c2545b", width = .2) +
  geom_text(aes(x = 0, y = fct_reorder(lab_text, streams),  label = lab_text),
            color = "black", hjust = 0, position = position_nudge(y = 0.3),
            fontface = "bold", family = "Nunito Bold", size = 4) +
  geom_text(aes(x = streams, y = fct_reorder(lab_text, streams), label = streams_text),
            color = "#cf7a7f", hjust = 1, position = position_nudge(x = -.02, y = 0.3),
            fontface = "bold", family = "Nunito Bold", size = 4) +
  theme_minimal() +
  theme(text = element_text(family = "Nunito Bold", face = "bold", size = 14),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  labs(y = "",
       x = "") +
  scale_x_continuous(limits = c(0,2.7), expand = c(0, 0),
                     breaks = scales::breaks_pretty(n = 10))

ggsave(file.path(home,"Analysis", "R","r4ds","label_above.png"), label_above, dpi = 300,
       height = 6, width = 14, units = "in")

# Lollipop

lollipop_bar <- df %>% 
  filter(num_rank <=10) %>% 
  ggplot(aes(x = streams, y = fct_reorder(lab_text, streams))) +
  geom_col(fill = "grey", width = .8) +
  geom_point(shape = 21, fill = "orange", color = "black", size = 20, stroke = 1) +
  geom_text(aes(x = streams, y = fct_reorder(lab_text, streams), label = streams),
            color = "black", hjust = 0.5, 
            fontface = "bold") +
  theme_minimal() +
  theme(text = element_text(family = "Nunito Bold", face = "bold", size = 14),
        axis.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_text(hjust = 0)) +
  labs(y = "",
       x = "Number of streams in billions") +
  scale_x_continuous(limits = c(0,2.7), expand = c(0, 0),
                     breaks = scales::breaks_pretty(n = 10)) +
  NULL

ggsave(file.path(home,"Analysis", "R","r4ds","lollipop_bar.png"), lollipop_bar, dpi = 300,
       height = 8, width = 16, units = "in")
