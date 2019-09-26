# Building a simple animation in R
rm(list = ls(all = T))
# Set project path

home.dir <- ifelse(Sys.info()["sysname"] == "Windows", Sys.getenv("USERPROFILE"), Sys.getenv("HOME"))
m.project.path <- path.expand(file.path(home.dir
                                        ,"Analysis"
                                        ,"TidyTuesday"
))

m.project.path <- gsub("\\\\","/",m.project.path)
if(!file.exists(m.project.path)) {
  if(dir.create(m.project.path, recursive = TRUE))
    stop("The project directory \""
         ,m.project.path
         ,"\"  has been created!\nPlease fill it with the relevant files and folders!")
  else
    stop("The project directory \""
         ,m.project.path
         ,"\"  could not be created!")
}

# Set relevant Directories

# Reporting directory
report.location = file.path(m.project.path, "cache", "doc")
if(!file.exists(report.location)) {
  if(dir.create(report.location, recursive = TRUE))
    stop("The project directory \""
         ,report.location
         ,"\"  has been created!\nPlease fill it with the relevant files and folders!")
  else
    stop("The project directory \""
         ,report.location
         ,"\"  could not be created!")
}
# scripts directory

scripts_location <- file.path(m.project.path, 'src', 'r')
if(!file.exists(scripts_location)) {
  if(dir.create(scripts_location, recursive = T))
    stop('The script directory \''
         , scripts_location
         , '\' has been created!\nPlease fill it with relevant files and folders!')
  else
    stop('The script directory \''
         ,scripts_location
         ,'\' could not be created!')
}

#packages 
ipk <- function(pkg){
  new.pkg <- list.of.pkgs[!(list.of.pkgs %in% installed.packages()[,"Package"])]
  if(length(new.pkg)) install.packages(new.pkg, dep = T)
  sapply(pkg, require, character.only = T)
}
list.of.pkgs <- c("ggplot2", "tidyverse", "gganimate", "directlabels", "png", "transformr", "grid")
ipk(list.of.pkgs)

# Random data
df = data.frame(A=sample(1:75, 50, replace=TRUE),
                B=sample(1:100, 50, replace=TRUE),
                stringsAsFactors = FALSE)
# In animation,a frame is one of the many still images which composes the complete moving picture. Rendering is a kind of computing to output the final result. In `gganimate` package its defaulted to 100 frames. You can change the number of frames under nfframes = parameter in animate function.
p = ggplot(df, aes(A, B)) +
  geom_line() +
  transition_reveal(A) +
  labs(title = 'A: {frame_along}')
animate(p, nframes = 40)
#transition_reveal(A) allows you to let data gradually appear.frame_along gives the position that the current frame corresponds to.
#You can use anim_save(file_location, plot) function to export animated chart in GIF format
# Frames per second (fps) is the amount of time spend on each frame per second. you can use parameter fps in `animate()`. animate(p, nframes = 40, fps = 2). a lower fps means a slower speed. Loop means continuously repeating animation over and over again. To end loop, you can `renderer = gifski_renderer(loop = F)` option in animate function.
# you can change plot layout by specifying width and height options inside animate()
# Advanced animation 
set.seed(123)
dates = paste(rep(month.abb[1:10], each=10), 2018)
df = data.frame(Product=rep(sample(LETTERS[1:10],10), 10),
                Period=factor(dates, levels=unique(dates)),
                Sales=sample(1:100,100, replace = TRUE))
#Ranking by period and sales
df = df %>% 
  arrange(Period, Sales) %>% 
  mutate(order = 1:n())
# Animation
p = df %>% 
  ggplot(aes(order, Sales)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title =  "Total Sales in {closest_state}", x = NULL) +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  scale_x_continuous(breaks = df$order,labels = df$Product, position = "top") + 
  transition_states(Period, transition_length = 1, state_length = 2) + 
  view_follow(fixed_y = T) + 
  ease_aes("cubic-in-out")

animate(p, nframes = 50, fps = 4)
anim_save(file.path(report.location, "bar_animation.gif"), p)

# Detailed Explanation
# 
# transition_states() animates plot by categorical or discrete variable. “States” are the animation sequences which plays. When a state transition is triggered, there will be a new state whose animation sequence will then run. In this case, state is Period column. state_length refers to relative length of the pause at the states. transition_length refers to relative length of the transition.
# view_follow(fixed_y=TRUE) means y-axis would be fixed when animation is running.
# ease_aes( ) refers to motion in animation that starts quickly and then decelerates. Or vice-versa.
# You can set theme using theme_set(theme_minimal())

# Indian general elections
# Read Data
df = read.table(text = 
                  " Year	Perc_Seats	Party
                1984	0.79	INC
                1989	0.38	INC
                1991	0.45	INC
                1996	0.27	INC
                1998	0.27	INC
                1999	0.22	INC
                2004	0.28	INC
                2009	0.4   INC
                2014	0.09	INC
                2019	0.1	  INC
                1984	0	    BJP
                1989	0.17	BJP
                1991	0.23	BJP
                1996	0.31	BJP
                1998	0.35	BJP
                1999	0.35	BJP
                2004	0.27	BJP
                2009	0.23	BJP
                2014	0.52	BJP
                2019	0.56	BJP
                ", header=TRUE)

# Set Theme
theme_set(theme_minimal())

# Plot and animate
p =  
  ggplot(data = df, aes(x= factor(Year), y=Perc_Seats, group=Party, colour=Party)) +
  geom_line(size=2, show.legend = FALSE) +
  scale_color_manual(values=c("#ff9933", "#006400")) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = 'Lok Sabha Election : % of seats won', 
       x = NULL, y = NULL) +
  geom_text(aes(label=scales::percent(Perc_Seats, accuracy = 1),
                vjust= -2), show.legend = FALSE) +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_dl(aes(label=Party), method="last.points") +
  transition_reveal(Year) +
  coord_cartesian(clip = 'off') + 
  ease_aes('cubic-in-out')

animate(p, fps = 10, width = 800, height = 400)
anim_save("election.gif", p)