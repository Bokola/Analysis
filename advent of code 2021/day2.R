# --- Day 1: Sonar Sweep ---
#   You're minding your own business on a ship at sea when the overboard alarm goes off! You rush to see if you can help. Apparently, one of the Elves tripped and accidentally sent the sleigh keys flying into the ocean!
# 
# Before you know it, you're inside a submarine the Elves keep ready for situations like this. It's covered in Christmas lights (because of course it is), and it even has an experimental antenna that should be able to track the keys if you can boost its signal strength high enough; there's a little meter that indicates the antenna's signal strength by displaying 0-50 stars.
# 
# Your instincts tell you that in order to save Christmas, you'll need to get all fifty stars by December 25th.
# 
# Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
#   
#   As the submarine drops below the surface of the ocean, it automatically performs a sonar sweep of the nearby sea floor. On a small screen, the sonar sweep report (your puzzle input) appears: each line is a measurement of the sea floor depth as the sweep looks further and further away from the submarine.
# 
# For example, suppose you had the following report:
#   
#   199
# 200
# 208
# 210
# 200
# 207
# 240
# 269
# 260
# 263
# This report indicates that, scanning outward from the submarine, the sonar sweep found depths of 199, 200, 208, 210, and so on.
# 
# The first order of business is to figure out how quickly the depth increases, just so you know what you're dealing with - you never know if the keys will get carried into deeper water by an ocean current or a fish or something.
# 
# To do this, count the number of times a depth measurement increases from the previous measurement. (There is no measurement before the first measurement.) In the example above, the changes are as follows:
# 
# 199 (N/A - no previous measurement)
# 200 (increased)
# 208 (increased)
# 210 (increased)
# 200 (decreased)
# 207 (increased)
# 240 (increased)
# 269 (increased)
# 260 (decreased)
# 263 (increased)
# In this example, there are 7 measurements that are larger than the previous measurement.
# 
# How many measurements are larger than the previous measurement?

ipk <- function(pkg){
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if(length(new.pkg))install.packages(new.pkg, repos = "https://cran.us.r-project.org", dep = T)
  sapply(pkg, require, character.only = T)
}

list.pkg <- c("tidyverse", "magrittr")
ipk(list.pkg)

home <- ifelse(Sys.info()["sysname"] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE")) %>%
  gsub("\\\\", "/",.) %>% file.path(., "Analysis", "advent of code 2021")

data <- read.table(file.path(home, "day2_data.txt"), header = F)  %>% `colnames<-`(c("direction", "distance"))

data <- data %>%
  mutate(f = ifelse(direction == "forward", distance, 0),
         v = ifelse(direction == 'up', distance * -1,
                    ifelse(direction == 'down', distance, 0)))

data %>% summarize(v = sum(v), f = sum(f)) %>% prod()

# --- Part Two ---
#   Based on your calculations, the planned course doesn't seem to make any sense.
#   You find the submarine manual and discover that the process is actually slightly more complicated.
# 
# In addition to horizontal position and depth, you'll also need to track a third value,
# aim, which also starts at 0. The commands also mean something entirely different than you first thought:
#   
#   down X increases your aim by X units.
# up X decreases your aim by X units.
# forward X does two things:
#   It increases your horizontal position by X units.
# It increases your depth by your aim multiplied by X.
# Again note that since you're on a submarine, down and up do the opposite of
# what you might expect: "down" means aiming in the positive direction.
# 
# Now, the above example does something different:
# 
# forward 5 adds 5 to your horizontal position, a total of 5. Because your aim is 0, your depth does not change.
# down 5 adds 5 to your aim, resulting in a value of 5.
# forward 8 adds 8 to your horizontal position, a total of 13. Because your aim is 5, your depth increases by 8*5=40.
# up 3 decreases your aim by 3, resulting in a value of 2.
# down 8 adds 8 to your aim, resulting in a value of 10.
# forward 2 adds 2 to your horizontal position, a total of 15. Because your aim is 10,
# your depth increases by 2*10=20 to a total of 60.
# After following these new instructions, you would have a horizontal position of 15
# and a depth of 60. (Multiplying these produces 900.)
# 
# Using this new interpretation of the commands, calculate the horizontal position
# and depth you would have after following the planned course. What do you get if 
# you multiply your final horizontal position by your final

directions <- tribble(
  ~ direction, ~x, ~ y,
  "down", 0, 1,
  "up", 0, -1,
  "forward", 1, 0
)

joined <- data %>% inner_join(directions, by = "direction")

# part 1
joined %>% 
  summarize(horizontal = sum(distance * x),
            depth = sum(distance * y),
            product = horizontal * depth)

# part 2

joined %>% mutate(aim = cumsum(distance * y)) %>%
  summarize(horizontal = sum(distance * x),
            depth = sum(aim * distance * x),
            product = horizontal * depth)
