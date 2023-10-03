
# data exploration --------------------------------------------------------
rm(list = ls(all.names = T))
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
              "viridis",
              "zoo")
ipk(list.pkg)

dat_file <- list.files(here(), pattern = ".csv")
dat <- read_csv(file.path(here(), dat_file)) %>% janitor::clean_names()
names(dat) <- c("women","men")
dat_l <- dat %>% pivot_longer(
  names_to = "gender", values_to = "shoats", cols = everything()
) %>% as.data.frame()

dat_p <- dat_l %>% group_by(gender) %>% dplyr::summarize(
  #n = n_distinct(gender),
  mean_shoats = mean(shoats)
)

p <- ggplot(dat_p, aes(x = gender, y = mean_shoats)) +
  geom_bar(stat= "identity") +
  ylab("mean no. of shoats by small holders") +
  theme_minimal()
p

ggsave("q1.pdf", path = here(), plot = p, width = 4, height = 4)

# mean and mode

means <- dat_l %>% group_by(gender) %>%
  summarize(
    mean = round(mean(shoats),2)
  )
means <- means %>%  rbind(c("All", 22.17)) 

sort(table(dat_l$shoats))

modes <- data.frame(
  gender = c("men", "women", "All"),
  mode = c(45, "2 or 7", "23 or 6")
)
out <- means %>% left_join(modes)
write_csv(out, file.path(here(), "q2.csv"))
median(dat$women)
