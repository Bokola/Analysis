ipk <- function(pkg) {
  new.pkg <- list.pkg[!(list.pkg %in% .packages(all.available = T))]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = T,
                     repos = "https://cran.us.r-project.org")
  sapply(pkg, library, character.only = T)
}
list.pkg <- c("tidyverse", "magrittr", "REDCapR")
ipk(list.pkg)

ggplot(data = mpg) +
  geom_point(aes(x = displ, y = hwy)) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, group = drv), se = F
  )

# distinct values
dat <- mpg
dat %>% distinct(drv, .keep_all = T)
dat %>% group_by(drv) %>% slice(1)
data %>% group_by(drv) %>% filter(row_number() == 1)

dat[!duplicated(dat[c('drv')]),]
# proportions with prop and group
ggplot(data = diamonds) +
  geom_bar(aes(x = cut, y = ..prop.., group = 1))
# using stat_count
ggplot(data = diamonds) +
  stat_count(aes(x = cut))

ggplot(data = diamonds) +
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.min = min,
    fun.max = max,
    fun = median
  )

dayWeek = seq(as.Date(paste0(format(Sys.Date(), "%Y-%m-"), 01)), 
              as.Date(paste0(format(Sys.Date(), "%Y-"), as.numeric(format(Sys.Date(), "%m"))+1, "-01"))-1,1) %>%
  subset(., format(., "%A")=='Tuesday') %>%
  subset(., abs(difftime(., as.Date("2018-05-15"), 
                         units="days")) == min(abs(difftime(., as.Date("2018-05-15"), units="days")))) %>%
  min(.)

# using REDCapR package
df <- redcap_read_oneshot(redcap_uri = "https://redcapdemo.vanderbilt.edu/redcap_v11.2.3/API/",
                          token ="2DA48097E8EF895BD94BC3727F3E18B3")$data
## Not run: 
uri      <- "https://bbmc.ouhsc.edu/redcap/api/"
token    <- "9A81268476645C4E5F03428B8AC3AA7B"


