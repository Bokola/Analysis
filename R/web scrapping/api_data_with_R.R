# text mining with R - twitter api
browseURL('https://www.earthdatascience.org/courses/earth-analytics/
          get-data-using-apis/intro-to-programmatic-data-access-r/')
ipk = function(pkg) {
  new.pkg = list.pkg[!(list.pkg %in% .packages(all.available = TRUE))]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE, repos = 'https://cran.us.r-project.org')
  ddpcr::quiet(sapply(pkg, require, character.only = TRUE))
}
list.pkg = c('magrittr',
             'tidyverse',
             'rtweet',
             'tidytext',
             'RCurl',
             'jsonlite',
             'httr',
             'RSocrata',
             'RcppSimdJson',
             'rbenchmark',
             'rjson',
             'curl')

ipk(list.pkg)

home = ifelse(Sys.info()['sysname'] == "Windows",
              Sys.getenv("USERPROFILE"),
              Sys.getenv("HOME"))
home = gsub("\\\\", "/", home)
data_dir = file.path(
  home,
  "Downloads"
)
# link to SODA parameters for accessing data
browseURL('https://dev.socrata.com/foundry/data.colorado.gov/tv8u-hswn')
df = read.socrata('https://data.colorado.gov/resource/tv8u-hswn.json?
                  $where=age between 20 and 40 and year between 2016 and 2025&county=Boulder&$select=year,age,femalepopulation')
res = GET('https://data.colorado.gov/resource/tv8u-hswn.json?county=Boulder')
# RcppSimdJson faster by 15 seconds.
browseURL('https://stackoverflow.com/questions/63971243/increase-the-speed-of-pulling-data-from-a-rest-api-in-r')
benchmark(value1 <- fromJSON(rawToChar(res$content)),
          value2 <- RcppSimdJson::fparse(res$content), 
          replications=1000
)
# locating api endpoints
browseURL('https://dev.socrata.com/docs/endpoints.html')

# download gapminder data with RCurl
file_url = 'https://raw.githubusercontent.com/jennybc/gapminder/master/inst/extdata/gapminder.tsv'
gap_data = readr::read_tsv(file_url) # tab separated

# create base url
# replace https with http for windows
base_url = 'http://data.colorado.gov/resource/tv8u-hswn.json?'
full_url = paste0(base_url, 'county=Boulder',
                 '&$where=age between 20 and 40',
                 '&$select=year, age, femalepopulation')
full_url
# use URLencode() to replace spaces in your url with ascii value for space - %20
# use fromJSON() function from `rjson` package to to import the data into a dataframe object
full_url = URLencode(full_url)
female_proj_data = RcppSimdJson::fparse(getURL(full_url))


