cat('Performing Data input ....!\n\n')

rawDataUrl <- "https://catalogue.data.gov.bc.ca/dataset/e43be180-7511-4e6f-84d3-ad6c9f5c3e2b/resource/79cfd06f-8f3e-41b6-a411-4b843ee39236/download/bc_liquor_store_product_price_list_jun_2019.csv"

options(stringsAsFactors = FALSE)
bcl <- read.csv(rawDataUrl, stringsAsFactors = FALSE) %>% apply(.,  2, tolower) %>%
  as.data.frame()
#bcl = apply(bcl, 2, tolower) %>% as.data.frame(.)


bcl <- dplyr::filter(bcl, ITEM_CATEGORY_NAME %in% c("beer", "refreshment beverages", "spirits", "wine")) %>%
  dplyr::rename(type = ITEM_CATEGORY_NAME, subtype = ITEM_SUBCATEGORY_NAME, country = PRODUCT_COUNTRY_ORIGIN_NAME, alcohol_content = PRODUCT_ALCOHOL_PERCENT, price = PRODUCT_PRICE, sweetness = SWEETNESS_CODE) %>%
  select(., type, subtype, country, alcohol_content,price,sweetness) %>%
  mutate(., type = ifelse(grepl('refr', type), 'refreshment', type))
bcl = bcl %>%
mutate(., price = as.numeric(price), alcohol_content = as.numeric(alcohol_content))


