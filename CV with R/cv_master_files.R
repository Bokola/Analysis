# This script builds both the HTML and PDF versions of your CV

# If you wanted to speed up rendering for googlesheets driven CVs you could use
# this script to cache a version of the CV_Printer class with data already
# loaded and load the cached version in the .Rmd instead of re-fetching it twice
# for the HTML and PDF rendering. This exercise is left to the reader.

# 1 - render_cv.r file ----------------------------------------------------



# Knit the HTML version
home = ifelse(Sys.info()[["sysname"]] == "Linux", Sys.getenv("HOME"), Sys.getenv("USERPROFILE"))
home = gsub("\\\\", "/", home)

dir = file.path(home, "Analysis", "CV with R")
datadrivencv::use_datadriven_cv(full_name = file.path(dir,"Basil Owiti Okola"))

setwd(dir)


# Knit the HTML version
rmarkdown::render("cv.rmd",
                  params = list(pdf_mode = FALSE),
                  output_file = "CV_Basil Owiti Okola.html")

# Knit the PDF version to temporary html location
tmp_html_cv_loc <- fs::file_temp(ext = ".html")
rmarkdown::render("cv.rmd",
                  params = list(pdf_mode = TRUE),
                  output_file = tmp_html_cv_loc)

# Convert to PDF using Pagedown
pagedown::chrome_print(input = tmp_html_cv_loc,
                       output = "CV_Basil Owiti Okola.pdf")




# 2 - cv.rmd file ---------------------------------------------------------





---
  title: "Basil Owiti Okola's CV"
author: Basil Owiti Okola
date: "`r Sys.Date()`"
params:
  pdf_mode:
  value: true
output:
  pagedown::html_resume:
  css: ['dd_cv.css', 'resume']
self_contained: true
---
  
  ```{r, include=FALSE}
knitr::opts_chunk$set(
  results='asis', 
  echo = FALSE
)

library(magrittr) # For the pipe
source("cv_printing_functions.r")

# Read in all data and initialize a CV printer object
CV <- create_CV_object(
  data_location = "https://docs.google.com/spreadsheets/d/1Cq7X-TA6iR0ZVFOD5QkfGxvZOWg3Zz2XnwAMz4YMInE",  
  pdf_mode = params$pdf_mode
)

# CV <- readr::read_rds('cached_positions.rds')
```


```{r}
# When in pdf export mode the little dots are unaligned, so fix that with some conditional CSS.
if(params$pdf_mode) {
  cat("
<style>
:root{
  --decorator-outer-offset-left: -6.5px;
}
</style>")
}
```


Aside
================================================================================
  
  ```{r}
# Build interactive network of positions colored by section
# and connected if they occurred in the same year
datadrivencv::build_network_logo(CV$entries_data)
```


```{r, results="asis"}
if(params$pdf_mode){
  cat("View this CV online with links at https://github.com/Bokola/Analysis/blob/master/CV%20with%20R/cv_Basil Owiti Okola.html")
} else {
  cat("[<i class='fas fa-download'></i> Download a PDF of this CV](https://github.com/Bokola/Analysis/blob/master/CV%20with%20R/cv_Basil Owiti Okola.pdf)")
}
```

Contact {#contact}
  --------------------------------------------------------------------------------
    
    ```{r}
  CV %>% print_contact_info()
  ```
  
  
  
  Language Skills {#skills}
    --------------------------------------------------------------------------------
      
      ```{r}
    CV %>% print_skill_bars()
    ```
    
    
    
    
    Main
    ================================================================================
      
      Basil Owiti Okola {#title}
        --------------------------------------------------------------------------------
          
          Leveraging statistical, programming and analytic skills in managing, analyzing data and communicating key findings that inform 
        policy makers in their day-today decisions
        
        ```{r}
        # Note the special double pipe so we modify the CV object in place
        # CV %<>% print_text_block("intro") 
        ```
        
        
        
        Education {data-icon=graduation-cap data-concise=true}
        --------------------------------------------------------------------------------
          
          ```{r}
        CV %<>% print_section('education')
        ```
        
        
        
        Work Experience {data-icon=laptop}
        --------------------------------------------------------------------------------
          
          ```{r}
        CV %<>% print_section('work_experience')
        ```
        
        
        
        Internships {data-icon=chalkboard-teacher}
        --------------------------------------------------------------------------------
          
          
          ```{r}
        CV %<>% print_text_block('Internship')
        ```
        
        