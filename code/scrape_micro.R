library(tidyverse)
library(rvest)
# the microdata in .csv form are actually quite large, and I'm not sure how to work with them in Git effectively (Git LFS returned some errors about exceeding quota). 
# This script just get the microdata straight from the Census website into RDS files without saving a .csv file anywhere in the repository (only into a temp file stored elsewhere on the computer). 
# it actually takes a while to run though (downloading takes  a while) and isn't really necessary to run twice. 
# updated 7/1 to only download files that don't already exist

links <- read_html("https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html") %>% 
  html_nodes(".uscb-text-link") %>% 
  html_attr("href") %>% 
  str_subset("CSV") %>% 
  str_remove("^/+")

walk(links, function(puf_url) {
  week <- parse_number(str_extract(puf_url, "(?<=Week)\\d+"))
  file_out <- file.path("fetch_data", "microdata", sprintf("week_%s.RDS", week))
  urltools::scheme(puf_url) <- "https"
  if (!file.exists(file_out)) {
    temp <- tempfile()
    download.file(puf_url, temp)
    filename <- unzip(temp, list = T)$Name %>%
      str_subset("pulse2020_puf*")
    read_csv(unz(temp, filename)) %>%
      saveRDS(file_out)
    print(str_glue("Writing file {file_out}"))
    unlink(temp)
  }
}) 

