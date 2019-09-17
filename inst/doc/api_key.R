## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, eval = TRUE, message=FALSE---------------------------------
library(rscopus)
library(dplyr)
if (have_api_key()) {
  res = author_df(last_name = "Muschelli", first_name = "John", verbose = FALSE, general = FALSE)
  names(res)
  head(res[, c("title", "journal", "description")])
  unique(res$au_id)
  unique(as.character(res$affilname_1))
  
  all_dat = author_data(last_name = "Muschelli", 
                        first_name = "John", verbose = FALSE, general = TRUE)
  res2 = all_dat$df
  res2 = res2 %>% 
    rename(journal = `prism:publicationName`,
           title = `dc:title`,
           description = `dc:description`)
  head(res[, c("title", "journal", "description")])
}

