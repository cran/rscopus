## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example, eval = TRUE, message=FALSE--------------------------------------
library(rscopus)
library(dplyr)
authorized = is_elsevier_authorized()
if (have_api_key()) {
  x = abstract_retrieval("S1053811915002700", identifier = "pii",
                         verbose = FALSE)
  res = bibtex_core_data(x)
  cat(res)
  if (authorized) {
    
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
}

