## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = TRUE
)

## ------------------------------------------------------------------------
library("rscopus")
library("dplyr")
library("tidyr")

## ------------------------------------------------------------------------
have_api_key()

## ------------------------------------------------------------------------
rscopus::subject_areas()

## ------------------------------------------------------------------------
# create the query
Query <- paste0("AF-ID(60006514) AND SUBJAREA(", 
                subject_areas(), 
                ") AND PUBYEAR = 2018 AND ACCESSTYPE(OA)")

## ------------------------------------------------------------------------
if (have_api_key()) {
  make_query = function(subj_area) {
    paste0("AF-ID(60006514) AND SUBJAREA(", 
           subj_area,
           ") AND PUBYEAR = 2018 AND ACCESSTYPE(OA)")
  }
  i = 3
  subj_area = subject_areas()[i]
  print(subj_area)
  completeArticle <- scopus_search(
    query = make_query(subj_area), 
    view = "COMPLETE", 
    count = 200)
  print(names(completeArticle))
  total_results = completeArticle$total_results
  total_results = as.numeric(total_results)
} else {
  total_results = 0
}

## ------------------------------------------------------------------------
if (have_api_key()) {
  
  # areas = subject_areas()[12:13]  
  areas = c("ENER", "ENGI")
  names(areas) = areas
  results = purrr::map(
    areas,
    function(subj_area) {
      print(subj_area)
      completeArticle <- scopus_search(
        query = make_query(subj_area), 
        view = "COMPLETE", 
        count = 200,
        verbose = FALSE)
      return(completeArticle)
    })
  entries = purrr::map(results, function(x) {
    x$entries
  })
  total_results = purrr::map_dbl(results, function(x) {
    as.numeric(x$total_results)
  })  
  total_results = sum(total_results, na.rm = TRUE)
  
  df = purrr::map(entries, gen_entries_to_df)
  MainEntry = purrr::map_df(df, function(x) {
    x$df
  }, .id = "subj_area")
  
  ddf = MainEntry %>%
               filter(as.numeric(`author-count.$`) > 99)
  if ("message" %in% colnames(ddf)) {
    ddf =  ddf %>% 
               select(message, `author-count.$`)
    print(head(ddf))
  }
  
  MainEntry = MainEntry %>% 
    mutate(
      scopus_id = sub("SCOPUS_ID:", "", `dc:identifier`),
      entry_number = as.numeric(entry_number),
      doi = `prism:doi`)    
  #################################
  # remove duplicated entries
  #################################
  MainEntry = MainEntry %>% 
    filter(!duplicated(scopus_id))  
  
  Authors = purrr::map_df(df, function(x) {
    x$author
  }, .id = "subj_area")  
  Authors$`afid.@_fa` = NULL
  
  Affiliation = purrr::map_df(df, function(x) {
    x$affiliation
  }, .id = "subj_area")    
  Affiliation$`@_fa` = NULL
  
  # keep only these non-duplicated records
  MainEntry_id = MainEntry %>% 
    select(entry_number, subj_area)
  Authors = Authors %>% 
    mutate(entry_number = as.numeric(entry_number))
  
  Affiliation = Affiliation %>% 
    mutate(entry_number = as.numeric(entry_number))    
  
  Authors = left_join(MainEntry_id, Authors)
  Affiliation = left_join(MainEntry_id, Affiliation)
  
  
  # first filter to get only OSU authors
  osuauth <- Authors %>%
    filter(`afid.$` == "60006514")
  
}

## ------------------------------------------------------------------------
if (total_results > 0) {
  cn = colnames(MainEntry)
  cn[grep("fund", tolower(cn))]
  
  tail(sort(table(MainEntry$`fund-sponsor`)))
  
  funderPoland <- filter(
    MainEntry, 
    `fund-sponsor` == "Ministerstwo Nauki i Szkolnictwa Wyższego" )  
  dim(funderPoland)
  
  osuFunders <- MainEntry %>%
    group_by(`fund-sponsor`) %>%
    tally() %>% 
    arrange(desc(n))
  osuFunders
}

## ------------------------------------------------------------------------
if (total_results > 0) {

  # if there are 100+ authors, you have to use the abstract_retrieval function to get the full author data
  # coerce to integer first
  MainEntry <- MainEntry %>% 
    mutate(`author-count.$` = as.integer(`author-count.$`))
  run_multi = any(MainEntry$`author-count.$` > 99)
  print(run_multi)
}

## ------------------------------------------------------------------------
if (total_results > 0) {
  if (run_multi) {
    
    MainEntry_99auth <- MainEntry %>%
      filter(`author-count.$` > 99)
    
    MainEntry_99auth_id = MainEntry_99auth %>% 
      select(entry_number, subj_area)
    auth99 = left_join(MainEntry_99auth_id, Authors)
    affil99 = left_join(MainEntry_99auth_id, Affiliation)

    
    missing_table = MainEntry %>% 
      ungroup() %>% 
      mutate_at( vars(scopus_id, doi), .funs = is.na) %>% 
      summarize_at(.vars = vars(scopus_id, doi), .funs = sum)
    print(missing_table)
  }
}

## ------------------------------------------------------------------------
if (total_results > 0) {
  # ids = MainEntry_99auth$scopus_id[1:3]
  ids = MainEntry_99auth$scopus_id
  names(ids) = ids
  big_list = purrr::map(
    ids, 
    abstract_retrieval, 
    identifier = "scopus_id",
    verbose = FALSE)
  
  all_affil_df = purrr::map_df(
    big_list, function(x) {
      d = gen_entries_to_df(
        x$content$`abstracts-retrieval-response`$affiliation)
      d$df
    }, .id = "scopus_id")  
  
  all_df = purrr::map_df(
    big_list, function(x) {
      d = gen_entries_to_df(
        x$content$`abstracts-retrieval-response`$authors$author)
      d$df
    }, .id = "scopus_id")
  
  ##########################
  # Remove prefix ce: for harmonization
  ##########################
  no_ce = function(x) {
    sub("^ce:", "", x)
  }
  all_df = all_df %>% 
    rename_all(.funs = no_ce) %>% 
    rename(authid = "@auid",
           `afid.$` = `affiliation.@id`,
           authname = "indexed-name")
  
  all_df$entry_number = NULL
  all_affil_df$entry_number = NULL
  
  author_table = all_df %>% 
    group_by(scopus_id) %>% 
    distinct(authid) %>% 
    tally()
  head(author_table)
  stopifnot(all(ids %in% author_table$scopus_id))
  # harmonizing with MainEntry
  author_table = author_table %>% 
    rename(`author-count.$` = n)
  MainEntry_99auth$`author-count.$` = NULL
  MainEntry_99auth = left_join(MainEntry_99auth, author_table)
  
  
  #######################
  # Harmonized
  #######################
  all_df = MainEntry_99auth %>% 
    select(entry_number, subj_area, scopus_id) %>% 
    left_join(all_df)
  print(setdiff(colnames(Authors), colnames(all_df)))
  
  # grab only relevant columns
  all_df = all_df[, colnames(Authors)]
  
  # remove the old entries
  Authors = anti_join(Authors, MainEntry_99auth_id)
  # put the new data in
  Authors = full_join(Authors, all_df)
  
  #######################
  # Harmonized
  #######################  
  all_affil_df = all_affil_df %>% 
    rename(`affiliation-url` = "@href",
           afid = "@id")
  all_affil_df = MainEntry_99auth %>% 
    select(entry_number, subj_area, scopus_id) %>% 
    left_join(all_affil_df)  
  setdiff(colnames(Affiliation), colnames(all_affil_df))  
  
  # remove the old entries
  Affiliation = anti_join(Affiliation, MainEntry_99auth_id)
  # put the new data in
  Affiliation = full_join(Affiliation, all_affil_df)
  
  MainEntry = anti_join(MainEntry, MainEntry_99auth_id)
  MainEntry = full_join(MainEntry, MainEntry_99auth)
}

