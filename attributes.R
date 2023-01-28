library(tidyverse)
library(rvest)
library(data.table)

total_df = read.csv("7volebneobdobie.csv")

attributes = as.data.frame(total_df$mp_id) %>% unique(.)
rownames(attributes) = NULL
colnames(attributes)[1] ="mp_id"

first_name = c()
last_name = c()
title = c()
cand_for = c()
birth = c()
natio = c()
place_resid = c()
region_resid = c()


for (i in 1:nrow(attributes)) {
  URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=poslanci/poslanec&PoslanecID=", attributes$mp_id[i], "&CisObdobia=7", collapse = NULL)
  download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)
  page = read_html("scrapedpage.html")
  
  content = page %>% html_nodes("#_sectionLayoutContainer__panelContent .grid_4:nth-child(1) span") %>% html_text()
  if (is_empty(content)) {
    first_name = c(first_name, NA)
  }
  else{
    first_name = c(first_name, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(3) span") %>% html_text()
  if (is_empty(content)) {
    last_name = c(last_name, NA)
  }
  else{
    last_name = c(last_name, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(2) span") %>% html_text()
  if (is_empty(content)) {
    title = c(title, NA)
  }
  else{
    title = c(title, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(4) span") %>% html_text()
  if (is_empty(content)) {
    cand_for = c(cand_for, NA)
  }
  else{
    cand_for = c(cand_for, content)
  }
  
  content = page %>% html_nodes(".alpha+ .grid_4.alpha span") %>% html_text()
  if (is_empty(content)) {
    birth = c(birth, NA)
  }
  else{
    birth = c(birth, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(6) span") %>% html_text()
  if (is_empty(content)) {
    natio = c(natio, NA)
  }
  else{
    natio = c(natio, content)
  }
  
  content = page %>% html_nodes(".grid_4+ .grid_4.alpha span") %>% html_text()
  if (is_empty(content)) {
    place_resid = c(place_resid, NA)
  }
  else{
    place_resid = c(place_resid, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(8) span") %>% html_text()
  if (is_empty(content)) {
    region_resid = c(region_resid, NA)
  }
  else{
    region_resid = c(region_resid, content)
  }
}

attributes$first_name <- first_name
attributes$last_name <- last_name
attributes$title <- title
attributes$cand_for <- cand_for
attributes$birth <- birth
attributes$natio <- natio
attributes$place_resid <- place_resid
attributes$region_resid <- region_resid

fwrite(attributes, file ="7volobdobie_attributes.csv")
