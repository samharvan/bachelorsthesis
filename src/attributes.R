library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

total_df = read.csv("7volebneobdobie.csv")

attributes = as.data.frame(total_df$mp_id) %>% unique(.)
rownames(attributes) = NULL
colnames(attributes)[1] ="mp_id"
mps = attributes$mp_id

first_name = c()
last_name = c()
title = c()
cand_for = c()
birth = c()
natio = c()
place_resid = c()
region_resid = c()

#nrow(attributes)
for (i in mps) {
  URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=poslanci/poslanec&PoslanecID=", i, "&CisObdobia=7", collapse = NULL)
  download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)
  page = read_html("scrapedpage.html")
  
  content = page %>% html_nodes("#_sectionLayoutContainer__panelContent .grid_4:nth-child(1) span") %>% html_text()
  if (is_empty(content)) {
    first_name = c(first_name, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    first_name = c(first_name, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(3) span") %>% html_text()
  if (is_empty(content)) {
    last_name = c(last_name, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    last_name = c(last_name, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(2) span") %>% html_text()
  if (is_empty(content)) {
    title = c(title, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    title = c(title, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(4) span") %>% html_text()
  if (is_empty(content)) {
    cand_for = c(cand_for, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    cand_for = c(cand_for, content)
  }
  
  content = page %>% html_nodes(".alpha+ .grid_4.alpha span") %>% html_text()
  if (is_empty(content)) {
    birth = c(birth, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    date_parts <- strsplit(content, "\\. ")
    day <- as.integer(date_parts[[1]][1])
    month <- as.integer(date_parts[[1]][2])
    year <- as.integer(date_parts[[1]][3])
    date_obj <- ymd(sprintf("%04d-%02d-%02d", year, month, day))
    content = format(date_obj, "%Y-%m-%d")
    birth = c(birth, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(6) span") %>% html_text()
  if (is_empty(content)) {
    natio = c(natio, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    natio = c(natio, content)
  }
  
  content = page %>% html_nodes(".grid_4+ .grid_4.alpha span") %>% html_text()
  if (is_empty(content)) {
    place_resid = c(place_resid, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    place_resid = c(place_resid, content)
  }
  
  content = page %>% html_nodes(".omega:nth-child(8) span") %>% html_text()
  if (is_empty(content)) {
    region_resid = c(region_resid, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    region_resid = c(region_resid, content)
  }
}


attributes = data.frame(mp_id = mps, first_name = first_name, last_name = last_name, title = title, cand_for = cand_for, birth = birth, natio = natio, place_resid = place_resid, region_resid = region_resid)

fwrite(attributes, file ="7volobdobie_attributes.csv")
