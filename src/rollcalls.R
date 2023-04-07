library(tidyverse)
library(rvest)
library(data.table)
library(lubridate)

total_df = read.csv("7volebneobdobie.csv")

rollcalls = as.data.frame(total_df$vote_id_total) %>% unique(.)
rownames(rollcalls) = NULL
colnames(rollcalls)[1] ="vote_id_total"
vote_id_total = rollcalls$vote_id_total


rollcall_id_relative = c()
date = c()
time = c()
rollcall_title = c()
rollcall_result = c()


for (i in vote_id_total) {
  URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=schodze/hlasovanie/hlasklub&ID=", i)
  download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)
  page = read_html("scrapedpage.html")
  
  content = page %>% html_nodes(".grid_4.omega span") %>% html_text()
  if (is_empty(content)) {
    rollcall_id_relative = c(rollcall_id_relative, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    rollcall_id_relative = c(rollcall_id_relative, content)
  }
  
  content = page %>% html_nodes(".grid_4+ .alpha span") %>% html_text()
  if (is_empty(content)) {
    rollcall_title = c(rollcall_title, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    rollcall_title = c(rollcall_title, content)
  }
  
  content = page %>% html_nodes("#_sectionLayoutContainer_ctl01_ctl00__votingResultCell span") %>% html_text()
  if (is_empty(content)) {
    rollcall_result = c(rollcall_result, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    rollcall_result = c(rollcall_result, content)
  }
  
  content = page %>% html_nodes(".alpha+ .grid_4 span") %>% html_text()
  if (is_empty(content)) {
    date = c(date, NA)
    time = c(time, NA)
  }
  else{
    content = str_remove(content, "\\s(?=[^\\s]*$)")
    datetime_str <- content
    datetime_obj <- dmy_hm(datetime_str)
    date_obj <- date(datetime_obj)
    date_for = format(date_obj, "%Y-%m-%d")
    time_for <- format(datetime_obj, "%H:%M:%S")
    date = c(date, date_for)
    time = c(time, time_for)
  }
}

df_rollcalls = data.frame(vote_id_total, rollcall_id_relative, rollcall_title, rollcall_result, date, time)

fwrite(df_rollcalls, file ="7volobdobie_rollcalls.csv")