#looks for all possible aliases used within the National Council website
library(rvest)
library(tidyr)

k=37162
URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=schodze/hlasovanie/hlasklub&ID=", k, collapse = NULL)
download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)
page = read_html("scrapedpage.html")
content = page %>% html_nodes(".hpo_result_block_title") %>% html_text()
my_df <- data.frame(content)
total_df <- separate_rows(my_df, content, sep = ",")
colnames(total_df) <- "my_col"

for (k in seq(from=37163, to=43652, by=12)) {
  URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=schodze/hlasovanie/hlasklub&ID=", k, collapse = NULL)
  download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)
  page = read_html("scrapedpage.html")
  content = page %>% html_nodes(".hpo_result_block_title") %>% html_text()
  my_df <- data.frame(content)
  my_df <- separate_rows(my_df, content, sep = ",")
  colnames(my_df) <- "my_col"
  total_df = rbind(total_df, my_df)
  total_df = unique(total_df)
}