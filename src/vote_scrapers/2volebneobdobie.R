library(rvest)
library(tidyverse)
library(data.table)
library(rlang)


URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=schodze/hlasovanie/hlasklub&ID=", "3066", collapse = NULL)

download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE)

page = read_html("scrapedpage.html")

content = page %>% html_nodes("#_sectionLayoutContainer_ctl01__resultsTable td") %>% html_text()

vote_df = data.frame(content, stringsAsFactors = FALSE) %>% filter(., content != "")

#loops that assign party aff to each MP at the time of the vote

for (i in 1:nrow(vote_df)) {
  if (grepl("Klub HZDS", vote_df[i,1])) {
    vote_df[i,2] <- "Klub HZDS"
  } else if (grepl("Klub SDK", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SDK"
  } else if (grepl("Klub SDĽ", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SDĽ"
  } else if (grepl("Klub SMK", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SMK"
  } else if (grepl("Klub SNS", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SNS"
  } else if (grepl("Klub SOP", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SOP"
  } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
    vote_df[i,2] <- "Klub Nezávislí"
  } else if (grepl("Klub KDH", vote_df[i,1])) {
    vote_df[i,2] <- "Klub KDH"
  } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
    vote_df[i,2] <- "Klub SOP-LDÚ-SZS"
  } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
    vote_df[i,2] <- "Poslanci, ktorí nie sú členmi poslaneckých klubov"
  } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
    vote_df[i,2] <- "Klub Nezávislí"
  } else if (grepl("Klub PSNS", vote_df[i,1])) {
    vote_df[i,2] <- "Klub PSNS"
  } else {
    vote_df[i,2] <- ""
  }
}

for (i in 1:nrow(vote_df)) {
  if (grepl("Klub SDK", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SDK"
    }
  } else if (grepl("Klub HZDS", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub HZDS"
    }
  } else if (grepl("Klub SDĽ", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SDĽ"
    }
  } else if (grepl("Klub SMK", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SMK"
    }
  } else if (grepl("Klub SNS", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SNS"
    }
  } else if (grepl("Klub SOP", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SOP"
    }
  } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub Nezávislí"
    }
  } else if (grepl("Klub KDH", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub KDH"
    }
  } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub SOP-LDÚ-SZS"
    }
  } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Poslanci, ktorí nie sú členmi poslaneckých klubov"
    }
  } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub Nezávislí"
    }
  } else if (grepl("Klub PSNS", vote_df[i,1])) {
    for (j in i:nrow(vote_df)) {
      vote_df[j,2] <- "Klub PSNS"
    }
  }
}

#loop for removing everything that isnt the full name of the MP and their vote

for (i in 1:nrow(vote_df)) {
  if (grepl("Klub SDK", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub HZDS", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub SDĽ", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub SMK", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub SNS", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub SOP", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub KDH", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else if (grepl("Klub PSNS", vote_df[i,1])) {
    vote_df <- vote_df[-i,,drop=F]
  } else {
  }
}

vote_df$mp_id = page %>% html_elements("#_sectionLayoutContainer_ctl01__resultsTable a") %>% html_attr("href") %>% str_extract(.,"(?<=PoslanecID=)[^&]*")

vote_df$vote <- vote_df$content %>% str_extract(.,"\\[([^\\n\\r])\\]")

vote_df$last_name <- vote_df$content %>% str_extract(.,"(?<=[?ZNP0]])[^,]*") %>% str_extract_all(.,"[^\\s]+")

vote_df$first_name <- vote_df$content %>% str_extract(.,", \\w+") %>% str_extract(.,"[^, ]+")

vote_df$vote_id_interm = page %>% html_nodes(".grid_4.omega span") %>% html_text() %>% str_extract(.,"[^\\s]+")

vote_df$vote_id_total = "3066"

col_order <- c("vote_id_interm", "vote_id_total", "mp_id", "V2", "first_name", "last_name", "vote", "content")
vote_df <- vote_df[, col_order]
total_df = vote_df


for (k in 3067:10959) {
  unlink("scrapedpage.html")
  URL_to_page = paste0("https://www.nrsr.sk/web/Default.aspx?sid=schodze/hlasovanie/hlasklub&ID=", k, collapse = NULL)
  try(download.file(URL_to_page, destfile = "scrapedpage.html", quiet=TRUE))
  test = file.exists("scrapedpage.html")

  if (test==F) {
    k = k-1
    next
  }
  else{
    page = try(read_html("scrapedpage.html"))
    content = page %>% html_nodes("#_sectionLayoutContainer_ctl01__resultsTable td") %>% html_text()
    if (is_empty(content)) {
      next
    }
    else{
      vote_df = data.frame(content, stringsAsFactors = FALSE) %>% filter(., content != "")
      
      #loops that assign party aff to each MP at the time of the vote
      
      for (i in 1:nrow(vote_df)) {
        if (grepl("Klub HZDS", vote_df[i,1])) {
          vote_df[i,2] <- "Klub HZDS"
        } else if (grepl("Klub SDK", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SDK"
        } else if (grepl("Klub SDĽ", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SDĽ"
        } else if (grepl("Klub SMK", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SMK"
        } else if (grepl("Klub SNS", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SNS"
        } else if (grepl("Klub SOP", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SOP"
        } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
          vote_df[i,2] <- "Klub Nezávislí"
        } else if (grepl("Klub KDH", vote_df[i,1])) {
          vote_df[i,2] <- "Klub KDH"
        } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
          vote_df[i,2] <- "Klub SOP-LDÚ-SZS"
        } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
          vote_df[i,2] <- "Poslanci, ktorí nie sú členmi poslaneckých klubov"
        } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
          vote_df[i,2] <- "Klub Nezávislí"
        } else if (grepl("Klub PSNS", vote_df[i,1])) {
          vote_df[i,2] <- "Klub PSNS"
        } else {
          vote_df[i,2] <- ""
        }
      }
      
      for (i in 1:nrow(vote_df)) {
        if (grepl("Klub SDK", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SDK"
          }
        } else if (grepl("Klub HZDS", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub HZDS"
          }
        } else if (grepl("Klub SDĽ", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SDĽ"
          }
        } else if (grepl("Klub SMK", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SMK"
          }
        } else if (grepl("Klub SNS", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SNS"
          }
        } else if (grepl("Klub SOP", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SOP"
          }
        } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub Nezávislí"
          }
        } else if (grepl("Klub KDH", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub KDH"
          }
        } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub SOP-LDÚ-SZS"
          }
        } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Poslanci, ktorí nie sú členmi poslaneckých klubov"
          }
        } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub Nezávislí"
          }
        } else if (grepl("Klub PSNS", vote_df[i,1])) {
          for (j in i:nrow(vote_df)) {
            vote_df[j,2] <- "Klub PSNS"
          }
          
        }
      }
      
      #loop for removing everything that isnt the full name of the MP and their vote
      
      for (i in 1:nrow(vote_df)) {
        if (grepl("Klub SDK", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub HZDS", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub SDĽ", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub SMK", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub SNS", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub SOP", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub Nezávislý", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub KDH", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub SOP-LDÚ-SZS", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Poslanci, ktorí nie sú členmi poslaneckých klubov", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub Nezávislí", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else if (grepl("Klub PSNS", vote_df[i,1])) {
          vote_df <- vote_df[-i,,drop=F]
        } else {
        }
      }
      
      vote_df$mp_id = page %>% html_elements("#_sectionLayoutContainer_ctl01__resultsTable a") %>% html_attr("href") %>% str_extract(.,"(?<=PoslanecID=)[^&]*")
      
      vote_df$vote <- vote_df$content %>% str_extract(.,"\\[([^\\n\\r])\\]")
      
      vote_df$last_name <- vote_df$content %>% str_extract(.,"(?<=[?ZNP0]])[^,]*") %>% str_extract_all(.,"[^\\s]+")
      
      vote_df$first_name <- vote_df$content %>% str_extract(.,", \\w+") %>% str_extract(.,"[^, ]+")
      
      vote_df$vote_id_interm = page %>% html_nodes(".grid_4.omega span") %>% html_text() %>% str_extract(.,"[^\\s]+")
      
      vote_df$vote_id_total = k
      
      col_order <- c("vote_id_interm", "vote_id_total", "mp_id", "V2", "first_name", "last_name", "vote", "content")
      vote_df <- vote_df[, col_order]
      total_df <- rbind(total_df, vote_df)
    }
  }
}

fwrite(total_df, file ="2volebneobdobie.csv")

