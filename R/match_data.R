library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(readr)

url <- "https://hbstatz.is/OlisDeildKarlaLeikir.php"

site <- read_html(url)

games <- site |> html_table()


links <- site |> 
  html_elements("a") |> 
  html_attr("href")

links <- links[str_detect(links, "ID=")]
links <- str_match(links, "php\\?(ID.*)")[, 2]

links <- str_c(url, links, sep = "?")

links[1]
