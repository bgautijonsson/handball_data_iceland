library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(readr)
library(tidyr)

url <- "https://hbstatz.is/OlisDeildKarlaLeikir.php"

page <- read_html(url)

d <- page |> 
  html_table() |> 
  list_rbind()

d |> 
  janitor::clean_names() |> 
  select(
    date = date1,
    heima = heimalid,
    uti = utilid,
    leikur
  ) |> 
  separate(
    leikur,
    into = c("mork_heima", "mork_uti"),
    sep = "-"
  ) |> 
  mutate_at(
    vars(starts_with("mork")),
    \(x) str_replace(x, " \\(.*$", "") |> 
        str_squish() |> 
        parse_number()
  ) |> 
  mutate(
    date = lubridate::as_date(date)
  ) |> 
  write_csv("data_results_2425.csv")
