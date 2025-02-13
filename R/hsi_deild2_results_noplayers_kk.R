library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(readr)
library(tidyr)
library(lubridate)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

base_url <- "https://www.hsi.is/stodutafla/?mot={mot_nr}"

mot_nr <- c(
  "2025" = 7644,
  "2024" = 6981,
  "2023" = 6143,
  "2022" = 5643,
  "2021" = 5262,
  "2020" = 4301,
  "2019" = 3083,
  "2018" = 2622,
  "2017" = 2181,
  "2016" = 2107,
  "2015" = 2034,
  "2014" = 1965,
  "2013" = 1899,
  "2012" = 1812,
  "2011" = 1738,
  "2010" = 1679
)

urls <- glue(base_url) |> as.character()
names(urls) <- names(mot_nr)



data <- urls |> 
  map(
    \(x) {
      page <- read_html(x)
      table <- page |> 
        html_table() |> 
        pluck(2)
      table
    }
  )



d <- data |> 
  list_rbind(names_to = "timabil")

d <- d |> 
  janitor::clean_names() |> 
  select(
    timabil, dagur, timi, leikur, urslit
  ) |> 
  mutate(
    dags = dmy_hm(str_c(dagur, timi, sep = " ")),
    heima = str_match(leikur, "^(.*)\\\n-")[, 2],
    gestir = str_match(leikur, " (.*)$")[, 2] |> str_squish()
  ) |> 
  separate(
    urslit,
    into = c("stig_heima", "stig_gestir"),
    convert = TRUE
  ) |> 
  select(
    timabil,
    dags,
    heima,
    gestir,
    stig_heima,
    stig_gestir
  )

d |> 
  write_csv("data/hsi_deild2_results_kk.csv")
