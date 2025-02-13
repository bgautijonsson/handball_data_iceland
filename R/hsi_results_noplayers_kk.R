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
  "2025" = 7641,
  "2024" = 6983,
  "2023" = 6149,
  "2022" = 5640,
  "2021" = 5260,
  "2020" = 4280,
  "2019" = 3080,
  "2018" = 2620,
  "2017" = 2180,
  "2016" = 2105,
  "2015" = 2032,
  "2014" = 1963,
  "2013" = 1897,
  "2012" = 1810,
  "2011" = 1736,
  "2010" = 1677,
  "2009" = 1593,
  "2008" = 1529,
  "2007" = 1460,
  "2006" = 1365,
  "2005" = 1334,
  "2004" = 1255,
  "2003" = 1036,
  "2002" = 949,
  "2001" = 848,
  "2000" = 737,
  "1999" = 614,
  "1998" = 451,
  "1997" = 327,
  "1996" = 166,
  "1995" = 11
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
  write_csv("data/hsi_results_kk.csv")
