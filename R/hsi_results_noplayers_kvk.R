library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(readr)
library(tidyr)
library(lubridate)
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

read_link <- function(link) {
  
  page <- link |> 
    read_html()
  
  players <- page |> 
    html_table()
  
  players[[1]]$Leikmaður
  
  players |> 
    map(
      \(x) x |> 
        filter(
          str_detect(
            Leikmaður,
            "^[0-9]+ - "
          )
        ) |> 
        mutate(
          Leikmaður = str_replace(Leikmaður, "^[0-9]+ - ", "")
        )
    ) |> 
    map(pluck, "Leikmaður") |> 
    map(str_c, collapse = ";")
}

base_url <- "https://www.hsi.is/stodutafla/?mot={mot_nr}"

mot_nr <- c(
  "2025" = 7642,
  "2024" = 6982,
  "2023" = 6146,
  "2022" = 5641,
  "2021" = 5261,
  "2020" = 4281,
  "2019" = 3082,
  "2018" = 2621,
  "2017" = 2179,
  "2016" = 2106,
  "2015" = 2033,
  "2014" = 1964,
  "2013" = 1898,
  "2012" = 1811,
  "2011" = 1737,
  "2010" = 1678,
  "2009" = 1594,
  "2008" = 1530,
  "2007" = 1461,
  "2006" = 1362,
  "2005" = 1267,
  "2004" = 1171,
  "2003" = 1037,
  "2002" = 947,
  "2001" = 849,
  "2000" = 738,
  "1999" = 615,
  "1998" = 456,
  "1997" = 328,
  "1996" = 168,
  "1995" = 13
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
  write_csv("data/hsi_results_kvk.csv")
