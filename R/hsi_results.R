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

get_players <- function(page) {
  
  links <- page |> 
    html_elements("a") |> 
    html_attr("href")
  
  links <- links[str_detect(links, "heima=[0-9]+&uti=[0-9]+")]
  
  links <- glue("https://www.hsi.is/{links}") |> 
    as.character()
  
  players <- map(links, read_link)
  
  heima <- players |> 
    map(pluck, 1) |> 
    unlist()
  
  gestir <- players |> 
    map(pluck, 2) |> 
    unlist()
  
  list(heima, gestir)
  
}

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



data <- urls[1:23] |> 
  map(
    \(x) {
      page <- read_html(x)
      table <- page |> 
        html_table() |> 
        pluck(2)
      tab_rows <- nrow(table)
      leikmenn <- get_players(page)
      leikmenn_heima <- character(tab_rows)
      leikmenn_gestir <- character(tab_rows)
      leikmenn_heima[seq_along(leikmenn[[1]])] <- leikmenn[[1]]
      leikmenn_gestir[seq_along(leikmenn[[2]])] <- leikmenn[[2]]
      table$leikmenn_heima <- leikmenn_heima
      table$leikmenn_gestir <- leikmenn_gestir
      table
    }
  )



d <- data[1:23] |> 
  #map(pluck("result")) |> 
  list_rbind(names_to = "timabil")

d <- d |> 
  janitor::clean_names() |> 
  select(
    timabil, dagur, timi, leikur, urslit, leikmenn_heima, leikmenn_gestir
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
    stig_gestir,
    leikmenn_heima,
    leikmenn_gestir
  )

d |> 
  write_csv("data/hsi_results_with_players.csv")
