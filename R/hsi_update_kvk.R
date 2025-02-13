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
  "2025" = 7642
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
  ) |> 
  mutate(
    timabil = as.numeric(timabil)
  )


old_data <- read_csv(
  "data/hsi_results_kk.csv"
) 

old_data |> 
  filter(timabil < 2025) |>
  bind_rows(d) |> 
  drop_na() |> 
  write_csv("data/hsi_results_kk.csv")
