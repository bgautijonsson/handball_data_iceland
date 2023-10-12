library(rvest)
library(dplyr)
library(purrr)
library(glue)
library(stringr)
library(readr)
d <- tribble(
  ~sex, ~type, ~data,
  "women", "players", list(html_table(read_html("https://hbstatz.is/test2hf.php"))[[1]]),
  "women", "goal keepers", list(html_table(read_html("https://hbstatz.is/test2hgkf.php"))[[1]]),
  "women", "teams", list(html_table(read_html("https://hbstatz.is/test2htf.php"))[[1]]),
  "men", "players", list(html_table(read_html("https://hbstatz.is/test2h.php"))[[1]]),
  "men", "goal keepers", list(html_table(read_html("https://hbstatz.is/test2hgk.php"))[[1]]),
  "men", "teams", list(html_table(read_html("https://hbstatz.is/test2ht.php"))[[1]])
)

remove_double_team_column <- function(datalist) {
  data <- datalist[[1]]
  drop_col <- which(names(data) == "Lið")[-1]
  
  data[, -drop_col]
}

d <- d |> 
  mutate(
    data = map(data, remove_double_team_column)
  )


d <- d |> 
  mutate(
    type = str_replace(type, " ", "-"),
    path = glue("handball_data_{sex}_{type}.csv")
  )

map2(
  d$data, d$path, write_csv
)
