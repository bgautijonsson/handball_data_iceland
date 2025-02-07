library(tidyverse)
library(metill)
theme_set(theme_metill())
Sys.setlocale("LC_ALL", "is_IS.UTF-8")

d <- read_csv("data/hsi_results.csv")

d |> 
  ggplot(aes(dags, stig_heima + stig_gestir)) +
  geom_point()
