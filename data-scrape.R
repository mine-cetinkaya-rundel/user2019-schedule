# load packages ----------------------------------------------------------------
library(rvest)
library(tidyverse)
library(glue)
library(tools)

# read schedule page -----------------------------------------------------------
page <- read_html("http://www.user2019.fr/talk_schedule/")

# extract table ----------------------------------------------------------------
tabs <- page %>%
  html_table("td", header = TRUE)

# function to process data -----------------------------------------------------
process_schedule <- function(day_tab, day_name){
  
  # remove unused columns ----
  raw <- day_tab %>% select(-2, -Slides)
  
  # create talks_long ----
  talks_long <- raw %>%
    slice(seq(1, nrow(raw), by = 2)) %>%
    mutate(info = as.character(glue("{Title} <br><br> _{Speaker}_")))
  
  # create talks_wide ----
  talks_wide <- talks_long %>%
    select(Time, Room, info) %>%
    pivot_wider(names_from = Room, values_from = info) %>%
    select(Time, `Concorde 1+2`, `Cassiopée`, `Caravelle 2`, 
           `Saint-Exupéry`, `Ariane 1+2`, `Guillaumet 1+2`)
  
  # create abstracts_long ----
  abstracts_long <- raw %>%
    slice(seq(2, nrow(raw), by = 2)) %>%
    rename(Abstract = Time) %>%
    select(Abstract) %>%
    bind_cols(talks_long, .) %>%
    mutate(Day = toTitleCase(day_name)) %>%
    select(Day, Time, Title, Speaker, Abstract, Session, Room, Chair)
  
  # write out ----
  write_csv(talks_wide, glue("data/{day_name}_talks_wide.csv"))
  write_csv(abstracts_long, glue("data/{day_name}_abstracts_long.csv"))
  
}

# pricess days -----------------------------------------------------------------
process_schedule(tabs[[1]], "wed")
process_schedule(tabs[[2]], "thu")
process_schedule(tabs[[3]], "fri")
