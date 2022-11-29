library(tidyverse)
library(jsonlite)


## simulation data used for map small multiples --------------------------------

df_simus_wide <-
  fromJSON(here::here("data", "simulations.json")) %>% 
  as.data.frame() %>% 
  rename("winner_overall" = winner) %>% 
  unnest(cols = c(evs, states)) %>% 
  unnest() %>% 
  pivot_longer(
    cols = -c(winner_overall, Biden, Trump),
    names_to = "var",
    values_to = "val"
  )

df_simus <- 
  df_simus_wide %>% 
  mutate(
    sim = if_else(val == "AK", row_number(), NA_integer_)
  ) %>% 
  fill(sim, .direction = "down") %>% 
  mutate(sim = as.numeric(as.factor(sim))) %>% 
  group_by(sim) %>% 
  mutate(state_id = ((row_number() - 1) %/% 3) + 1) %>% 
  dplyr::select(-Biden, -Trump) %>% 
  mutate(var = stringr::str_remove(var, "[0-9]+")) %>%
  filter(state_id != 23) %>% 
  pivot_wider(
    id_cols = c(sim, winner_overall, state_id),
    names_from = var,
    values_from = val
  ) %>% 
  ungroup() %>% 
  dplyr::select(-state_id)

## save data
readr::write_csv(df_simus, here::here("data", "simulations.csv"))



## Probability over time for time series ---------------------------------------

df_probs <-
  fromJSON(here::here("data", "probability_over_time.json")) %>% 
  as.data.frame() %>% 
  unnest(c(candidates)) %>% 
  unnest(c(dates)) %>% 
  unnest() %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  rename(
    "electoral_votes_avg" = mean,
    "electoral_votes_upr" = hi, 
    "electoral_votes_lwr" = lo,
    "popular_votes_avg" = mean1,
    "popular_votes_upr" = hi1, 
    "popular_votes_lwr" = lo1
  ) %>% 
  dplyr::select(-updated)

## save data
readr::write_csv(df_probs, here::here("data", "probability_over_time.csv"))



## electoral votes used for distributions --------------------------------------

df_votes <-
  fromJSON(here::here("data", "electoral_votes.json")) %>% 
  as.data.frame() %>% 
  unnest(c(candidates)) %>% 
  unnest(c(distribution)) %>% 
  dplyr::select(-type)

df_votes_expanded <-
  df_votes %>% 
  mutate(chance = as.integer(chance * 10000)) %>% 
  uncount(chance)

## save data
readr::write_csv(df_votes, here::here("data", "electoral_votes.csv"))
readr::write_csv(df_votes_expanded, here::here("data", "electoral_votes_expanded.csv"))
