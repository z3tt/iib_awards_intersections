library(tidyverse)
library(jsonlite)

colors <- c("#606FAF", "#E4544B")

theme_set(theme_minimal(base_size = 14, base_family = "Atlas Grotesk"))
theme_update(panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(), 
             legend.position = "top")

Sys.setlocale("LC_TIME", "C")


## simulation data used for map small multiples --------------------------------

df_simus <- readr::read_csv(here::here("fivethirtyeight", "simulations.csv"))


ggplot(df_simus, aes(y = fct_rev(factor(state)), fill = winner)) + 
  geom_bar() + 
  scale_fill_manual(values = colors) + 
  coord_cartesian(expand = FALSE) +
  labs(x = "Number of simulation outcomes", y = NULL, fill = "Winner:")


ggplot(df_simus, aes(y = fct_rev(factor(sim)), fill = winner)) + 
  geom_bar() + 
  scale_fill_manual(values = colors) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Number of states won", y = "Simulation", fill = "Winner:")



## Probability over time -------------------------------------------------------

df_probs <- readr::read_csv(here::here("fivethirtyeight", "probability_over_time.csv"))


ggplot(df_probs, aes(x = date, y = winprob, color = candidate, fill = candidate)) + 
  geom_line(color = "white", size = 2.8) + 
  geom_line(size = .9) + 
  #geom_step(size = .9) + 
  geom_point(
    data = filter(df_probs, date == max(date)),
    shape = 21, color = "white", stroke = 1.5, size = 3.5
  ) +
  scale_x_date(date_labels = "%b. %d", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 100), breaks = 0:5*20) +
  scale_color_manual(values = colors) + 
  scale_fill_manual(values = colors, guide = "none") + 
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = "Chance of winning", color = "Winner:") +
  theme(panel.grid.major.y = element_line(color = "grey95"))
  

ggplot(df_probs, aes(x = date, y = electoral_votes_avg, group = candidate)) + 
  geom_ribbon(aes(fill = candidate, ymin = electoral_votes_lwr, ymax = electoral_votes_upr), alpha = .2) +
  geom_line(color = "white", size = 2.8) + 
  geom_line(aes(color = candidate), size = .9) + 
  geom_point(
    data = filter(df_probs, date == max(date)),
    aes(fill = candidate), show.legend = FALSE,
    shape = 21, color = "white", stroke = 1.5, size = 3.5
  ) +
  scale_x_date(date_labels = "%b. %d") +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = NULL, y = "Electoral votes", color = "Winner:", fill = "Winner:")


ggplot(df_probs, aes(x = date, y = popular_votes_avg, group = candidate)) + 
  geom_ribbon(aes(fill = candidate, ymin = popular_votes_lwr, ymax = popular_votes_upr), alpha = .2) +
  geom_line(color = "white", size = 2.8) + 
  geom_line(aes(color = candidate), size = .9) + 
  geom_point(
    data = filter(df_probs, date == max(date)),
    aes(fill = candidate), show.legend = FALSE,
    shape = 21, color = "white", stroke = 1.5, size = 3.5
  ) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) + 
  scale_x_date(date_labels = "%b. %d") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = NULL, y = "Popular votes", color = "Winner:", fill = "Winner:")



## electoral vote distributions ------------------------------------------------

df_votes <- readr::read_csv(here::here("fivethirtyeight", "electoral_votes.csv"))
df_votes_expanded <- readr::read_csv(here::here("fivethirtyeight", "electoral_votes_expanded.csv"))


ggplot(df_votes, aes(x = evs, y = chance, fill = candidate)) + 
  geom_col(alpha = .7) +
  geom_line(aes(y = zoo::rollmean(chance, 15, na.pad = TRUE)), color = "white", size = 2.8) +
  geom_line(aes(y = zoo::rollmean(chance, 15, na.pad = TRUE), color = candidate), size = .9) +
  geom_hline(yintercept = 0) +
  annotate(geom = "rect", xmin = -Inf, xmax = 270, ymin = -Inf, ymax = Inf, fill = "white", alpha = .5) +
  geom_vline(xintercept = 270) +
  facet_wrap(~candidate, ncol = 1) + 
  scale_y_continuous(guide = "none") +
  scale_color_manual(values = colors, guide = "none") +
  scale_fill_manual(values = colors, guide = "none") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Electoral votes", y = NULL)

ggplot(df_votes_expanded, aes(x = evs, fill = candidate)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = .7) +
  geom_line(stat = "density", color = "white", size = 2.8, adjust = 1.5) +
  geom_line(stat = "density", aes(color = candidate), size = .9, adjust = 1.5) +
  geom_hline(yintercept = 0) +
  annotate(geom = "rect", xmin = -Inf, xmax = 270, ymin = -Inf, ymax = Inf, fill = "white", alpha = .5) +
  geom_vline(xintercept = 270) +
  facet_wrap(~candidate, ncol = 1) + 
  scale_y_continuous(guide = "none") +
  scale_color_manual(values = colors, guide = "none") +
  scale_fill_manual(values = colors, guide = "none") +
  coord_cartesian(expand = FALSE, clip = "off") +
  labs(x = "Electoral votes", y = NULL)
