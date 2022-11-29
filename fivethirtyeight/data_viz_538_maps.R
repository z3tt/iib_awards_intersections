pacman::p_load(tidyverse, officer, rvg, usmap)

colors <- c("#606FAF", "#E4544B")

colors <- c("#606FAF", "#E4544B")
bg <- "#F0EFEF"

theme_set(theme_minimal(base_size = 14, base_family = "Familjen Grotesk"))
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(color = bg, fill = bg),
  axis.text = element_text(family = "DecimaMonoPro", color = "#999999"),
  axis.title = element_text(size = rel(.9)),
  plot.title = element_text(face = "bold", hjust = .5, size = rel(1.7), margin = margin(b = 10)),
  plot.subtitle = element_text(hjust = .5, size = rel(1.05), margin = margin(b = 15)), 
  legend.position = "none"
)

colors <- c("#606FAF", "#E4544B")
bg <- "#F0EFEF"

theme_set(theme_minimal(base_size = 14, base_family = "Familjen Grotesk"))
theme_update(
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(color = bg, fill = bg),
  axis.text = element_text(family = "DecimaMonoPro", color = "#999999"),
  axis.title = element_text(size = rel(.9)),
  plot.title = element_text(face = "bold", hjust = .5, size = rel(1.7), margin = margin(b = 10)),
  plot.subtitle = element_text(hjust = .5, size = rel(1.05), margin = margin(b = 15)), 
  legend.position = "none"
)


## simulation data used for map small multiples --------------------------------

df_simus <- readr::read_csv(here::here("data", "simulations.csv"))


# plot1 = df_simus %>%
#   mutate(winner = factor(winner, levels = c("Trump", "Biden"))) %>%
#   ggplot(aes(y = fct_rev(factor(state)), fill = winner)) + 
#   geom_bar() + 
#   scale_fill_manual("Winner:", values = colors, breaks = c("Biden","Trump")) + 
#   coord_cartesian(expand = FALSE) +
#   labs(x = "Number of simulation outcomes", y = NULL) 
# my_vec_graph <- dml(ggobj  = plot1)
# doc <- officer::read_pptx() %>%
#   add_slide( layout = "Title and Content", master = "Office Theme") %>%
#   ph_with(my_vec_graph, location = ph_location(left = 0, top = 0,
#                                                width = 7, 
#                                                height =7, bg = "white") )
# print(doc, target = "plot1.pptx")
# 
# plot2 = df_simus %>%
#   mutate(winner = factor(winner, levels = c("Trump", "Biden"))) %>%
#   ggplot(aes(y = fct_rev(factor(sim)), fill = winner)) + 
#   geom_bar() + 
#   scale_fill_manual("Winner:", values = colors, breaks = c("Biden","Trump")) + 
#   coord_cartesian(expand = FALSE, clip = "off") +
#   labs(x = "Number of states won", y = "Simulation")
# my_vec_graph <- dml(ggobj  = plot2)
# doc <- officer::read_pptx() %>%
#   add_slide( layout = "Title and Content", master = "Office Theme") %>%
#   ph_with(my_vec_graph, location = ph_location(left = 0, top = 0,
#                                                width = 7, 
#                                                height =7, bg = "white") )
# print(doc, target = "plot2.pptx")


## Faceted Maps --------------------------------

df_states = df_simus %>% 
  select(-abbr) %>% 
  left_join(
    usmap::us_map(regions = "states") %>% 
      rename(state = full) %>% 
      select(-abbr)
  ) %>%
  mutate(
    winner_color = case_when(
      winner_overall == "Trump" & winner == "Trump" ~ colors[2],
      winner_overall == "Trump" & winner == "Biden" ~ "white",
      winner_overall == "Biden" & winner == "Trump" ~ "white",
      winner_overall == "Biden" & winner == "Biden" ~ colors[1]
    ),
    map_label = case_when(
      winner_overall == "Trump" ~ " A TRUMP WIN!",
      winner_overall == "Biden" ~ " A BIDEN WIN!"
    )
  )

ggplot() + 
  geom_polygon(
    data = df_states, 
    aes(x = x, y = y, group = group),
    color = "black", fill = "white", size = .8
  ) +
  geom_polygon(
    data = df_states, 
    aes(x = x, y = y, fill = winner_color, group = group),
    color = "grey85", size = .2
  ) +
  facet_wrap(~sim, ncol = 6) +
  scale_fill_manual(name = NULL, values = c(colors, "white")) +
  geom_text(
    data = df_states, x = median(df_states$x), y = min(df_states$y), 
    aes(label = map_label,color = map_label), family = "DecimaMonoPro"
  )+
  scale_color_manual(name = NULL, values = c(colors))+
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    strip.text = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

ggsave("faceted_map.png", width = 15, height = 8, bg = bg)
