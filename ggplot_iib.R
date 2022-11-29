################################################################################
#            Information is Beautiful Awards Intersections Workshop            #
#     "Designing Charts in R: Reproducible Graphic Design with {ggplot2}"      #
#                     Cédric Scherer | November 29, 2022                       #
################################################################################


## to run all codes, install the following typefaces and restart RStudio:
## Roboto Condensed, Cabinet Grotesk, Tabular (all located in the `fonts` folder) 

## also, install the following packages: tidyverse, systemfonts, ggrepel
# install.packages("tidyverse")
# install.packages("systemfonts")
# install.packages("ggrepel")


library(tidyverse)


bikes <- readr::read_csv(
  here::here("data", "london-bikes-custom.csv"),
  ## or: "https://raw.githubusercontent.com/z3tt/graphic-design-ggplot2/main/data/london-bikes-custom.csv"
  col_types = "Dcfffilllddddc"
)

bikes$season <- forcats::fct_inorder(bikes$season)


?ggplot ## or place your cursor on the function and hit F1

ggplot(data = bikes)


ggplot(data = bikes) +
  aes(x = temp_feel, y = count)

ggplot(
  data = bikes,
  mapping = aes(x = temp_feel, y = count)
)

ggplot(
  bikes,
  aes(x = temp_feel, y = count)
)

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point()

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    color = "#28a87d",
    alpha = .5,
    shape = "X",
    stroke = 1,
    size = 4
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    color = "#28a87d",
    alpha = .5
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    aes(color = season),
    alpha = .5
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    aes(color = temp_feel > 20),
    alpha = .5
  )


## Exercise --------------------------------------------------------------------

## - Create a chart showing a time series of `temp_feel`.
##     - What is the difference between `geom_line()` and `geom_path()`?
##     - Map the color of the lines to `day_night`.
##     - Add points for each observation.
##     - Turn the points into diamonds.

## add your code here:
ggplot(bikes, aes(x = date, y = temp_feel))


## - Bonus: Create a time series of bike counts in 2016 during nights only,
##          with points colored by season.


## Exercise End ----------------------------------------------------------------

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    aes(color = season),
    alpha = .5
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season)
  ) +
  geom_point(
    alpha = .5
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season)
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_smooth(
    method = "lm"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season)
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_smooth(
    method = "lm"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    aes(color = season),
    alpha = .5
  ) +
  geom_smooth(
    method = "lm"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point(
    aes(color = season),
    alpha = .5
  ) +
  geom_smooth(
    aes(group = day_night),
    method = "lm"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season,
        group = day_night)
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_smooth(
    method = "lm"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season,
        group = day_night)
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_smooth(
    method = "lm",
    color = "black"
  )

g <-
  ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season,
        group = day_night)
  ) +
  geom_point(
    alpha = .5
  ) +
  geom_smooth(
    method = "lm",
    color = "black"
  )

class(g)

g +
  geom_rug(
    alpha = .2
  )

g +
  geom_rug(
    alpha = .2,
    show.legend = FALSE
  )

g +
  labs(
    x = "Feels-like temperature (°C)",
    y = "Reported bike shares",
    title = "TfL bike sharing trends"
  )

g <- g +
  labs(
    x = "Feels-like temperature (°C)",
    y = "Reported bike shares",
    title = "TfL bike sharing trends",
    color = NULL
  )

g

g +
  labs(
    subtitle = "Reported bike rents versus feels-like temperature in London",
    caption = "Data: TfL",
    tag = "Fig. 1",
    color = "Season:"
  )

g +
  labs(
    x = "",
    caption = "Data: TfL"
  )

g +
  labs(
    x = NULL,
    caption = "Data: TfL"
  )

g + theme_light()

g + theme_minimal()

g + theme_light(
  base_size = 14,
  base_family = "Roboto Condensed"
)

theme_set(theme_light())

g

theme_set(theme_light(
  base_size = 14,
  base_family = "Roboto Condensed"
))

g

library(systemfonts)

system_fonts() %>%
  filter(str_detect(family, "Cabinet")) %>%
  pull(name) %>%
  sort()

register_variant(
  name = "Cabinet Grotesk Black",
  family = "Cabinet Grotesk",
  weight = "heavy",
  features = font_feature(letters = "stylistic")
)

g +
  theme_light(
    base_size = 18,
    base_family = "Cabinet Grotesk Black"
  )

g +
  theme(
    panel.grid.minor = element_blank()
  )

g +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

g +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )

g +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

g +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "top",
    plot.title.position = "plot"
  )

theme_update(
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold"),
  legend.position = "top",
  plot.title.position = "plot"
)

g





g +
  facet_wrap(
    vars(day_night)
  )

g +
  facet_wrap(
    ~ day_night
  )

g +
  facet_wrap(
    ~ is_workday + day_night
  )

g +
  facet_grid(
    rows = vars(day_night),
    cols = vars(is_workday)
  )

g +
  facet_grid(
    day_night ~ is_workday
  )

g +
  facet_grid(
    day_night ~ is_workday + season
  )

g +
  facet_grid(
    day_night ~ is_workday,
    scales = "free"
  )

g +
  facet_grid(
    day_night ~ is_workday,
    scales = "free",
    space = "free"
  )

g +
  facet_grid(
    day_night ~ is_workday,
    scales = "free_y",
    space = "free_y"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count,
        color = season)
  ) +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

ggplot(
    bikes,
    aes(x = season, y = temp_feel)
  ) +
  geom_boxplot() +
  scale_x_discrete() +
  scale_y_continuous()

g +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_color_discrete()

g +
  scale_x_binned() +
  scale_y_log10() +
  scale_color_viridis_d()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1)
  ) +
  scale_y_continuous() +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5)
  ) +
  scale_y_continuous() +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C")
  ) +
  scale_y_continuous() +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous() +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0)
  ) +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    limits = c(0, NA)
  ) +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    limits = c(5000, 20000)
  ) +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000
  ) +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_discrete()

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_discrete(
    type = c("#3c89d9", "#1ec99b", "#f7b01b", "#a26e7c")
  )

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = c("#3c89d9", "#1ec99b", "#f7b01b", "#a26e7c")
  )

colors_sorted <- c(
  `autumn` = "#a26e7c",
  `spring` = "#1ec99b",
  `summer` = "#f7b01b",
  `winter` = "#3c89d9"
)

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0),
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = colors_sorted
  )

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_brewer(
    palette = "Dark2"
  )

RColorBrewer::display.brewer.all()

RColorBrewer::display.brewer.all(colorblindFriendly = TRUE)

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = colors_sorted,
    name = NULL
  )

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = colors_sorted,
    name = NULL,
    labels = stringr::str_to_title
  )

g +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = seq(0, 30, by = 5), 
    labels = function(x) paste0(x, "°C"),
    name = "Feels-like temperature"
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0), 
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = colors_sorted,
    name = NULL,
    labels = stringr::str_to_title,
    guide = guide_legend(
      override.aes = list(size = 5)
    )
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  coord_cartesian()

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  coord_cartesian(
    ylim = c(NA, 15000)
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  coord_cartesian(
    ylim = c(NA, 15000)
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  scale_y_continuous(
    limits = c(NA, 15000)
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  coord_cartesian(
    ylim = c(NA, 15000),
    clip = "off"
  )

ggplot(
    filter(bikes, is_holiday == TRUE),
    aes(x = temp_feel, y = count)
  ) +
  geom_point() +
  geom_text(
    aes(label = season),
    nudge_x = .3,
    hjust = 0
  ) +
  coord_cartesian(
    clip = "off"
  )

ggplot(
    filter(bikes, is_holiday == TRUE),
    aes(x = temp_feel, y = count)
  ) +
  geom_point() +
  ggrepel::geom_text_repel(
    aes(label = season),
    nudge_x = .3,
    hjust = 0
  ) +
  coord_cartesian(
    clip = "off"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = count)
  ) +
  geom_point() +
  coord_cartesian(
    expand = FALSE,
    clip = "off"
  )

ggplot(
    bikes,
    aes(x = temp_feel, y = temp)
  ) +
  geom_point() +
  coord_fixed()

ggplot(
    bikes,
    aes(x = temp_feel, y = temp)
  ) +
  geom_point() +
  coord_fixed(ratio = 4)

ggplot(
    bikes,
    aes(x = weather_type)
  ) +
  geom_bar() +
  coord_cartesian()

ggplot(
    bikes,
    aes(x = weather_type)
  ) +
  geom_bar() +
  coord_flip()

ggplot(
    bikes,
    aes(y = weather_type)
  ) +
  geom_bar() +
  coord_cartesian()

ggplot(
    bikes,
    aes(x = weather_type)
  ) +
  geom_bar() +
  coord_flip()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(y = fct_infreq(weather_type))
  ) +
  geom_bar()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(y = fct_rev(
      fct_infreq(weather_type)
    ))
  ) +
  geom_bar()

# ggsave(g, filename = "my_plot.png")

# ggsave("my_plot.png")

# ggsave("my_plot.png", width = 8, height = 5, dpi = 600)

# ggsave("my_plot.pdf", width = 20, height = 12, unit = "cm", device = cairo_pdf)



## WRAP-UP ---------------------------------------------------------------------

library(tidyverse)

bikes_adj <-
  readr::read_csv(
    here::here("data", "london-bikes-custom.csv"),
    col_types = "Dcfffilllddddc"
  ) %>%
  mutate(
    season = factor(season, levels = c("spring", "summer", "autumn", "winter")),
    day_night = stringr::str_to_title(day_night),
    is_workday = factor(is_workday, level = c(TRUE, FALSE),
                        labels = c("Workday", "Weekend or Holiday"))
  )

bikes_adj

library(systemfonts)

register_variant(
  name = "Cabinet Grotesk Regular S01",
  family = "Cabinet Grotesk",
  weight = "normal",
  features = font_feature(letters = "stylistic")
)

register_variant(
  name = "Cabinet Grotesk Bold S01",
  family = "Cabinet Grotesk",
  weight = "bold",
  features = font_feature(letters = "stylistic")
)

register_variant(
  name = "Cabinet Grotesk Black S01",
  family = "Cabinet Grotesk",
  weight = "heavy",
  features = font_feature(letters = "stylistic")
)

g1 <- 
  ggplot(bikes_adj, aes(x = temp_feel, y = count)) +
  ## point outline
  geom_point(
    color = "black", fill = "white",
    shape = 21, size = 2.8
  ) +
  ## opaque point background
  geom_point(
    color = "white", size = 2.2
  ) +
  ## colored, semi-transparent points
  geom_point(
    aes(color = forcats::fct_relabel(season, stringr::str_to_title)),
    size = 2.2, alpha = .55
  ) +
  geom_smooth(
    aes(group = day_night), method = "lm", color = "black"
  ) +
  facet_grid(
    day_night ~ is_workday,
    scales = "free_y", 
    space = "free_y"
  )

g1

g2 <- g1 +
  scale_x_continuous(
    expand = c(mult = 0, add = 1),
    breaks = 0:6*5, 
    labels = function(x) paste0(x, "°C")
  ) +
  scale_y_continuous(
    expand = c(mult = .02, add = 0),
    limits = c(0, NA),
    breaks = 0:5*10000, 
    labels = scales::label_comma()
  ) +
  scale_color_manual(
    values = c("#3c89d9", "#1ec99b", "#F7B01B", "#a26e7c"), 
    guide = guide_legend(override.aes = list(size = 5))
  ) +
  labs(
    x = "Feels-Like Temperature", y = NULL,
    caption = "Data: Transport for London (TfL), Jan 2015—Dec 2016",
    title = "Reported TfL bike rents versus feels-like temperature in London, 2015–2016",
    color = NULL
  ) 

g2

g3 <- g2 +
  theme_light(
    base_size = 18, base_family = "Cabinet Grotesk"
  ) +
  ## more theme adjustments
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(family = "Cabinet Grotesk Black S01", size = rel(1.6)),
    axis.text = element_text(family = "Tabular"),
    axis.title.x = element_text(hjust = 0, color = "grey30", margin = margin(t = 12)),
    strip.text = element_text(family = "Cabinet Grotesk Bold S01", size = rel(1.15)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.2, "lines"),
    legend.position = "top",
    legend.text = element_text(size = rel(1)),
    ## for fitting my slide background
    legend.key = element_rect(color = "#f8f8f8", fill = "#f8f8f8"),
    legend.background = element_rect(color = "#f8f8f8", fill = "#f8f8f8"),
    plot.background = element_rect(color = "#f8f8f8", fill = "#f8f8f8")
  )

g3

theme_set(
  theme_light(
    base_size = 18, base_family = "Cabinet Grotesk"
  )
)

theme_update(
  plot.title.position = "plot",
  plot.caption.position = "plot",
  plot.title = element_text(family = "Cabinet Grotesk Black S01", size = rel(1.6)),
  axis.text = element_text(family = "Tabular"),
  axis.title.x = element_text(hjust = 0, color = "grey30", margin = margin(t = 12)),
  strip.text = element_text(family = "Cabinet Grotesk Bold S01", size = rel(1.15)),
  panel.grid.major.x = element_blank(),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(1.2, "lines"),
  legend.position = "top",
  legend.text = element_text(size = rel(1)),
  ## for fitting my slide background
  legend.key = element_rect(color = "#f8f8f8", fill = "#f8f8f8"),
  legend.background = element_rect(color = "#f8f8f8", fill = "#f8f8f8"),
  plot.background = element_rect(color = "#f8f8f8", fill = "#f8f8f8")
)

g2


## APPENDIX --------------------------------------------------------------------

# reset theme
theme_set(theme_light(base_size = 14, base_Family = "Roboto Condensed"))

ggplot(bikes, aes(x = temp_feel, y = count)) +
  stat_smooth(geom = "smooth")

ggplot(bikes, aes(x = temp_feel, y = count)) +
  geom_smooth(stat = "smooth")

ggplot(bikes, aes(x = date, y = temp_feel)) +
  geom_point(stat = "identity")

ggplot(bikes, aes(x = date, y = temp_feel)) +
  stat_identity(geom = "point")

ggplot(bikes, aes(x = is_weekend)) +
  geom_bar(stat = "count")

ggplot(bikes, aes(x = is_weekend)) +
  stat_count(geom = "bar")

ggplot(
    bikes, 
    aes(x = season, y = temp_feel)
  ) +
  stat_summary() 

ggplot(
    bikes, 
    aes(x = season, y = temp_feel)
  ) +
  stat_summary(
    fun.data = mean_se, ## the default
    geom = "pointrange"  ## the default
  ) 

ggplot(
    bikes, 
    aes(x = season, y = temp_feel)
  ) +
  geom_boxplot() +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "#28a87d",
    size = 3
  ) 

ggplot(
    bikes, 
    aes(x = season, y = temp_feel)
  ) +
  stat_summary() +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = after_stat(y))
  )

ggplot(
    bikes, 
    aes(x = season, y = temp_feel)
  ) +
  stat_summary() +
  stat_summary(
    fun = mean,
    geom = "text",
    aes(label = after_stat(
      paste0(round(y, 2), "°C"))
    ),
    hjust = -.2,
    size = 3.5
  )

g +
  facet_wrap(
    ~ day_night,
    ncol = 1
  )

g +
  facet_wrap(
    ~ day_night,
    ncol = 1,
    scales = "free"
  )

g +
  facet_wrap(
    ~ day_night,
    ncol = 1,
    scales = "free_y"
  )

g +
  facet_grid(
    day_night ~ is_workday,
    scales = "free",
    switch = "y"
  )

g +
  facet_wrap(
    ~ day_night,
    labeller = label_both
  )

g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = label_both
  )

g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      day_night = stringr::str_to_title
    )
  )

codes <- c(
  `TRUE` = "Workday",
  `FALSE` = "Weekend or Holiday"
)

g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      day_night = stringr::str_to_title,
      is_workday = codes
    )
  )

codes <- c(
  `TRUE` = "Workday",
  `FALSE` = "Weekend or Holiday"
)

g +
  facet_wrap(
    ~ is_workday + day_night,
    labeller = labeller(
      .default = stringr::str_to_title,
      is_workday = codes
    )
  )

g +
  facet_grid(
    day_night ~ is_workday,
    scales = "free",
    space = "free",
    labeller = labeller(
      day_night = stringr::str_to_title,
      is_workday = codes
    )
  )

ggplot(
    bikes,
    aes(x = date, y = count,
        color = season)
  ) +
  geom_point() +
  scale_x_date(
    name = NULL,
    date_breaks = "4 months"
  )

ggplot(
    bikes,
    aes(x = date, y = count,
        color = season)
  ) +
  geom_point() +
  scale_x_date(
    name = NULL,
    date_breaks = "20 weeks"
  )

ggplot(
    bikes,
    aes(x = date, y = count,
        color = season)
  ) +
  geom_point() +
  scale_x_date(
    name = NULL,
    date_breaks = "6 months",
    date_labels = "%Y/%m/%d"
  )

ggplot(
    bikes,
    aes(x = date, y = count,
        color = season)
  ) +
  geom_point() +
  scale_x_date(
    name = NULL,
    date_breaks = "6 months",
    date_labels = "%b '%y"
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  scale_x_discrete(
    name = "Period",
    labels = c("Dec-Feb", "Mar-May", "Jun-Aug", "Sep-Nov")
  )

ggplot(
    bikes,
    aes(x = season, y = count)
  ) +
  geom_boxplot() +
  scale_x_discrete(
    name = "Season",
    expand = c(.5, 0) ## add, mult
  )

ggplot(
    bikes,
    aes(x = as.numeric(season), y = count)
  ) +
  geom_boxplot(
    aes(group = season)
  )

ggplot(
    bikes,
    aes(x = as.numeric(season),
        y = count)
  ) +
  geom_boxplot(
    aes(group = season)
  ) +
  scale_x_continuous(
    name = "Season",
    breaks = 1:4,
    labels = levels(bikes$season)
  )

ggplot(
    bikes,
    aes(x = as.numeric(season) + 
            as.numeric(season) / 8,
        y = count)
  ) +
  geom_boxplot(
    aes(group = season)
  ) +
  scale_x_continuous(
    name = "Season",
    breaks = 1:4,
    labels = levels(bikes$season)
  )

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = weather_type,
        fill = weather_type)
  ) +
  geom_bar() +
  coord_polar()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = weather_type,
        fill = weather_type)
  ) +
  geom_bar() +
  coord_cartesian()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = fct_infreq(weather_type),
        fill = weather_type)
  ) +
  geom_bar(width = 1) +
  coord_polar()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = fct_infreq(weather_type),
        fill = weather_type)
  ) +
  geom_bar(width = 1) +
  coord_cartesian()

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = fct_infreq(weather_type),
        fill = weather_type)
  ) +
  geom_bar() +
  coord_polar(theta = "x")

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = fct_infreq(weather_type),
        fill = weather_type)
  ) +
  geom_bar() +
  coord_polar(theta = "y")

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = 1, fill = weather_type)
  ) +
  geom_bar(position = "stack") +
  coord_polar(theta = "y") 

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = 1, fill = weather_type)
  ) +
  geom_bar(position = "stack") +
  coord_cartesian() 

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = 1,
        fill = fct_rev(fct_infreq(weather_type)))
  ) +
  geom_bar(position = "stack") +
  coord_polar(theta = "y") +
  scale_fill_discrete(name = NULL)

ggplot(
    filter(bikes, !is.na(weather_type)),
    aes(x = 1,
        fill = fct_rev(fct_infreq(weather_type)))
  ) +
  geom_bar(position = "stack") +
  coord_cartesian() +
  scale_fill_discrete(name = NULL)

ggplot(
    bikes,
    aes(x = temp, y = count)
  ) +
  geom_point() +
  coord_trans(y = "log10")

ggplot(
    bikes,
    aes(x = temp, y = count,
        group = day_night)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  coord_trans(y = "log10")

ggplot(
    bikes,
    aes(x = temp, y = count,
        group = day_night)
  ) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_log10()

countries <- rnaturalearth::ne_countries(
  returnclass = "sf"
)

ggplot() +
  geom_sf(
    data = countries,
    color = "#79dfbd",
    fill = "#28a87d",
    size = .3
  )

ggplot() +
  geom_sf(
    data = countries,
    color = "#79dfbd",
    fill = "#28a87d",
    size = .3
  ) +
  coord_sf(
    crs = "+proj=moll"
  )

ggplot() +
  geom_sf(
    data = countries,
    color = "#79dfbd",
    fill = "#28a87d",
    size = .3
  ) +
  coord_sf(
    crs = "+proj=bonne +lat_1=10"
  )

oceans <- rnaturalearth::ne_download(
  category = "physical", type = "ocean", returnclass = "sf"
)

ggplot() +
  geom_sf(
    data = oceans,
    fill = "#d8f1f6"
  ) +
  geom_sf(
    data = countries,
    color = "#79dfbd",
    fill = "#28a87d",
    size = .3
  ) +
  coord_sf(
    crs = "+proj=bonne +lat_1=10"
  ) +
  theme_void()

ggplot() +
  geom_sf(
    data = oceans,
    fill = "#d8f1f6",
    color = "white"
  ) +
  geom_sf(
    data = countries,
    aes(fill = economy),
    color = "white",
    size = .3
  ) +
  coord_sf(
    crs = "+proj=bonne +lat_1=10"
  ) +
  scale_fill_viridis_d(option = "magma") +
  theme_void() +
  theme(legend.position = "top")
