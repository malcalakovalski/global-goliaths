# Figure 1
# https://r-graphics.org/recipe-bar-graph-labels

# NOTE:
# Errors in spreadsheet
#
library("janitor")
library("tidyverse")
library("conflicted")
library("showtext")
library("ggthemes")
library("ggtext")
library("RColorBrewer")
conflict_prefer("filter", "dplyr")
get_breaks <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)
}
get_employment_share <- function() {
  readxl::read_xlsx("data/employment_share.xlsx") %>%
    janitor::clean_names() %>%
    fill(everything(), .direction = "up") %>%
    mutate(
      across(
        .cols = !year & !all_countries,
        .fns = ~ .x / all_countries,
        .names = "{.col}"
      )
    ) %>%
    select(-all_countries) %>%
    pivot_longer(-year) %>%
    arrange(value) %>% # First sort by val. This sort the dataframe but NOT the factor levels
    mutate(name = factor(name,
      levels = c(
        "low_income",
        "lower_middle_income",
        "upper_middle_income",
        "high_income"
      ),
      labels = c(
        "Low income",
        "Lower middle income",
        "Upper middle income",
        "High income"
      )
    ))
}
plot_employment_share <- function(df) {
  df %>%
    ggplot(aes(
      x = year,
      y = value,
      color = name
    )) +
    geom_line(size = 1, alpha = 0.7) +
    theme_hc() +
    scale_color_brewer(
      name = "",
      type = "qual",
      palette = "Set1"
    ) +
    scale_x_continuous(
      name = "",
      breaks = seq(1982, 2017, 5)
    ) +
    scale_y_continuous(
      name = "",
      labels = scales::percent,
      limits = c(0, 1)
    ) +
    labs(
      title = "Share of U.S. MNC Foreign Affiliate Employment by Host Country Income",
      subtitle = "Most MNC employees are in high wage countries"
    )
}
custom_theme <- function() {
  theme(
    plot.title.position = "plot",
    text = element_text(family = "Roboto", color = "grey20"),
    plot.title = element_textbox_simple(
      family = "Merriweather",
      size = 16,
      face = "bold",
      lineheight = 1,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    ),

    plot.caption = element_textbox_simple(),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines"),
    legend.position = "bottom",
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_textbox_simple(
      family = "Merriweather",
      size = 12,
      padding = margin(2.5, 0, 2.5, 2.5),
      margin = margin(0, 5.5, 5.5, 5.5),
      hjust = 0
    ),
    axis.title.x = element_textbox_simple(
      width = NULL,
      padding = margin(4, 4, 4, 4),
      margin = margin(4, 0, 0, 0)
    )
  )
}


p <-
  get_employment_share() %>%
  plot_employment_share() +
  theme_classic() +
  custom_theme() +
  geom_text(
    data = get_employment_share() %>% filter(year == max(year)), aes(
      label = name,
      x = year,
      y = value,
      color = name
    ),
    hjust = 0, nudge_x = 0.4
  ) +
  guides(color = FALSE) +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    plot.margin = margin(0.1, 3.5, 0.1, 0.1, "cm")
  )




ggsave("figures/fig-2.svg", dpi = 600)


library(officer)
library(rvg)

library("tidyverse")
library("ggthemes")
library("ggtext")
mnc_share <-
  readxl::read_xlsx("data/role-mnc.xlsx")

mnc_share %>%
  group_by(mnc_type) %>%
  pivot_longer(-mnc_type) %>%
  arrange(value) %>%
  ggplot(aes(x = reorder(name, value), y = value, fill = mnc_type)) +
  geom_col(width = 0.5, position = "dodge", aes(fill = mnc_type)) +
  labs(
    title = "Share of U.S. MNC Foreign Affiliate Employment by Host Country Income",
    subtitle = "Multinationals are major players in the economy",
    x = ""
  ) +
  theme_hc() +
  coord_flip() +
  scale_y_continuous(name = "", labels = scales::percent_format()) +
  scale_fill_brewer(name = "", labels = c("U.S. arms of foreign-headquartered firms", "U.S. Parents"), type = "qual", palette = "Paired") +
  theme(
    plot.title.position = "plot"
  ) +
  ggsave("figures/fig-1.pdf", dpi = 600)
