librarian::shelf('tidyverse', 'ggthemes', 'ggtext','sysfonts', 'janitor', 'gghighlight')

source('R/theme_goliath.R')

df <-
  readxl::read_xlsx("data/fig_3.xlsx") %>% 
  clean_names() 

df_minus <- df %>% mutate(name2 = name) %>% select(-name)


df <- 
  df %>% 
  mutate(
    across(
      -c('date', 'total'),
      ~ .x / total
    )
  ) %>% 
  select(-total, -other) %>% 
  pivot_longer(-date) %>% 
  mutate(name = factor(name,
                       levels = c(
                         "asia",
                         "canada",
                         "europe",
                         "latin_am"
                       ),
                       labels = c(
                         "Asia",
                         "Canada",
                         "Europe",
                         "Latin America"
                       )
  ))


 p1 <- df %>%  ggplot(aes(x = date, y = value, color = name)) +
  geom_line(size = 1 ) +
  geom_line(data = df_minus, aes(x = date, y = value, group = name2), color = 'grey', alpha = 0.4) +
  theme_goliath() +
  labs(title = "Multinational's Markets Have Shifted Toward Asia") +
  scale_x_continuous(name = '') +
   scale_y_continuous(name = '', labels = scales::percent_format()) +
   scale_color_brewer(name = '', type = 'qual', palette = 'Set2') +
   theme(legend.position = "none") +
facet_wrap(~ name)

 ggsave('figures/figure_3.svg',p1, dpi = 700, device = svg)
 
 p2 <-
   df %>%  ggplot(aes(x = date, y = value, color = name)) +
   geom_line(alpha = 2, size = 1) +
   geom_line(data = df_minus, aes(x = date, y = value, group = name2), color = 'grey', alpha = 0.4) +
   theme_goliath() +
   scale_color_manual(values = palette_goliath_main)
   labs(title = "Multinational's Markets Have Shifted Toward Asia") +
   gghighlight(name == 'Asia') +
   scale_x_continuous(name = '') +
   scale_y_continuous(name = '', labels = scales::percent_format()) +
   scale_color_brewer(name = '', type = 'qual', palette = 'Set2') +
   theme(legend.position = "none")

 
 ggsave('figures/figure_3_alternative.svg',p2, dpi = 700, device = svg)
 