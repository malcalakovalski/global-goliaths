librarian::shelf('tidyverse', 'ggthemes', 'ggtext','sysfonts')
source('R/theme_goliath.R')
mnc_share <-
  readxl::read_xlsx("data/role-mnc.xlsx")

totals <- mnc_share %>% 
  pivot_longer(-mnc_type) %>% 
  group_by(name) %>% 
  summarize(total = sum(value))
mnc_share %>%
  pivot_longer(-mnc_type) %>%
  group_by(name) %>% 
  summarize(mnc_type, name, prop = sum(value), value) %>% 
  mutate(y_label = paste0(round(prop * 100, 1), '%')) %>% 
  arrange(value) %>%
  ggplot(aes(x = reorder(name, value), y = value, fill = mnc_type)) +
  geom_col(width = 0.5, aes(fill = mnc_type)) +
  labs(
    title = "Multinationals are major players in the U.S. economy",
    subtitle = "Multinationals' Share of Economic Activity in 2017, by category",
    caption = 'The original figure appears in *Global Goliaths*, Brookings Institution Press, 2021. <br>
Source:  Bureau of Economic Analysis, National Income and Product Accounts; National Science Foundation, Science and Engineering Indicators; Census Bureau Annual Capital Expenditure Survey',
    x = ""
  ) +
  guides(fill = guide_legend(reverse = T)) +
  coord_flip() +
  scale_y_continuous(name = "", labels = scales::percent_format(),
                     limits = c(0,1)) +
  scale_fill_goliath('cool',
                     name = '',
                     labels = c("U.S. Arms of foreign-headquartered firms", "U.S. Parents")) +
 theme_goliath() +
  
  theme(
    plot.title.position = "plot",
    axis.ticks.x = element_line(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(size = 0.3, colour = 'grey80'),
  )  +
  geom_text(aes(label = y_label), position = position_dodge(0.7),
            vjust = 1.5, color = "white", family = "Roboto")

  



