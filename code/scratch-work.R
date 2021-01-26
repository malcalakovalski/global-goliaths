# NOTE:
# Errors in spreadsheet
# 
conflict_prefer("filter", "dplyr")
get_breaks <- function(k) {
  step <- k
  function(y) seq(floor(min(y)), ceiling(max(y)), by = step)       
}
get_employment_share <- function(){
  readxl::read_xlsx('data/employment_share.xlsx') %>%
    clean_names() %>%
    fill(everything(), .direction ='up') %>%
    mutate(
      across(
        .cols = !year & !all_countries,
        .fns = ~ .x / all_countries,
        .names = '{.col}'
      )
    ) %>%
    select(-all_countries)
}


get_employment_share() %>%
  pivot_longer(-year) %>%
  arrange(value) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
  mutate(name = factor(name, levels = c('low_income',
                                        'lower_middle_income',
                                        'upper_middle_income',
                                        'high_income'),
                       labels = c('Low income',
                                  'Lower middle income',
                                  'Upper middle income',
                                  'High income'))) %>%
  ggplot(aes(x = year,
             y = value,
             color = name)) +
  geom_line(size = 1,  alpha = 0.7) +
  theme_hc() +
  custom_theme() +
  scale_color_brewer(name = '', 
                     type = 'qual', 
                     palette = 'Set1') +
  scale_x_continuous(name = '',
                     breaks = seq(1982, 2017, 5)) +
  scale_y_continuous(name = '',
                     labels = scales::percent,
                     limits = c(0, 1)) +
  labs(title = 'The Share of U.S. MNC Foreign Affiliate Employment by Host Country Income')
