rm(list=ls())

library("readxl")
library("dplyr")
library("ggplot2")
library("devtools")
library("gghutchins")

#devtools::load_all()

####### COMMENT
# I would create a project for this and then use 
# read_excel('data_folder_name/state_revenue.xlsx')
# this avoid using absolute paths and makes it easier to run on a different computer!
# also saves you from specificying a directory
# setwd("C:/Users/tpowell/OneDrive - The Brookings Institution/hutchins/lsheiner/pandemic_state_and_local/")
data <- read_excel("state_revenue_blog_data.xlsx")

# Take variables from decimal form to percent


# Suggestion:
# you can also do this
data %>% 
  mutate(
    across(
      .cols = ends_with('tax_share'),
      .fns = ~ . * 100
    ),
    sev_tax_tax_share = sev_tax_tax_share / 100
  )

# Alternatively specify that it's a percentage using scales::percent_format() in ggplot

data %>% 
  mutate(sev_tax_tax_share = sev_tax_tax_share / 100) %>% 
  ggplot(mapping = aes(x = sev_tax_tax_share, y = pct_chg_revenue, color = total_tax_share))+
  geom_point() +
  scale_color_hutchins(pal = 'cool',discrete = FALSE) +
  scale_x_continuous(name = 'Severance Tax Share',
                     labels = scales::percent_format()) +
  scale_y_continuous(name = 'Change in revenue',
                     labels = scales::percent_format()) +
  labs(title = 'Percent Revenue Loss vs. Severance Tax Share') +
  geom_smooth(method = 'lm', formula = y ~ x) 
  
data$pct_chg_revenue <- 100*data$pct_chg_revenue
data[c(6:12,27)] <- mutate_all(select(data, ends_with("tax_share")),.funs = funs(. * 100))
# Undoing the *100 on severance tax tax share because
# it was already in percent
data$sev_tax_tax_share <- data$sev_tax_tax_share/100

fit <- lm(pct_chg_revenue ~ oxford_stringency_index +
            total_covid_cases_per100k +
            pp_change_unemp_rate +
            school_close_index +
            sev_tax_tax_share, data=data)
summary(fit) # show results

# setwd("C:/Users/tpowell/OneDrive - The Brookings Institution/hutchins/lsheiner/pandemic_state_and_local/graphs")
plot <- ggplot(data = data) +
  geom_point(aes(x = sev_tax_tax_share, y = pct_chg_revenue, color = total_tax_share), ) +
  theme_hutchins() + scale_color_continuous() +
  labs(title = "Percent Revenue Loss vs. Severance Tax Share")V
finalize_plot(plot, width = 1000, height = 618, save_filepath = "C:/Users/tpowell/OneDrive - The Brookings Institution/hutchins/lsheiner/pandemic_state_and_local/graphs/sev_tax.png")
  plot(plot)
