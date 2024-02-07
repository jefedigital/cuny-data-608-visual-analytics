# Load necessary libraries
library(tidyverse)
library(readxl)
library(patchwork)
library(scales)

data <- read_excel('story-1-data.xlsx', sheet='analysis')


## gdp_rank vs investment_pc

df_sum <- data %>%
  select(c(investment_b, biden_win)) %>%
  group_by(biden_win) %>%
  summarize(total_investment_b = sum(investment_b))


df_top <- data %>%
  select(c('region','biden_win','investment_pc','gdp_m','area_sqkm','gdp_rank','poverty_lvl','poverty_rank')) %>%
  mutate(region = factor(region, levels = region[order(investment_pc)]),
         gdp_m = round(gdp_m/1000,1),
         investment_pc = round(investment_pc,0)) %>%
  arrange(desc(investment_pc)) %>%
  slice(1:20)


# plots

plot_sum <- df_sum %>%
  ggplot(aes(x=biden_win, y=total_investment_b, fill=factor(biden_win))) +
  geom_bar(stat='identity', position='identity') +
  scale_fill_manual(values = c('red','blue')) +
  labs(x = "Total Investment by Party", y = "Total Investment ($B)") +
  geom_text(aes(label = paste0('$', round(total_investment_b,2))),
            color='white', position = position_nudge(y = -3), hjust = .5, size=5) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  )


inv_rank_right <- df_top %>%
  ggplot(aes(x = region, y = investment_pc, fill=factor(biden_win))) +
  geom_bar(stat = "identity", position = "identity") +
  scale_fill_manual(values = c('red','blue')) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "State", y = "Investment per Capita ($)") +
  geom_text(aes(label = paste0('$', scales::comma(investment_pc))),
            color='white', hjust=1.25, size=3) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none',
    axis.text.y = element_text(hjust = 0.5),
    plot.margin = margin(5.5, 5.5, 5.5, 5.5)
  )

gdp_rank_left <- df_top %>%
ggplot(aes(x = region, y = -gdp_m)) +
  geom_bar(stat = "identity", position = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "State", y = "GDP ($B) and Rank") +
  # geom_text(aes(label = paste0('$', gdp_m, ' (', gdp_rank, ')')),
  geom_text(aes(label = paste0(gdp_rank)),
    color='white', position = position_nudge(y = 3), hjust = -0.1, size=3) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  )


poverty_rank_left <- df_top %>%
  ggplot(aes(x = region, y = -poverty_lvl)) +
  geom_bar(stat = "identity", position = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "State", y = "Poverty Level % and Rank") +
  geom_text(aes(label = paste0(poverty_lvl, '% (', poverty_rank, ')')),
            color='white', hjust = -0.25, size=3) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  )


gdp_rank_left <- df_top %>%
  ggplot(aes(x = region, y = -gdp_m)) +
  geom_bar(stat = "identity", position = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "State", y = "GDP ($B) and Rank") +
  # geom_text(aes(label = paste0('$', gdp_m, ' (', gdp_rank, ')')),
  geom_text(aes(label = paste0(gdp_rank)),
            color='white', position = position_nudge(y = 3), hjust = -0.1, size=3) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  )

area_sqkm_left <- df_top %>%
  ggplot(aes(x = region, y = -area_sqkm)) +
  geom_bar(stat = "identity", position = "identity") +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(x = "State", y = "Land Square Km (K)") +
  geom_text(aes(label = round(area_sqkm/1000,0)),
            color='white', position = position_nudge(y = 3), hjust = -0.25, size=3) +
  theme_minimal() +
  theme(
    plot.title = element_blank(),
    axis.text.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'none'
  )


# show
plot_sum
gdp_rank_left + inv_rank_right
poverty_rank_left + inv_rank_right
area_sqkm_left + inv_rank_right


