# Coronavirus mortality projections by demographic breakdown
# All population numbers are in thousands, sticking with the bayesPop convention

library(dplyr)
library(tidyr)
library(purrr)
library(cowplot)
library(gganimate)
library(stringr)
library(forcats)
library(wpp2019)
library(readxl)
library(readr)

# Read in and merge the sex-specific WPP 2019 data for the year 2020
data(popM)
data(popF)

popM <- select(popM, age, country = name, pop_m = `2020`)
popF <- select(popF, pop_f = `2020`)

pop <- bind_cols(popM, popF) %>%
  mutate(country = as.character(country), pop = pop_m + pop_f)

# Recode to match the age categories from the Italian COVID data
pop$age <- recode(
  pop$age,
  `0-4` = '0-9',
  `5-9` = '0-9',
  `10-14` = '10-19',
  `15-19` = '10-19',
  `20-24` = '20-29',
  `25-29` = '20-29',
  `30-34` = '30-39',
  `35-39` = '30-39',
  `40-44` = '40-49',
  `45-49` = '40-49',
  `50-54` = '50-59',
  `55-59` = '50-59',
  `60-64` = '60-69',
  `65-69` = '60-69',
  `70-74` = '70-79',
  `75-79` = '70-79',
  `80-84` = '80-89',
  `85-89` = '80-89',
  `90-94` = '90+',
  `95-99` = '90+',
  `100+` = '90+'
)

pop$age <- fct_relevel(pop$age, '90+', after = Inf)

pop <- group_by(pop, age, country) %>%
  summarize(pop_m = sum(pop_m), pop_f = sum(pop_f), pop = sum(pop))

# Read in Liliana's age-specific CFRs (from Italian data)
# Convert lethality rate from a percentage to a proportion
italy_rates <- read_excel('data/italy-30march.xlsx') %>%
  select(age, lethality_rate) %>%
  mutate(age = as_factor(age), lethality_rate = lethality_rate / 100)

pop <- left_join(pop, italy_rates, by = 'age')

# Get the total population for each country
total_pop <- group_by(pop, country) %>%
  summarize(total_pop = sum(pop))

pop <- left_join(pop, total_pop)

calculate_deaths <- function(infection_rate, df = pop) {
  mutate(
    df,
    infection_rate = infection_rate,
    dead = pop*infection_rate*lethality_rate,
    dead_per_1000 = dead/total_pop*1000
    )
}

pop_grid <- map(seq(0, 1, 0.05), calculate_deaths) %>%
  bind_rows()

write_csv(pop_grid, 'data/corona_pop_proj.csv')

p1 <- filter(
    pop_grid,
    country %in% c('Italy', 'United States of America', 'Nigeria'),
    infection_rate %in% c(0.1, 0.4)) %>%
  mutate(
    country = factor(country, levels = c('Italy', 'United States of America', 'Nigeria')),
    infection_rate = recode(infection_rate, `0.1` = '10%', `0.4` = '40%')) %>%
  ggplot(aes(age, dead_per_1000, color = country, group = interaction(country, infection_rate), linetype = infection_rate)) +
    geom_line() +
    scale_color_brewer('', palette = 'Dark2') +
    labs(x = 'Age', y = 'Expected deaths per 1,000', linetype = 'Infection rate') +
    theme_cowplot()

p2 <- filter(pop, country %in% c('Italy', 'United States of America', 'Nigeria')) %>%
  mutate(pop_prop = pop / total_pop, country = factor(country, levels = c('Italy', 'United States of America', 'Nigeria'))) %>%
  ggplot(aes(age, pop_prop, color = country, group = country)) +
  geom_line() +
  scale_color_brewer('', palette = 'Dark2') +
  labs(x = 'Age', y = 'Proportion') +
  theme_cowplot()

p12 <- plot_grid(p1, p2, nrow = 2)

save_plot('figs/corona_lineplots.pdf', p12, nrow = 2, base_asp = 2.4)

anim <- filter(
  pop_grid,
  country %in% c('Italy', 'United States of America', 'Nigeria')) %>%
  mutate(
    country = factor(country, levels = c('Italy', 'United States of America', 'Nigeria'))) %>%
  ggplot(aes(age, dead_per_1000, color = country, group = country)) +
  geom_line() +
  scale_color_brewer('', palette = 'Dark2') +
  labs(x = 'Age', y = 'Expected deaths per 1,000', subtitle = 'Infection rate: {closest_state}') +
  theme_cowplot() +
  transition_states(infection_rate, transition_length = 0) +
  shadow_trail(distance = 0.25, alpha = 0.3)

animated <- animate(anim, nframes = 150, width = 600, height = 400)
anim_save('figs/corona_lineplot_animation.gif', animated)
