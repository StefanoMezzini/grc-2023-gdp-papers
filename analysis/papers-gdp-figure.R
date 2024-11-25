# MIT License
# 
# Copyright (c) 2024 Stefano Mezzini
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library('mgcv') # for GAMs
library('dplyr') # for data wrangling
library('tidyr') # for data wrangling
library('ggplot2') # for fancy plots
library('gratia') # for ggplot-based model diagnostics and plots
theme_set(theme_bw() + theme(panel.grid = element_blank()))

# publication data
# allow GPD to vary between years
d <-
  # count number of papers per year
  read.csv('data/Movement Ecology data - data.csv') %>%
  transmute(year = Year,
            country = First.Author.Affiliation,
            collected_data = Include.paper...Y.or.N. == 'Y') %>%
  group_by(year, country) %>%
  summarise(n = n(),
            n_newd = sum(collected_data),
            .groups = 'drop') %>%
  # add GDP
  left_join(read.csv('data/gdp/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6224532.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp', names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))) %>%
              select(Country.Name, year, gdp),
            by = c('country' = 'Country.Name', 'year')) %>%
  left_join(read.csv('data/population/API_SP.POP.TOTL_DS2_en_csv_v2_56.csv', skip = 4) %>%
              select(Country.Name, X1960:X2023) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'pop', names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))) %>%
              select(Country.Name, year, pop),
            by = c('country' = 'Country.Name', 'year')) %>%
  mutate(country = factor(country))

# check total papers per country across years
d %>%
  group_by(country) %>%
  summarise(n = sum(n)) %>%
  ungroup() %>%
  arrange(desc(n))

# missing US GDP in 2023
# GDP estimate: https://www.bea.gov/news/2024/gross-domestic-product-fourth-quarter-and-year-2023-second-estimate
# population estimate: https://www.census.gov/newsroom/press-releases/2023/population-trends-return-to-pre-pandemic-norms.html
filter(d, is.na(gdp))
d <- mutate(d,
            pop = if_else(is.na(pop), 334914895, pop),
            gdp = if_else(is.na(gdp), 27.36e12, gdp)) %>%
  # add values per 1e6 people and GPD/capita
  mutate(n_1e6_pop = n / (pop / 1e6),
         n_newd_1e6_pop = n_newd / (pop / 1e6),
         gdp_capita = gdp / pop)

# not a large range of years
ggplot(d, aes(year)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'black') +
  labs(x = 'Publication year', y = 'Number of publications')
mean(d$year)
diff(range(d$year))

# ~30% of countries only have one observation
obs_by_country <-
  d %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  pull(n)
hist(obs_by_country, xlab = 'Numer of data points per country', breaks = 11)
mean(obs_by_country == 1)

# ~40% of observations are 1
mean(d$n == 1)

# fit HGAMs ----
m <-
  gam(n_1e6_pop ~ s(gdp_capita, k = 10) + s(country, bs = 're'),
      family = Gamma(link = 'log'),
      data = d,
      method = 'REML')
appraise(m, method = 'simulate', n_simulate = 1e4)
draw(m, residuals = TRUE)

m_newd <-
  gam(n_newd_1e6_pop ~ s(gdp_capita, k = 10) + s(country, bs = 're'),
      family = Gamma(link = 'log'),
      data = d,
      subset = n_newd_1e6_pop > 0, # some have 0 with new data
      method = 'REML')
appraise(m_newd, method = 'simulate', n_simulate = 1e4)
draw(m_newd, residuals = TRUE)

# make predictions ----
newd <- tibble(gdp_capita = seq(1900, 11e4, length.out = 400),
               country = 'new country')
preds <- bind_rows(
  bind_cols(newd,
            predict(m, newdata = newd, type = 'link', se.fit = TRUE,
                    unconditional = TRUE, exclude = 's(country)') %>%
              as.data.frame() %>%
              transmute(mu = exp(fit),
                        lwr = exp(fit - 1.96 * se.fit),
                        upr = exp(fit + 1.96 * se.fit),
                        data = 'All publications')),
  bind_cols(newd,
            predict(m_newd, newdata = newd, type = 'link', se.fit = TRUE,
                    unconditional = TRUE, exclude = 's(country)') %>%
              as.data.frame() %>%
              transmute(mu = exp(fit),
                        lwr = exp(fit - 1.96 * se.fit),
                        upr = exp(fit + 1.96 * se.fit),
                        data = 'Publications with new field data only')))

# plot the predictions ----
pivot_longer(d, cols = c(n_1e6_pop, n_newd_1e6_pop),
             names_to = 'data', values_to = 'n_1e6_pop') %>%
  mutate(data = if_else(data == 'n_1e6_pop', 'All publications',
                        'Publications with new field data only')) %>%
  ggplot() +
  facet_wrap(~ data) +
  geom_ribbon(aes(gdp_capita / 1e3, ymin = lwr, ymax = upr), preds,
              alpha = 0.3, fill = '#BB5566')+
  geom_point(aes(gdp_capita / 1e3, n_1e6_pop), alpha = 0.3) +
  geom_line(aes(gdp_capita / 1e3, mu), preds, color = '#BB5566', lwd = 1) +
  labs(x = 'Yearly GDP per capita (thousands of USD)',
       y = 'Number of publications per million people')

ggsave('figures/papers-gdp.png', width = 8, height = 4, dpi = 600)
