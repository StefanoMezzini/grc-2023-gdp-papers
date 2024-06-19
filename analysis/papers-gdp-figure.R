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
papers <- read.csv('data/Movement Ecology data - data.csv') %>%
  transmute(year = Year,
            country = First.Author.Affiliation,
            collected_data = Include.paper...Y.or.N. == 'Y')

# allow GPD to vary between years
d_yearly <-
  # count number of papers per year
  papers %>%
  group_by(year, country) %>%
  summarise(n_papers = n(), .groups = 'drop') %>%
  # add GDP
  left_join(read.csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6224532.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp', names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))) %>%
              select(Country.Name, year, gdp),
            by = c('country' = 'Country.Name', 'year')) %>%
  mutate(country = factor(country))

# check total papers per country across years
d %>%
  group_by(country) %>%
  summarise(n = sum(n_papers)) %>%
  ungroup() %>%
  arrange(desc(n))

# missing US GDP in 2023
# GDP estimate: https://www.bea.gov/news/2024/gross-domestic-product-fourth-quarter-and-year-2023-second-estimate
# population estimate: https://www.census.gov/newsroom/press-releases/2023/population-trends-return-to-pre-pandemic-norms.html
filter(d_yearly, is.na(gdp))
d_yearly <- mutate(d_yearly, gdp = if_else(is.na(gdp), 27.36e12, gdp))

# only with new field data
d_yearly_newd <-
  # count number of papers per year
  papers %>%
  filter(collected_data) %>%
  group_by(year, country) %>%
  summarise(n_papers = n(), .groups = 'drop') %>%
  # add GDP
  left_join(read.csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6224532.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp', names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))) %>%
              select(Country.Name, year, gdp),
            by = c('country' = 'Country.Name', 'year')) %>%
  mutate(country = factor(country))

# need to add GDP again
filter(d_yearly_newd, is.na(gdp))
d_yearly_newd <- mutate(d_yearly_newd,
                        gdp = if_else(is.na(gdp), 27.36e12, gdp))

# not a large range of years
ggplot(d_yearly, aes(year)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'black') +
  labs(x = 'Publication year', y = 'Number of publications')
mean(d_yearly$year)
diff(range(d_yearly$year))

# ~30% of countries only have one observation
obs_by_county <-
  d_yearly %>%
  group_by(country) %>%
  summarise(n = n()) %>%
  pull(n)
hist(obs_by_county, xlab = 'Numer of data points per country', breaks = 11)
mean(obs_by_county == 1)

# ~40% of observations are 1
mean(d_yearly$n_papers == 1)

# fit HGAMs ----
m <-
  gam(n_papers ~ s(log10(gdp), k = 5) + s(country, bs = 're'),
      family = poisson(link = 'log'),
      data = d_yearly,
      method = 'REML')
appraise(m, method = 'simulate', n_simulate = 1e4)
draw(m, residuals = TRUE)

m_newd <-
  gam(n_papers ~ s(log10(gdp), k = 5) + s(country, bs = 're'),
      family = poisson(link = 'log'),
      data = d_yearly_newd,
      method = 'REML')
appraise(m_newd, method = 'simulate', n_simulate = 1e4)
draw(m_newd, residuals = TRUE)

# make predictions ----
newd <- tibble(gdp = 10^seq(10, 13.5, length.out = 400),
               country = 'new country')
preds <- bind_rows(
  bind_cols(newd,
            predict(m, newdata = newd, type = 'link', se.fit = TRUE,
                    unconditional = TRUE) %>%
              as.data.frame() %>%
              transmute(mu = exp(fit),
                        lwr = exp(fit - 1.96 * se.fit),
                        upr = exp(fit + 1.96 * se.fit),
                        data = 'All publications')),
  bind_cols(newd,
            predict(m_newd, newdata = newd, type = 'link', se.fit = TRUE,
                    unconditional = TRUE) %>%
              as.data.frame() %>%
              transmute(mu = exp(fit),
                        lwr = exp(fit - 1.96 * se.fit),
                        upr = exp(fit + 1.96 * se.fit),
                        data = 'Publications with new data only')))

# plot the predictions ----
bind_rows(mutate(d_yearly, data = 'All publications'),
          mutate(d_yearly_newd,
                 data = 'Publications with new data only')) %>%
  ggplot() +
  facet_wrap(~ data) +
  geom_ribbon(aes(gdp / 1e12, ymin = lwr, ymax = upr), preds, alpha = 0.3)+
  geom_point(aes(gdp / 1e12, n_papers), alpha = 0.3) +
  geom_line(aes(gdp / 1e12, mu), preds) +
  scale_x_log10() +
  labs(x = expression('Yearly GDP (trillion USD,'~log[10]~'scale)'),
       y = 'Number of publications')

ggsave('figures/papers-gdp.png', width = 12, height = 6, dpi = 600)
