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
theme_set(theme_bw())

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
  # add GPD/capita
  left_join(read.csv('data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_6224630.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp_per_capita',
                           names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))) %>%
              select(Country.Name, year, gdp_per_capita),
            by = c('country' = 'Country.Name', 'year'))

# missing US GDP in 2023
# GDP estimate: https://www.bea.gov/news/2024/gross-domestic-product-fourth-quarter-and-year-2023-second-estimate
# population estimate: https://www.census.gov/newsroom/press-releases/2023/population-trends-return-to-pre-pandemic-norms.html
filter(d_yearly, is.na(gdp))
d_yearly <- mutate(d_yearly,
                   gdp = if_else(is.na(gdp), 27.36e12, gdp),
                   gdp_per_capita = if_else(is.na(gdp_per_capita),
                                            27.36e12 / 334914895,
                                            gdp_per_capita))

plot(gdp / gdp_per_capita ~ year,
     filter(d_yearly, country == 'United States'))
abline(h = 334914895, col = 'red')

# only with new field data
d_yearly_newd <-
  # count number of papers per year
  papers %>%
  filter(collected_data) %>%
  group_by(year, country) %>%
  summarise(n_papers = n()) %>%
  # add GDP
  left_join(read.csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6224532.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp', names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))),
            by = c('country' = 'Country.Name', 'year')) %>%
  # add GPD/capita
  left_join(read.csv('data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_6224630.csv',
                     skip = 3) %>%
              pivot_longer(X1960:X2022, values_drop_na = TRUE,
                           values_to = 'gdp_per_capita',
                           names_to = 'year') %>%
              mutate(year = as.integer(substr(year, 2, nchar(year)))),
            by = c('country' = 'Country.Name', 'year'))

# need to add GDP again
filter(d_yearly_newd, is.na(gdp))
d_yearly_newd <- mutate(d_yearly_newd,
                        gdp = if_else(is.na(gdp), 27.36e12, gdp),
                        gdp_per_capita = if_else(is.na(gdp_per_capita),
                                                 27.36e12 / 334914895,
                                                 gdp_per_capita))

# not a large range of years
ggplot(d_yearly, aes(year)) +
  geom_histogram(binwidth = 1, fill = 'grey', color = 'black') +
  labs(x = 'Publication year', y = 'Number of papers')
mean(d_yearly$year)

# all data
ggplot(d_yearly) +
  geom_point(aes(gdp / 1e12, n_papers), alpha = 0.2) +
  geom_smooth(aes(gdp / 1e12, n_papers), color = 'black', method = 'gam',
              formula = y ~ s(x, k = 10),
              method.args = list(family = poisson(link = 'log'))) +
  scale_x_log10() +
  labs(x = expression('Yearly GDP (trillion USD,'~log[10]~'scale)'),
       y = 'Number of papers')

ggsave('figures/papers-gdp.png', width = 6, height = 6, dpi = 600)

# papers with new empirical data
ggplot(d_yearly_newd) +
  geom_point(aes(gdp / 1e12, n_papers), alpha = 0.2) +
  geom_smooth(aes(gdp / 1e12, n_papers), color = 'black', method = 'gam',
              formula = y ~ s(x, k = 5),
              method.args = list(family = poisson(link = 'log'))) +
  scale_x_log10() +
  labs(x = expression('Yearly GDP (trillion USD,'~log[10]~'scale)'),
       y = 'Number of papers with new empirical data')

ggsave('figures/papers-gdp-new-empirical-data.png', width = 6, height = 6, dpi = 600)

# assuming GDP for 2018 is representative (pre-covid, mid-time series) ----
d_2018 <-
  left_join(read.csv('data/Movement Ecology data - data.csv') %>%
              transmute(country = First.Author.Affiliation)%>%
              group_by(country) %>%
              summarise(n_papers = n()),
            read.csv('data/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_6224532.csv',
                     skip = 3) %>%
              transmute(country = Country.Name, gdp = X2018),
            by = c('country')) %>%
  left_join(read.csv('data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_6224630.csv',
                     skip = 3) %>%
              transmute(country = Country.Name, gdp_per_capita = X2018),
            by = c('country'))

cowplot::plot_grid(
  # yearly GDP
  ggplot() +
    geom_point(aes(gdp, n_papers), d_yearly, alpha = 0.3) +
    geom_smooth(aes(gdp, n_papers), d_yearly, method = 'gam',
                formula = y ~ s(x, k = 3),
                method.args = list(family = poisson(link = 'log')),
                color = 'black') +
    scale_x_log10() +
    labs(x = 'Yearly GDP', y = 'Number of papers'),
  
  # yearly GDP per capita
  ggplot(d_yearly, aes(gdp_per_capita, n_papers)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = 'gam', formula = y ~ s(x, k = 3),
                method.args = list(family = poisson(link = 'log')),
                color = 'black') +
    scale_x_log10() +
    labs(x = 'Yearly GDP per capita', y = 'Number of papers'),
  
  # 2018 GDP
  ggplot() +
    geom_point(aes(gdp, n_papers), d_2018, alpha = 0.3) +
    geom_smooth(aes(gdp, n_papers), d_2018, method = 'gam',
                formula = y ~ s(x, k = 3),
                method.args = list(family = poisson(link = 'log')),
                color = 'black') +
    scale_x_log10() +
    labs(x = '2018 GDP', y = 'Number of papers'),
  
  # 2018 GDP per capita
  ggplot() +
    geom_point(aes(gdp_per_capita, n_papers), d_2018, alpha = 0.3) +
    geom_smooth(aes(gdp_per_capita, n_papers), d_2018, method = 'gam',
                formula = y ~ s(x, k = 3),
                method.args = list(family = poisson(link = 'log')),
                color = 'black') +
    scale_x_log10() +
    labs(x = '2018 GDP per capita', y = 'Number of papers'))
