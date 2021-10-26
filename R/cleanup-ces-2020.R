# ---
# this R script loads and cleans up the CES 2020 Common Content, 
# creating the RData file available on the code repository. 
# You can find the csv file below at the Harvard Dataverse 
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi%3A10.7910/DVN/E9N6PH
#
# author: Joe Ornstein
# date: 2021-10-26
# version: 0.1
# ---

library(tidyverse)
library(janitor)

ces_raw <- read_csv('data/raw/CES20_Common_OUTPUT_vv.csv') %>% 
  # clean up the names (janitor function)
  clean_names()

ces <- ces_raw %>%
  # recode some variables
  mutate(gender = if_else(gender == 1, 'Male', 'Female'),
         educ = case_when(educ == 1 ~ 'No HS',
                          educ == 2 ~ 'High school graduate',
                          educ == 3 ~ 'Some college',
                          educ == 4 ~ '2-year',
                          educ == 5 ~ '4-year',
                          educ == 6 ~ 'Post-grad'),
         race = case_when(race == 1 ~ 'White',
                          race == 2 ~ 'Black',
                          race == 3 ~ 'Hispanic',
                          race == 4 ~ 'Asian',
                          TRUE ~ 'Other'),
         age = 2020 - birthyr, 
         national_economy = case_when(cc20_302 == 1 ~ 'Gotten much better',
                                      cc20_302 == 2 ~ 'Gotten somewhat better',
                                      cc20_302 == 3 ~ 'Stayed about the same',
                                      cc20_302 == 4 ~ 'Gotten somewhat worse',
                                      cc20_302 == 5 ~ 'Gotten much worse',
                                      cc20_302 == 6 ~ 'Not sure'),
         someone_diagnosed_with_covid = if_else(cc20_309a_5 == 2, 'Yes', 'No'),
         trump_approval = case_when(cc20_320a == 1 ~ 'Strongly approve',
                                    cc20_320a == 2 ~ 'Somewhat approve',
                                    cc20_320a == 3 ~ 'Somewhat disapprove',
                                    cc20_320a == 4 ~ 'Strongly disapprove',
                                    cc20_320a == 5 ~ 'Not sure'),
         lost_work_during_covid = if_else(cc20_309c_1 == 1 |
                                            cc20_309c_2 == 1 |
                                            cc20_309c_3 == 1 |
                                            cc20_309c_4 == 1 |
                                            cc20_309c_5 == 1 |
                                            cc20_309c_6 == 1, 
                                          'Yes', 'No'),
         social_media_24h = if_else(cc20_300_1 == 1, 'Yes', 'No'),
         tv_news_24h = if_else(cc20_300_2 == 1, 'Yes', 'No'),
         newspaper_24h = if_else(cc20_300_3 == 1, 'Yes', 'No'),
         radio_news_24h = if_else(cc20_300_4 == 1, 'Yes', 'No'),
         assault_rifle_ban = if_else(cc20_330b == 1, 'Support', 'Oppose'),
         increase_border_patrols = if_else(cc20_331b == 1, 'Support', 'Oppose'),
         china_tariffs = if_else(cc20_338a == 1, 'Support', 'Oppose'),
         pew_religimp = case_when(pew_religimp == 1 ~ 'Very important',
                                  pew_religimp == 2 ~ 'Somewhat important',
                                  pew_religimp == 3 ~ 'Not too important',
                                  pew_religimp == 4 ~ 'Not at all important'))

# inputstate is a fips code. merge with maps::state.fips to get state names
state_names <- maps::state.fips %>% 
  select(fips, abb) %>% 
  # missing alaska and hawaii
  bind_rows(
    tibble(
      fips = c(2, 15),
      abb = c('AK', 'HI'))) %>% 
  unique %>% 
  arrange(fips)

ces <- ces %>% 
  mutate(fips = inputstate) %>% 
  left_join(state_names, by = 'fips') %>% 
  # select the variables you want
  select(caseid, gender, educ, race, age, abb, national_economy,
         someone_diagnosed_with_covid, lost_work_during_covid,
         trump_approval,
         social_media_24h, tv_news_24h, newspaper_24h, radio_news_24h,
         assault_rifle_ban, increase_border_patrols, china_tariffs,
         pew_religimp) %>% 
  # order the factor variables
  mutate(national_economy = factor(national_economy,
                                   levels = c('Not sure', 'Gotten much worse',
                                              'Gotten somewhat worse', 'Stayed about the same',
                                              'Gotten somewhat better', 'Gotten much better')),
         trump_approval = factor(trump_approval,
                                 levels = c('Strongly disapprove', 'Somewhat disapprove',
                                            'Not sure', 'Somewhat approve', 'Strongly approve')),
         educ = factor(educ,
                       levels = c('No HS', 'High school graduate',
                                  'Some college', '2-year', '4-year',
                                  'Post-grad')))

# keep only the states with at least 500 respondents
states_to_keep <- ces %>% 
  count(abb) %>% 
  filter(n > 500) %>% 
  pull(abb)

ces <- filter(ces, abb %in% states_to_keep)

# write cleaned dataset to file
save(ces, file = 'data/CES-2020.RData')
