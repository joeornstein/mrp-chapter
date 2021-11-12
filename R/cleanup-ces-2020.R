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
         educ = case_when(educ == 1 ~ 'No_HS',
                          educ == 2 ~ 'High_school',
                          educ == 3 ~ 'Some_college',
                          educ == 4 ~ '2_year',
                          educ == 5 ~ '4_year',
                          educ == 6 ~ 'Post_grad'),
         race = case_when(race == 1 ~ 'White',
                          race == 2 ~ 'Black',
                          race == 3 ~ 'Hispanic',
                          race == 4 ~ 'Asian',
                          TRUE ~ 'Other'),
         age = 2020 - birthyr, 
         pew_religimp = case_when(pew_religimp == 1 ~ 'Very_important',
                                  pew_religimp == 2 ~ 'Somewhat_important',
                                  pew_religimp == 3 ~ 'Not_too_important',
                                  pew_religimp == 4 ~ 'Not_at_all_important'),
         homeowner = as.numeric(ownhome == 1),
         urban = case_when(urbancity == 1 ~ 'Urban',
                           urbancity %in% c(2,3,5) ~ 'Suburban',
                           urbancity == 4 ~ 'Rural'),
         parent = as.numeric(child18 == 1),
         # there are about 161 respondents that don't check anything in the military household sequence. Better to code them as not military households.
         military_household = as.numeric(milstat_1 == 1 |
                                           milstat_2 == 1 |
                                           milstat_3 == 1 |
                                           milstat_4 == 1),
         defund_police = 2 - cc20_334d)

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
  # select the variables we want to keep in the
  # cleaned up dataset
  select(caseid, gender, educ, race, age, abb,
         pew_religimp, homeowner, urban, parent, 
         military_household, defund_police)

# merge with state-level covariates
homicide_rates <- read_csv('data/raw/cdc-homicide-rates.csv') %>% 
  filter(YEAR == 2019) %>% 
  select(abb = STATE,
         homicide_rate = RATE)

election2020 <- read_csv('data/raw/1976-2020-president.csv') %>% 
  filter(year == 2020) %>% 
  filter(party_simplified %in% c('DEMOCRAT', 'REPUBLICAN')) %>% 
  select(abb = state_po,
         party_simplified,
         candidatevotes) %>% 
  pivot_wider(id_cols = abb,
              names_from = party_simplified,
              values_from = candidatevotes) %>% 
  mutate(biden_vote_share = DEMOCRAT / (DEMOCRAT + REPUBLICAN) * 100) %>% 
  select(abb, biden_vote_share)

ces <- ces %>% 
  left_join(homicide_rates, by = 'abb') %>% 
  left_join(election2020, by = 'abb')

# keep only the states with at least 500 respondents
states_to_keep <- ces %>% 
  count(abb) %>% 
  filter(n > 500) %>% 
  pull(abb)

ces <- filter(ces, abb %in% states_to_keep)

# drop 198 observations with missing values
ces <- na.omit(ces)

# write cleaned dataset to file
save(ces, file = 'data/CES-2020.RData')
