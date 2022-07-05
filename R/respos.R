# Multilevel Regression and Poststratification


library(tidyverse)

# load CES dataset
load( 'data/CES-2020-All.RData' )


target <- ces |> 
  group_by(abb) |> 
  summarize(pct_support = mean(defund_police),
            num_respondents = n())

# plot the results from the CES nationally-representative sample
p <- target |>
  mutate(abb = fct_reorder(abb, pct_support)) |> 
  ggplot() +
  geom_point(mapping = aes(x = pct_support,
                           y = abb))

p


# draw a sample
set.seed(42)  # set a random seed

ces_sample <- ces |> 
  slice_sample(n = 3000)

sample_summary <- ces_sample |> 
  group_by(abb) |> 
  summarize(estimate = mean(defund_police),
            num_respondents = n())


# add points for the sample means
p +
  geom_point(data = sample_summary,
             mapping = aes(x = estimate,
                           y = abb),
             color = 'red')

## Step 1: Fit a model -------

model1 <- glm(defund_police ~ gender +
                age_cat + 
                educ + 
                military_household,
              family = 'binomial',
              data = ces_sample)

library(modelsummary)

modelplot(model1)


## Step 2: Postratification -----------------


# need to know in every state, for every
# combination of characteristics, how many people
# are in that group in the broader population

psframe <- ces |> 
  count(abb, gender, age_cat, educ, military_household)

# predict proability of supporting the policy on psframe
psframe$predicted_probability <- predict(model1,
                                         psframe,
                                         type = 'response')

# MRP estimates are a weighted average of predicted probabilities
mrp <- psframe |> 
  group_by(abb) |> 
  summarize(estimate = 
              weighted.mean(predicted_probability, n))

# Let's see how we did
p +
  geom_point(data = sample_summary,
             mapping = aes(x = estimate,
                           y= abb),
             color = 'red') +
  geom_point(data = mrp,
             mapping = aes(x = estimate,
                           y = abb),
             color = 'blue')


# look at the average error
target |> 
  left_join(sample_summary |> select(-num_respondents)) |> 
  summarize(mean_abs_error = mean(abs(estimate - pct_support)))

target |> 
  left_join(mrp) |> 
  summarize(mean_abs_error = mean(abs(estimate - pct_support)))


## Next, we'll make an even *better* model --------------

model2 <- glm(defund_police ~ age_cat + pew_religimp + gender +
                race + urban + abb + educ +
                biden_vote_share +
                homicide_rate, 
              data = ces_sample)

modelplot(model2)

# step 2: poststratification
psframe <- ces |> 
  count(abb, age_cat, pew_religimp, gender,
          race, urban, educ,
          biden_vote_share, homicide_rate)

# predict proability of supporting the policy on psframe
psframe$predicted_probability <- predict(model2,
                                         psframe,
                                         type = 'response')

# MRP estimates are a weighted average of predicted probabilities
mrp2 <- psframe |> 
  group_by(abb) |> 
  summarize(estimate = 
              weighted.mean(predicted_probability, n))

# plot it to see how we did
p +
  geom_point(data = sample_summary,
             mapping = aes(x = estimate,
                           y= abb),
             color = 'red') +
  geom_point(data = mrp2,
             mapping = aes(x = estimate,
                           y = abb),
             color = 'blue')


target |> 
  left_join(mrp2) |> 
  summarize(mean_abs_error = mean(abs(estimate - pct_support)))


## Fit a regularized model --------------------

library(tidymodels)

model3 <- logistic_reg(engine = 'glmnet',
                       penalty = 0.01,
                       mixture = 1) |> 
  fit(formula = factor(defund_police) ~ age_cat + 
        pew_religimp + gender +
        race + urban + abb + educ +
        biden_vote_share +
        homicide_rate, 
      data = ces_sample)

tidy(model3)

psframe <- ces |> 
  count(abb, age_cat, pew_religimp, gender,
        race, urban, educ,
        biden_vote_share, homicide_rate)

# predict probability of supporting the policy on psframe
psframe <- psframe |> 
  bind_cols(predict(model3,
                    psframe,
                    type = 'prob'))

# MRP estimates are a weighted average of predicted probabilities
mrp3 <- psframe |> 
  group_by(abb) |> 
  summarize(estimate = 
              weighted.mean(.pred_1, n))

# plot it to see how we did
p +
  geom_point(data = sample_summary,
             mapping = aes(x = estimate,
                           y= abb),
             color = 'red') +
  geom_point(data = mrp3,
             mapping = aes(x = estimate,
                           y = abb),
             color = 'blue')

target |> 
  left_join(mrp3) |> 
  summarize(mean_abs_error = mean(abs(estimate - pct_support)))


library(ggrepel)

target |> 
  left_join(mrp3) |> 
  ggplot(mapping = aes(x= estimate, 
                       y = pct_support,
                       label = abb)) +
  geom_text_repel()

target |> 
  left_join(mrp3) |> 
  summarize(correlation = cor(estimate, pct_support))

target |> 
  left_join(sample_summary |> select(-num_respondents)) |> 
  summarize(correlation = cor(estimate, pct_support))



## Synthetic Poststratification ------------------------


model4 <- glm(defund_police ~ age_cat + pew_religimp,
              data = ces_sample)

modelplot(model4)


# I know the distribution of age in every state,
# but I need to include information on religion too.

state_populations <- ces |> 
  group_by(abb) |> 
  summarize(num_respondents = n())

psframe <- ces |> 
  count(abb, age_cat) |> 
  left_join(state_populations) |> 
  mutate(prob_age = n/ num_respondents) |> 
  select(abb, age_cat, prob_age)

marginal_distribution_religion <- ces |> 
  count(abb, pew_religimp) |> 
  left_join(state_populations) |> 
  mutate(prob_religion = n / num_respondents) |> 
  select(abb, pew_religimp, prob_religion)

synthetic_psframe <- psframe |> 
  left_join(marginal_distribution_religion, by = 'abb') |> 
  mutate(prob = prob_age * prob_religion)

# poststratify
synthetic_psframe$predicted_probability <- 
  predict(model4, synthetic_psframe, type = 'response')

mrp4 <- synthetic_psframe |> 
  group_by(abb) |> 
  summarize(estimate = weighted.mean(predicted_probability, prob))

# plot it to see how we did
p +
  geom_point(data = sample_summary,
             mapping = aes(x = estimate,
                           y= abb),
             color = 'red') +
  geom_point(data = mrp4,
             mapping = aes(x = estimate,
                           y = abb),
             color = 'blue')


target |> 
  left_join(mrp4) |> 
  summarize(mean_abs_error = mean(abs(estimate - pct_support)))



