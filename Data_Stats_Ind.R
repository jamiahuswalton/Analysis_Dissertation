library(lme4)
library(lmerTest)
library(car)
library(emmeans)
library(ggplot2)

#--------# Data modified
individual_aggregate_data_stats <- my_aggregate_data
individual_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data,col_name = "Condition", value = "A") # without none condition

# individual_aggregate_data_stats <- remove_measures_with_given_value(data_set =  individual_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Alone") # without none condition
# individual_aggregate_data_stats <- remove_measures_with_given_value(data_set =  individual_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Together") # without none condition

# Factor the columns
# Factors columns - general, the columns that have a specific set of values should be facors. Factoring a columns has shown to work better with ANOVA function
individual_aggregate_data_stats$SessionOrder <- as.factor(individual_aggregate_data_stats$SessionOrder)
individual_aggregate_data_stats$Team <- as.factor(individual_aggregate_data_stats$Team)
individual_aggregate_data_stats$Player_ID <- as.factor(individual_aggregate_data_stats$Player_ID)
individual_aggregate_data_stats$Condition <- as.factor(individual_aggregate_data_stats$Condition)
individual_aggregate_data_stats$Target <- as.factor(individual_aggregate_data_stats$Target)
individual_aggregate_data_stats$Dominate.Strategy <- as.factor(individual_aggregate_data_stats$Dominate.Strategy)



# Individual Score----
fit_rand_indsocre <- lmer(IndividualScore~Target+SessionOrder+(1|Team)+(1|Player_ID), data = individual_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_indsocre, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=IndividualScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Individual Score") 

# Target
emmeans(fit_rand_indsocre, list(pairwise ~ Target), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=IndividualScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Individual Score") 

# Session Order
emmeans(fit_rand_indsocre, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=SessionOrder, y=IndividualScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Session",
       y = "Individual Score") 


# COrrect Items Collected ----
# --- Target, Session
fit_rand_CI_ind <- lmer(CI_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = individual_aggregate_data_stats)

emmeans(fit_rand_CI_ind, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=CI_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

#--- Target
emmeans(fit_rand_CI_ind, list(pairwise ~ Target), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=CI_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

#--- Session
emmeans(fit_rand_CI_ind, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=SessionOrder, y=CI_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Session",
       y = "Count") 


# Incorrect Items Collected ----
fit_rand_II_ind <- lmer(II_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = individual_aggregate_data_stats)

# Target, session
emmeans(fit_rand_II_ind, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=II_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

# Target
emmeans(fit_rand_II_ind, list(pairwise ~ Target), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=II_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count")

# Session
emmeans(fit_rand_II_ind, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=SessionOrder, y=II_ind)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count")


# Errors (Total)----
fit_rand_Error_total_ind <- lmer(ERROR_ind_total~Target+SessionOrder+(1|Team)+(1|Player_ID), data = individual_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_Error_total_ind, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=ERROR_ind_total)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 

# Target
emmeans(fit_rand_Error_total_ind, list(pairwise ~ Target ), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=Target, y=ERROR_ind_total)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 

# Session
emmeans(fit_rand_Error_total_ind, list(pairwise ~ SessionOrder ), adjust = "tukey")
ggplot(individual_aggregate_data_stats, aes(x=SessionOrder, y=ERROR_ind_total)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 
  