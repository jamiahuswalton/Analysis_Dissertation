


#--------# Data modified ----
team_aggregate_data_stats <- my_aggregate_data

team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data,col_name = "Condition", value = "A") # without none condition

# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Alone") # without none condition
# 
# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Together")


# Factor the columns ----
# Factors columns - general, the columns that have a specific set of values should be facors. Factoring a columns has shown to work better with ANOVA function
team_aggregate_data_stats$SessionOrder <- as.factor(team_aggregate_data_stats$SessionOrder)
team_aggregate_data_stats$Team <- as.factor(team_aggregate_data_stats$Team)
team_aggregate_data_stats$Player_ID <- as.factor(team_aggregate_data_stats$Player_ID)
team_aggregate_data_stats$Condition <- as.factor(team_aggregate_data_stats$Condition)
team_aggregate_data_stats$Dominate.Strategy <- as.factor(team_aggregate_data_stats$Dominate.Strategy)
team_aggregate_data_stats$Condition <- as.factor(team_aggregate_data_stats$Condition)
team_aggregate_data_stats$Target <- as.factor(team_aggregate_data_stats$Target)

#Models (Mixed Effect Linear Regression)


# Team Score----
fit_rand_teamscore <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_teamscore, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Team Score") 

# Target
emmeans(fit_rand_teamscore, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Team Score") 

# Session Order
emmeans(fit_rand_teamscore, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Session",
       y = "Team Score") 

# COrrect Items Collected ----
# --- Target, Session
fit_rand_CI_team <- lmer(CI_team~Target+SessionOrder+(1|Team) + (1|Dominate.Strategy), data = team_aggregate_data_stats)

emmeans(fit_rand_CI_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=CI_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

#--- Target
emmeans(fit_rand_CI_team, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=CI_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

#--- Session
emmeans(fit_rand_CI_team, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=CI_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Session",
       y = "Count")


# Incorrect Items Collected ----
fit_rand_II_team <- lmer(II_team~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, session
emmeans(fit_rand_II_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=II_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count") 

# Target
emmeans(fit_rand_II_team, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=II_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count")

# Session
emmeans(fit_rand_II_team, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=II_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Count")




# Errors (Unique) ----
fit_rand_Error_unique_team <- lmer(ERROR_team_unique~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_Error_unique_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=ERROR_team_unique)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 


# Target
emmeans(fit_rand_Error_unique_team, list(pairwise ~ Target ), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=ERROR_team_unique)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 

# Session
emmeans(fit_rand_Error_unique_team, list(pairwise ~ SessionOrder ), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=ERROR_team_unique)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 


# Errors (Total)----
fit_rand_Error_total_team <- lmer(ERROR_team_total~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_Error_total_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=ERROR_team_total)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 

# Target
emmeans(fit_rand_Error_total_team, list(pairwise ~ Target ), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=ERROR_team_total)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 

# Session
emmeans(fit_rand_Error_total_team, list(pairwise ~ SessionOrder ), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=ERROR_team_total)) +
  geom_point() +
  # geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Error Count") 


# Distance Traveled ----
fit_rand_dis_team <- lmer(Dis_total_team~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_dis_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=Dis_total_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Distance")

# Target
emmeans(fit_rand_dis_team, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=Dis_total_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Distance")

# Session
emmeans(fit_rand_dis_team, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=Dis_total_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Distance") 



# Time remaining ----
fit_rand_time_remaining_team <- lmer(timeRemaining_team~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_time_remaining_team, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=timeRemaining_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)") 

# Target
emmeans(fit_rand_time_remaining_team, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=timeRemaining_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)") 

# Session
emmeans(fit_rand_time_remaining_team, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=timeRemaining_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)") 


# Total time transmitting ----
fit_rand_time_transm_total <- lmer(transmitting_total_team_sec~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_time_transm_total, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=transmitting_total_team_sec)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)")

# Target
emmeans(fit_rand_time_transm_total, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=transmitting_total_team_sec)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)")

# Session
emmeans(fit_rand_time_transm_total, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=transmitting_total_team_sec)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Time (s)")


# Utterance ----
fit_rand_utterance_count <- lmer(utterance_count_team~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)

# Target, Session
emmeans(fit_rand_utterance_count, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=utterance_count_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Utterance Count")

# Target
emmeans(fit_rand_utterance_count, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=utterance_count_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Utterance Count")

# Session
emmeans(fit_rand_utterance_count, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=utterance_count_team)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Utterance Count") 


