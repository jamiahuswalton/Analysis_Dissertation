# Tidy data

# clean_aggregate_data_stats <- my_aggregate_data
clean_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data, col_name = "Condition", value = "A") # without none condition

# Re factor the columns
columns_to_refactor <- c("SessionOrder", "Team", "Player_ID", "Condition", "Dominate.Strategy", "Condition", "Target")
clean_aggregate_data_stats <- re_factor_columns(clean_aggregate_data_stats, columns_to_refactor)

# What is the N for Teams
N_teams <- length(levels(factor(clean_aggregate_data_stats$Team)))

# What is the N for Inds
N_ind <- length(levels(factor(clean_aggregate_data_stats$Player_ID) ))

# Data set - Team and Individual
team_data <- clean_aggregate_data_stats %>%
  filter(Player == 1)

ind_data <- clean_aggregate_data_stats

# Fit mmodel

data_modified_team <- team_data %>%
  filter(Dominate.Strategy == "Go Together")%>%
  select(TeamScore, CI_team, II_team, timeRemaining_team, ERROR_team_total, Collection_rate_correct_item_team, Target, SessionOrder, Team)
model_team <- lmer(timeRemaining_team~Target+SessionOrder+(1|Team), data = data_modified_team)
summary(model_team)

data_modified_ind <- ind_data %>%
  filter(Dominate.Strategy == "Go Together") %>%
  select(IndividualScore, 
         CI_ind, 
         II_ind, 
         timeRemaining_ind,
         ERROR_ind_total, 
         Collection_rate_correct_item_ind,
         Dis_total_ind,
         Mental.Demand,
         Target, 
         SessionOrder, 
         Team, 
         Player_ID, 
         Dominate.Strategy)
model_ind <- lmer(Mental.Demand~Target+SessionOrder+(1|Team)+(1|Player_ID), data = data_modified_ind)
summary(model_ind)


# Estimated Marginal Means
emmeans(model_ind, list(pairwise ~ Target), adjust = "tukey")
emmeans(model_team, list(pairwise ~ Target), adjust = "tukey")
