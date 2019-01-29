# Tidy data ----

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

# Fit and pick model ----

# Team
data_modified_team <- team_data %>%
  select(TeamScore, 
         CI_team, 
         II_team, 
         timeRemaining_team, 
         ERROR_team_total, 
         Collection_rate_team,
         Collection_rate_correct_item_team, 
         Dis_total_team,
         transmitting_total_team_sec,
         utterance_count_team,
         Target, 
         SessionOrder, 
         Team)

# Model selection
    # TeamScore   
model.null <- lmer(TeamScore ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(TeamScore~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Time remaining - Team
model.null <- lmer(timeRemaining_team ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(timeRemaining_team~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Correct Items Collected - Team
model.null <- lmer(CI_team ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(CI_team~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Incorrect Items Collected 
    # Unique Errors Committed - Team
    


# Fit mmodel - Based on strategy

data_modified_team_strategy <- team_data %>%
  filter(Dominate.Strategy == "Go Together")%>%
  select(TeamScore, CI_team, II_team, timeRemaining_team, ERROR_team_total, Collection_rate_correct_item_team, Target, SessionOrder, Team)
model_team_strategy <- lmer(timeRemaining_team~Target+SessionOrder+(1|Team), data = data_modified_team_strategy)
summary(model_team_strategy)

emmeans(model_team, list(pairwise ~ Target), adjust = "tukey")


data_modified_ind_strategy <- ind_data %>%
  filter(Dominate.Strategy == "Go Together") %>%
  select(IndividualScore, 
         CI_ind, 
         II_ind, 
         timeRemaining_ind,
         ERROR_ind_total, 
         Collection_rate_correct_item_ind,
         Dis_total_ind,
         Mental.Demand,
         Physical.Demand,
         Temporal.Demand,
         Effort,
         Frustration,
         Performance,
         Target, 
         SessionOrder, 
         Team, 
         Player_ID, 
         Dominate.Strategy)
model_ind_strategy <- lmer(Temporal.Demand ~ Target+SessionOrder+(1|Team)+(1|Player_ID), data = data_modified_ind_strategy)
summary(model_ind_strategy)


# Estimated Marginal Means
emmeans(model_ind, list(pairwise ~ Target), adjust = "tukey")
emmeans(model_team, list(pairwise ~ Target), adjust = "tukey")
