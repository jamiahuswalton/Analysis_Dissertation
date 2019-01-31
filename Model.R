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

# Model selection ----
  # Team Data to fit
data_modified_team <- team_data %>%
  select(TeamScore, 
         CI_team, 
         II_team, 
         timeRemaining_team, 
         ERROR_team_total,
         ERROR_team_unique,
         Target, 
         SessionOrder, 
         Team)

    # TeamScore   
model.null <- lmer(TeamScore ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(TeamScore~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

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
comparision.results

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
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Incorrect Items Collected - Team
model.null <- lmer(II_team ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(II_team~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # In this case, the Null model was not significantly different then the other modeles.

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Unique Errors Committed - Team
model.null <- lmer(ERROR_team_unique ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(ERROR_team_unique~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Total Errors - Team
model.null <- lmer(ERROR_team_total ~ 1 + (1|Team), REML = FALSE, data = data_modified_team )
model.All <- lmer(ERROR_team_total~Target*SessionOrder+(1|Team), REML = FALSE, data = data_modified_team)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

  # Individual data to fit
data_modified_ind <- ind_data %>%
  select(IndividualScore, 
         CI_ind, 
         II_ind, 
         timeRemaining_ind, 
         ERROR_ind_total,
         ERROR_ind_unique,
         Target, 
         SessionOrder, 
         Team, 
         Player_ID)

    # Individual Score
model.null <- lmer(IndividualScore ~ 1 + (1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind )
model.All <- lmer(IndividualScore~Target*SessionOrder+ + (1 | Player_ID), REML = FALSE, data = data_modified_ind)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Correct Items collected - Individual
model.null <- lmer(CI_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind )
model.All <- lmer(CI_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Incorrect Items collected - Individual
model.null <- lmer(II_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind )
model.All <- lmer(II_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # The null model produced the lowest AIC value. 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Uniqe Errors - Individual
model.null <- lmer(ERROR_ind_unique ~ 1 + (1|Team) + (1|Player_ID), REML = FALSE, data = data_modified_ind )
model.All <- lmer(ERROR_ind_unique~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction  

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Total Errors - Individual
model.null <- lmer(ERROR_ind_total ~ 1 + (1|Team) + (1|Player_ID), REML = FALSE, data = data_modified_ind )
model.All <- lmer(ERROR_ind_total~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = FALSE, data = data_modified_ind)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction  

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")
    


# Fit mmodel - Based on strategy ----

data_modified_team_strategy <- team_data %>%
  filter(Dominate.Strategy == "Go Together")%>%
  select(TeamScore, 
         CI_team, 
         II_team, 
         timeRemaining_team, 
         ERROR_team_total, 
         Collection_rate_correct_item_team, 
         Target, 
         SessionOrder, 
         Team)
model_team_strategy <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = data_modified_team_strategy)
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
