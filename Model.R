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
  # Team Data to fit ----
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

    # TeamScore ----
response_variable <- "TeamScore"
model.null <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "null", is.team = T)
model.All <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "All", is.team = T)
model.NoInteraction <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoInteraction", is.team = T)
model.NoTarget <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoTarget", is.team = T)
model.NoSession <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoSession", is.team = T)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Time remaining - Team ----
response_variable <- "timeRemaining_team"
model.null <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "null", is.team = T)
model.All <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "All", is.team = T)
model.NoInteraction <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoInteraction", is.team = T)
model.NoTarget <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoTarget", is.team = T)
model.NoSession <- model_data_Target_Session(df = team_data, dependent =  response_variable, model.type =  "NoSession", is.team = T)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Correct Items Collected - Team ----
model.null <- lmer(CI_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team )
model.All <- lmer(CI_team ~ Target * SessionOrder + (1|Team), REML = TRUE, data = data_modified_team)
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

    # Incorrect Items Collected - Team ----
model.null <- lmer(II_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team )
model.All <- lmer(II_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team)
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

    # Unique Errors Committed - Team ----
model.null <- lmer(ERROR_team_unique ~ 1 + (1|Team), REML = TRUE, data = data_modified_team )
model.All <- lmer(ERROR_team_unique~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team)
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

    # Total Errors - Team ----
model.null <- lmer(ERROR_team_total ~ 1 + (1|Team), REML = TRUE, data = data_modified_team )
model.All <- lmer(ERROR_team_total~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team)
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

    # Time Remaining - Team ----
model.null <- lmer(timeRemaining_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team )
model.All <- lmer(timeRemaining_team ~ Target * SessionOrder + (1|Team), REML = TRUE, data = data_modified_team)
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

  # Individual data to fit ----
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

    # Individual Score ----
model.null <- lmer(IndividualScore ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(IndividualScore~Target*SessionOrder+ + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

    # Correct Items collected - Individual ----
model.null <- lmer(CI_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(CI_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

    # Incorrect Items collected - Individual ----
model.null <- lmer(II_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(II_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

    # Uniqe Errors - Individual ----
model.null <- lmer(ERROR_ind_unique ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(ERROR_ind_unique~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

    # Total Errors - Individual ----
model.null <- lmer(ERROR_ind_total ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(ERROR_ind_total~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

    # Time Remaining - Individual ----
model.null <- lmer(timeRemaining_ind ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind )
model.All <- lmer(timeRemaining_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind)
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

data_modified_team_GT <- team_data %>%
  filter(Dominate.Strategy == "Go Together")%>%
  select(TeamScore, 
         CI_team, 
         II_team, 
         timeRemaining_team, 
         ERROR_team_total,
         ERROR_team_unique,
         Target, 
         SessionOrder, 
         Team)

data_modified_team_GA <- team_data %>%
  filter(Dominate.Strategy == "Go Alone")%>%
  select(TeamScore, 
         CI_team, 
         II_team, 
         timeRemaining_team, 
         ERROR_team_total,
         ERROR_team_unique,
         Target, 
         SessionOrder, 
         Team)

    # TeamScore - Go Together  ----
model.null <- lmer(TeamScore ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(TeamScore ~ Target * SessionOrder + (1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # TeamScore - Go Alone ----
model.null <- lmer(TeamScore ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(TeamScore ~ Target * SessionOrder + (1|Team), REML = TRUE, data = data_modified_team_GA)
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

    # Correct Items Collected - Team - Go Together ----
model.null <- lmer(CI_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(CI_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # Correct Items Collected - Team - Go Alone ----
model.null <- lmer(CI_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(CI_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GA)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # Null model produced lowest AIC value

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Incorrect Items Collected - Team - Go Together ----
model.null <- lmer(II_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(II_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # Incorrect Items Collected - Team - Go Alone ----
model.null <- lmer(II_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(II_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GA)
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

    # Unique Errors Committed - Team - Go Together ----
model.null <- lmer(ERROR_team_unique ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(ERROR_team_unique~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # Unique Errors Committed - Team - Go Alone ----
model.null <- lmer(ERROR_team_unique ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(ERROR_team_unique~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GA)
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

    # Total Errors - Team - Go Together ----
model.null <- lmer(ERROR_team_total ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(ERROR_team_total~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # Total Errors - Team - Go Alone ----
model.null <- lmer(ERROR_team_total ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(ERROR_team_total~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GA)
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

    # Time Remaining - Team - Go Together ----
model.null <- lmer(timeRemaining_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GT )
model.All <- lmer(timeRemaining_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GT)
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

    # Time Remaining - Team - Go Alone ----
model.null <- lmer(timeRemaining_team ~ 1 + (1|Team), REML = TRUE, data = data_modified_team_GA )
model.All <- lmer(timeRemaining_team~Target*SessionOrder+(1|Team), REML = TRUE, data = data_modified_team_GA)
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

  # Individual data to fit - Strategy ----
data_modified_ind_GT <- ind_data %>%
  filter(Dominate.Strategy == "Go Together") %>%
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

data_modified_ind_GA <- ind_data %>%
  filter(Dominate.Strategy == "Go Alone") %>%
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
    # Individual Score - Go Together ----
model.null <- lmer(IndividualScore ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT )
model.All <- lmer(IndividualScore~Target*SessionOrder + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # The null model produced the lowest AIC 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Individual Score - Go Alone ----
model.null <- lmer(IndividualScore ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
model.All <- lmer(IndividualScore~Target*SessionOrder+ + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
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

    # Correct Items collected - Individual - Go Together ----
model.null <- lmer(CI_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT )
model.All <- lmer(CI_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
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

    # Correct Items collected - Individual - Go Alone ----
model.null <- lmer(CI_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA )
model.All <- lmer(CI_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # THe null model produced the lowest AIC value 

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Incorrect Items collected - Individual - Go Together ----
model.null <- lmer(II_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT )
model.All <- lmer(II_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
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

    # Incorrect Items collected - Individual - Go Alone ----
model.null <- lmer(II_ind ~ 1 + (1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA )
model.All <- lmer(II_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.null # Null model produced the lowest AIC

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Uniqe Errors - Individual - Go Together ----
model.null <- lmer(ERROR_ind_unique ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GT )
model.All <- lmer(ERROR_ind_unique~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
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

    # Uniqe Errors - Individual - Go Alone ----
model.null <- lmer(ERROR_ind_unique ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GA )
model.All <- lmer(ERROR_ind_unique~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
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

    # Total Errors - Individual - Go Together ----
model.null <- lmer(ERROR_ind_total ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GT )
model.All <- lmer(ERROR_ind_total~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
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

    # Total Errors - Individual - Go Alone ----
model.null <- lmer(ERROR_ind_total ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GA)
model.All <- lmer(ERROR_ind_total~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
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


    # Time Remaining - Individual - Go Together ----
model.null <- lmer(timeRemaining_ind ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GT)
model.All <- lmer(timeRemaining_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GT)
model.NoInteraction <- update(model.All, . ~. - Target:SessionOrder)
model.NoTarget <- update(model.All, . ~. - Target)
model.NoSession <- update(model.All, . ~. - SessionOrder)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoSession  

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")


    # Time Remaining - Individual - Go Alone ----
model.null <- lmer(timeRemaining_ind ~ 1 + (1|Team) + (1|Player_ID), REML = TRUE, data = data_modified_ind_GA)
model.All <- lmer(timeRemaining_ind~Target*SessionOrder+(1|Team) + (1 | Player_ID), REML = TRUE, data = data_modified_ind_GA)
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