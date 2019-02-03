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
  
  # Individual Data to Fit ----
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

data_modified_ind_GT <- ind_data %>%
  filter(Dominate.Strategy == "Go Together") %>%
  select(IndividualScore, 
         CI_ind, 
         II_ind, 
         timeRemaining_ind, 
         ERROR_ind_total,
         ERROR_ind_unique,
         Collection_rate_correct_item_ind,
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
         Collection_rate_correct_item_ind,
         Target, 
         SessionOrder, 
         Team, 
         Player_ID)

  # General Dependent Vairbale ----
    # Team Level ----
response_variable <- "TeamScore"
data_focus_team <- data_modified_team_GA
model.null <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "null", is.team = TRUE)
model.All <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "All", is.team = TRUE)
model.NoInteraction <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoInteraction", is.team = TRUE)
model.NoTarget <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoTarget", is.team = TRUE)
model.NoSession <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoSession", is.team = TRUE)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

    # Individul Level ----
response_variable <- "Collection_rate_correct_item_ind"
data_focus_ind <- data_modified_ind_GT
model.null <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "null", is.team = FALSE)
model.All <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "All", is.team = FALSE)
model.NoInteraction <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoInteraction", is.team = FALSE)
model.NoTarget <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoTarget", is.team = FALSE)
model.NoSession <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoSession", is.team = FALSE)

comparision.results <- anova(model.null, model.All, model.NoInteraction, model.NoTarget, model.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")
