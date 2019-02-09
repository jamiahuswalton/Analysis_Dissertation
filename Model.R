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
         Performance,
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
         Performance,
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
         Performance,
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
         Performance,
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
         Performance,
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
         Performance,
         Collection_rate_correct_item_ind,
         Target, 
         SessionOrder, 
         Team, 
         Player_ID)

  # General Dependent Vairbale ----
    # Team Level ----
response_variable <- "timeRemaining_team"
data_focus_team <- data_modified_team
model.null <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "null", is.team = TRUE)
model.All <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "All", is.team = TRUE)
model.NoInteraction <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoInteraction", is.team = TRUE)
model.NoTarget <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoTarget", is.team = TRUE)
model.NoSession <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoSession", is.team = TRUE)
model.NoInteraction.NoTarget <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoInteraction_NoTarget", is.team = TRUE)
model.NoInteraction.NoSession <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoInteraction_NoSession", is.team = TRUE)
model.NoTarget.NoSession <- model_data_Target_Session(df = data_focus_team, dependent =  response_variable, model.type =  "NoTarget_NoSession", is.team = TRUE)


comparision.results <- anova(model.null, 
                             model.All, 
                             model.NoInteraction,
                             model.NoTarget, 
                             model.NoSession, 
                             model.NoInteraction.NoTarget,
                             model.NoInteraction.NoSession,
                             model.NoTarget.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.team <- model.NoInteraction

summary(selected.model.team)

emmeans(selected.model.team, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.team, list(pairwise ~ SessionOrder), adjust = "tukey")

r.squaredGLMM(selected.model.team)[1,"R2c"]

      # Histogram ----

setwd(figure_directory)
ggplot(selected.model.team, aes(x = residuals(selected.model.team))) +
  geom_histogram() + 
  labs(title = paste("Histogram of Residuals"), x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

    # Individul Level ----
response_variable <- "timeRemaining_ind"
data_focus_ind <- data_modified_ind
model.null <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "null", is.team = FALSE)
model.All <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "All", is.team = FALSE)
model.NoInteraction <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoInteraction", is.team = FALSE)
model.NoTarget <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoTarget", is.team = FALSE)
model.NoSession <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoSession", is.team = FALSE)
model.NoInteraction.NoTarget <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoInteraction_NoTarget", is.team = FALSE)
model.NoInteraction.NoSession <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoInteraction_NoSession", is.team = FALSE)
model.NoTarget.NoSession <- model_data_Target_Session(df = data_focus_ind, dependent =  response_variable, model.type =  "NoTarget_NoSession", is.team = FALSE)

comparision.results <- anova(model.null, 
                             model.All, 
                             model.NoInteraction, 
                             model.NoTarget, 
                             model.NoSession, 
                             model.NoInteraction.NoTarget,
                             model.NoInteraction.NoSession,
                             model.NoTarget.NoSession)
comparision.results

rownames(comparision.results)[which(comparision.results$AIC == min(comparision.results$AIC))] # This line of code pickes the model with the lowest AIC score

selected.model.ind <- model.NoInteraction

summary(selected.model.ind)

emmeans(selected.model.ind, list(pairwise ~ Target), adjust = "tukey")
emmeans(selected.model.ind, list(pairwise ~ SessionOrder), adjust = "tukey")


# Test
rmodel<- rlmer(data = data_focus_team, as.formula(paste("timeRemaining_team","~ Target + SessionOrder + (1|Team)")))
rmodel2<- rlmer(data = data_focus_team, as.formula(paste("timeRemaining_team","~ Target + SessionOrder + (1|Team)")), 
                rho.sigma.e = psi2propII(smoothPsi, k = 1),
                rho.sigma.b = psi2propII(smoothPsi, k = 1))
rsb <- list(psi2propII(smoothPsi), psi2propII(smoothPsi, k = 1))

rmodel3<- update(rmodel2, rho.sigma.b = rsb)

compare(rmodel, rmodel2, model.NoInteraction, show.rho.functions = FALSE)

r.squaredGLMM(model.NoInteraction)

summary(model.NoInteraction)

summary(a.mes(m.1.adj = 58.0, m.2.adj = 66.6, sd.adj = 6.91, R = 1, n.1 =  117, n.2 = 117, q = 2))
