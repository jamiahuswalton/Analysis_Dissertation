# Analysis I

# Tidy data

# Data ----

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

# Is data missing?

is_missing_data <- F
for(i in seq_along(team_data)){
  missing_total <- sum(is.na(team_data[[i]]))
  if(missing_total > 0){
    is_missing_data <- T
    break
  }
}

if(is_missing_data){
  stop("There is missing data")
}

# Pick dependent variable ----
my_teamScore <- "TeamScore"
my_CI_team <- "CI_team"
my_II_team <- "II_team"
my_time_remaining_team <- "Time_remaining"
my_errors_uniqe_team <- "Errors_Unique"
my_collection_rate_team <- "team_collection_rate"
my_collection_rate_correct_item_team <- "team_collection_rate_correct_items"

my_individualScore <- "IndividualScore"
my_CI_ind <- "CI_ind"
my_II_ind <- "II_ind"
my_time_remaining_ind <- "Time_remaining_ind"
my_errors_uniqe_ind <- "ERROR_ind_unique"
my_collection_rate_ind <- "Collection_rate_ind"
my_collection_rate_correct_item_ind <- "Collection_rate_correct_item_ind"

dependet_variable <- my_CI_team



# Explore models ----
# team_data_exploritory <- team_data %>% filter(Dominate.Strategy == "Go Together")
team_data_exploritory<- team_data

model1 <- lm(TeamScore ~ 1, data= team_data_exploritory)
model2 <- lm(TeamScore ~ Target, data= team_data_exploritory)
model3 <- lm(TeamScore ~ Target + SessionOrder, data= team_data_exploritory)
model4 <- lm(TeamScore ~ Target * SessionOrder, data= team_data_exploritory)
model5 <- lmer(TeamScore ~ 1 + (1|Team), data = team_data_exploritory)
model6 <- lmer(TeamScore ~ Target + (1|Team), data = team_data_exploritory)
model7 <- lmer(TeamScore ~ Target + SessionOrder + (1|Team), data = team_data_exploritory)
model8 <- lmer(TeamScore ~ Target * SessionOrder + (1|Team), data = team_data_exploritory)
model9 <- lmer(TeamScore ~ Target * SessionOrder + Dominate.Strategy + (1|Team), data = team_data_exploritory)
model10 <- lmer(TeamScore ~ Target * SessionOrder + Dominate.Strategy*SessionOrder + (1|Team), data = team_data_exploritory)
anova(model5,model6, model7, model8, model9, model10)
anova(model7, model9)
anova(model3, model2, model4, test = "Chisq")
anova(model6, model7, model8)
anova(model2, model3, model4)



ind_data_exploritory <- ind_data %>%
  filter(Dominate.Strategy == "Go Together")
ind_data_exploritory <- ind_data

model1 <- lm(timeRemaining_ind ~ 1, data= ind_data_exploritory)
model2 <- lm(timeRemaining_ind ~ Target, data= ind_data_exploritory)
model3 <- lm(timeRemaining_ind ~ Target + SessionOrder, data= ind_data_exploritory)
model4 <- lm(timeRemaining_ind ~ Target * SessionOrder, data= ind_data_exploritory)
model5 <- lmer(timeRemaining_ind ~ 1 + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
model6 <- lmer(timeRemaining_ind ~ Target + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
model7 <- lmer(timeRemaining_ind ~ Target + SessionOrder + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
model8 <- lmer(timeRemaining_ind ~ Target * SessionOrder + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
model9 <- lmer(timeRemaining_ind ~ Target * SessionOrder + Dominate.Strategy + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
model10 <- lmer(timeRemaining_ind ~ Target * SessionOrder + Dominate.Strategy*SessionOrder + (1|Team) + (1|Player_ID), data = ind_data_exploritory)
anova(model2, model3, model4)
anova(model6, model7, model8)


# Fit Model ----

if(dependet_variable == my_teamScore){
  fit_rand_dependent <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = team_data)
  # File names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_TeamScoreMode.png"
  historgram_plot_file_name <- "Residuals_Histogram_TeamScoreModel.png"
  QQ_plot_file_name <- "Residual_QQ_TeamScore.png"
} else if(dependet_variable == my_CI_team){
  dependent_data<- team_data
  fit_rand_dependent <- lmer(CI_team~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_CI_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_CI_team.png"
  QQ_plot_file_name <- "Residual_QQ_CI_team.png"
} else if (dependet_variable == my_II_team){
  dependent_data<- team_data
  fit_rand_dependent <- lmer(II_team~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_II_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_II_team.png"
  QQ_plot_file_name <- "Residual_QQ_II.png"
} else if(dependet_variable == my_time_remaining_team){
  dependent_data<- team_data
  fit_rand_dependent <- lmer(timeRemaining_team~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_time_remaining_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_time_remaining_team.png"
  QQ_plot_file_name <- "Residual_QQ_time_remaining_team.png"
} else if(dependet_variable == my_errors_uniqe_team) {
  dependent_data<- team_data
  fit_rand_dependent <- lmer(ERROR_team_unique~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_time_ERROR_unique_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_ERROR_unique_team.png"
  QQ_plot_file_name <- "Residual_QQ_ERROR_unique_team.png"
} else if(dependet_variable == my_collection_rate_team){
  dependent_data<- team_data
  fit_rand_dependent <- lmer(Collection_rate_team~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_collection_rate_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_collection_rate_team.png"
  QQ_plot_file_name <- "Residual_QQ_collection_rate_team.png"
} else if(dependet_variable == my_collection_rate_correct_item_team){
  dependent_data<- team_data
  fit_rand_dependent <- lmer(Collection_rate_correct_item_team~Target+SessionOrder+(1|Team), data = team_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_collection_rate_correct_item_team.png"
  historgram_plot_file_name <- "Residuals_Histogram_collection_rate_correct_item_team.png"
  QQ_plot_file_name <- "Residual_QQ_collection_rate_correct_item_team.png"
} else if(dependet_variable == my_individualScore){
  fit_rand_dependent <- lmer(IndividualScore~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_IndividualScore_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_IndividualScore_ind.png"
  QQ_plot_file_name <- "Residual_QQ_IndividualScore_ind.png"
} else if(dependet_variable == my_CI_ind){
  fit_rand_dependent <- lmer(CI_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_CI_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_CI_ind.png"
  QQ_plot_file_name <- "Residual_QQ_CI_ind.png"
} else if(dependet_variable == my_II_ind){
  fit_rand_dependent <- lmer(II_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_II_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_II_ind.png"
  QQ_plot_file_name <- "Residual_QQ_II_ind.png"
} else if(dependet_variable == my_time_remaining_ind){
  fit_rand_dependent <- lmer(timeRemaining_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_time_remaining_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_time_remaining_ind.png"
  QQ_plot_file_name <- "Residual_QQ_time_remaining_ind.png"
} else if(dependet_variable == my_errors_uniqe_ind){
  fit_rand_dependent <- lmer(ERROR_ind_unique~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_ERROR_unique_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_ERROR_unique_ind.png"
  QQ_plot_file_name <- "Residual_QQ_ERROR_unique_ind.png"
} else if(dependet_variable == my_collection_rate_ind){
  fit_rand_dependent <- lmer(Collection_rate_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_collection_rate_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_collection_rate_ind.png"
  QQ_plot_file_name <- "Residual_QQ_collection_rate_ind.png"
} else if(dependet_variable == my_collection_rate_correct_item_ind){
  fit_rand_dependent <- lmer(Collection_rate_correct_item_ind~Target+SessionOrder+(1|Team)+(1|Player_ID), data = ind_data)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_correct_collection_rate_ind.png"
  historgram_plot_file_name <- "Residuals_Histogram_correct_collection_rate_ind.png"
  QQ_plot_file_name <- "Residual_QQ_correct_collection_rate_ind.png"
} else {
  stop("The dependent variable is not recognized")
}

summary(fit_rand_dependent)

# Assumption: Your data needs to show homoscedasticity, which is where the variances along the line of best fit remain similar as you move along the line ----
# Fitted values
setwd(figure_directory)
ggplot(fit_rand_dependent, aes(x = .fitted, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)+
  labs(title = paste("Fitted-Value Plot"), x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave(filename = fitted_plot_file_name)

# Assumption #7: Finally, you need to check that the residuals (errors) are approximately normally distributed ----
residuals_formula <- residuals(fit_rand_dependent)

# Histogram of residuals
setwd(figure_directory)
ggplot(fit_rand_dependent, aes(x = residuals_formula)) +
  geom_histogram() + 
  labs(title = paste("Histogram of Residuals"), x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))+
ggsave(filename = historgram_plot_file_name)

# QQ plot
setwd(figure_directory)
ggplot(data = team_data, aes(sample = residuals_formula)) +
  stat_qq() +
  stat_qq_line()+
  labs(title = "Normal Q-Q Plot", x = "Theoretical", y = "Sample") +
   theme(plot.title = element_text(hjust = 0.5)) +
  ggsave(filename = QQ_plot_file_name)

# Effect size, R^2 (GLMM) ----
r.squaredGLMM(fit_rand_dependent)
#R2m (Fixed effects)

#R2C (entire model)

# Generate "Show me the data" figures

# Generate plots for Individual performance

# The N text to add to title for Inds 
N_ind_full_text <- paste("(N = ", N_ind, ")", sep = "")

myfigure_titles <- c("Individual Scores", 
                     "Correct Items Collected", 
                     "Incorrect Items Collected", 
                     "Distance (Total)", 
                     "Time remaining (Ind)", 
                     "Errors (Unique)",
                     "Collection Rate (Individual)",
                     "Collection Rate for Correct Items (Individual)")

myy_values_ind <- c("IndividualScore", 
                    "CI_ind", "II_ind", 
                    "Dis_total_ind", 
                    "timeRemaining_team", 
                    "ERROR_ind_unique",
                    "Collection_rate_ind",
                    "Collection_rate_correct_item_ind")

myy_labels_ind <- c("Individual Score", 
                    "Correct Items (Individual)", 
                    "Incorrect Items (Individual)", 
                    "Distance (Total)", "Time (s)", 
                    "Errors(Unique)",
                    "Rate (sec per item)",
                    "Rate (correct items)(sec per item)")

myx_values <- c("SessionOrder", 
                "Target")

myx_labels_ind <- c("Session", 
                    "Target")

myplot_types <- c("Group_Bar", 
                  "Boxplot", 
                  "Point_plot")

generate_figures_ind(Data = ind_data,
                     num_of_players = N_ind,
                     figure_titles = myfigure_titles,
                     y_values_ind = myy_values_ind,
                     y_labels_ind = myy_labels_ind,
                     x_values_ind = myx_values,
                     x_labels_ind = myx_labels_ind,
                     plot_types_ind = myplot_types,
                     filelocation = figure_directory)

#Generate Plots for Team performance

# What is the N for Teams
myfigure_titles <- c("Team Scores", 
                     "Correct Items Collected", 
                     "Incorrect Items Collected", 
                     "Distance (Total)", 
                     "Time Remaining (Team)", 
                     "Errors (Unique)",
                     "Collection Rate (Team)",
                     "Collection Rate for Correct Items (Team)")
myy_values_team <- c("TeamScore", 
                     "CI_team", 
                     "II_team", 
                     "Dis_total_team", 
                     "timeRemaining_team", 
                     "ERROR_team_unique",
                     "Collection_rate_team",
                     "Collection_rate_correct_item_team")
myy_labels_team <- c("Team Score", 
                     "Correct Items (Team)", 
                     "Incorrect Items (Team)", 
                     "Distance (Total)", 
                     "Time (s)", 
                     "Errors (Unique)",
                     "Rate (sec per item)",
                     "Rate (correct items)(sec per item)")
myx_values <- c("SessionOrder", 
                "Target")
myx_labels_team <- c("Session", 
                     "Target")
myplot_types <- c("Group_Bar", 
                  "Boxplot", 
                  "Point_plot")

generate_figures_team(Data = team_data,
                      num_of_teams = N_teams,
                      figure_titles = myfigure_titles,
                      y_values_team = myy_values_team,
                      y_labels_team = myy_labels_team,
                      x_values = myx_values,
                      x_labels_team = myx_labels_team,
                      plot_types = myplot_types, 
                      filelocation = figure_directory)



# Estimated Marginal Means (EMM) ----

# Target, Session
emmeans(fit_rand_dependent, list(pairwise ~ Target + SessionOrder), adjust = "tukey")
# ggplot(clean_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
#   geom_jitter() +
#   geom_boxplot(alpha = .8) +
#   facet_grid(. ~ SessionOrder) +
#   labs(x = "Target",
#        y = "Team Score")

# Target
emmeans(fit_rand_dependent, list(pairwise ~ Target), adjust = "tukey")
# ggplot(clean_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
#   geom_jitter() +
#   geom_boxplot(alpha = .8) +
#   # facet_grid(. ~ SessionOrder) +
#   labs(x = "Target",
#        y = "Team Score") 

# Session Order
emmeans(fit_rand_dependent, list(pairwise ~ SessionOrder), adjust = "tukey")
# ggplot(clean_aggregate_data_stats, aes(x=SessionOrder, y=TeamScore)) +
#   geom_jitter() +
#   geom_boxplot(alpha = .8) +
#   # facet_grid(. ~ SessionOrder) +
#   labs(x = "Session",
#        y = "Team Score") 

# ANOVA ----
anova(fit_rand_dependent, test = "Chisq")
summary(aov(IndividualScore ~ Target*SessionOrder + Error(Player_ID/Target), data=dependent_data))
pairwise.t.test(dependent_data$IndividualScore, dependent_data$SessionOrder)