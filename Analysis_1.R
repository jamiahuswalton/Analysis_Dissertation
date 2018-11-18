# Analysis I (Assumptions were take from Laerd for multipl regression)

setwd(figure_directory)

# Posible Dependent variables
my_teamScore <- "TeamScore"
my_CI_team <- "CI_team"

# Dependant variable
dependet_variable <- my_CI_team

# Data ----
# Team
# team_aggregate_data_stats <- my_aggregate_data
team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data,col_name = "Condition", value = "A") # without none condition

# Re factor the columns
columns_to_refactor <- c("SessionOrder", "Team", "Player_ID", "Condition", "Dominate.Strategy", "Condition", "Target")
team_aggregate_data_stats <- re_factor_columns(team_aggregate_data_stats, columns_to_refactor)

# Number of teams and players ----
# What is the N for Teams
N_teams <- length(levels(factor(team_aggregate_data_stats$Team) ))

# Fit Model ----

if(dependet_variable == my_teamScore){
  fit_rand_dependent <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)
  # File names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_TeamScoreMode.png"
  historgram_plot_file_name <- "Residuals_Histogram_TeamScoreModel.png"
  QQ_plot_file_name <- "Residual_QQ_TeamScore.png"
} else if(dependet_variable == my_CI_team){
  fit_rand_dependent <- lmer(CI_team~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)
  #Files names for assumptions
  fitted_plot_file_name <- "Residuals_Fitted_Plot_CI.png"
  historgram_plot_file_name <- "Residuals_Histogram_CI.png"
  QQ_plot_file_name <- "Residual_QQ_CI.png"
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
ggplot(data = team_aggregate_data_stats, aes(sample = residuals_formula)) +
  stat_qq() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical", y = "Sample") +
   theme(plot.title = element_text(hjust = 0.5)) +
  ggsave(filename = QQ_plot_file_name)

# Effect size, R^2 (GLMM) ----
r.squaredGLMM(fit_rand_dependent)
#R2m (Fixed effects)

#R2C (entire model)

# Generate "Show me the data" figures
# What is the N for Teams

myfigure_titles <- c("Team Scores", "Correct Items Collected", "Incorrect Items Collected", "Distance (Total)", "Time Remaining (Team)", "Errors (Unique)")
myy_values_team <- c("TeamScore", "CI_team", "II_team", "Dis_total_team", "timeRemaining_team", "ERROR_team_unique")
myy_labels_team <- c("Team Score", "Correct Items (Team)", "Incorrect Items (Team)", "Distance (Total)", "Time(s)", "Count")
myx_values <- c("SessionOrder", "Target")
myx_labels_team <- c("Session", "Target")
myplot_types <- c("Group_Bar", "Boxplot", "Point_plot")

generate_figures(Data = team_aggregate_data_stats,
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
ggplot(team_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Team Score") 

# Target
emmeans(fit_rand_dependent, list(pairwise ~ Target), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=Target, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Target",
       y = "Team Score") 

# Session Order
emmeans(fit_rand_dependent, list(pairwise ~ SessionOrder), adjust = "tukey")
ggplot(team_aggregate_data_stats, aes(x=SessionOrder, y=TeamScore)) +
  geom_jitter() +
  geom_boxplot(alpha = .8) +
  # facet_grid(. ~ SessionOrder) +
  labs(x = "Session",
       y = "Team Score") 
