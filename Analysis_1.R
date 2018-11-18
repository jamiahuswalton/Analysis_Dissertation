# Analysis I (Assumptions were take from Laerd for multipl regression)

setwd(figure_directory)


# Data ----
# Team
team_aggregate_data_stats <- my_aggregate_data
team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data,col_name = "Condition", value = "A") # without none condition
# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Alone") # without none condition
# team_aggregate_data_stats <- remove_measures_with_given_value(data_set =  team_aggregate_data_stats,col_name = "Dominate.Strategy", value = "Go Together")

# Re factor the columns
columns_to_refactor <- c("SessionOrder", "Team", "Player_ID", "Condition", "Dominate.Strategy", "Condition", "Target")
team_aggregate_data_stats <- re_factor_columns(team_aggregate_data_stats, columns_to_refactor)

# Number of teams and players ----
# What is the N for Teams
N_teams <- length(levels(factor(team_aggregate_data_stats$Team) ))

# Fit Model ----
# new_level_order <- levels(team_aggregate_data_stats$Target)[c(2,4,1)] # This assumes condition A was 
# team_aggregate_data_stats$Target <-  factor(team_aggregate_data_stats$Target, new_level_order)
fit_rand_teamscore <- lmer(TeamScore~Target+SessionOrder+(1|Team), data = team_aggregate_data_stats)
summary(fit_rand_teamscore)

# Assumption: Your data needs to show homoscedasticity, which is where the variances along the line of best fit remain similar as you move along the line ----
# Fitted values
setwd(figure_directory)
ggplot(fit_rand_teamscore, aes(x = .fitted, y = .resid)) +
  geom_point() + 
  geom_hline(yintercept = 0)+
  labs(title = paste("Fitted-Value Plot"), x = "Fitted Values", y = "Residuals") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave(filename = "Residuals_Fitted_Plot_TeamSCoreMode.png")

# Assumption #7: Finally, you need to check that the residuals (errors) are approximately normally distributed ----
residuals_formula_teamscore <- residuals(fit_rand_teamscore)

# Histogram of residuals
setwd(figure_directory)
ggplot(fit_rand_teamscore, aes(x = residuals_formula_teamscore)) +
  geom_histogram() + 
  labs(title = paste("Histogram of Residuals"), x = "", y = "") +
  theme(plot.title = element_text(hjust = 0.5))+
ggsave(filename = "Residuals_Histogram_TeamScoreModel.png")

# QQ plot
# qqnorm(residuals(fit_rand_teamscore))
# qqline(residuals(fit_rand_teamscore))
# savePlot(filename = "Residual_QQ", type = "png")
setwd(figure_directory)
ggplot(data = team_aggregate_data_stats, aes(sample = residuals_formula_teamscore)) +
  stat_qq() +
  labs(title = "Normal Q-Q Plot", x = "Theoretical", y = "Sample") +
   theme(plot.title = element_text(hjust = 0.5)) +
  ggsave(filename = "Residual_QQ.png")

# Effect size, R^2 (GLMM) ----
r.squaredGLMM(fit_rand_teamscore)
#R2m (Fixed effects)

#R2C (entire model)

# Generate "Show me the data" figures
# What is the N for Teams

myfigure_titles <- c("Team Scores", "Correct Items Collected", "Incorrect Items Collected", "Distance (Total)", "Time Remaining (Team)")
myy_values_team <- c("TeamScore", "CI_team", "II_team", "Dis_total_team", "timeRemaining_team")
myy_labels_team <- c("Team Score", "Correct Items (Team)", "Incorrect Items (Team)", "Distance (Total)", "Time(s)")
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
                 filelocation = myFileLocation)

# Estimated Marginal Means (EMM) ----

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
