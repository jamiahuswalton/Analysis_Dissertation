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



# Is data missing?
# Team
is_missing_data <- F
for(i in seq_along(team_data)){
  missing_total <- sum(is.na(team_data[[i]]))
  if(missing_total > 0){
    is_missing_data <- T
    break
  }
}
if(is_missing_data){
  stop("Missing data in team_data")
}

# Individual
is_missing_data <- F
for(i in seq_along(ind_data)){
  missing_total <- sum(is.na(ind_data[[i]]))
  if(missing_total > 0){
    is_missing_data <- T
    break
  }
}
if(is_missing_data){
  stop("Missing data in ind_data")
}


# Explore 

setwd(figure_directory)

# Is there an interaction between the session order and the Target levels (Team)?
plot_data_team <- team_data %>%
  select(Target, SessionOrder, TeamScore) %>%
  group_by(SessionOrder, Target) %>%
  summarise(TeamScoreAverage = mean(TeamScore), 
            Stdv =sd(TeamScore), n = length(TeamScore), 
            StEr = sd(TeamScore) / sqrt(length(TeamScore)))

ggplot(data = plot_data_team, aes(x = Target, y = TeamScoreAverage, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = TeamScoreAverage - StEr, ymax = TeamScoreAverage + StEr), width = 0.2) +
  labs(y = "Score", x = "Target", title = "Time Remaining Vs. Team Score")


plot_data_team <- team_data %>%
  select(Target, TeamScore) %>%
  group_by(Target) %>%
  summarise(TeamScoreAverage = mean(TeamScore),
            Stdv =sd(TeamScore), n = length(TeamScore), 
            StEr = sd(TeamScore) / sqrt(length(TeamScore)))
ggplot(data = plot_data_team, aes(x = Target, y = TeamScoreAverage, color = Target, shape = Target)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = TeamScoreAverage - StEr, ymax = TeamScoreAverage + StEr), width = 0.2) +
  labs(y = "Score", x = "Target", title = "Time Remaining Vs. Team Score")


# Is there an interaction between Session Order and Target level (Individual)?
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, IndividualScore) %>%
  group_by(SessionOrder, Target) %>%
  summarise(IndividualScoreAverage = mean(IndividualScore), 
            Stdv =sd(IndividualScore), 
            n = length(IndividualScore), 
            StEr = sd(IndividualScore) / sqrt(length(IndividualScore)))

ggplot(data = plot_data_ind, aes(x = Target, y = ITndividualScoreAverage, color = SessionOrder, shape=SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = IndividualScoreAverage - StEr, ymax = IndividualScoreAverage + StEr), width = 0.2) + 
  ggsave(filename = "Ind_Interaction_between_Session_and_Target.png")

# Is there an interaction between dominate strategy and target levels (Individual)?
# Individual Score
plot_data_ind <- ind_data %>%
  select(Target, IndividualScore, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(IndividualScoreAverage = mean(IndividualScore), 
            SD = sd(IndividualScore), 
            SE = sd(IndividualScore)/sqrt(length(IndividualScore)), 
            N = length(IndividualScore))

ggplot(data= plot_data_ind, aes(x = Target, y = IndividualScoreAverage, color = factor(Dominate.Strategy), shape = factor(Dominate.Strategy))) +
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = IndividualScoreAverage - SE, ymax = IndividualScoreAverage + SE, width = 0.2)) +
  facet_grid(.~Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_score.png")

# Individual time remaining
plot_data_ind <-  ind_data %>%
  select(Target, timeRemaining_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(TimeRemainingAverage = mean(timeRemaining_ind),
            SD = sd(timeRemaining_ind),
            SE = sd(timeRemaining_ind) / sqrt(length(timeRemaining_ind)),
            N = length(timeRemaining_ind))

ggplot(data = plot_data_ind, aes(x = Target, y = TimeRemainingAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = TimeRemainingAverage - SE, ymax = TimeRemainingAverage + SE, width = 0.2)) + 
  facet_grid(.~Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_timeRemaining.png", scale = 1.5)


# Individual Total Errors
plot_data_ind <-  ind_data %>%
  select(Target, ERROR_ind_total, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(ERROR_ind_totalAverage = mean(ERROR_ind_total),
            SD = sd(ERROR_ind_total),
            SE = sd(ERROR_ind_total) / sqrt(length(ERROR_ind_total)),
            N = length(ERROR_ind_total))

ggplot(data = plot_data_ind, aes(x = Target, y = ERROR_ind_totalAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = ERROR_ind_totalAverage - SE, ymax = ERROR_ind_totalAverage + SE, width = 0.2)) +
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_errorsTotal.png")
