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
  select(Target, SessionOrder, timeRemaining_team) %>%
  group_by(SessionOrder, Target) %>%
  summarise(timeRemaining_teamAverage = mean(timeRemaining_team), 
            Stdv =sd(timeRemaining_team), n = length(timeRemaining_team), 
            StEr = sd(timeRemaining_team) / sqrt(length(timeRemaining_team)))

ggplot(data = plot_data_team, aes(x = Target, y = timeRemaining_teamAverage, color = SessionOrder, shape=SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder)) + 
  geom_errorbar(aes(ymin = timeRemaining_teamAverage - StEr, ymax = timeRemaining_teamAverage + StEr), width = 0.2) 



# Is there an interaction between Session Order and Target level (Individual)?
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, IndividualScore) %>%
  group_by(SessionOrder, Target) %>%
  summarise(IndividualScoreAverage = mean(IndividualScore), 
            Stdv =sd(IndividualScore), 
            n = length(IndividualScore), 
            StEr = sd(IndividualScore) / sqrt(length(IndividualScore)))

ggplot(data = plot_data_ind, aes(x = Target, y = IndividualScoreAverage, color = SessionOrder, shape=SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = IndividualScoreAverage - StEr, ymax = IndividualScoreAverage + StEr), width = 0.2) + 
  ggsave(filename = "Ind_Interaction_between_Session_and_Target.png")

# Is there an interaction between dominate strategy and target levels (Individual)?
plot_data_ind <- ind_data %>%
  select(Target, IndividualScore, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(IndividualScoreAverage = mean(IndividualScore), 
            SD = sd(IndividualScore), 
            SE = sd(IndividualScore)/sqrt(length(IndividualScore)), 
            N = length(IndividualScore))

ggplot(data= plot_data_ind, aes(x = Target, y = IndividualScoreAverage, color = factor(Dominate.Strategy))) +
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = IndividualScoreAverage - SE, ymax = IndividualScoreAverage + SE, width = 0.2)) +
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target.png")
