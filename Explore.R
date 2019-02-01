# Tidy data ----

# clean_aggregate_data_stats <- my_aggregate_data
clean_aggregate_data_stats <- remove_measures_with_given_value(data_set =  my_aggregate_data, col_name = "Condition", value = "A") # without none condition

# Re factor the columns
columns_to_refactor <- c("SessionOrder", 
                         "Team", 
                         "Player_ID", 
                         "Condition", 
                         "Dominate.Strategy", 
                         "Condition", 
                         "Target",
                         "Confident_team_comm_important_details_quickly")
clean_aggregate_data_stats <- re_factor_columns(clean_aggregate_data_stats, columns_to_refactor)

# What is the N for Teams
N_teams <- length(levels(factor(clean_aggregate_data_stats$Team)))

# What is the N for Inds
N_ind <- length(levels(factor(clean_aggregate_data_stats$Player_ID) ))

# Data set - Team and Individual
team_data <- clean_aggregate_data_stats %>%
  filter(Player == 1)

ind_data <- clean_aggregate_data_stats



# Is data missing? ----
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

# Performance Metrics ----
# Is there an interaction between the session order and the Target levels - Team Score(Team)? - NO ----
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
  labs(y = "Score", x = "Target", title = "Team Score Vs. Target", color = "Session", shape = "Session")


# Is there an interaction between the session order and the Target levels - Correct Items Colleced (Team)? ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, CI_team) %>%
  group_by(SessionOrder, Target) %>%
  summarise(CI_teamAverage = mean(CI_team), 
            Stdv =sd(CI_team), n = length(CI_team), 
            StEr = sd(CI_team) / sqrt(length(CI_team)))

ggplot(data = plot_data_team, aes(x = Target, y = CI_teamAverage, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = CI_teamAverage - StEr, ymax = CI_teamAverage + StEr), width = 0.2) +
  labs(y = "Count", x = "Target", title = "Correct Items Vs. Target", color = "Session", shape = "Session")


# Is there an interaction between Session Order and Target level - Individual Score (Individual)? ----
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

# Is there an interaction between dominate strategy and target levels (Individual)? ----
# Individual Score ----
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

# Individual time remaining ----
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


# Individual Total Errors ----
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
  facet_grid(.~ Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_errorsTotal.png")


# Incorrect Items Collected ----
plot_data_ind <-  ind_data %>%
  select(Target, II_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(II_indAverage = mean(II_ind),
            SD = sd(II_ind),
            SE = sd(II_ind) / sqrt(length(II_ind)),
            N = length(II_ind))

ggplot(data = plot_data_ind, aes(x = Target, y = II_indAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = II_indAverage - SE, ymax = II_indAverage + SE, width = 0.2)) +
  facet_grid(.~ Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_IncorrectItems.png")


# Correct item collection rate ----
plot_data_ind <- ind_data %>%
  select(Target, Collection_rate_correct_item_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(Collection_rate_correct_item_indAverage = mean(Collection_rate_correct_item_ind),
            SD = sd(Collection_rate_correct_item_ind),
            SE = sd(Collection_rate_correct_item_ind) / sqrt(length(Collection_rate_correct_item_ind)),
            N = length(Collection_rate_correct_item_ind))

ggplot(data = plot_data_ind, aes(x = Target, y = Collection_rate_correct_item_indAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = Collection_rate_correct_item_indAverage - SE, ymax = Collection_rate_correct_item_indAverage + SE, width = 0.2)) +
  facet_grid(.~ Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_CorrectItemCollectionRate.png")


# Distance traveled by an individual----
plot_data_ind <- ind_data %>%
  select(Target, Dis_total_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(Dis_total_indAverage = mean(Dis_total_ind),
            SD = sd(Dis_total_ind),
            SE = sd(Dis_total_ind) / sqrt(length(Dis_total_ind)),
            N = length(Dis_total_ind))

ggplot(data = plot_data_ind, aes(x = Target, y = Dis_total_indAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = Dis_total_indAverage - SE, ymax = Dis_total_indAverage + SE, width = 0.2)) +
  facet_grid(.~ Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_disTraveled.png")


# Self-reported performance by individual ----
plot_data_ind <- ind_data %>%
  select(Target, Performance, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(PerformanceAverage = mean(Performance),
            SD = sd(Performance),
            SE = sd(Performance) / sqrt(length(Performance)),
            N = length(Performance))

ggplot(data = plot_data_ind, aes(x = Target, y = PerformanceAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = PerformanceAverage - SE, ymax = PerformanceAverage + SE, width = 0.2)) +
  facet_grid(.~ Dominate.Strategy) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_PerformanceAverage.png")


# Self-reported Mental Demand by individual ----
plot_data_ind <- ind_data %>%
  select(Target, Mental.Demand, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(Mental.DemandAverage = mean(Mental.Demand),
            SD = sd(Mental.Demand),
            SE = sd(Mental.Demand) / sqrt(length(Mental.Demand)),
            N = length(Mental.Demand))

ggplot(data = plot_data_ind, aes(x = Target, y = Mental.DemandAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = Mental.DemandAverage - SE, ymax = Mental.DemandAverage + SE, width = 0.2)) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_Mental.DemandAverage.png")


# Self-reported Frustration ----
plot_data_ind <- ind_data %>%
  select(Target, Frustration, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(FrustrationAverage = mean(Frustration),
            SD = sd(Frustration),
            SE = sd(Frustration) / sqrt(length(Frustration)),
            N = length(Frustration))

ggplot(data = plot_data_ind, aes(x = Target, y = FrustrationAverage, color = Dominate.Strategy)) + 
  geom_point(size = 3) + 
  geom_line(aes(group = Dominate.Strategy, color = factor(Dominate.Strategy))) + 
  geom_errorbar(aes(ymin = FrustrationAverage - SE, ymax = FrustrationAverage + SE, width = 0.2)) + 
  ggsave(filename = "Ind_Interaction_between_Strategy_and_Target_Mental.DemandAverage.png")



# Surevy Individual Level ----

# Did you notice the feedback? ----
plot_data_ind <- ind_data %>%
  select(Target, NoticeFeedback) %>%
  group_by(Target) %>%
  count(NoticeFeedback)

ggplot(data = plot_data_ind, aes(x = NoticeFeedback, y = n, fill = NoticeFeedback)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, NoticeFeedback) %>%
  group_by(Target) %>%
  count(NoticeFeedback, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = NoticeFeedback, y = n, fill = NoticeFeedback)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Dominate.Strategy ~ Target)

# Did you find the feedback helpful? ----
plot_data_ind <- ind_data %>%
  select(Target, Feedback_Helpful) %>%
  group_by(Target) %>%
  count(Feedback_Helpful)

ggplot(data = plot_data_ind, aes(x = Feedback_Helpful, y = n, fill = Feedback_Helpful)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = n, y = n + 1)) +
  facet_grid(Target ~ .)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, Feedback_Helpful) %>%
  group_by(Target) %>%
  count(Feedback_Helpful, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = Feedback_Helpful, y = n, fill = Feedback_Helpful)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = n, y = n + 1)) +
  facet_grid(Target ~ Dominate.Strategy)

# How do you rate your own performance? ----
plot_data_ind <- ind_data %>%
  select(Target, My_Performance) %>%
  group_by(Target) %>%
  count(My_Performance)

ggplot(data = plot_data_ind, aes(x = My_Performance, y = n, fill = My_Performance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, My_Performance) %>%
  group_by(Target) %>%
  count(My_Performance, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = My_Performance, y = n, fill = My_Performance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Dominate.Strategy~ Target)

# How do you rate your team performance? ----
plot_data_ind <- ind_data %>%
  select(Target, Team_Performance) %>%
  group_by(Target) %>%
  count(Team_Performance)

ggplot(data = plot_data_ind, aes(x = Team_Performance, y = n, fill = Team_Performance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlim("Excellent", "Good", "Average", "Poor", "Very poor") + # This line is used to order the x values
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, Team_Performance) %>%
  group_by(Target) %>%
  count(Team_Performance, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = Team_Performance, y = n, fill = Team_Performance)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlim("Excellent", "Good", "Average", "Poor", "Very poor") + # This line is used to order the x values
  facet_grid(Dominate.Strategy~ Target)


# How do you rate your team communication? ----
plot_data_ind <- ind_data %>%
  select(Target, Our_Communication) %>%
  group_by(Target) %>%
  count(Our_Communication)

ggplot(data = plot_data_ind, aes(x = Our_Communication, y = n, fill = Our_Communication)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  xlim("Excellent", "Good", "Average", "Poor", "Very poor") + 
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, Our_Communication) %>%
  group_by(Target) %>%
  count(Our_Communication, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = Our_Communication, y = n, fill = Our_Communication)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_text(aes(label = n, y = n + 1)) +
  facet_grid(Dominate.Strategy~ Target)

# Did your team perform well? ----
plot_data_ind <- ind_data %>%
  select(Target, Team_perform_well) %>%
  group_by(Target) %>%
  count(Team_perform_well)

ggplot(data = plot_data_ind, aes(x = Team_perform_well, y = n, fill = Team_perform_well)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, Team_perform_well) %>%
  group_by(Target) %>%
  count(Team_perform_well, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = Team_perform_well, y = n, fill = Team_perform_well)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Dominate.Strategy~ Target)


# How do you believe your belifs line up with other team members belief of the task ----
plot_data_ind <- ind_data %>%
  select(Target, Belief_incompatible_with_team) %>%
  group_by(Target) %>%
  count(Belief_incompatible_with_team)

ggplot(data = plot_data_ind, aes(x = Belief_incompatible_with_team, y = n, fill = Belief_incompatible_with_team)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)

# Was another members belif about the task incompatiable with the other members belif? ----
plot_data_ind <- ind_data %>%
  select(Target, Member_belief_incompatible_with_other_member) %>%
  group_by(Target) %>%
  count(Member_belief_incompatible_with_other_member)

ggplot(data = plot_data_ind, aes(x = Member_belief_incompatible_with_other_member, y = n, fill = Member_belief_incompatible_with_other_member)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)


# How confident are you that your team can communicate inmportant details quickly? ----
plot_data_ind <- ind_data %>%
  select(Target, Confident_team_comm_important_details_quickly) %>%
  group_by(Target) %>%
  count(Confident_team_comm_important_details_quickly)

ggplot(data = plot_data_ind, aes(x = Confident_team_comm_important_details_quickly, y = n, fill = Confident_team_comm_important_details_quickly)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, Confident_team_comm_important_details_quickly) %>%
  group_by(Target) %>%
  count(Confident_team_comm_important_details_quickly, Dominate.Strategy)

ggplot(data = plot_data_ind, aes(x = Confident_team_comm_important_details_quickly, y = n, fill = Confident_team_comm_important_details_quickly)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(Dominate.Strategy ~ Target)


# How confident are you that your team can communicate inmportant events? ----
plot_data_ind <- ind_data %>%
  select(Target, Confident_team_comm_important_events) %>%
  group_by(Target) %>%
  count(Confident_team_comm_important_events)

ggplot(data = plot_data_ind, aes(x = factor(Confident_team_comm_important_events), y = n, fill = factor(Confident_team_comm_important_events))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(.~ Target)
