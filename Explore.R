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
  # Team ----
dependent_response_team <- "timeRemaining_team"
y_label_team <- "Time (Sec)"
x_label_team <- "Target"
title_response_team <- "Time Remianing Vs. Target"

    # What does the raw data look like split up by session and Target? ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  labs(y = y_label_team, x = x_label_team, title = title_response, fill = "Teams")

      # By Strategy ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team, Dominate.Strategy) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  facet_grid(. ~ Dominate.Strategy) +
  labs(y = y_label_team, x = x_label_team, title = title_response, fill = "Teams") 

 
    # Is there an interaction between the session order and the Target levels? -----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response) %>%
  group_by(SessionOrder, Target) %>%
  summarise(Average = mean(.data[[dependent_response]]), 
            Stdv =sd(.data[[dependent_response]]), 
            n = length(.data[[dependent_response]]), 
            StEr = sd(.data[[dependent_response]]) / sqrt(length(.data[[dependent_response]])))

ggplot(data = plot_data_team, aes(x = Target, y = Average, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label, x = x_label, title = title_response, color = "Session", shape = "Session")

      # By Streategy ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response, Dominate.Strategy) %>%
  group_by(SessionOrder, Target, Dominate.Strategy) %>%
  summarise(Average = mean(.data[[dependent_response]]), 
            Stdv =sd(.data[[dependent_response]]), 
            n = length(.data[[dependent_response]]), 
            StEr = sd(.data[[dependent_response]]) / sqrt(length(.data[[dependent_response]])))

ggplot(data = plot_data_team, aes(x = Target, y = Average, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  facet_grid(. ~ Dominate.Strategy) + 
  labs(y = y_label, x = x_label, title = title_response, color = "Session", shape = "Session")

  # Individual ----
dependent_response_ind <- "Performance"
y_label_ind <- "TLX Score"
x_label_ind <- "Target"
title_response_ind <- "Performance Vs. Target"

    # What does the raw data look like? ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Player_ID) %>%
  mutate(rank_order = -rank(.data[[dependent_response_ind]]))

ggplot(data = plot_data_ind, aes_string(x = "Target", y = dependent_response_ind, fill = "Player_ID", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  ggsave(filename = paste("Ind_Raw_Data_Session_and_Target_", dependent_response_ind, ".png", sep = ""))

      # By Streategy ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Player_ID, Dominate.Strategy) %>%
  mutate(rank_order = -rank(.data[[dependent_response_ind]]))

ggplot(data = plot_data_ind, aes_string(x = "Target", y = dependent_response_ind, fill = "Player_ID", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  facet_grid(. ~ Dominate.Strategy) + 
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  ggsave(filename = paste("Ind_Raw_Data_Session_and_Target_", dependent_response_ind, ".png", sep = ""))


    # Is there an interaction between the session order and the Target levels? ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind) %>%
  group_by(SessionOrder, Target) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = SessionOrder, shape=SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  ggsave(filename = paste("Ind_Interaction_between_Session_and_Target_", dependent_response_ind, ".png", sep = ""))

      # By strategy ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Dominate.Strategy) %>%
  group_by(SessionOrder, Target, Dominate.Strategy) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = SessionOrder, shape=SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  facet_grid(. ~ Dominate.Strategy)+
  ggsave(filename = paste("Ind_Interaction_between_Session_and_Target_", dependent_response_ind, ".png", sep = ""))  
  








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
