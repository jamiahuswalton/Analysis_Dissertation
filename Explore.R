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
dependent_response_team <- "TeamScore"
y_label_team <- "Count"
x_label_team <- "Target"
title_response_team <- "Utterance Count Vs. Target"

    # What is the distrabution of the data ----
plot_data_team <- team_data %>%
  select(dependent_response_team, Target)

ggplot(data = plot_data_team) + 
  geom_histogram(aes_string(x = dependent_response_team), bins = 10) +
  facet_grid(. ~ Target)

ggplot(data = )

    # What does the raw data look like split up by session and Target? ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  labs(y = y_label_team, x = x_label_team, title = title_response_team, fill = "Teams")

      # By Strategy ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team, Dominate.Strategy) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  facet_grid(. ~ Dominate.Strategy) +
  labs(y = y_label_team, x = x_label_team, title = title_response_team, fill = "Teams") 

 
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
dependent_response_ind <- "Collection_rate_correct_item_ind"
y_label_ind <- "Correct Item Collection Rate"
x_label_ind <- "Target"
title_response_ind <- "Rate Vs. Target"

# What is the distrabution of the data ----
plot_data_ind <- ind_data %>%
  select(dependent_response_ind, Target)

ggplot(data = plot_data_ind) + 
  geom_histogram(aes_string(x = dependent_response_ind), bins = 10) +
  facet_grid(. ~ Target)

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

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, dependent_response_ind) %>%
  group_by(Dominate.Strategy, Target) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = Dominate.Strategy, shape=Dominate.Strategy)) +
  geom_point(size = 3) +
  geom_line(aes(group=Dominate.Strategy, color = Dominate.Strategy)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  facet_grid(. ~ Dominate.Strategy) +
  ggsave(filename = paste("Ind_Interaction_between_Session_and_Target_", dependent_response_ind, ".png", sep = ""))


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

ggplot(data = plot_data_ind, aes(x = SessionOrder, y = Average, color = Target, shape=Target)) +
  geom_point(size = 3) +
  geom_line(aes(group=Target, color = Target)) + 
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

plot_data_ind <- ind_data %>%
  select(Target, dependent_response_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = Dominate.Strategy, shape=Dominate.Strategy)) +
  geom_point(size = 3) +
  geom_line(aes(group=Dominate.Strategy, color = Dominate.Strategy)) + 
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


# Testing

dependent_response_ind <- "timeRemaining_ind"
y_label_ind <- "Time Remaining"
x_label_ind <- "Target"
title_response_ind <- "Time Remaining (Individual) Vs. Target"
value_threshold <- 120
plot_name <- "Bar_TimeRemaining_ByTarget_BySessionOrder_Ind.png"
setwd(figure_directory)

plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Player_ID) %>%
  mutate(rank_order = -rank(.data[[dependent_response_ind]])) %>%
  mutate(above_value_threshold = .data[[dependent_response_ind]] > value_threshold)

names <- ifelse(plot_data_ind[,"above_value_threshold"], as.character( plot_data_ind[["Player_ID"]]), "")

ggplot(data = plot_data_ind, 
       aes(x = factor(SessionOrder), 
           y = .data[[dependent_response_ind]], 
           fill = Player_ID, group = rank_order,
           label = names )) +
  geom_text_repel(stat = "identity", position = position_jitterdodge(), force = 5) +
  geom_point(position = position_jitterdodge()) +
  facet_grid(. ~ Target) + 
  guides(fill = FALSE) +
  coord_flip()+
  xlim("4", "3", "2") +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Player ID") +
  ggsave(filename = plot_name)


# plot_data_ind$above_value_threshold <- as.logical(plot_data_ind$above_value_threshold)
plot_data_ind %>%
  filter(above_value_threshold) %>%
  group_by(Target, SessionOrder) %>%
  summarise(N_fast = length(.data[[dependent_response_ind]])) 

view(plot_data_ind)

## Carter Engen looking at individual collection_rate comparisons
dependent_response_ind <- "Collection_rate_ind"
y_label_ind <- "Collection Rate"
x_label_ind <- "Target"
title_response_ind <- "Rate Vs. Target"

# What is the distrabution of the data ----
plot_data_ind <- ind_data %>%
  select(dependent_response_ind, Target)

ggplot(data = plot_data_ind) + 
  geom_histogram(aes_string(x = dependent_response_ind), bins = 10) +
  facet_grid(. ~ Target)
# The data show a slightly normal distribution throughout, which most distributions centered around a rate of 55-65.
# Ind_Team and Ind show more similar distributions, whereas Team shows a slightly more flattened distribution.

# What does the raw data look like? ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Player_ID) %>%
  mutate(rank_order = -rank(.data[[dependent_response_ind]]))

ggplot(data = plot_data_ind, aes_string(x = "Target", y = dependent_response_ind, fill = "Player_ID", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players")

# By Streategy ----
plot_data_ind <- ind_data %>%
  select(Target, SessionOrder, dependent_response_ind, Player_ID, Dominate.Strategy) %>%
  mutate(rank_order = -rank(.data[[dependent_response_ind]]))

ggplot(data = plot_data_ind, aes_string(x = "Target", y = dependent_response_ind, fill = "Player_ID", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  facet_grid(. ~ Dominate.Strategy) + 
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players")

plot_data_ind <- ind_data %>%
  select(Target, Dominate.Strategy, dependent_response_ind) %>%
  group_by(Dominate.Strategy, Target) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = Dominate.Strategy, shape=Dominate.Strategy)) +
  geom_point(size = 3) +
  geom_line(aes(group=Dominate.Strategy, color = Dominate.Strategy)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  facet_grid(. ~ Dominate.Strategy)

# It is interesting that the Go Alone strategy seems to give a lower collection rate than Go Together, in general. 
# The exception to this is the similarity between Ind/Alone and Team/Together strategies, which seem to have the lowest collection rates of
# all strategies (and are nearly identical in rate).

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
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players")

ggplot(data = plot_data_ind, aes(x = SessionOrder, y = Average, color = Target, shape=Target)) +
  geom_point(size = 3) +
  geom_line(aes(group=Target, color = Target)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players")
# This graph is interesting because of the large decrease in colletion rate in Ind_Team targets between session orders 3 and 4.
# The other two (Ind & Team) Targets also show decreases in rates b/t sessions 3 & 4, but not as drastic as Ind_Team does.
# Additionally, Ind_Team decreases in Collection Rate an almost identical rate as the other two Targets b/t sessions 2 & 3; however,
# the other two Targets start out with lower collection rates to begin with, with all of them ending near 45-50.

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
  facet_grid(. ~ Dominate.Strategy)
# This plot matches most plot trends that I've seen so far in this code; however one thing that stands out is the increase
# in collection rate from Ind to Team at the 4th Session Order when looking at the Go Alone strategy.  Most other plots have shown
# a general decrease in collection rate between Ind & Team target levels, whereas this one shows an increase in rate. I would
# be interested in exploring this further to see what is creating that rise in rate b/t the two target levels. (Especially since 
# this rise was not shown well in the plot that had onlly Rate v. Target level grouped by strategy)

plot_data_ind <- ind_data %>%
  select(Target, dependent_response_ind, Dominate.Strategy) %>%
  group_by(Target, Dominate.Strategy) %>%
  summarise(Average = mean(.data[[dependent_response_ind]]), 
            Stdv =sd(.data[[dependent_response_ind]]), 
            n = length(.data[[dependent_response_ind]]), 
            StEr = sd(.data[[dependent_response_ind]]) / sqrt(length(.data[[dependent_response_ind]])))

ggplot(data = plot_data_ind, aes(x = Target, y = Average, color = Dominate.Strategy, shape=Dominate.Strategy)) +
  geom_point(size = 3) +
  geom_line(aes(group=Dominate.Strategy, color = Dominate.Strategy)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_ind, x = x_label_ind, title = title_response_ind, fill = "Players") +
  facet_grid(. ~ Dominate.Strategy)

# In general, the Individual condition seems to show a lower (better) collection rate, with Ind_Team usually landing in the middle and the Ind Condition showing the highest (worst) collection rates.

## Carter Engen looking at team collection_rate comparisons

dependent_response_team <- "Collection_rate_team"
y_label_team <- "Count"
x_label_team <- "Target"
title_response_team <- "Collection Rate Vs. Target"

# What is the distrabution of the data ----
plot_data_team <- team_data %>%
  select(dependent_response_team, Target)

ggplot(data = plot_data_team) + 
  geom_histogram(aes_string(x = dependent_response_team), bins = 10) +
  facet_grid(. ~ Target)
# The data follow a generally normal distribution, with each having a similar center.

ggplot(data = )

# What does the raw data look like split up by session and Target? ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  labs(y = y_label_team, x = x_label_team, title = title_response_team, fill = "Teams")

# By Strategy ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Team, Dominate.Strategy) %>%
  mutate(rank_order = -rank(.data[[dependent_response_team]]))

ggplot(data = plot_data_team, aes_string(x = "Target", y = dependent_response_team, fill = "Team", group = "rank_order")) +
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(. ~ SessionOrder) + 
  guides(fill = FALSE) +
  facet_grid(. ~ Dominate.Strategy) +
  labs(y = y_label_team, x = x_label_team, title = title_response_team, fill = "Teams") 


# Is there an interaction between the session order and the Target levels? -----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team) %>%
  group_by(SessionOrder, Target) %>%
  summarise(Average = mean(.data[[dependent_response_team]]), 
            Stdv =sd(.data[[dependent_response_team]]), 
            n = length(.data[[dependent_response_team]]), 
            StEr = sd(.data[[dependent_response_team]]) / sqrt(length(.data[[dependent_response_team]])))

ggplot(data = plot_data_team, aes(x = Target, y = Average, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  labs(y = y_label_team, x = x_label_team, title = title_response_team, color = "Session", shape = "Session")
# Ind_Team collection rate seems higher than other Target levels, except when ordered in the 4th Session.
# Ind_Team rate spreads out when compared to Ind or Team, seems like an odd patern

# By Streategy ----
plot_data_team <- team_data %>%
  select(Target, SessionOrder, dependent_response_team, Dominate.Strategy) %>%
  group_by(SessionOrder, Target, Dominate.Strategy) %>%
  summarise(Average = mean(.data[[dependent_response_team]]), 
            Stdv =sd(.data[[dependent_response_team]]), 
            n = length(.data[[dependent_response_team]]), 
            StEr = sd(.data[[dependent_response_team]]) / sqrt(length(.data[[dependent_response_team]])))

ggplot(data = plot_data_team, aes(x = Target, y = Average, color = SessionOrder, shape = SessionOrder)) +
  geom_point(size = 3) +
  geom_line(aes(group=SessionOrder, color = SessionOrder)) + 
  geom_errorbar(aes(ymin = Average - StEr, ymax = Average + StEr), width = 0.2) +
  facet_grid(. ~ Dominate.Strategy) + 
  labs(y = y_label_team, x = x_label_team, title = title_response_team, color = "Session", shape = "Session")
# Team lower rate on Go Together than Go Alone, interesting that Team/Together and Ind/Alone are both lowest collection rates.
# Team & Go Alone higher in 4th session than compared to other Target levels, seems interesting since team was lower collection rate in other comparisons.
# 2nd & 3rd sessions of Go Alone match oddly well regardless of Target