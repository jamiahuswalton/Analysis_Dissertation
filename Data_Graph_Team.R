# Location to save figures
setwd(figure_directory)

# image_scale <- 1.35


# What is the N for Teams
N_teams <- length(levels(factor(team_aggregate_data_stats$Team) ))

# The N text to add to title for teams 
N_teams_full_text <- paste("(N = ", N_teams, ")", sep = "")

figure_titles <- c("Team Scores", "Correct Items Collected", "Incorrect Items Collected", "Distance (Total)")
y_values_team <- c("TeamScore", "CI_team", "II_team", "Dis_total_team")
y_labels_team <- c("Team Score", "Correct Items (Team)", "Incorrect Items (Team)", "Distance (Total)")
x_values <- c("SessionOrder", "Target")
x_labels_team <- c("Session", "Target")
plot_types <- c("Group_Bar", "Boxplot", "Point_plot")

for(y_current in y_values_team){
  for (x_current in x_values){
    index_for_y <- which(y_current == y_values_team)
    index_for_x <- which(x_current == x_values)
    
    for(plot in plot_types){
      x_label <- x_labels_team[index_for_x]
      y_label <- y_labels_team[index_for_y]
      figure_title <- figure_titles[index_for_y]
      # print(paste("x: ", x_current," ","y: ", y_current, " ", "Plot: ", plot, " ", "Label: ", x_label," ", "Label (y): ", y_label , sep = ""))
      filename_graph <- paste("team_",y_label,"_by_",x_label,"_",plot,".png", sep = "")
      
      if(plot == "Group_Bar"){
        # print(paste("team_",y_label,"_by_",x_label,"_",plot, sep = ""))
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current, fill = "Team")) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = paste(figure_title, N_teams_full_text) , x = x_label, y = y_label) +
          guides(fill=guide_legend(title="Team"))
        ggsave(filename = filename_graph)
        
      } else if(plot == "Boxplot"){
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current)) +
          geom_boxplot() +
          labs(title = paste(figure_title, N_teams_full_text), x = x_label, y = y_label)
        ggsave(filename = filename_graph)
      } else if(plot == "Point_plot"){
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current)) +
          geom_point() +
          labs(title = paste(figure_title, N_teams_full_text), x = x_label, y = y_label)
        ggsave(filename = filename_graph)
      }
    }
  }
}

# Test to plot avergages
# ggplot(data = team_aggregate_data_stats, aes_string(x = "Target", y = "CI_team")) + stat_summary(fun.y = "mean", geom = "bar")


# 6 ----
# Team Score - by Session order - Group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = TeamScore, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Session Order", y = "Score") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_session_groupBar.png")

# Team Score -  by Session order - Box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), TeamScore)) +
  geom_boxplot() +
  labs(title = paste("Team Scores ", N_teams_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Team_Scores_by_session_botplot.png")

# Team Score -  by Session order - Point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), TeamScore)) +
  geom_point() +
  labs(title = paste("Team Scores ", N_teams_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Team_Scores_by_session_point.png")

# Team Score - by Session order - Group bar graph (Split by Strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = TeamScore, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Session Order", y = "Score") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_session_groupBar_SplitByStrategy.png")

# Team Score -  by Session order - Box plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), TeamScore)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ", N_teams_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Team_Scores_by_session_botplot_SplitByStrategy.png")

# Team Score -  by Session order - Point plot (Split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), TeamScore)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ", N_teams_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Team_Scores_by_session_point_SplitByStrategy.png")


# 6 ----
# Team Score - by condition - Group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Feedback Target", y = "Score") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_condition_groupBar.png")

# Team Score - by condition - Boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore)) +
  geom_boxplot() +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Feedback Target", y = "Score") 
ggsave(filename = "Team_Scores_by_condition_boxplot.png")

# Team Score - by condition - Point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore)) +
  geom_point() +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Feedback Target", y = "Score") 
ggsave(filename = "Team_Scores_by_condition_point.png")

# Team Score - by condition - Group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ", N_teams_full_text) , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_condition_groupBar_SplitByStrategy.png")

# Team Score - by condition - box plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ") , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_condition_boxplot_SplitByStrategy.png")

#Team Score - by condition - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = TeamScore)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Team Scores ") , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_Scores_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Correct Items collected - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_groupbar.png")

# Team - Correct Items collected - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team)) +
  geom_boxplot() +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_boxplot.png")

# Team - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team)) +
  geom_point() +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text ) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_point.png")

# Team - Correct Items collected - by session order - group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_groupbar_SplitByStrategy.png")

# Team - Correct Items collected - by session order - boxplot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_boxplot_SplitByStrategy.png")

# Team - Correct Items collected - by session order - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text ) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_session_point_SplitByStrategy.png")


# 6 ----
# Team - Correct Items collected - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = CI_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_condition_groupbar.png")

# Team - Correct Items collected - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_team)) +
  geom_boxplot() +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Team_CI_by_condition_boxplot.png")

# Team - Correct Items collected - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_team)) +
  geom_point() +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Team_CI_by_condition_point.png")

# Team - Correct Items collected - by condition - group bar graph (Split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = CI_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_CI_by_condition_groupbar_SplitByStrategy.png", scale = image_scale)

# Team - Correct Items collected - by condition - boxplot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Team_CI_by_condition_boxplot_SplitByStrategyt.png")

# Team - Correct Items collected - by condition - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Team) ", N_teams_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Team_CI_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Inorrect Items collected - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = II_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_II_by_session_groupbar.png")

# Team - Correct Items collected - by session order - box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_team)) +
  geom_boxplot() +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Team_II_by_session_boxplot.png")

# Team - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_team)) +
  geom_point() +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Team_II_by_session_pointplot.png")

# Team - Inorrect Items collected - by session order - group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = II_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_II_by_session_groupbar_SplitByStrategy.png")

# Team - Correct Items collected - by session order - box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Team_II_by_session_boxplot_SplitByStrategy.png")

# Team - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Team_II_by_session_point_SplitByStrategy.png")


# 6 ----
# Team - Incorrect Items collected - by condition order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_II_by_condition_groupbar.png")

# Team - Incorrect Items collected - by condition order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team)) +
  geom_boxplot() +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Team_II_by_condition_boxplot.png")

# Team - Incorrect Items collected - by condition order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team)) +
  geom_point() +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Team_II_by_condition_pointplot.png")

# Team - Incorrect Items collected - by condition order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_II_by_condition_groupbar_SplitByStrategy.png")

# Team - Incorrect Items collected - by condition order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Team_II_by_condition_boxplot_SplitByStrategy.png")

# Team - Incorrect Items collected - by condition order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Team) ", N_teams_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Team_II_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Unique Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_unique_errors_by_session_groupbar.png")

# Team - Unique Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique)) +
  geom_boxplot() +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_session_boxplot.png")

# Team - Unique Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique)) +
  geom_point() +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_session_pointplot.png")

# Team - Unique Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_unique_errors_by_session_groupbar_SplitByStrategy.png")

# Team - Unique Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_session_boxplot_SplitByStrategy.png")

# Team - Unique Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_unique)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Unique) ", N_teams_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_session_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Unique Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text), x = "Feedback Target", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_unique_errors_by_condition_groupbar.png")

# Team - Unique Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique)) +
  geom_boxplot() +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_condition_boxplot.png")

# Team - Unique Errors - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique)) +
  geom_point() +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_condition_pointplot.png")

# Team - Unique Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text), x = "Feedback Target", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_unique_errors_by_condition_groupbar_SplitByStrategy.png")

# Team - Unique Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_condition_boxplot_SplitByStrategy.png")

# Team - Unique Errors - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_unique)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Unique) ", N_teams_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Team_unique_errors_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Total Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_total_errors_by_session_groupbar.png")

# Team - Total Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total)) +
  geom_boxplot() +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_session_boxplot.png")

# Team - Total Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total)) +
  geom_point() +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_session_pointplot.png")

# Team - Total Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_total_errors_by_session_groupbar_SplitByStrategy.png")

# Team - Total Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_session_boxplot_SplitByStrategy.png")

# Team - Total Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_team_total)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Team Errors (Total) ", N_teams_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_session_SplitByStrategy.png")


# 6 ----
# Team - Total Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_total_errors_by_condition_groupbar.png")

# Team - total errors - by condition - points
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total)) +
  geom_boxplot() +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count")
ggsave(filename = "Team_total_errors_by_condition_boxplot.png")

# Team - Total Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total)) +
  geom_point() +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_condition_pointplot.png")

# Team - Total Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_total_errors_by_condition_groupbar_SplitByStrategy.png")

# Team - total errors - by condition - points
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total, fill = factor(Team))) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_total_errors_by_condition_boxplot_SplitByStrategy.png")

# Team - Total Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_team_total)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Errors (Total) ", N_teams_full_text), x = "Feedback Target", y = "Error Count") 
ggsave(filename = "Team_total_errors_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Utterances - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_utterances_by_session_groupbar.png")

# Team - Utterances - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team)) +
  geom_boxplot() +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Team_utterances_by_session_boxplot.png")

# Team - Utterances - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team)) +
  geom_point() +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Team_utterances_by_session_pointplot.png")

# Team - Utterances - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_utterances_by_session_groupbar_SplitByStrategy.png")

# Team - Utterances - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Team_utterances_by_session_boxplot_SplitByStrategy.png")

# Team - Utterances - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Team_utterances_by_session_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Utterances - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_utterances_by_condition_groupbar.png")

# Team - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team)) +
  geom_boxplot() +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Team_utterances_by_condition_boxplot.png")

# Team - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team)) +
  geom_point() +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Team_utterances_by_condition_pointplot.png")

# Team - Utterances - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_utterances_by_condition_groupbar_SplitByStrategy.png")

# Team - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Team_utterances_by_condition_boxplot_SplitByStrategy.png")

# Team - Utterances - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Team Utterances ", N_teams_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Team_utterances_by_condition_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Time Remaining - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_timeRemaining_by_session_groupbar.png")

# Team - Time remaining - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team)) +
  geom_boxplot() +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_session_boxplot.png")

# Team - Time remaining - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team)) +
  geom_point() +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_session_pointplot.png")

# Team - Time Remaining - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_timeRemaining_by_session_groupbar_SplitByStrategy.png")

# Team - Time remaining - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_session_boxplot_SplitByStrategy.png")

# Team - Time remaining - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_session_pointplot_SplitByStrategy.png")


# 6 ----
# Team - Time Remaining - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_timeRemaining_by_condition_groupbar.png")

# Team - Time remaining - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team)) +
  geom_boxplot() +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_condition_boxplot.png")

# Team - Time remaining - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team)) +
  geom_point() +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_condition_pointplot.png")

# Team - Time Remaining - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_timeRemaining_by_condition_groupbar_SplitByStrategy.png")

# Team - Time remaining - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_condition_boxplot_SplitByStrategy.png")

# Team - Time remaining - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_teams_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Team_timeRemaining_by_condition_pointplot_SplitByStrategy.png")




# 3 ----
# Team - score - by strategy - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy) , y = TeamScore, fill = factor(Team))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Score by Strategy ", N_teams_full_text), x = "Strategy", y = "Time (Seconds)") + 
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_score_by_strategy_groupbar.png")

# Team - score - by strategy - boxplot 
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy), y = TeamScore)) + 
  geom_boxplot() +
  labs(title = paste( "Score by Strategy ", N_teams_full_text), x = "Strategy", y = "Time (Seconds)")
ggsave(filename = "Team_score_by_strategy_boxplot.png")

# Team - score -  by strategy - point
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy), y = TeamScore)) + 
  geom_point() +
  labs(title = paste( "Score by Strategy ", N_teams_full_text), x = "Strategy", y = "Time (Seconds)")
ggsave(filename = "Team_score_by_strategy_pointplot.png")


# 6 ----
# Distance traveled - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), y = Dis_total_team, fill = factor(Team))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_disTraveled_by_session_groupbar.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_boxplot() +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_session_boxplot.png")

# Distance traveled - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = Dis_total_team)) +
  geom_point() +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_session_pointplot.png")

# Distance traveled - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), y = Dis_total_team, fill = factor(Team))) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_disTraveled_by_session_groupbar_SplitByStrategy.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_session_boxplot_SplitByStrategy.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = Dis_total_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_session_pointplot_SplitByStrategy.png")


# 6 ----
# Distance traveled - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target), y = Dis_total_team, fill = factor(Team))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_disTraveled_by_condition_groupbar.png")

# Distance traveled - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_boxplot() +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_condition_boxplot.png")

# Distance traveled - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_point() +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_condition_pointplot.png")

# Distance traveled - by caondition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target), y = Dis_total_team, fill = factor(Team))) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Team"))
ggsave(filename = "Team_disTraveled_by_condition_groupbar_SplitByStrategy.png")

# Distance traveled - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_condition_boxplot_SplitByStrategy.png")

# Distance traveled - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_team)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_teams_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Team_disTraveled_by_condition_pointplot_SplitByStrategy.png")



#----
#--------------#
# # Test
# ggplot(data = team_aggregate_data_stats, aes(TeamScore, y = utterance_count_team)) +
#   geom_point() +
#   facet_grid(facets = . ~  Dominate.Strategy) +
#   labs(title = paste( "Utterance vs. Team Score ", N_teams_full_text), y = "Utterance Score", x = "Team Score")

