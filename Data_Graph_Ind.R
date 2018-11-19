library(ggplot2)

# Location to save figures
setwd(figure_directory)

image_scale <- 1.35


# What is the N for Inds
N_ind <- length(levels(factor(team_aggregate_data_stats$Player_ID) ))

# The N text to add to title for Inds 
N_ind_full_text <- paste("(N = ", N_ind, ")", sep = "")

# Test

figure_titles <- c("Individual Scores", "Correct Items Collected")
y_values_team <- c("IndividualScore", "CI_ind")
y_labels_team <- c("Individual Score", "Correct Items (Individual)")
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
      filename_graph <- paste("ind_",y_label,"_by_",x_label,"_",plot,".png", sep = "")
      
      if(plot == "Group_Bar"){
        # print(paste("team_",y_label,"_by_",x_label,"_",plot, sep = ""))
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current, fill = "Player_ID")) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = paste(figure_title, N_ind_full_text) , x = x_label, y = y_label) +
          guides(fill=FALSE)
        ggsave(filename = filename_graph)
        
      } else if(plot == "Boxplot"){
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current)) +
          geom_boxplot() +
          labs(title = paste(figure_title, N_ind_full_text), x = x_label, y = y_label)
        ggsave(filename = filename_graph)
      } else if(plot == "Point_plot"){
        ggplot(data = team_aggregate_data_stats, aes_string(x = x_current, y = y_current)) +
          geom_point() +
          labs(title = paste(figure_title, N_ind_full_text), x = x_label, y = y_label)
        ggsave(filename = filename_graph)
      }
    }
  }
}


# 6
# INd Score - by Session order - Group bar graph ----
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = IndividualScore, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Session Order", y = "Score") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_Scores_by_session_groupBar.png")

# Individual Score -  by Session order - Box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), IndividualScore)) +
  geom_boxplot() +
  labs(title = paste("Individual Scores ", N_ind_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Ind_Scores_by_session_botplot.png")

# Individual Score -  by Session order - Point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), IndividualScore)) +
  geom_point() +
  labs(title = paste("Individual Scores ", N_ind_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Ind_Scores_by_session_point.png")

# Individual Score - by Session order - Group bar graph (Split by Strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = IndividualScore, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Session Order", y = "Score") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_Scores_by_session_groupBar_SplitByStrategy.png")

# Individual Score -  by Session order - Box plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), IndividualScore)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ", N_ind_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Ind_Scores_by_session_botplot_SplitByStrategy.png")

# Individual Score -  by Session order - Point plot (Split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), IndividualScore)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ", N_ind_full_text), x = "Session Order", y = "Score")
ggsave(filename = "Ind_Scores_by_session_point_SplitByStrategy.png")


# 6
# Individual Score - by condition - Group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Feedback Target", y = "Score") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_Scores_by_condition_groupBar.png")

# Individual Score - by condition - Boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore)) +
  geom_boxplot() +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Feedback Target", y = "Score") 
ggsave(filename = "Ind_Scores_by_condition_boxplot.png")

# Individual Score - by condition - Point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore)) +
  geom_point() +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Feedback Target", y = "Score") 
ggsave(filename = "Ind_Scores_by_condition_point.png")

# Individual Score - by condition - Group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ", N_ind_full_text) , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_Scores_by_condition_groupBar_SplitByStrategy.png")

# Individual Score - by condition - box plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ") , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_Scores_by_condition_boxplot_SplitByStrategy.png")

#Individual Score - by condition - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = IndividualScore)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy ) +
  labs(title = paste("Individual Scores ") , x = "Feedback Target", y = "Score") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_Scores_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Correct Items collected - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_CI_by_session_groupbar.png")

# Player_ID - Correct Items collected - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind)) +
  geom_boxplot() +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_session_boxplot.png")

# Player_ID - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind)) +
  geom_point() +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text ) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_session_point.png")

# Player_ID - Correct Items collected - by session order - group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Correct Items collected - by session order - boxplot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Correct Items collected - by session order - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = CI_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text ) , x = "Session Order", y = "Count") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_session_point_SplitByStrategy.png")


# 6
# Player_ID - Correct Items collected - by condition - group bar graph ----
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = CI_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_CI_by_condition_groupbar.png")

# Player_ID - Correct Items collected - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_ind)) +
  geom_boxplot() +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_CI_by_condition_boxplot.png")

# Player_ID - Correct Items collected - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_ind)) +
  geom_point() +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_CI_by_condition_point.png")

# Player_ID - Correct Items collected - by condition - group bar graph (Split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = CI_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_CI_by_condition_groupbar_SplitByStrategy.png", scale = image_scale)

# Player_ID - Correct Items collected - by condition - boxplot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_CI_by_condition_boxplot_SplitByStrategyt.png")

# Player_ID - Correct Items collected - by condition - point plot (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(Target), CI_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Correct Items Collected (Player_ID) ", N_ind_full_text) , x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_CI_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Inorrect Items collected - by session order - group bar graph ----
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = II_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_II_by_session_groupbar.png")

# Player_ID - Correct Items collected - by session order - box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_ind)) +
  geom_boxplot() +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Ind_II_by_session_boxplot.png")

# Player_ID - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_ind)) +
  geom_point() +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Ind_II_by_session_pointplot.png")

# Player_ID - Inorrect Items collected - by session order - group bar graph (split by strategy)
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = II_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_II_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Correct Items collected - by session order - box plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Ind_II_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Correct Items collected - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), II_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text) , x = "Session Order", y = "Count") 
ggsave(filename = "Ind_II_by_session_point_SplitByStrategy.png")


# 6
# Player_ID - Incorrect Items collected - by condition order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_II_by_condition_groupbar.png")

# Player_ID - Incorrect Items collected - by condition order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind)) +
  geom_boxplot() +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_II_by_condition_boxplot.png")

# Player_ID - Incorrect Items collected - by condition order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind)) +
  geom_point() +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_II_by_condition_pointplot.png")

# Player_ID - Incorrect Items collected - by condition order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_II_by_condition_groupbar_SplitByStrategy.png")

# Player_ID - Incorrect Items collected - by condition order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_II_by_condition_boxplot_SplitByStrategy.png")

# Player_ID - Incorrect Items collected - by condition order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = II_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Incorrect Items Collected (Player_ID) ", N_ind_full_text), x = "Feedback Target", y = "Count")
ggsave(filename = "Ind_II_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Unique Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_unique_errors_by_session_groupbar.png")

# Player_ID - Unique Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique)) +
  geom_boxplot() +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_session_boxplot.png")

# Player_ID - Unique Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique)) +
  geom_point() +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_session_pointplot.png")

# Player_ID - Unique Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_unique_errors_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Unique Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Unique Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_unique)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Unique) ", N_ind_full_text) , x = "Session Order", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_session_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Unique Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text), x = "Feedback Target", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_unique_errors_by_condition_groupbar.png")

# Player_ID - Unique Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique)) +
  geom_boxplot() +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_condition_boxplot.png")

# Player_ID - Unique Errors - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique)) +
  geom_point() +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_condition_pointplot.png")

# Player_ID - Unique Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text), x = "Feedback Target", y = "Unique Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_unique_errors_by_condition_groupbar_SplitByStrategy.png")

# Player_ID - Unique Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_condition_boxplot_SplitByStrategy.png")

# Player_ID - Unique Errors - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_unique)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Unique) ", N_ind_full_text) , x = "Feedback Target", y = "Unique Error Count") 
ggsave(filename = "Ind_unique_errors_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Total Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_total_errors_by_session_groupbar.png")

# Player_ID - Total Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total)) +
  geom_boxplot() +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_session_boxplot.png")

# Player_ID - Total Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total)) +
  geom_point() +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_session_pointplot.png")

# Player_ID - Total Errors - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_total_errors_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Total Errors - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Total Errors - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = ERROR_ind_total)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste("Player_ID Errors (Total) ", N_ind_full_text), x = "Session Order", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_session_SplitByStrategy.png")


# 6
# Player_ID - Total Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count") + 
  # guides(fill=guide_legend(title="Player_ID"))
  guides(fill=FALSE)
ggsave(filename = "Ind_total_errors_by_condition_groupbar.png")

# Player_ID - total errors - by condition - points
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total)) +
  geom_boxplot() +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count")
ggsave(filename = "Ind_total_errors_by_condition_boxplot.png")

# Player_ID - Total Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total)) +
  geom_point() +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_condition_pointplot.png")

# Player_ID - Total Errors - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_total_errors_by_condition_groupbar_SplitByStrategy.png")

# Player_ID - total errors - by condition - points
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total, fill = factor(Player_ID))) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_total_errors_by_condition_boxplot_SplitByStrategy.png")

# Player_ID - Total Errors - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = ERROR_ind_total)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Errors (Total) ", N_ind_full_text), x = "Feedback Target", y = "Error Count") 
ggsave(filename = "Ind_total_errors_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Utterances - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_utterances_by_session_groupbar.png")

# Player_ID - Utterances - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player)) +
  geom_boxplot() +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_session_boxplot.png")

# Player_ID - Utterances - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player)) +
  geom_point() +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_session_pointplot.png")

# Player_ID - Utterances - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_utterances_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Utterances - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Utterances - by session order - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = utterance_count_player)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text) , x = "Session Order", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_session_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Utterances - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_utterances_by_condition_groupbar.png")

# Player_ID - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player)) +
  geom_boxplot() +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_condition_botplot_boxplot.png")

# Player_ID - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player)) +
  geom_point() +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_condition_pointplot.png")

# Player_ID - Utterances - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_utterances_by_condition_groupbar_SplitByStrategy.png")

# Player_ID - Utterances - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_condition_boxplot_SplitByStrategy.png")

# Player_ID - Utterances - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = utterance_count_player)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Player_ID Utterances ", N_ind_full_text), x = "Feedback Target", y = "Utterance count")
ggsave(filename = "Ind_utterances_by_condition_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Time Remaining - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_timeRemaining_by_session_groupbar.png")

# Player_ID - Time remaining - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind)) +
  geom_boxplot() +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_session_boxplot.png")

# Player_ID - Time remaining - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind)) +
  geom_point() +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_session_pointplot.png")

# Player_ID - Time Remaining - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_timeRemaining_by_session_groupbar_SplitByStrategy.png")

# Player_ID - Time remaining - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_session_boxplot_SplitByStrategy.png")

# Player_ID - Time remaining - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = timeRemaining_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_session_pointplot_SplitByStrategy.png")


# 6
# Player_ID - Time Remaining - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_timeRemaining_by_condition_groupbar.png")

# Player_ID - Time remaining - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind)) +
  geom_boxplot() +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_condition_boxplot.png")

# Player_ID - Time remaining - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind)) +
  geom_point() +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_condition_pointplot.png")

# Player_ID - Time Remaining - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (seconds)") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_timeRemaining_by_condition_groupbar_SplitByStrategy.png")

# Player_ID - Time remaining - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_condition_boxplot_SplitByStrategy.png")

# Player_ID - Time remaining - by condition - point plot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = timeRemaining_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Time Remaining ", N_ind_full_text), x = "Feedback Target", y = "Time (Seconds)")
ggsave(filename = "Ind_timeRemaining_by_condition_pointplot_SplitByStrategy.png")




# 3
# Player_ID - score - by strategy - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy) , y = IndividualScore, fill = factor(Player_ID))) +
  geom_bar(stat="identity", position = 'dodge') +
  labs(title = paste( "Score by Strategy ", N_ind_full_text), x = "Strategy", y = "Time (Seconds)") + 
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_score_by_strategy_groupbar.png")

# Player_ID - score - by strategy - boxplot 
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy), y = IndividualScore)) + 
  geom_boxplot() +
  labs(title = paste( "Score by Strategy ", N_ind_full_text), x = "Strategy", y = "Time (Seconds)")
ggsave(filename = "Ind_score_by_strategy_boxplot.png")

# Player_ID - score -  by strategy - point
ggplot(data = team_aggregate_data_stats, aes(factor(Dominate.Strategy), y = IndividualScore)) + 
  geom_point() +
  labs(title = paste( "Score by Strategy ", N_ind_full_text), x = "Strategy", y = "Time (Seconds)")
ggsave(filename = "Ind_score_by_strategy_pointplot.png")


# 6
# Distance traveled - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), y = Dis_total_ind, fill = factor(Player_ID))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_disTraveled_by_session_groupbar.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_boxplot() +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_session_boxplot.png")

# Distance traveled - by session order - point
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = Dis_total_ind)) +
  geom_point() +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_session_pointplot.png")

# Distance traveled - by session order - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder), y = Dis_total_ind, fill = factor(Player_ID))) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_disTraveled_by_session_groupbar_SplitByStrategy.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_session_boxplot_SplitByStrategy.png")

# Distance traveled - by session order - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(SessionOrder) , y = Dis_total_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_session_pointplot_SplitByStrategy.png")


# 6
# Distance traveled - by condition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target), y = Dis_total_ind, fill = factor(Player_ID))) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_disTraveled_by_condition_groupbar.png")

# Distance traveled - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_boxplot() +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_condition_boxplot.png")

# Distance traveled - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_point() +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_condition_pointplot.png")

# Distance traveled - by caondition - group bar graph
ggplot(data = team_aggregate_data_stats, aes(factor(Target), y = Dis_total_ind, fill = factor(Player_ID))) +
  geom_bar(stat = "identity", position = 'dodge') +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance") +
  guides(fill=guide_legend(title="Player_ID"))
ggsave(filename = "Ind_disTraveled_by_condition_groupbar_SplitByStrategy.png")

# Distance traveled - by condition - boxplot
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_boxplot() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_condition_boxplot_SplitByStrategy.png")

# Distance traveled - by condition - point
ggplot(data = team_aggregate_data_stats, aes(factor(Target) , y = Dis_total_ind)) +
  geom_point() +
  facet_grid(facets = .~ Dominate.Strategy) +
  labs(title = paste( "Total Distance ", N_ind_full_text), x = "Feedback Target", y = "Distance")
ggsave(filename = "Ind_disTraveled_by_condition_pointplot_SplitByStrategy.png")




#--------------#
# # Test
# ggplot(data = team_aggregate_data_stats, aes(IndividualScore, y = utterance_count_player)) +
#   geom_point() +
#   facet_grid(facets = . ~  Dominate.Strategy) +
#   labs(title = paste( "Utterance vs. Individual Score ", N_ind_full_text), y = "Utterance Score", x = "Individual Score")

