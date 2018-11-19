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