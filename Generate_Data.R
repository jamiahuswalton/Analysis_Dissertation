# Generate aggergate data

#These are the teams that should be removed from analysis (i.e., removed from the raw data)
#Teams 1 - 6 ran through the study with a different interface version (i.e., the periodic report). Team 9 had technical difficulties
team_to_remove_from_raw_data <- c(1:6, 9)

# Clean data (i.e., remove teams and unwanted data (or data used for the game)) ----
clean_positionTable <- remove_a_team_from_raw_data(positionTable,team_to_remove_from_raw_data,"teamnumber")
clean_inventory_data <- remove_a_team_from_raw_data(inventory_table, team_to_remove_from_raw_data, "teamnumber")
clean_error_log_data <- remove_a_team_from_raw_data(error_log_data, team_to_remove_from_raw_data, "teamnumber") 


# Define variables to use to generate aggregate ----

# The different conditions that all team participated in
condition_list<- c("A", "B", "C", "D")

# The order the teams conducted the session (A = None, B = Ind, C = Team, D = Ind_Team)
set_1 <- c("A", "B", "C", "D")
set_2 <- c("A", "C", "B", "D")
set_3 <- c("A", "D", "C", "B")
set_4 <- c("A", "B", "D", "C")
set_5 <- c("A", "C", "D", "B")
set_6 <- c("A", "D", "B", "C")

# NASA TLX scale names
TLX_Scale_Names <- c("Mental.Demand", "Physical.Demand", "Temporal.Demand", "Performance", "Effort", "Frustration" )

# Post Session questions names
PostSession_names <- c("NoticeFeedback", 
                       "Feedback_Helpful", 
                       "My_Performance", 
                       "Team_Performance", 
                       "Our_Communication",
                       "P1_can_communi",
                       "P1_performance",
                       "P1_work_with_again",
                       "P2_can_communi",
                       "P2_performance",
                       "P2_work_with_again",
                       "P3_can_communi",
                       "P3_performance",
                       "P3_work_with_again",
                       "Team_perform_well",
                       "Belief_incompatible_with_team",
                       "Member_belief_incompatible_with_other_member",
                       "Confident_team_comm_important_details_quickly",	
                       "Confident_team_comm_important_events",
                       "Confident_team_accurately_assess_handling_information",
                       "Confident_team_quickly_assess_handling_information",
                       "Confident_team_accurately_transfer_information")

# Data frame that contains the session order set. The session in the first row was the first condition, second row was the seconds condition...
counter_balance_set <- data.frame(cbind(set_1, set_2, set_3, set_4, set_5, set_6)) # Put the session orders into a data frame

# The player number for each team (assuming each team only had 3 team members)
player_number_list<- c(1,2,3) # Number of players

# The list of teams that have data. This should only be used after the data is cleaned (i.e., remove unwanted teams)
team_number_list <- list_of_team_numbers(clean_positionTable, "teamnumber")

# This is the strategy barrier distance. This is used to determine what strategy the players were using during a check.
# If all players are beyond this distane then the team is consider to be using the go alone method.
# If all players are within this distance then the team is consider to be using the go together method.
# If 1 player is within the barrier and the others are outside of the barrier (or vice versar), then the team is consider to be using a mix method
strategy_barrier_dis_my <- 5

# The names of the columns in the aggraget data that will be generated
go_together_name <- "Go Together"
go_alone_name <- "Go Alone"
mix_name <- "Mix"
col_names<- c("Team", 
              "Condition", 
              "Player",
              "Player_ID",
              "SessionOrder",
              "Counterbalance_set",
              "Target",
              "timeRemaining_ind",
              "timeRemaining_team",
              "IndividualScore",  
              "TeamScore", 
              "CI_ind", 
              "II_ind", 
              "Collection_rate_ind",
              "Collection_rate_correct_item_ind",
              "ERROR_ind_unique",
              "ERROR_ind_total",
              "Error_rate_ind",
              "Dis_total_ind",
              "CI_team", 
              "II_team",
              "Collection_rate_team",
              "Collection_rate_correct_item_team",
              "ERROR_team_unique",
              "ERROR_team_total",
              "Error_rate_team",
              "Dis_total_team",
              "transmitting_total_ind_sec",
              "transmitting_total_team_sec",
              "utterance_count_player",
              "utterance_count_team",
              go_together_name,
              go_alone_name,
              mix_name,
              "Dominate Strategy",
              TLX_Scale_Names,
              PostSession_names)


# The data frame that will be used to store the aggragate data (May add this into a function)
my_aggregate_data <- generate_aggragate_data(team_number_list, condition_list, clean_positionTable, clean_error_log_data, clean_inventory_data, 
                                             player_number_list, strategy_barrier_dis_my, counter_balance_set, col_names, TLX_Scale_Names, PostSession_names)

# Save aggregate data to csv file
setwd(folder_location_database)
write.csv(file = aggregate_folder_location,x = my_aggregate_data)

# Read aggregaate data
my_aggregate_data <- read.csv(file =  aggregate_folder_location)
