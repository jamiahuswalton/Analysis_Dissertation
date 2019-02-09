# Remove teams from raw data (Clean raw data) ----
remove_a_team_from_raw_data <- function(data, teams_to_be_removed, name_of_team_number_col){
  clean_data <- data
  for(team in teams_to_be_removed){
    team_to_remove_from_inventory <- which(clean_data[,name_of_team_number_col] == team)
    clean_data <- clean_data[-team_to_remove_from_inventory,]
  }
  return(clean_data)
}

# Remove measures with a give value ----
remove_measures_with_given_value <- function(data_set, col_name, value){
  rows_to_move <- which(as.vector(data_set[,col_name]) == value) 
  
  return(data_set[-rows_to_move,])
}

# Clean inventory table----
clean_inventory_data_by_removing_game_enteries <- function(inventory_data, column_to_look_into, value_to_remove){
  inventory_entries_to_remove <- which(inventory_data[,column_to_look_into] == value_to_remove)
  
  return(inventory_data[-inventory_entries_to_remove,])
}

# Function to count the total number of errors commited by a team (regarless of if this rule was broken before) ----
total_number_of_errors_team <- function(data_errors, teamNum, condition){
  team_errors_data <- data_errors %>% 
    filter(teamnumber == teamNum & expcondition == condition)
  return(length(team_errors_data[,"ID"]))
}

# Function to calculate the total error rate (i.e., Duration / total errors) for a team Units: Sec / Error.
# Duration is retruned if error count is 0.
error_rate_team <- function(data_position, data_errors, teamNum, condition){
  
  error_count_team <- total_number_of_errors_team(data_errors, teamNum, condition)
  data_team <- data_position %>% filter(teamnumber == teamNum & playernum == 1 & expcondition == condition)
  data_team_last_line <- tail(data_team,1) 
  duration_team<- data_team_last_line[1,"duration"]
  
  if(error_count_team == 0){
    return(duration_team)
  } else if(error_count_team > 0){
    return(duration_team / error_count_team)
  } else if(error_count_team < 0){
    message<- paste("error_rate_team: the error count generated for team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Function to count the total number of errors commited by a player (regarless of if this rule was broken before) ----
total_number_of_errors_individual <- function(data_errors, teamNum, player, condition){
  ind_errors_data <- data_errors %>% 
    filter(teamnumber == teamNum & playernum == player & expcondition == condition)
  total_errors_individual <- length(ind_errors_data[,"ID"])
  return(total_errors_individual)
}

# Function to calculate the total error rate (i.e., Duration / total errors) for an individual. Units: Sec / Error
# Duration is retruned if error count is 0.
error_rate_ind <- function(data_position, data_errors, teamNum, playerNum, condition){
  
  error_count_ind <- total_number_of_errors_individual(data_errors, teamNum, playerNum, condition)
  data_ind <- data_position %>% filter(teamnumber == teamNum & playernum == playerNum & expcondition == condition)
  data_ind_last_line <- tail(data_ind,1) 
  duration_ind<- data_ind_last_line[1,"duration_ind"]
  
  if(error_count_ind == 0){
    return(duration_ind)
  } else if(error_count_ind > 0){
    return(duration_ind / error_count_ind)
  } else if(error_count_ind < 0){
    message<- paste("error_rate_ind: the error count generated for player", playerNum, "in team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Function to factor the columns ----
re_factor_columns <- function(userData, columnNames){
  factorData <- userData
  for(column in columnNames){
    print(column)
    factorData[,column] <- factor(factorData[,column])
  }
  return(factorData)
}

# Generate team number list in a given data set ----
list_of_team_numbers <- function(data, team_number_col_name){
  return(as.numeric(levels(factor(as.vector(data[,team_number_col_name])))))
}

# Function to count the number of times a game strategy was used ----
strategy_count_vector<- function(position_data, experimentalcondition, teamnumber_current, playernumber_one, playernumber_two, playernumber_three, strategy_barrier_distance){
  
  #Need to get the position Data for all players (3 players)
  is_player_1 <- as.vector(position_data$expcondition == experimentalcondition & position_data$teamnumber == teamnumber_current & 
                             position_data$playernum == playernumber_one)
  is_player_2<- as.vector(position_data$expcondition == experimentalcondition & position_data$teamnumber == teamnumber_current & 
                            position_data$playernum == playernumber_two)
  is_player_3<- as.vector(position_data$expcondition == experimentalcondition & position_data$teamnumber == teamnumber_current & 
                            position_data$playernum == playernumber_three)
  is_team_current<- as.vector(position_data$expcondition == experimentalcondition & position_data$teamnumber == teamnumber_current)
  
  
  pos_player_1<- position_data[is_player_1,]
  pos_player_2<- position_data[is_player_2,]
  pos_player_3<- position_data[is_player_3,]
  
  #This is the length of the position data. They will not have the same number of enteries due to time issues. The number of strategies checked will be equal to the lowest number 
  number_of_checkes <- min(c(length(pos_player_1[,1]), length(pos_player_2[,1]), length(pos_player_3[,1]))) 
  
  #Make sure they are in the correct order (The ID value at the top should be the lowest)
  
  #Calculate calculate the centroiz value for X and Z (Y value does not change in unity)
  x_1 <- as.vector(pos_player_1$pos_x[1:number_of_checkes]) 
  x_2 <- as.vector(pos_player_2$pos_x[1:number_of_checkes]) 
  x_3 <- as.vector(pos_player_3$pos_x[1:number_of_checkes])
  
  z_1 <- as.vector(pos_player_1$pos_z[1:number_of_checkes]) 
  z_2 <- as.vector(pos_player_2$pos_z[1:number_of_checkes]) 
  z_3 <- as.vector(pos_player_3$pos_z[1:number_of_checkes])
  
  x_centroid<- (x_1 + x_2 + x_3)/3
  z_centroid<- (z_1 + z_2 + z_3)/3
  
  #Calculate the distance from each player to the centroid
  player_1_distance_from_centroid<- sqrt((x_1 - x_centroid)^2 + (z_1 - z_centroid)^2)
  player_2_distance_from_centroid<- sqrt((x_2 - x_centroid)^2 + (z_2 - z_centroid)^2)
  player_3_distance_from_centroid<- sqrt((x_3 - x_centroid)^2 + (z_3 - z_centroid)^2)
  
  #determine the strategy being used based on distance
  strategy_vectors<- seq(1:number_of_checkes)
  for(index in seq(1:number_of_checkes)){
    #Performing math operations on logic vectors turns TRUEs in 1's and FALSE's into 0's
    check_distances<- c(player_1_distance_from_centroid[index] < strategy_barrier_distance, player_2_distance_from_centroid[index] < strategy_barrier_distance, 
                        player_3_distance_from_centroid[index] < strategy_barrier_distance)
    
    #1 = "Go Together", 2 = "Go Alone", 3 = "Mix"
    if(sum(check_distances) == 3){
      #Go Together - this means that everyone is within the strategy border
      strategy_vectors[index]<- 1
      
    } else if(sum(check_distances) == 0){
      paste("Go Alone")
      #Go Alone - this means that everyone was outside the strategy border
      strategy_vectors[index] <- 2
    } else {
      paste("Mix")
      #Mix - this means that some were inside the borader and others were outside the border
      strategy_vectors[index] <- 3
    }
  }
  
  # 1 = "Go Together", 2 = "Go Alone", 3 = "Mix"
  count_go_together <- sum(strategy_vectors == 1)
  count_go_alone <- sum(strategy_vectors == 2)
  count_mix <- sum(strategy_vectors == 3)
  
  
  return(c(count_go_together, count_go_alone, count_mix))
}

# Function that counts the number of utterences for a player in a given condition ----
utterance_count_for_a_player<- function(positionData, condition, team_number, player_num){
  #Get players table data
  is_player_1 <- as.vector(positionData$expcondition == condition & positionData$teamnumber == team_number & 
                             positionData$playernum == player_num)
  
  current_player <- positionData[is_player_1,]
  
  #Determine the number of utterences
  current_player_transmission_set<- current_player$istransmitting
  
  #Need a variable that indicates if the player is currently talking
  isTalking <- FALSE
  
  #Utterance count
  utterance_count <- 0
  
  # Count the number of utterances
  # An utterance is defined as a consecutive set of checks were the intervale value is great than 0, (i.e., ~.25)
  for(index in seq(from = 1, to = length(current_player_transmission_set))){
    #get current check interval
    current_check_interval<- current_player_transmission_set[index]
    
    #If the index is 1, then this means it is the first check. The first check is different than the following checks
    if(index != 1){
      # This means that the current index is not one. As a result, there should have been a previosu check value
      # Get the previous value
      previous_check_interval<- current_player_transmission_set[index-1]
      
      if(current_check_interval > 0){
        #This means that the player was talking during this interval check
        
        #Is this the last vector index?
        if(index != length(current_player_transmission_set)){
          # This is not the last vaule in the vector
          
          #If the previous value was 0, that means that the player just started talking. We set the isTalking value to true
          if(previous_check_interval == 0.00){
            #This means that the player just started talking
          } else if(previous_check_interval > 0){
            #This means that the paleyer is continuing to talk
          }
        } else {
          # This is the last value in the vector. 
          #Since this is the last vector value and the value of it is greater than 0, this means the player was last talking before the game ended. 
          #This will count as a utterence. The loop should end after this current pass
          utterance_count <- utterance_count + 1
        }
        
        #The player is talking during this check
        isTalking <- TRUE
        
      } else if(current_check_interval == 0){
        # Was the player talking previously?
        if(isTalking){
          #This means that the player was talking previously but is not longer talking. This is the end of an utterance, so count it
          utterance_count <- utterance_count + 1
        } else{
          #This means that the player was not talkin previously and is still not talking
        }
        
        #The player is no longer talking
        isTalking <- FALSE
      } else {
        #isTalking <- FALSE
      }
      
    } else if(index == 1){
      # This means that this is the first index value.
      
      #If the player is talking, set isTalking to true
      if(current_check_interval > 0){
        isTalking <- TRUE
      }
    }
  }
  
  # Return utterance count for this player in a given condition
  return(utterance_count)
  
}

# Function to generate the IDs for a player on a given team (Assuming there are 3 members per team)----
generate_player_id <- function(playernum, teamnum){
  
  # This function assumes that player IDs are assigned in sequential order.
  # For example, in team 1, player 1 is P1, 2 is p2, and P3. Team 2, player 1 is P4, 2 is P5, 3 is P6
  
  max_player_ID_value <- 3 * teamnum # 3 is hardcoded because there should have only three elements
  palyer_ID <- ""
  if(playernum == 3){
    player_ID_num <- max_player_ID_value
    
    if(player_ID_num < 10){
      palyer_ID <- paste("P0", player_ID_num, sep = "")
    } else if(player_ID_num >= 10){
      palyer_ID <- paste("P", player_ID_num, sep = "")
    }
  } else if(playernum == 2 ){
    player_ID_num <- max_player_ID_value - 1
    
    if(player_ID_num < 10){
      palyer_ID <- paste("P0", player_ID_num, sep = "")
    } else if(player_ID_num >= 10){
      palyer_ID <- paste("P", player_ID_num, sep = "")
    }
  } else if(playernum == 1) {
    player_ID_num <- max_player_ID_value - 2
    
    if(player_ID_num < 10){
      palyer_ID <- paste("P0", player_ID_num, sep = "")
    } else if(player_ID_num >= 10){
      palyer_ID <- paste("P", player_ID_num, sep = "")
    }
  }
  
  return(palyer_ID)
}

#Function to retrive the NASA TLX value for a specific scale, for a specific player, in a specific team, in a specific condition
scale_value_NASA_TLX <- function (TLX_table, teamNum, playerNum, condition, scale){
  
  player_tlx <- TLX_table %>%
    filter(Team == teamNum, Player == playerNum, Condition == condition)
  if(length(player_tlx[,scale] == 1)){
    return(player_tlx[,scale])
  } else if(length(player_tlx[,scale]) == 0){
    stop(paste("No TLX data found for player", playerNum, "in team", teamNum, "in condition", condition))
  } else {
    stop(paste("There is not exactly 1 entery for player", playerNum, "in team", teamNum, "in condition", condition))
  }
}

# Get post-session value
post_session_survey_value<- function(data_post_session, team, player, condition, survey_value){
  player_data<- data_post_session %>%
    filter(Condition == condition, Team == team, Player == player)
  
  if(length(player_data[[1]]) != 1){
    message <- paste("Could not find post session survey data for player", player, "in team", team, "for condition", condition)
    stop(message)
  }
  
  return(player_data[[survey_value]])
}

# Function to find session order ----
session_order_number <- function(teamNum, counter_balance_set_dataframe, condition){
  set_index <- teamNum %% length(counter_balance_set_dataframe) #If this equal 0 then that means this team used the last set
  
  if(set_index == 0){
    #This means that this team used the last set
    set_index = length(counter_balance_set_dataframe)
  }
  
  # Select the correct session order list
  session_set_in_order <- counter_balance_set_dataframe[,set_index] # Assumes the set is in a data frame
  
  #Find the index of the session order and that is the session order (e.g., an index of 3 means this session was the 3rd session)
  current_session_order <- which(condition == session_set_in_order) 
  
  return(current_session_order)
}

# Total distance traveled by a player ----
total_distance_traveled_by_player<- function(position_data, experimentalcondition, teamnumber_current, playerNum, col_name_X, col_name_y){
  
  # Player data
  player_data_index <- which(position_data$teamnumber == teamnumber_current & 
                               position_data$playernum == playerNum & 
                               position_data$expcondition == experimentalcondition)
  
  x <- position_data[player_data_index,col_name_X]
  y <- position_data[player_data_index,col_name_y]
  
  x.1 <- x[1:length(x) - 1]
  x.2 <- x[2:length(x)]
  
  y.1 <- y[1:length(y)-1]
  y.2 <- y[2:length(y)]
  
  total_distance <- sum(sqrt((x.2 - x.1)^2 + (y.2 - y.1)^2)) 
  
  return(total_distance)
}

# Total distance traveled by team ----
total_distance_traveled_by_team <- function(position_data, experimentalcondition, teamnumber_current, col_name_X, col_name_y){
  # Assuming there are three members in each team
  distance_traveled_by_each_player <-  rep(x = 0, times = 3)
  for(player in seq(1,3)){
    distance_traveled_by_each_player[player] <- total_distance_traveled_by_player(position_data, experimentalcondition, teamnumber_current, player, col_name_X, col_name_y)
  }
  
  return(sum(distance_traveled_by_each_player))
}

# Total items (correct and incorrect)(team and individual) collected by a player in a given session
total_items_collected_in_session_by_individual <- function(inventory_data, team_num, player_num, condition_name){
  inventory_data_filtered <- inventory_data %>%
    filter(teamnumber == team_num & expcondition == condition_name & playernum == player_num & itemid != -1)
  
  return(length(inventory_data_filtered[,1]))
}

# Function to calculate the Collection Rate (i.e., duration / total items collected): Sec / Error.
# Duration is retruned if error count is 0.
collection_rate_ind <- function(data_position, data_inventory, teamnum, playernumber, condition){
  # This is the item collection rate for an individual
  # The units for this value is Sec / item. 
  # This takes into account the total items (incorrect or correct) collected by the individual
  
  total_items_collected <- total_items_collected_in_session_by_individual(data_inventory, teamnum, playernumber, condition)
  player_data <- data_position %>% filter(teamnumber == teamnum & playernum == playernumber & expcondition == condition)
  player_data_last_line <- tail(player_data, 1)
  duration_ind <- player_data_last_line[1,"duration_ind"]
  
  if(total_items_collected == 0){
    return(duration_ind)
  } else if(total_items_collected > 0){
    return(duration_ind / total_items_collected )
  } else if(total_items_collected < 0){
    message<- paste("collection_rate_ind: the total item count for player", playerNum, "in team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Total correct items (team and individual items) collected by a player in a given session
total_correct_items_collected_in_session_by_individual <- function(inventory_data, team_num, player_num, condition_name){
  inventory_data_filtered <- inventory_data %>%
    filter(playernum == player_num & teamnumber == team_num & expcondition == condition_name & itemid != -1 & boughtcorrectly == 1)
  return(length(inventory_data_filtered[,"itemid"]))
}

# Function to calculate the Collection Rate for correct items (i.e., duration / total items collected): Sec / Error.
# Duration is retruned if error count is 0.
collection_rate_correct_items_ind <- function(data_position, data_inventory, teamnum, playernumber, condition){
  # This is the item collection rate for an individual
  # The units for this value is Sec / item. 
  # This takes into account the total correct items (team and individual) collected by the individual
  
  total_items_collected <- total_correct_items_collected_in_session_by_individual(data_inventory, teamnum, playernumber, condition)
  player_data <- data_position %>% filter(teamnumber == teamnum & playernum == playernumber & expcondition == condition)
  player_data_last_line <- tail(player_data, 1)
  duration_ind <- player_data_last_line[1,"duration_ind"]
  
  if(total_items_collected == 0){
    return(duration_ind)
  } else if(total_items_collected > 0){
    return(duration_ind / total_items_collected)
  } else if(total_items_collected < 0){
    message<- paste("collection_rate_correct_items_ind: the total correct item count for player", playerNum, "in team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Total items (correct and incorrect)(team and individual) collected by a team in a given session
total_items_collected_in_session_by_team <- function(data_inventory, team_num, condition_name){
  inventory_data_filtered <- data_inventory %>%
    filter(teamnumber == team_num & expcondition == condition_name & itemid != -1)
  
  return(length(inventory_data_filtered[,"itemid"]))
}

# Function to calculate the Collection Rate (i.e., duration / total items collected): Sec / Error.
# Duration is retruned if error count is 0.
collection_rate_team <-  function(data_position, data_inventory, teamnum, condition){
  # This is the item collection rate for a team
  # The units for this value is Sec / item. 
  # This takes into account the total items (incorrect or correct) collected by the team
  
  total_items_collected <- total_items_collected_in_session_by_team(data_inventory, teamnum, condition)
  team_data <- data_position %>% filter(teamnumber == teamnum & playernum == 1, expcondition == condition)
  team_data_last_line <- tail(team_data, 1)
  duration_team <- team_data_last_line[1,"duration"]
  
  if(total_items_collected == 0){
    return(duration_team)
  } else if(total_items_collected > 0){
    return(duration_team / total_items_collected)
  } else if(total_items_collected < 0){
    message<- paste("collection_rate_team: the total item count for team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Total correct items (team and individual items) collected by a team in a given session
total_correct_items_collected_in_session_by_team <- function(data_inventory, team_num, condition_name){
  inventory_data_filtered <- data_inventory %>%
    filter(teamnumber == team_num & expcondition == condition_name & itemid != -1 & boughtcorrectly == 1)
  
  return(length(inventory_data_filtered[,"itemid"]))
}

# Function to calculate the Collection Rate for correct items (i.e., duration / total items collected): Sec / Error.
# Duration is retruned if error count is 0.
collection_rate_correct_items_team <- function(data_position, data_inventory, teamnum, condition){
  # This is the item collection rate for a team
  # The units for this value is Sec / item. 
  # This takes into account the total correct items (team and individual) collected by a team
  
  total_items_collected <- total_correct_items_collected_in_session_by_team(data_inventory, teamnum, condition)
  team_data <- data_position %>% filter(teamnumber == teamnum & playernum == 1, expcondition == condition)
  team_data_last_line <- tail(team_data, 1)
  duration_team <- team_data_last_line[1,"duration"]
  
  if(total_items_collected == 0){
    return(duration_team)
  } else if(total_items_collected > 0){
    return(duration_team / total_items_collected)
  } else if(total_items_collected < 0){
    message<- paste("collection_rate_correct_items_team: the total correct item count for team", teamNum, "in condition", condition)
    stop(message)
  }
}

# Check to see if the random numbers in demographics surveys match the random numbers in the post surveys ----
is_demographic_rand_num_in_post_survey <- function(post_session_table, demo_survey, team_col_name, player_col_name, rand_num_col_name){
  #team_col_name : for the post_session_table
  #rand_num_col_name: This name should be the same for the post_session_table survey and the demo_survey is the name in the 
  is_in_post_survey <- T
  
  # Make sure post session data is good
  post_session_survey_is_good <- is_post_session_data_correct(post_session_table, team_col_name, player_col_name, rand_num_col_name)
  if(!post_session_survey_is_good){
    message <- paste("Something wrong with post_session survey.")
    stop(message)
  }
  
  rand_num_list_demo <- as.vector(demo_survey[,rand_num_col_name])
  
  if(sum(duplicated(rand_num_list_demo)) > 0){ # If there is more than one of the same rand number, then something is wrong
    message <- paste("There are duplicate rand num values.")
    is_in_post_survey <- F
    stop(message)
  }
  
  for(rand_num in rand_num_list_demo){
    rand_in_post_survey <- rand_num == as.vector(post_session_table[,rand_num_col_name])
    
    if(sum(rand_in_post_survey) == 0){
      message <- paste("The rand number ", rand_num, " was not found in the post session", sep = "")
      stop(message)
    }
  }
  
  return(is_in_post_survey)
}

# Check to make sure TLX survey is correct (i.e., make sure every player in the key has the correct number of TLX values 4) ----
is_TLX_survey_correct <- function(data_tlx, rand_num_list){
  is_valid <- T
  
  for (rand in rand_num_list) {
    data_tlxTemp <- data_tlx %>%
      filter(Rand.Num == rand)
    
    if(length(data_tlxTemp[[1]]) != 4){
      message <- paste("There is not exactly 4 entries in the TLX for random number ", rand)
      is_valid <- F
      stop(message)
      break
    }
  }
  return(is_valid)
}

# Function to Check to make sure each rand number in the post survey responses are the same for each player in each team ( for all of the conditions) ----
is_post_session_data_correct <- function(post_session_data, team_number_column_name, player_num_col_name, rand_num_col_name){
  is_correct <- T
  team_list <- as.integer(levels(factor(post_session_data[,team_number_column_name]))) # The teams that are in the data set
  
  for(team in team_list){
    for(player in c(1,2,3)){
      team_col <- as.vector(post_session_data[,team_number_column_name])
      player_num_col <- as.vector(post_session_data[,player_num_col_name])
      player_index <- which(team_col == team & player_num_col == player)
      num.of.enteries <- length(player_index)
      
      if(num.of.enteries != 4){ # -- Check to see if there is exactly 4 entiers, if more/less than 4 then something is wrong
        message <- paste("There is either more than or less than 4 data points for team ", team, " and player ", player, sep = "")
        is_correct <- F
        stop(message)
      }
      
      # -- Pick first value and make sure it is the same throughout
      player_data <- post_session_data[player_index,]
      
      first.rand.num <- player_data[1, rand_num_col_name]
      all.rand.num <- player_data[,rand_num_col_name]
      
      if(sum(all.rand.num != first.rand.num) > 0){
        # This means one or more rand numbers enteries are not the same as the others 
        message <- paste("One or more of the rand number enteries are not the same as the other enteriesf. Team ", team, 
                         ", player ", player, ", rand ", first.rand.num, sep = "")
        is_correct <- F
        stop(message)
      }
      
      # Check to make sure the player numbers are correct
      team_index <- which(team_col == team)
      team_data <- post_session_data[team_index,]
      player_col <- as.vector(team_data[,"Player"]) # Get the player colum (i.e., 1,2, or 3)
      
      
      player_factors <- levels(factor(player_col))
      # print(paste("Factors: ", player_factors))
      
      for(player in player_factors){ # Check to make sure there are 4 enteries for each player
        num_of_enteries_for_player <- sum(player == player_col)
        
        if(num_of_enteries_for_player !=4){
          message <- paste("There is more or less than 4 enteries for player ", player, " in team ", team, ".", sep = "")
          is_correct <-F
          stop(message)
        }
      }
    }
  }
  
  
  return(is_correct)
}

# Generate aggragate data (final team score, final individual score, ) ----
generate_aggragate_data <- function(team_numbers, condition_list, clean_position_data, clean_error_data, clean_invent_data, 
                                    player_num_list, strategy_barrier_dis, counter_balance_set, col.names, names_TLX, names_PostSession){
  # Final data output
  number_of_columns<- length(col.names)
  data_output_final<- matrix(0, nrow = 0, ncol = number_of_columns)
  
  # Give column names
  colnames(data_output_final)<- col.names
  
  for(team in team_numbers){
    for(condition in condition_list){
      
      #Count the number of times strategies were used
      current_strategy_vector<- strategy_count_vector(position_data = clean_position_data, 
                                                      experimentalcondition = condition, 
                                                      teamnumber_current = team,
                                                      playernumber_one = player_num_list[1],
                                                      playernumber_two = player_num_list[2],
                                                      playernumber_three = player_num_list[3],
                                                      strategy_barrier_distance = strategy_barrier_dis)
      current_go_together_count<- current_strategy_vector[1]
      current_go_alone_count <- current_strategy_vector[2]
      current_mix<- current_strategy_vector[3]
      
      #Get the total utterence count for a team
      player_utterance_count_list<- rep(x = 0, times = length(player_num_list))
      
      #Get the total number of errors for team
      total_errors_team <- total_number_of_errors_team(clean_error_data, team, condition)
      
      #Find the count for each palyer (i.e., index 1 is player 1)
      for(player in player_num_list){
        current_player_utterence_count <- utterance_count_for_a_player(positionData = clean_position_data, condition, team, player)
        
        #ErrorCheck
        if(length(current_player_utterence_count) > 1){
          stop("utterance_count_for_a_player() function produceted a vector that had a length greater than 1. Should only be 1 element.")
        }
        
        player_utterance_count_list[player] <- current_player_utterence_count[1] #This is hard codded because there should only be one value
      }
      
      #Total Distance traveled - Team
      total_dis_team <- total_distance_traveled_by_team(position_data = clean_position_data, condition, team, "pos_x", "pos_z")
      
      # Get player 1 data
      is_player_1<- as.vector(clean_position_data$expcondition == condition & clean_position_data$teamnumber == team & 
                              clean_position_data$playernum == 1) #Use the team score recorded from player 1
      player_data_1<- clean_position_data[is_player_1,]
      last_line_1 <- length(player_data_1[,1]) #Last line index
      
      #Team Score
      team_final_score<- as.vector(player_data_1[last_line_1,"teamscore"])
      
      #Team time remaining
      time_remaining_team <- as.vector(player_data_1[last_line_1,"gametimer"])
      
      # Collection rate for team in a condition (correct and incorrect items)(team and individual)
      team_collection_rate <- collection_rate_team(clean_position_data, clean_invent_data, team, condition)
      
      # Collection rate for team in a condition (correct items) (team and individual items)
      team_collection_rate_correct_items <- collection_rate_correct_items_team(clean_position_data, clean_invent_data, team, condition)
      
      # Error rate for the team 
      team_error_rate <- error_rate_team(clean_position_data, clean_error_data, team, condition)
      
      for(player in player_num_list){
        # The next step is to add all of the values for each cloumn
        
        is_player<- as.vector(clean_position_data$expcondition == condition & clean_position_data$teamnumber == team & 
                                clean_position_data$playernum == player)
        
        player_data<- clean_position_data[is_player,]
        #The order of the variables is determined by the cloumn name vector - col_names
        
        #Last Line in Data Table
        last_line<- length(player_data[,1]) #Last line index
        
        # Set the target feedback and if there was feedback present
        if(condition == "A"){
          target_for_feedback<- c("None")
        } else if(condition == "B"){
          target_for_feedback<- c("Ind")
        } else if (condition == "C"){
          target_for_feedback<- c("Team")
        } else if (condition == "D"){
          #The only other condition is condition D
          target_for_feedback<- c("Ind_Team")
        } else {
          stop("A condition value was not recognized.")
        }
        
        
        # Get the time remaining team and individual
        time_remaining_ind <- as.vector(player_data[last_line,"gametimer_ind"])
        
        #Get the Individual Score
        individual_final_score<- as.vector(player_data[last_line,"individualscore"])
        
        #Correct Individual Items Collected
        correct_individual_items_collected<- as.vector(player_data[last_line, "ci_ind"])
        
        #Incorrect Individual Items Collected
        incorrect_individual_item_collected<- as.vector(player_data[last_line, "ii_ind"])
        
        #errors Individual
        errors_individual_unique<- as.vector(player_data[last_line, "error_ind"])
        
        #Get the total number of errors for player
        total_errors_ind <- total_number_of_errors_individual(data_errors = clean_error_data, team, player, condition)
        
        # Get total distance traveled by player
        total_dis_ind <- total_distance_traveled_by_player(position_data = clean_position_data, condition, team, player, "pos_x", "pos_z")
        
        #Correct Team Items Collected
        correct_team_items_collected<- as.vector(player_data[last_line, "ci_team"])
        
        #Incorrect Team Items collected
        incorrect_team_item_collected<- as.vector(player_data[last_line, "ii_team"])
        
        #Errors Team
        errors_team_unique<- as.vector(player_data[last_line, "error_team"])
        
        #Transmission
        total_transmition_ind<- sum(as.vector(player_data$istransmitting))
        
        #Utterance count
        current_utterance_count_player <- player_utterance_count_list[player]
        current_utterance_count_team <- sum(player_utterance_count_list)
        
        # total team transmission time
        current_team_index <- which(condition == clean_position_data[,"expcondition"] & team == clean_position_data[,"teamnumber"])
        current_team_position_data <- clean_position_data[current_team_index,]
        total_transmition_team <- sum(current_team_position_data$istransmitting)
        
        
        #Dominate Strategy. The dominitate strategy was the strategy used the highest number of times
        if(max(current_strategy_vector) == current_go_together_count){
          
          dominate_strategy_used<- go_together_name
          
        } else if (max(current_strategy_vector) == current_go_alone_count){
          
          dominate_strategy_used<- go_alone_name
        } else if (max(current_strategy_vector) == current_mix){
          dominate_strategy_used<- mix_name
        } else {
          stop("The max count in the strategy vector does not equal any of the selected strategy counts")
        }
        
        # Find player id
        current_player_id <- generate_player_id(player,team)
        
        #---------------------------------------------------------------
        #Find the session order
        current_session_order <- session_order_number(team, counter_balance_set, condition)
        
        #---------------------------------------------------------------
        
        # Collection rate for individual (correct and incorrect)(team and individual items)
        individual_collection_rate <- collection_rate_ind(clean_position_data, clean_invent_data, team, player, condition)
        
        # Collection rate for individual (correct items)(team and individual items)
        individual_collection_rate_correct_items <- collection_rate_correct_items_ind(clean_position_data, clean_invent_data, team, player, condition)
        
        # TLX values for player
        TLX_values<- vector()
        for(name in names_TLX){
          value<- scale_value_NASA_TLX(NASA_TLX_table, team, player, condition, name)
          TLX_values<- append(TLX_values,value)
        }
        
        # Post-Session Survey values
        Post_Session_Values<- vector()
        for (name in names_PostSession) {
          index<- which(names_PostSession == name)
          value<- as.character(post_session_survey_value(post_session_table, team, player,condition, name))
          Post_Session_Values[index] <- value
        }
        
        # Error Rate for individual
        ind_error_rate <- error_rate_ind(clean_position_data, clean_error_data, team, player, condition)
        
        #This should be the same as the col_names variable above.
        data_output_final<- rbind(data_output_final, 
                                  c(team, 
                                    condition, 
                                    player, 
                                    current_player_id,
                                    current_session_order,
                                    target_for_feedback,
                                    time_remaining_ind,
                                    time_remaining_team,
                                    individual_final_score, 
                                    team_final_score, 
                                    correct_individual_items_collected, 
                                    incorrect_individual_item_collected, 
                                    individual_collection_rate,
                                    individual_collection_rate_correct_items,
                                    errors_individual_unique,
                                    total_errors_ind,
                                    ind_error_rate,
                                    total_dis_ind,
                                    correct_team_items_collected,
                                    incorrect_team_item_collected,
                                    team_collection_rate,
                                    team_collection_rate_correct_items,
                                    errors_team_unique,
                                    total_errors_team,
                                    team_error_rate,
                                    total_dis_team,
                                    total_transmition_ind,
                                    total_transmition_team,
                                    current_utterance_count_player,
                                    current_utterance_count_team,
                                    current_go_together_count,
                                    current_go_alone_count,
                                    current_mix,
                                    dominate_strategy_used,
                                    TLX_values, 
                                    Post_Session_Values))
      }
    }
    
    # Progress Bar
    team_index <- which(team == team_numbers)
    progress(team_index / length(team_numbers) * 100)
  }
  
  return(data_output_final)
}

# Generate figures for dependent variables with specified x variables ----
generate_figures_team <- function(Data, num_of_teams, figure_titles, y_values_team, y_labels_team, x_values, x_labels_team, plot_types, filelocation){ # Need to change the variables
  previous_wd_location <- getwd()
  
  setwd(filelocation)
  
  # What is the N for Teams
  N_teams <- num_of_teams
  
  # The N text to add to title for teams 
  N_teams_full_text <- paste("(N = ", N_teams, ")", sep = "")
  
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
          Data_ordered <- Data %>%
            mutate(position = rank(-Data[,y_current], ties.method="first"))
          ggplot(data = Data_ordered, aes_string(x = x_current, y = y_current, fill = "Team", group = "position")) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = paste(figure_title, N_teams_full_text) , x = x_label, y = y_label) +
            guides(fill=guide_legend(title="Team"))
          ggsave(filename = filename_graph)
          
        } else if(plot == "Boxplot"){
          ggplot(data = Data, aes_string(x = x_current, y = y_current)) +
            geom_boxplot() +
            labs(title = paste(figure_title, N_teams_full_text), x = x_label, y = y_label)
          ggsave(filename = filename_graph)
        } else if(plot == "Point_plot"){
          ggplot(data = Data, aes_string(x = x_current, y = y_current)) +
            geom_point() +
            labs(title = paste(figure_title, N_teams_full_text), x = x_label, y = y_label)
          ggsave(filename = filename_graph)
        }
      }
    }
    
    # Progress Bar
    y_index <- which(y_current == y_values_team)
    progress(y_index / length(y_values_team) * 100)
  }
  
  setwd(previous_wd_location)
}

# Generate figures for dependent variables with specified x variables (Individual) ----
generate_figures_ind <- function(Data, num_of_players, figure_titles, y_values_ind, y_labels_ind, x_values_ind, x_labels_ind, plot_types_ind, filelocation){
  previous_wd_location <- getwd()
  
  # What is the N for Inds
  N_ind <- num_of_players
  
  # The N text to add to title for Inds 
  N_ind_full_text <- paste("(N = ", N_ind, ")", sep = "")
  
  for(y_current in y_values_ind){
    for (x_current in x_values_ind){
      index_for_y <- which(y_current == y_values_ind)
      index_for_x <- which(x_current == x_values_ind)
      
      for(plot in plot_types_ind){
        x_label <- x_labels_ind[index_for_x]
        y_label <- y_labels_ind[index_for_y]
        figure_title <- figure_titles[index_for_y]
        filename_graph <- paste("ind_",y_label,"_by_",x_label,"_",plot,".png", sep = "")
        
        if(plot == "Group_Bar"){
          Data_filtered<- Data %>%
            mutate(position = rank(-Data[,y_current], ties.method="first"))
          # print(paste("team_",y_label,"_by_",x_label,"_",plot, sep = ""))
          ggplot(data = Data_filtered, aes_string(x = x_current, y = y_current, fill = "Player_ID", group = "position")) +
            geom_bar(stat = "identity", position = "dodge") +
            labs(title = paste(figure_title, N_ind_full_text) , x = x_label, y = y_label) +
            guides(fill=FALSE)
          ggsave(filename = filename_graph)
          
        } else if(plot == "Boxplot"){
          ggplot(data = Data, aes_string(x = x_current, y = y_current)) +
            geom_boxplot() +
            labs(title = paste(figure_title, N_ind_full_text), x = x_label, y = y_label)
          ggsave(filename = filename_graph)
        } else if(plot == "Point_plot"){
          ggplot(data = Data, aes_string(x = x_current, y = y_current)) +
            geom_point() +
            labs(title = paste(figure_title, N_ind_full_text), x = x_label, y = y_label)
          ggsave(filename = filename_graph)
        }
      }
    }
    
    # Progress Bar
    y_index <- which(y_current == y_values_ind)
    progress(y_index / length(y_values_ind) * 100)
  }
  
  setwd(previous_wd_location)
}

# Model the data for the team level anlysis ----
model_data_Target_Session <- function(df, dependent, model.type, is.team){
  
  if(is.team){
    if(model.type == "null"){
      lmer(data = df, as.formula(paste(dependent,"~ 1 + (1|Team)")))
    } else if(model.type == "All"){
      lmer(data = df, as.formula(paste(dependent,"~ Target * SessionOrder + (1|Team)")))
    } else if(model.type == "NoInteraction"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + SessionOrder + (1|Team)")))
    } else if(model.type == "NoTarget"){
      lmer(data = df, as.formula(paste(dependent,"~ SessionOrder + Target:SessionOrder + (1|Team)")))
    } else if(model.type == "NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + Target:SessionOrder + (1|Team)")))
    } else if(model.type == "NoInteraction_NoTarget"){
      lmer(data = df, as.formula(paste(dependent,"~ SessionOrder + (1|Team)")))
    } else if(model.type == "NoInteraction_NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + (1|Team)")))
    } else if(model.type == "NoTarget_NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target:SessionOrder + (1|Team)")))
    } else{
      stop("Model.type not supported")
    }
  } else {
    # Run this code if individual level model
    if(model.type == "null"){
      lmer(data = df, as.formula(paste(dependent,"~ 1 + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "All"){
      lmer(data = df, as.formula(paste(dependent,"~ Target * SessionOrder + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoInteraction"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + SessionOrder + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoTarget"){
      lmer(data = df, as.formula(paste(dependent,"~ SessionOrder + Target:SessionOrder + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + Target:SessionOrder + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoInteraction_NoTarget"){
      lmer(data = df, as.formula(paste(dependent,"~ SessionOrder + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoInteraction_NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target + (1|Team) + (1| Player_ID)")))
    } else if(model.type == "NoTarget_NoSession"){
      lmer(data = df, as.formula(paste(dependent,"~ Target:SessionOrder + (1|Team) + (1| Player_ID)")))
    } else{
      stop("Model.type not supported")
    }
  }
}

#Test ----

# dependent <- "TeamScore"
# data.to.use <- team_data
# 
# test <- function(df, dependent, model.type){
#   if(model.type == "null"){
#     lmer(data = df, as.formula(paste(dependent,"~ 1 + (1|Team)")))
#   } else if(model.type == "All"){
#     lmer(data = df, as.formula(paste(dependent,"~ Target + SessionOrder + (1|Team)")))
#   }
# }
# 
# # anova(test(team_data, "TeamScore", "null"), test(team_data, "TeamScore", "All"))
# anova(model_data_Target_Session(team_data, "TeamScore", "null",T), model_data_Target_Session(team_data, "TeamScore", "All",T), model_data_Target_Session(team_data, "TeamScore", "NoInteraction",T))
# test1 <- model_data_Target_Session(team_data, "TeamScore", "All",T)
# summary(update(test1, . ~ . -Target:Session))
# lmer(as.formula(paste(dependent, "~ Target * SessionOrder + (1|Team)")), data.to.use)
