# Folder and files locations

# Folder locatons
figure_directory <- "C:\\Users\\jamia\\Box\\TMET2\\DATA TMET2\\Data_And_Calcuations\\Figures"
main_work_directory_name <- "C:\\Users\\jamia\\Box\\TMET2\\DATA TMET2\\Data_And_Calcuations\\Raw Data\\"

# File output names and file locations
file_name_output <- "team_player_aggragate_stats.csv"
database_folder_name <- "Database"
survey_folder_name <- "Survey"
folder_location_database <- paste(main_work_directory_name, database_folder_name, sep = "")
aggregate_folder_location <- paste(folder_location_database,"\\", file_name_output, sep = "") #This will combine the final file name and the desiered folder location
folder_location_survey <- paste(main_work_directory_name, survey_folder_name, sep = "")

# Files names
# ---- Database file names
positiondatafilename<- "position_for_players.csv"
inventory_data_filename<- "inventory_activity.csv"
error_log_filename<- "error_log.csv"

# ---- Surevy files 
post_session_filename <- "Data_Post-Session.csv"
demographic_filename <- "Data_Demographics_Pre-Survey.csv"
NASA_TLX_filename <- "Data_NASA-TLX.csv"
Rand_num_key_filename <- "Data_RandNum_Key.csv"
