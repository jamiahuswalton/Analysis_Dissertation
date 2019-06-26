# Folder and files locations

# Folder locatons
  # This is the folder where the figures are saved
figure_directory <- "C:\\Users\\jamia\\Box\\TMET2\\DATA TMET2\\Data_And_Calcuations\\Figures"
  # This is the folder that contains all of the raw data.
  # This would include the database data and the survey data.
  # File sturcture is setup assuming that there will be subfolders that contain the respective data. 
  # See the file output names below
main_work_directory_name <- "C:\\Users\\jamia\\Box\\TMET2\\DATA TMET2\\Data_And_Calcuations\\Raw Data\\"

# File output names and file locations
  # This will be the name of the final aggragate file. Remember to include the '.csv' at the end
file_name_output <- "team_player_aggragate_stats.csv"
  # This is the name of the subfolder (i.e., the parent folder is the 'main_work_directory_name') that contains the raw database data.
database_folder_name <- "Database"
  # This is the name of the subfolder (i.e., the parent folder is the 'main_work_directory_name') that contains the raw survey data. 
survey_folder_name <- "Survey"
  # This variable should build the absolute file path to the database folder that contains raw database data
folder_location_database <- paste(main_work_directory_name, database_folder_name, sep = "")
  # This points to the location where the aggregate file will be saved.
  # Currently, the file is saved in the database folder.
  # It is important to note that this file path includes the file name (i.e., 'file_name_output')
aggregate_folder_location <- paste(folder_location_database,"\\", file_name_output, sep = "") #This will combine the final file name and the desiered folder location
  # This variable should build the absolute file path to the survey folder that contains raw survey data
folder_location_survey <- paste(main_work_directory_name, survey_folder_name, sep = "")

# Files names
# ---- Database file names
positiondatafilename<- "position_for_players.csv"
inventory_data_filename<- "inventory_activity.csv"
error_log_filename<- "error_log.csv"
familiarity_filename<- "familiarity.csv"

# ---- Surevy files 
post_session_filename <- "Data_Post-Session.csv"
demographic_filename <- "Data_Demographics_Pre-Survey.csv"
NASA_TLX_filename <- "Data_NASA-TLX.csv"
Rand_num_key_filename <- "Data_RandNum_Key.csv"
overal_post_session_filename<- "Data_Overall_Post.csv"
