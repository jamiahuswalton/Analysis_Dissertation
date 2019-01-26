# Read data into project

# ---- Read in data from database folder
setwd(folder_location_database)

positionTable <- read.csv(positiondatafilename)
inventory_table<- read.csv(inventory_data_filename)
error_log_data<- read.csv(error_log_filename)

# ---- Read in data from sruvey folder

setwd(folder_location_survey)

post_session_table <- read.csv(post_session_filename)
demographic_table <- read.csv(demographic_filename)
NASA_TLX_table <- read.csv(NASA_TLX_filename)
Rand_num_key<- read.csv(Rand_num_key_filename)
