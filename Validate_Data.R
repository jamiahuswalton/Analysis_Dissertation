# Validate data and make sure everything is correct and clean

# Make sure post session survey and demographic survey is clean and correct
is_post_session_data_correct(post_session_table, "Team", "Player", "Rand")
is_demographic_rand_num_in_post_survey(post_session_table, demographic_table, "Team", "Player", "Rand")

# Need a function to check the TLX (maybe I can just use the post session function)
is_TLX_survey_correct(NASA_TLX_table, demographic_table$Rand)
