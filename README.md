# Analysis_Dissertation
The purpose of this repositroy it to develop R scripts to analysis team performance for Jamiahus Walton's dissertation

# How to use
You should run all lines in the files in the following order:

- Data_functions
- Folder_File_Location
- Packages
- Read_Data
- Valiadate_Data
- Generate_Data


# Generate Data
## First Time
Run this whole file if you want to generate the aggregate data. The aggregate data is store in the variable my_aggregate_data at the bottom of the script. After the data is generated, it is saved as a CSV file in the location of your choice. 

## Next time
If you have already generated the aggregate data, then you do not need to run the whole script. You just need to run the line that reads the CSV from a specified location. This value will be stored in the my_aggregate_data variable. This line of code is at the bottom.

# Explore - Model Data
Once the data is loaded, you use the other files to look at the data (e.g., Explore or Model).

# Analysis RMarkdown
The analysis notes and observations for the data is stored in the Analysis.rmd file. 