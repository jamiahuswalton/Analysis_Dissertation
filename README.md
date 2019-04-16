# Analysis_Dissertation
The purpose of this repositroy it to develop R scripts to analysis team performance for Jamiahus Walton's dissertation

# How to Generate / Load Data

## First Time

First you need run the following scripts:
1. Data_functions: Run the whole script
2. Folder_File_Location: Run the whole script
3. Packages: Run the whole script
4. Read_Data: Run the whole script
5. Valiadate_Data: Run the whole script
6. Generate_Data: Run the whole script

File 2: These are the location for the data on my system. This file may look different depending on where the files are located. 
**Note:** ***You will need to change the file locations to fit your machine. Use the comments to understand the purpose of the variable.*** 

File 4: Problems may arise here if you significantly modify the values in the Folder_File_Location script

File 3: The purpose of this script is to make sure there are no issues in the data. More validation checks may be added later

File 6: This is the script that generate all of the aggragate data using the function and file locations generated in the previous scripts (Data_functions and Folder_File_Location). The aggregate data is store in the variable my_aggregate_data at the bottom of the script. After the data is generated, it is saved as a CSV file in the location defined in the Folder_file_Location script. 

## Next time
Run File 1 to File 3.

Then you only need to run the last line in File 6. This is the line that reads the previously generated data into the my_aggregate_data variable. 
# Explore - Model Data
Once the data is loaded, you use the other files to look at the data (e.g., Explore or Model).

# Analysis RMarkdown
The analysis notes and observations for the data is stored in the Analysis.rmd file. 
