## For MAC or LINUX

### For Macbook or Linux users, the following code should be used to set the folder locations.

library(activPAL)
library(tidyverse)
'force = TRUE'

## be careful about the directory setup
## it depends on how you name the folders and
## where you started mapping the folders
dir <- ("/Volumes/IMSLAB/IMSL RESEARCH PROJECTS/SPARX III/SPARX III CORE/SPARX DATA ANALYSIS/SPARX ACTIVPAL PROCESSING/AP_Processing_Directory/")
setwd(dir)

(Events_Files_To_Process_folder_location <- "Events_Files_To_Process/")
Temp_Output_folder_location <- "Temp_Output/"
Confirmed_Output_folder_location <- "Confirmed_Output/Randy_macbook/"


### Confirm the folder on Mac
# You should be able to see the some files listed in the folder;
# otherwise, you may need to adjust the folder location.

list.files(Events_Files_To_Process_folder_location)
# list.files(Temp_Output_folder_location)
# list.files(Confirmed_Output_folder_location)


### Run the following code to process the data
# activpal.process.folder.macbook(Events_Files_To_Process_folder_location,
#                                 Temp_Output_folder_location,
#                                 Confirmed_Output_folder_location)

Last_Batched_Ids <- activpal.process.folder.macbook(Events_Files_To_Process_folder_location,
                                                    Temp_Output_folder_location,
                                                    Confirmed_Output_folder_location)

prepare.ex.times.macbook(Temp_Output_folder_location)

make.index.file.macbook(Events_Files_To_Process_folder_location,
                        Temp_Output_folder_location)

individual.chart.overlay.macbook(Temp_Output_folder_location)

## run after adjusting estimate exercise times

apSummary.macbook(Events_Files_To_Process_folder_location,
                  Confirmed_Output_folder_location)








