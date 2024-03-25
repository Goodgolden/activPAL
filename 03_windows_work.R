## For MAC or LINUX

### For windows or Linux users, the following code should be used to set the folder locations.

library(activPAL)
'force = TRUE'


Events_Files_To_Process_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Events_Files_To_Process\\"

Temp_Output_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Temp_Output\\"

Confirmed_Output_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Confirmed_Output\\"

### Confirm the folder on Mac
# You should be able to see the some files listed in the folder;
# otherwise, you may need to adjust the folder location.

list.files(Events_Files_To_Process_folder_location)
# list.files(Temp_Output_folder_location)
# list.files(Confirmed_Output_folder_location)


### Run the following code to process the data
activpal.process.folder.windows(Events_Files_To_Process_folder_location,
                                Temp_Output_folder_location)

Last_Batched_Ids <- activpal.process.folder.windows(Events_Files_To_Process_folder_location,
                                                    Temp_Output_folder_location)

prepare.ex.times.windows(Temp_Output_folder_location)

make.index.file.windows(Events_Files_To_Process_folder_location,
                        Temp_Output_folder_location)

individual.chart.overlay.windows(Temp_Output_folder_location)

## run after adjusting estimate exercise times

apSummary.windows(Events_Files_To_Process_folder_location,
                  Confirmed_Output_folder_location)








