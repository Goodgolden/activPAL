
## 2.1.2 For WINDOWS -----------------------------------------------------------
## if you are using a MAC or LINUX machine, do not run this block of code
Events_Files_To_Process_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Events_Files_To_Process\\"

Temp_Output_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Temp_Output\\"

Confirmed_Output_folder_location <-
  "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Confirmed_Output\\"


## 2.2 Double check connection -------------------------------------------------
## if you see the files listed, then you are right to go
## otherwise change the folder location until you can see the files
## (if you did not link to the server, the output will be **character(0)**)
list.files(Events_Files_To_Process_folder_location)

## 2.3 Data analysis -----------------------------------------------------------
activpal.process.folder(Events_Files_To_Process_folder_location,
                        Temp_Output_folder_location)

Last_Batched_Ids <- activpal.process.folder(Events_Files_To_Process_folder_location,
                                            Temp_Output_folder_location)

prepare.ex.times(Temp_Output_folder_location)

make.index.file(Events_Files_To_Process_folder_location,
                Temp_Output_folder_location)

individual.chart.overlay.windows(Temp_Output_folder_location)




## 2.4 Run after adjusting estimate exercise times	--------------------------------
apSummary(Events_Files_To_Process_folder_location,
          Confirmed_Output_folder_location)

