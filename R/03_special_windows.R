#' Title activpal.process.folder.windows
#'
#' @param Events_Files_To_Process_folder_location
#' @param Temp_Output_folder_location
#'
#' @return current_batched_ids
#' @export
activpal.process.folder.windows <-
  function(Events_Files_To_Process_folder_location,
           Temp_Output_folder_location,
           Confirmed_Output_folder_location) {

    # Assumes that the name of the events files contain
    # the text Events in the file name
    file_names <- list.files(Events_Files_To_Process_folder_location,
                             pattern = "*.csv",
                             recursive = TRUE)
    file_names <- file_names[grep("Events.csv",
                                  file_names)]

    # Create an empty data frame to load the data into
    all_summary <- list()
    current_batched_ids <- vector()

    for (i in (1:length(file_names))) {
      # Change substr to get the prefix of the filename that
      # matches the File code field (column 2)
      # in the daily validation file
      # break_point <- regexpr(" |-",i)[1]
      curr_uid <- parse.file.name(file_names[i])
      id <- unlist(strsplit(curr_uid, "-"))[1]

      # Load the file
      events_file <- read.csv(paste(Events_Files_To_Process_folder_location,
                                    file_names[i],
                                    sep = ""),
                              row.names = NULL,
                              sep = ",",
                              stringsAsFactors = FALSE)
      # head(events_file)
      # colnames(events_file) <- c(tail(colnames(events_file),-1),"")
      # events_file <- events_file[,-ncol(events_file)]

      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400,
                                     origin = "1899-12-30", tz = "UTC")

      valid_day_list <- valid.days(events_file)
      events_file$date <- as.Date(events_file$Time)
      events_file <- events_file[which(events_file$date %in% valid_day_list), ]
      stepping_summary <- list()

      colnames(events_file)[4] <- "Event.Type"
      colnames(events_file)[3] <- "Duration..s."

      if (nrow(events_file[which(events_file$Event.Type == 2.0), ]) > 0) {
        min_value <- c(2700)
        max_value <- c(86400)
        for (j in (1:length(min_value))) {
          # time_value <- activpal.stepping.process.file(events_file,j,
          #                                              0.7,86400,1000,FALSE)

          time_value1 <- activpal.stepping.process.file(events_file,
                                                        min_value[j],
                                                        0.1,
                                                        max_value[j],
                                                        72000, TRUE)


          time_value2 <- sed.start.end.marker(events_file,
                                              time_value1,
                                              event = 0,
                                              duration.min = 30,
                                              duration.max = 600,
                                              interval = 600)
          #    	time_value3 <- sed.activity.marker(events_file,time_value2,
          #    	                                   duration=600)

          time_value3 <- stand.start.end.marker(events_file,
                                                time_value2,
                                                event = 1,
                                                duration.min = 30,
                                                duration.max = 600,
                                                interval = 600)

          time_value4 <- transportation.activity.marker(events_file,
                                                        time_value3,
                                                        duration = 3)

          time_value <- exercise.log(time_value4,
                                     Events_Files_To_Process_folder_location,
                                     id)

          # names(time_value)

          time_value_use <- cbind(time_value[, (1:6)],
                                  time_value[, (20)],
                                  time_value[, (7:8)],
                                  time_value[, (13:14)],
                                  time_value[, (19)],
                                  time_value[, (9:12)],
                                  time_value[, (15:18)])

          names(time_value_use) <- c("Date", "Ex_Start",
                                     "Ex_End", "Num_Steps", "Step_Minutes",
                                     "Cadence", "Exercise_Log",
                                     "Sed_Start_Marker", "Sed_End_Marker",
                                     "Stand_Start_Marker", "Stand_End_Marker",
                                     "Transportation_Marker",
                                     "Sed_Start_Marker_Mins_From_Ex",
                                     "Sed_Start_Marker_Mins_Duration",
                                     "Sed_End_Marker_Mins_From_Ex",
                                     "Sed_End_Marker_Mins_Duration",
                                     "Stand_Start_Marker_Mins_From_Ex",
                                     "Stand_Start_Marker_Mins_Duration",
                                     "Stand_End_Marker_Mins_From_Ex",
                                     "Stand_End_Marker_Mins_Duration")
          time_value_use$Step_Minutes <- time_value_use$Step_Minutes / 60

          if (nrow(time_value_use) > 0) {
            time_value_use$Window_Size <- min_value[j]
            stepping_summary[[j]] <- time_value_use
            message(paste(curr_uid, min_value[j], sep = " "))
          } else {
            message(paste(curr_uid, min_value[j], "- No Values", sep = " "))
          }
        }
        stepping_summary <- bind_rows(stepping_summary)
        if (nrow(stepping_summary) > 0) {
          stepping_summary$Uid_Date <- curr_uid
          stepping_summary$ID <- id

          stepping_summary$Ex_Start <- as.POSIXlt(stepping_summary$Ex_Start,
                                                  format = "%Y/%m/%d %H:%M:%S")
          stepping_summary$Ex_End <- as.POSIXlt(stepping_summary$Ex_End,
                                                format = "%Y/%m/%d %H:%M:%S")
          stepping_summary$Date <- as.Date(stepping_summary$Date,
                                           format = "%Y/%m/%d")
          stepping_summary$Ex_Start <- format(stepping_summary$Ex_Start)
          stepping_summary$Ex_End <- format(stepping_summary$Ex_End)
          stepping_summary$Ex_Date <- format(stepping_summary$Date, usetz = TRUE)

          #         all_summary[[i]] <- stepping_summary
          current_batched_ids <- c(current_batched_ids, id)

          # Tue Mar 19 16:55:36 2024 ------------------------------
          ## in macbook we have to created the folder first
          ## then add the files into the folder
          dir.create(paste(Temp_Output_folder_location,
                           id,
                           sep = ""),
                     showWarnings = FALSE)

          writexl::write_xlsx(stepping_summary,
                              paste(Temp_Output_folder_location,
                                    id, "\\",
                                    id,
                                    "_ex_times_temp.xlsx",
                                    sep = ""))

          write.table(stepping_summary,
                      file = paste(Temp_Output_folder_location,
                                   id, "\\",
                                   id,
                                   "_ex_times_temp.csv",
                                   sep = ""),
                      sep = ",",
                      row.names = FALSE)

          write.table(current_batched_ids,
                      file = paste(Temp_Output_folder_location,
                                   "Last_Batched_Ids.csv",
                                   sep = ""),
                      sep = ",",
                      row.names = FALSE)

          stepping_summary1 <- filter(stepping_summary,
                                      !is.na(Exercise_Log),
                                      Exercise_Log != 0)

          dir.create(paste(Confirmed_Output_folder_location,
                           id,
                           sep = ""), showWarnings = FALSE)

          write.table(stepping_summary1,
                      file = paste(Confirmed_Output_folder_location,
                                   id, "\\", id,
                                   "_ex_times_confirmed.csv",
                                   sep = ""),
                      sep = ",",
                      row.names = FALSE)
          writexl::write_xlsx(stepping_summary1,
                              paste(Confirmed_Output_folder_location,
                                    id, "\\",
                                    id,
                                    "_ex_times_confirmed.xlsx",
                                    sep = ""))
        }
      }
    }
    #    return(bind_rows(all_summary))
    return(current_batched_ids)
  }



#' Title apSummary.windows
#'
#' @param Events_Files_To_Process_folder_location
#' @param Confirmed_Output_folder_location
#'
#' @return
#' @export
apSummary.windows <- function(Events_Files_To_Process_folder_location,
                              Confirmed_Output_folder_location) {
  file_names <- list.files(Events_Files_To_Process_folder_location,
                           pattern = "*Events.csv",
                           recursive = TRUE)

  for (i in file_names) {
    id <- unlist(strsplit(i, "-"))[1]

    print(id)

    ex.times.list <- list.files(Confirmed_Output_folder_location,
                                pattern = "*_ex_times_confirmed.xlsx",
                                recursive = TRUE)

    ## To find the ex_times_confirmed.csv files for the id
    ## "5179/5179_ex_times_confirmed.csv"
    ## not very efficient but works
    ex.times.path <- ex.times.list[grep(id, ex.times.list)]


    if (length(ex.times.path) == 0) {
      print(id)
      print("No Exercise Times")
    }

    if (length(ex.times.path) > 0) {
      events_file <- read.csv(paste(Events_Files_To_Process_folder_location,
                                    i,
                                    sep = ""),
                              row.names = NULL,
                              sep = ",",
                              stringsAsFactors = FALSE)
      # events_file <- readxl::read_excel(paste(Events_Files_To_Process_folder_location,
      #                                         i,
      #                                         sep = ""))
      ## reading files form the Events_Files_To_Process_folder_location
      # View(events_file)

      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400,
                                     origin = "1899-12-30",
                                     tz = "UTC")
      events_file$Date <- as.Date(events_file$Time)

      events_file$date <- as.Date(events_file$Time)

      events_file$exercise <- 0

      # View(events_file)
      # names(events_file)
      colnames(events_file)[4] <- "Event.Type"
      colnames(events_file)[3] <- "Duration..s."

      # 	events_file <- split.days(events_file)

      ## Split events that cross two days ##

      events_file <- events_file[order(events_file$Time), ]
      rownames(events_file) <- 1:nrow(events_file)
      events_file$date <- as.Date(events_file$Time)
      events_file$diff <- (difftime(events_file$Time,
                                    events_file$date,
                                    tz = "UTC",
                                    units = "secs") +
                             events_file$Duration..s.) - 86400
      cross.days <- which(events_file$diff > 0)
      events_file <- rbind(events_file, events_file[cross.days, ])
      events_file[cross.days, ]$Duration..s. <-
        round(events_file[cross.days, ]$Duration..s. - events_file[cross.days, ]$diff, 1)
      events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$Duration..s. <-
        round(events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$diff, 1)
      events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$Time <-
        events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$date + 1
      events_file <- events_file[order(events_file$Time), ]
      rownames(events_file) <- 1:nrow(events_file)
      events_file$date <- as.Date(events_file$Time)

      ### 	a valid day is set for 20hrs of wear	###

      valid_day_list <- valid.days(events_file)
      events_file <- events_file[which(events_file$date %in% valid_day_list), ]


      #  	id <- unlist(strsplit(i,"-"))[1]

      # ex.times <- read.csv(paste(Confirmed_Output_folder_location, id, "\\",
      #                            id, "_ex_times_confirmed.csv", sep = ""))

      ex.times <- readxl::read_excel(paste(Confirmed_Output_folder_location, id, "\\",
                                           id, "_ex_times_confirmed.xlsx", sep = ""))

      ex.times$Ex_Start <- strptime(ex.times$Ex_Start, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
      ex.times$Ex_End <- strptime(ex.times$Ex_End, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


      et <- dim(ex.times)[1]

      # 	e <- 1
      # 	head(events_file)
      # 	unique(events_file$exercise)

      for (e in (1:et)) {
        start <- ex.times$Ex_Start[e]
        end <- ex.times$Ex_End[e]

        events_file$exercise[which(events_file$Time >= start & events_file$Time < end)] <- 1
      }

      n <- dim(events_file)[1]

      for (f in (1:(n - 1))) {
        temp.num.steps <- events_file$CumulativeStepCount[f + 1] -
          events_file$CumulativeStepCount[f]
        if (f == 1) {
          num.steps <- temp.num.steps
        }
        if (f != 1) {
          num.steps <- c(num.steps, temp.num.steps)
        }
      }
      num.steps <- c(events_file$CumulativeStepCount[1], num.steps)
      events_file$Num.Steps <- num.steps


      #     	dates <- as.Date(strptime(ex.times$Date,format="%m/%d/%Y",tz="UTC"))

      dates <- valid_day_list

      date.counter <- 1

      # 	d <- dates[1]
      # 	head(events_file)

      for (d in dates) {
        temp.day <- events_file[which(events_file$date == d), ]

        td <- dim(temp.day)[1]
        (temp.day.ex <- temp.day[which(temp.day$exercise == 1), ])
        tde <- dim(temp.day.ex)[1]

        tot.steps <- sum(temp.day$Num.Steps[which(temp.day$Event.Type == 2)]) * 2
        ex.steps <- sum(temp.day.ex$Num.Steps[which(temp.day.ex$Event.Type == 2)]) * 2
        ex.step.mins <- sum(temp.day.ex$Duration..s.[which(temp.day.ex$Event.Type == 2)]) / 60
        stand.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type == 1)]) / 60
        sed.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type == 0 | temp.day$Event.Type == 5)]) / 60
        tot.step.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type == 2)]) / 60
        tot.min <- (sum(temp.day$Duration..s.[which(temp.day$Event.Type != 4)]) / 60)
        tot.cycling.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type == 2.1)]) / 60

        wake.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type != 3.1 & temp.day$Event.Type != 3.2 & temp.day$Event.Type != 4.0)]) / 60
        sleep.min <- sum(temp.day$Duration..s.[which(temp.day$Event.Type == 3.1 | temp.day$Event.Type == 3.2)]) / 60

        non.ex.step.min <- tot.step.min - ex.step.mins
        non.ex.num.step <- tot.steps - ex.steps

        temp.summary <- data.frame("Participant ID" = id,
                                   "Day" = date.counter,
                                   "Date Worn" = unique(temp.day$date),
                                   "Total Steps" = tot.steps,
                                   "Exercise Steps" = ex.steps,
                                   "Steps w/o Exercise" = non.ex.num.step,
                                   "Sleep (min)" = sleep.min,
                                   "Wake (min)" = wake.min,
                                   "Time Standing (min)" = stand.min,
                                   "Time Walking (min)" = tot.step.min,
                                   "Time Sedentary (min)" = sed.min,
                                   "Total Time (min)" = tot.min,
                                   "Total Other (min)" = tot.cycling.min,
                                   check.names = FALSE)

        if (date.counter == 1) {
          summary <- temp.summary
        }
        if (date.counter > 1) {
          summary <- rbind(summary, temp.summary)
        }

        date.counter <- date.counter + 1
      }
      write.table(summary, file = paste(Confirmed_Output_folder_location,
                                        id,
                                        "\\",
                                        id,
                                        "_summary_confirmed.csv",
                                        sep = ""),
                  row.names = FALSE, sep = ",")
    }
  }
}
