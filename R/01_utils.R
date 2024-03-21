# Title:	packages to make overlay charts


#	functions to identify possible exercise bouts -----------------------------------

# library(devtools)
# install_github("PALkitchen/activPAL")
# library(activPAL)

## 1.0 dependencies ---------------------------------------------------------------
# library(dplyr)

## 1.1 fun_call -------------------------------------------------------------------
#' Title Check for functions used in a function
#'
#' @param f the interested function
#'
#' @return functions which are used in the interested function

fun_call <- function(f) {

  leaf <- function (e, w) {
    r <- try(eval(e), silent = TRUE)
    if(!is.null(r) && is.function(r)) ret <<- c(ret, as.character(e))
  }

  call <- function (e, w) {
    codetools::walkCode(e[[1]], w)
    for (a in as.list(e[-1])) if (!missing(a)) walkCode(a, w)
  }

  ret <- c()
  codetools::walkCode(body(f),
                      makeCodeWalker(call = call,
                                     leaf = leaf,
                                     write = cat))
  return(functions = unique(ret))
}



## 1.2 identify possible exercise bouts	 ----------------------------------------
#' Title activpal.process.folder.windows
#'
#' @param Events_Files_To_Process_folder_location
#' @param Temp_Output_folder_location
#'
#' @return current_batched_ids

activpal.process.folder.windows <-
  function(Events_Files_To_Process_folder_location,
           Temp_Output_folder_location) {

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

          stepping_summary$Date <- as.Date(stepping_summary$Date,
                                           format = "%Y/%m/%d")

          #         all_summary[[i]] <- stepping_summary
          current_batched_ids <- c(current_batched_ids, id)

          write.table(stepping_summary, file = paste(Temp_Output_folder_location,
                                                     id, "\\", id,
                                                     "_ex_times_temp.csv",
                                                     sep = ""), sep = ",",
                      row.names = FALSE)
          write.table(current_batched_ids,
                      file = paste(Temp_Output_folder_location,
                                   "Last_Batched_Ids.csv", sep = ""),
                      sep = ",", row.names = FALSE)
        }
      }
    }
    #    return(bind_rows(all_summary))
    return(current_batched_ids)
  }



#' Title activpal.process.folder.macbook
#'
#' @param Events_Files_To_Process_folder_location
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
activpal.process.folder.macbook <-
  function(Events_Files_To_Process_folder_location,
           Temp_Output_folder_location) {

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

    # browser()
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
                                                        72000,
                                                        TRUE)


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

          stepping_summary$Date <- as.Date(stepping_summary$Date,
                                           format = "%Y/%m/%d")

          #         all_summary[[i]] <- stepping_summary
          current_batched_ids <- c(current_batched_ids, id)

          # Tue Mar 19 16:55:36 2024 ------------------------------
          ## in macbook we have to created the folder first
          ## then add the files into the folder
          dir.create(paste(Temp_Output_folder_location,
                           id,
                           sep = ""), showWarnings = FALSE)

          write.csv(stepping_summary,
                      file = paste(Temp_Output_folder_location,
                                   id, "/", id,
                                   "_ex_times_temp.csv",
                                   sep = ""),
                      # sep = ",",
                      row.names = FALSE)
          write.table(current_batched_ids,
                      file = paste(Temp_Output_folder_location,
                                   "Last_Batched_Ids.csv", sep = ""),
                      # sep = ",",
                      row.names = FALSE)
        }
      }
    }
    #    return(bind_rows(all_summary))
    return(current_batched_ids)
  }


## 1.3 parse.file.name -----------------------------------------------------------
#' Title parse.file.name
#'
#' @param file_name
#'
#' @return
#' @export

parse.file.name <-
  function(file_name) {

    device_serial <- ""
    serial_start <- regexpr("AP[[:digit:]]{6,7}",
                            file_name)
    serial_length <- attr(regexpr("AP[[:digit:]]{6,7}",
                                  file_name),
                          "match.length")

    if (serial_start > -1) {
      # Take text to the left of the device serial as the ID
      device_serial <- substr(file_name, 1, (serial_start - 1))
    } else {
      # No device serial within the filename
      # Takes the leftmost portion of the file name on
      # the assumption that it is the ID for the recording
      break_point <- regexpr(" |-", file_name)[1]
      if (break_point == -1) {
        break_point <- 10
      } else {
        break_point <- break_point
      }
      device_serial <- substr(file_name, 1, (break_point - 1))
    }

    uid <- device_serial

    recording_date <- ""
    date_start <- regexpr("[[:digit:]]{1,2}[[:alpha:]]{3}[[:digit:]]{2}",
                          file_name)
    date_length <- attr(regexpr("[[:digit:]]{1,2}[[:alpha:]]{3}[[:digit:]]{2}",
                                file_name), "match.length")

    if (date_start > -1) {
      # device serial within file name
      recording_date <- substr(file_name, date_start,
                               (date_start + date_length - 1))
    }

    if (recording_date != "") {
      uid <- paste(uid, recording_date, sep = "")
    }

    return(uid)
  }



## 1.4 parse.device.serial -------------------------------------------------------
#' Title parse.device.serial
#'
#' @param file_name
#'
#' @return
#' @export
parse.device.serial <-
  function(file_name) {
    start_pos <- regexpr("AP[[:digit:]]{6}", file_name)[1]
    if (start_pos == -1) {
      return(NULL)
    }
    return(substr(file_name, start_pos, start_pos + 7))
  }


## 1.5 load.validation.date ------------------------------------------------------
#' Title load.validation.date
#'
#' @param data
#' @param v
#'
#' @return
#' @export
valid.days <-
  function(data,
           v = 72000) {

    data <- data[, c(1, 3, 4)]
    colnames(data) <- c("time", "interval", "activity")
    data$end_time <- as.numeric(data$time) %% 86400 + data$interval

    span <- which(data$end_time > 86400)
    if (length(span) > 0) {
      to_split_pre <- data[which(data$end_time > 86400), ]
      to_split_post <- data[which(data$end_time > 86400), ]

      to_split_post$interval <- to_split_post$end_time - 86400
      to_split_pre$interval <- to_split_pre$interval - to_split_post$interval
      to_split_post$time <- to_split_post$time + to_split_pre$interval

      data <- data[-span, ]
      data <- rbind(data, to_split_pre)
      data <- rbind(data, to_split_post)
    }
    data$date <- as.Date(data$time)
    valid_days <- data %>%
      group_by(date) %>%
      filter(activity != 4) %>%
      summarise(valid_time = sum(interval)) %>%
      filter(valid_time >= v)
    valid_days <- valid_days$date
    return(valid_days)
  }



## 1.6 sed.start.end.marker ------------------------------------------------------
#' Title sed.start.end.marker
#'
#' @param events_file
#' @param time_value
#' @param event
#' @param duration.min
#' @param duration.max
#' @param interval
#'
#' @return
#' @export
#'

sed.start.end.marker <-
  function(events_file,
           time_value,
           event = 0,
           duration.min = 30,
           duration.max = 600,
           interval = 600) {

    time_value$sed.start.marker <- NA
    time_value$sed.end.marker <- NA
    time_value$sed.start.marker_mins_from_ex <- NA
    time_value$sed.start.marker_mins_duration <- NA
    time_value$sed.end.marker_mins_from_ex <- NA
    time_value$sed.end.marker_mins_duration <- NA

    start_times <- time_value[, 2]
    start.cushion.start <- start_times - (interval)
    start.cushion.end <- start_times + (interval)

    for (i in (1:length(start.cushion.start))) {
      start.temp.events <-
        events_file[which(events_file$Time >= start.cushion.start[i] &
                            events_file$Time <= start.cushion.end[i] &
                            events_file$Event.Type == event &
                            events_file$Duration..s. > duration.min &
                            events_file$Duration..s. < duration.max), ]

      if (dim(start.temp.events)[1] > 0) {
        start.marker <- 1
        diff.start <- as.numeric(abs(difftime(start.temp.events$Time,
                                              start_times[i],
                                              units = "mins")))
        d.s <- length(diff.start)
        min.start <- min(diff.start)
        inds.start <- (1:d.s)[which(diff.start == min.start)]
        start.mins_from_ex <- min.start
        start.mins_duration <- (start.temp.events$Duration..s.[inds.start]) / 60
      } else {
        start.marker <- 0
        start.mins_from_ex <- NA
        start.mins_duration <- NA
      }

      end_times <- time_value$end_time
      end.cushion.start <- end_times - (interval)
      end.cushion.end <- end_times + (interval)

      end.temp.events <-
        events_file[which(events_file$Time >= end.cushion.start[i] &
                            events_file$Time <= end.cushion.end[i] &
                            events_file$Event.Type == event &
                            events_file$Duration..s. > duration.min &
                            events_file$Duration..s. < duration.max), ]

      if (dim(end.temp.events)[1] > 0) {
        end.marker <- 1
        diff.end <- as.numeric(abs(difftime(end.temp.events$Time,
                                            end_times[i], units = "mins")))
        d.e <- length(diff.end)
        min.end <- min(diff.end)
        inds.end <- (1:d.e)[which(diff.end == min.end)]
        end.mins_from_ex <- min.end
        end.mins_duration <- (end.temp.events$Duration..s.[inds.end]) / 60
      } else {
        end.marker <- 0
        end.mins_from_ex <- NA
        end.mins_duration <- NA
      }

      time_value$sed.start.marker[i] <- start.marker
      time_value$sed.end.marker[i] <- end.marker

      time_value$sed.start.marker_mins_from_ex[i] <- start.mins_from_ex
      time_value$sed.start.marker_mins_duration[i] <- start.mins_duration
      time_value$sed.end.marker_mins_from_ex[i] <- end.mins_from_ex
      time_value$sed.end.marker_mins_duration[i] <- end.mins_duration
    }

    return(time_value)
  }


## 1.7 stand.start.end.marker ----------------------------------------------------
#' Title stand.start.end.marker
#'
#' @param events_file
#' @param time_value
#' @param event
#' @param duration.min
#' @param duration.max
#' @param interval
#'
#' @return
#' @export
#'

stand.start.end.marker <-
  function(events_file,
           time_value,
           event = 1,
           duration.min = 30,
           duration.max = 600,
           interval = 600) {

    time_value$stand.start.marker <- NA
    time_value$stand.end.marker <- NA
    time_value$stand.start.marker_mins_from_ex <- NA
    time_value$stand.start.marker_mins_duration <- NA
    time_value$stand.end.marker_mins_from_ex <- NA
    time_value$stand.end.marker_mins_duration <- NA

    start_times <- time_value[, 2]
    start.cushion.start <- start_times - (interval)
    start.cushion.end <- start_times + (interval)
    for (i in (1:length(start.cushion.start))) {
      start.temp.events <-
        events_file[which(events_file$Time >= start.cushion.start[i] &
                            events_file$Time <= start.cushion.end[i] &
                            events_file$Event.Type == event &
                            events_file$Duration..s. > duration.min &
                            events_file$Duration..s. < duration.max), ]
      if (dim(start.temp.events)[1] > 0) {
        start.marker <- 1
        diff.start <- as.numeric(abs(difftime(start.temp.events$Time,
                                              start_times[i], units = "mins")))
        d.s <- length(diff.start)
        min.start <- min(diff.start)
        inds.start <- (1:d.s)[which(diff.start == min.start)]
        start.mins_from_ex <- min.start
        start.mins_duration <- (start.temp.events$Duration..s.[inds.start]) / 60
      } else {
        start.marker <- 0
        start.mins_from_ex <- NA
        start.mins_duration <- NA
      }

      end_times <- time_value$end_time
      end.cushion.start <- end_times - (interval)
      end.cushion.end <- end_times + (interval)

      end.temp.events <-
        events_file[which(events_file$Time >= end.cushion.start[i] &
                            events_file$Time <= end.cushion.end[i] &
                            events_file$Event.Type == event &
                            events_file$Duration..s. > duration.min &
                            events_file$Duration..s. < duration.max), ]

      if (dim(end.temp.events)[1] > 0) {
        end.marker <- 1
        diff.end <- as.numeric(abs(difftime(end.temp.events$Time,
                                            end_times[i],
                                            units = "mins")))
        d.e <- length(diff.end)
        min.end <- min(diff.end)
        inds.end <- (1:d.e)[which(diff.end == min.end)]
        end.mins_from_ex <- min.end
        end.mins_duration <- (end.temp.events$Duration..s.[inds.end]) / 60
      } else {
        end.marker <- 0
        end.mins_from_ex <- NA
        end.mins_duration <- NA
      }

      time_value$stand.start.marker[i] <- start.marker
      time_value$stand.end.marker[i] <- end.marker

      time_value$stand.start.marker_mins_from_ex[i] <- start.mins_from_ex
      time_value$stand.start.marker_mins_duration[i] <- start.mins_duration
      time_value$stand.end.marker_mins_from_ex[i] <- end.mins_from_ex
      time_value$stand.end.marker_mins_duration[i] <- end.mins_duration
    }

    return(time_value)
  }


## 1.8 sed.activity.marker -------------------------------------------------------
#' Title sed.activity.marker
#'
#' @param events_file
#' @param time_value
#' @param duration
#'
#' @return
#' @export
#'

sed.activity.marker <-
  function(events_file, time_value, duration) {
    time_value$sed.event <- NA
    te <- dim(time_value)[1]

    for (i in (1:te)) {
      sed.temp.events <-
        events_file[which(events_file$Time >= time_value$Time[i] &
                            events_file$Time <= time_value$end_time[i] &
                            events_file$Event.Type == 0 &
                            events_file$Duration..s. >= duration), ]
      if (dim(sed.temp.events)[1] > 0) {
        sed.event <- 1
      } else {
        sed.event <- 0
      }

      time_value$sed.event[i] <- sed.event
    }

    return(time_value)
  }

## 1.9 transportation.activity.marker --------------------------------------------
#' Title transportation.activity.marker
#'
#' @param events_file
#' @param time_value
#' @param duration
#'
#' @return
#' @export
#'

transportation.activity.marker <-
  function(events_file, time_value, duration) {
    time_value$transportation.event <- NA
    te <- dim(time_value)[1]

    for (i in (1:te)) {
      transportation.temp.events <- events_file[which(events_file$Time >= time_value$Time[i] & events_file$Time <= time_value$end_time[i] & events_file$Event.Type == 5 & events_file$Duration..s. >= duration), ]
      if (dim(transportation.temp.events)[1] > 0) {
        transportation.event <- 1
      } else {
        transportation.event <- 0
      }

      time_value$transportation.event[i] <- transportation.event
    }

    return(time_value)
  }


## 1.10 exercise.log -------------------------------------------
#' Title exercise.log
#'
#' @param time_value
#' @param folder_location
#' @param id
#'
#' @return
#' @export
#'

exercise.log <-
  function(time_value, folder_location, id) {
    time_value$exercise.log <- NA

    exercise.log <- read.csv(paste(folder_location, "exercise_log.csv", sep = ""), row.names = NULL, sep = ",", stringsAsFactors = FALSE)
    names(exercise.log) <- c("id", "date", "exercise")

    temp.log <- exercise.log[which(exercise.log$id == id), ]

    tl <- dim(temp.log)[1]

    if (tl > 0) {
      temp.log$date <- as.POSIXct(temp.log$date, format = "%m/%d/%y", tz = "UTC")
      time_value$date <- as.POSIXct(time_value$date)

      tv <- dim(time_value)[1]
      for (i in (1:tv)) {
        temp <- temp.log[which(temp.log$date == time_value$date[i]), ]
        if (dim(temp)[1] > 0) {
          exercise <- temp$exercise
        } else {
          exercise <- NA
        }

        time_value$exercise.log[i] <- exercise
      }
    }


    return(time_value)
  }

# function to prepare ex times file for chart generation -------------------------


## 1.11 ex.times ---------------------------------------------------------------
#' Title prepare.ex.times.windows
#'
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
prepare.ex.times.windows <- function(Temp_Output_folder_location) {

  last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                     "Last_Batched_Ids.csv",
                                     sep = ""),
                               ",",
                               header = TRUE)

  last.batched.ids <- as.character(last.batched.ids$x)

  lbi <- length(last.batched.ids)

  for (l in last.batched.ids) {
    temp_ex_times <- read.csv(paste(Temp_Output_folder_location,
                                    l, "\\", l,
                                    "_ex_times_temp.csv",
                                    sep = ""))

    id <- as.character(l)
    head(temp_ex_times)
    temp.overlay <- temp_ex_times[, (2:3)]
    head(temp.overlay)
    names(temp.overlay) <- c("start_time", "end_time")
    temp.overlay$category <- "exercise"
    month <- substr(temp.overlay$start_time, 6, 7)
    day <- substr(temp.overlay$start_time, 9, 10)
    year <- substr(temp.overlay$start_time, 1, 4)
    hours <- substr(temp.overlay$start_time, 12, 13)
    minutes <- substr(temp.overlay$start_time, 15, 16)

    temp.overlay$start_time <- paste(day, "/",
                                     month, "/",
                                     year, " ",
                                     hours, ":",
                                     minutes,
                                     sep = "")

    end.month <- substr(temp.overlay$end_time, 6, 7)
    end.day <- substr(temp.overlay$end_time, 9, 10)
    end.year <- substr(temp.overlay$end_time, 1, 4)
    end.hours <- substr(temp.overlay$end_time, 12, 13)
    end.minutes <- substr(temp.overlay$end_time, 15, 16)

    temp.overlay$end_time <- paste(end.day, "/",
                                   end.month, "/", end.year,
                                   " ", end.hours, ":",
                                   end.minutes, sep = "")
    # 		temp.overlay$start_time <- parse_date_time(temp.overlay$start_time,
    # 		                                           orders="ymd HMS")
    # 		temp.overlay$end_time <-  parse_date_time(temp.overlay$end_time,
    # 		                                          orders="ymd HMS")

    write.table(temp.overlay,
                file = paste(Temp_Output_folder_location,
                             l, "\\", l,
                             "_overlay_times_temp.csv",
                             sep = ""),
                sep = ",",
                row.names = FALSE)
  }
}


#' Title prepare.ex.times.macbook
#'
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
prepare.ex.times.macbook <- function(Temp_Output_folder_location) {

  last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                     "Last_Batched_Ids.csv",
                                     sep = ""),
                               ",",
                               header = TRUE)

  last.batched.ids <- as.character(last.batched.ids$x)

  lbi <- length(last.batched.ids)

  for (l in last.batched.ids) {
    temp_ex_times <- read.csv(paste(Temp_Output_folder_location,
                                    l, "/", l,
                                    "_ex_times_temp.csv",
                                    sep = ""))

    id <- as.character(l)
    head(temp_ex_times)
    temp.overlay <- temp_ex_times[, (2:3)]
    head(temp.overlay)
    names(temp.overlay) <- c("start_time", "end_time")
    temp.overlay$category <- "exercise"
    month <- substr(temp.overlay$start_time, 6, 7)
    day <- substr(temp.overlay$start_time, 9, 10)
    year <- substr(temp.overlay$start_time, 1, 4)
    hours <- substr(temp.overlay$start_time, 12, 13)
    minutes <- substr(temp.overlay$start_time, 15, 16)

    temp.overlay$start_time <- paste(day, "/",
                                     month, "/",
                                     year, " ",
                                     hours, ":",
                                     minutes,
                                     sep = "")

    end.month <- substr(temp.overlay$end_time, 6, 7)
    end.day <- substr(temp.overlay$end_time, 9, 10)
    end.year <- substr(temp.overlay$end_time, 1, 4)
    end.hours <- substr(temp.overlay$end_time, 12, 13)
    end.minutes <- substr(temp.overlay$end_time, 15, 16)

    temp.overlay$end_time <- paste(end.day, "/",
                                   end.month, "/", end.year,
                                   " ", end.hours, ":",
                                   end.minutes, sep = "")
    # 		temp.overlay$start_time <- parse_date_time(temp.overlay$start_time,
    # 		                                           orders="ymd HMS")
    # 		temp.overlay$end_time <-  parse_date_time(temp.overlay$end_time,
    # 		                                          orders="ymd HMS")

    write.table(temp.overlay,
                file = paste(Temp_Output_folder_location,
                             l, "/", l,
                             "_overlay_times_temp.csv",
                             sep = ""),
                sep = ",",
                row.names = FALSE)
  }
}


## 1.12 make.index.file ---------------------------------------------------------
#' Title make.index.file.windows
#'
#' @param Events_Files_To_Process_folder_location
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
make.index.file.windows <-
  function(Events_Files_To_Process_folder_location,
           Temp_Output_folder_location) {

    list.events <- list.files(Events_Files_To_Process_folder_location,
                              pattern = "Events.csv")

    last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                       "Last_Batched_Ids.csv",
                                       sep = ""), ",",
                                 header = TRUE)

    last.batched.ids <- as.character(last.batched.ids$x)

    index_file <- data.frame(file_id = NA,
                             events_file = NA,
                             overlay_file = NA)

    index_file <- index_file[-1, ]

    for (l in last.batched.ids) {
      file_id <- l

      events_file <- list.events[grep(file_id, list.events)]

      events_file <- paste(Events_Files_To_Process_folder_location,
                           events_file, sep = "")

      overlay_file <- paste(Temp_Output_folder_location,
                            file_id, "\\", file_id,
                            "_overlay_times_temp.csv",
                            sep = "")

      temp_file <- data.frame(file_id = file_id,
                              events_file = events_file,
                              overlay_file = overlay_file)

      index_file <- rbind(index_file, temp_file)

      write.table(temp_file,
                  file = paste(Temp_Output_folder_location,
                               file_id, "\\",
                               file_id,
                               "_Index_File.csv",
                               sep = ""),
                  sep = ",",
                  row.names = FALSE)
    }

    # write.table(index_file,
    #             file = paste(Temp_Output_folder_location,
    #                          "Last_Index_File.csv",
    #                          sep=""),
    #             sep=",",
    #             row.names=FALSE)
  }


#' Title make.index.file.macbook
#'
#' @param Events_Files_To_Process_folder_location
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
make.index.file.macbook <-
  function(Events_Files_To_Process_folder_location,
           Temp_Output_folder_location) {

    list.events <- list.files(Events_Files_To_Process_folder_location,
                              pattern = "Events.csv")

    last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                       "Last_Batched_Ids.csv",
                                       sep = ""), ",",
                                 header = TRUE)

    last.batched.ids <- as.character(last.batched.ids$x)

    index_file <- data.frame(file_id = NA,
                             events_file = NA,
                             overlay_file = NA)

    index_file <- index_file[-1, ]

    for (l in last.batched.ids) {
      file_id <- l

      events_file <- list.events[grep(file_id, list.events)]

      events_file <- paste(Events_Files_To_Process_folder_location,
                           events_file, sep = "")

      overlay_file <- paste(Temp_Output_folder_location,
                            file_id, "/", file_id,
                            "_overlay_times_temp.csv",
                            sep = "")

      temp_file <- data.frame(file_id = file_id,
                              events_file = events_file,
                              overlay_file = overlay_file)

      index_file <- rbind(index_file, temp_file)

      write.table(temp_file,
                  file = paste(Temp_Output_folder_location,
                               file_id, "/",
                               file_id,
                               "_Index_File.csv",
                               sep = ""),
                  sep = ",",
                  row.names = FALSE)
    }

    # write.table(index_file,
    #             file = paste(Temp_Output_folder_location,
    #                          "Last_Index_File.csv",
    #                          sep=""),
    #             sep=",",
    #             row.names=FALSE)
  }

## 1.13 individual.chart.overlay -------------------------------------------------
#' Title Individual Chart Overlay
#'
#' @param Temp_Output_folder_location
#'
#' @return

individual.chart.overlay.macbook <-
  function(Temp_Output_folder_location) {
    last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                       "Last_Batched_Ids.csv",
                                       sep = ""), ",",
                                 header = TRUE)

    last.batched.ids <- as.vector(last.batched.ids$x)

    for (l in last.batched.ids) {
      file_id <- l
      activity.with.overlay.chart.folder_drive(paste(Temp_Output_folder_location,
                                                     file_id, "/", file_id,
                                                     "_Index_File.csv",
                                                     sep = ""),
                                               paste(Temp_Output_folder_location,
                                                     file_id, "/",
                                                     sep = ""))
    }
  }

#' Title individual.chart.overlay.windows
#'
#' @param Temp_Output_folder_location
#'
#' @return
#' @export
#'

individual.chart.overlay.windows <-
  function(Temp_Output_folder_location) {
    last.batched.ids <- read.csv(paste(Temp_Output_folder_location,
                                       "Last_Batched_Ids.csv",
                                       sep = ""), ",",
                                 header = TRUE)

    last.batched.ids <- as.vector(last.batched.ids$x)

    for (l in last.batched.ids) {
      file_id <- l
      activity.with.overlay.chart.folder_drive(paste(Temp_Output_folder_location,
                                                     file_id, "\\", file_id,
                                                     "_Index_File.csv",
                                                     sep = ""),
                                               paste(Temp_Output_folder_location,
                                                     file_id, "\\",
                                                     sep = ""))
    }
  }


## 1.14 apSummary ---------------------------------------------------------------

#' Title apSummary.windows
#'
#' @param Events_Files_To_Process_folder_location
#' @param Confirmed_Output_folder_location
#'
#' @return
#' @export
#'
#' @examples
apSummary.windows <- function(Events_Files_To_Process_folder_location,
                              Confirmed_Output_folder_location) {
  file_names <- list.files(Events_Files_To_Process_folder_location,
                           pattern = "*Events.csv",
                           recursive = TRUE)

  for (i in file_names) {
    id <- unlist(strsplit(i, "-"))[1]

    print(id)

    ex.times.list <- list.files(Confirmed_Output_folder_location,
                                pattern = "*_ex_times_confirmed.csv",
                                recursive = TRUE)
    ex.times.path <- ex.times.list[grep(id, ex.times.list)]

    if (length(ex.times.path) == 0) {
      print(id)
      print("No Exercise Times")
    }

    if (length(ex.times.path) > 0) {
      events_file <- read.csv(paste(Events_Files_To_Process_folder_location,
                                    i, sep = ""),
                              row.names = NULL,
                              sep = ",",
                              stringsAsFactors = FALSE)

      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400,
                                     origin = "1899-12-30",
                                     tz = "UTC")
      events_file$Date <- as.Date(events_file$Time)

      events_file$date <- as.Date(events_file$Time)

      events_file$exercise <- 0

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

      ex.times <- read.csv(paste(Confirmed_Output_folder_location, id, "\\",
                                 id, "_ex_times_confirmed.csv", sep = ""))

      ex.times$Ex_Start <- strptime(ex.times$Ex_Start, format = "%m/%d/%Y %H:%M", tz = "UTC")
      ex.times$Ex_End <- strptime(ex.times$Ex_End, format = "%m/%d/%Y %H:%M", tz = "UTC")

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
        temp.day.ex <- temp.day[which(temp.day$exercise == 1), ]
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

        temp.summary <- data.frame("Participant ID" = id, "Day" = date.counter, "Date Worn" = unique(temp.day$date), "Total Steps" = tot.steps, "Exercise Steps" = ex.steps, "Steps w/o Exercise" = non.ex.num.step, "Sleep (min)" = sleep.min, "Wake (min)" = wake.min, "Time Standing (min)" = stand.min, "Time Walking (min)" = tot.step.min, "Time Sedentary (min)" = sed.min, "Total Time (min)" = tot.min, "Total Other (min)" = tot.cycling.min, check.names = FALSE)

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

#' Title apSummary.macbook
#'
#' @param Events_Files_To_Process_folder_location
#' @param Confirmed_Output_folder_location
#'
#' @return
#' @export
#'
#' @examples
apSummary.macbook <- function(Events_Files_To_Process_folder_location,
                              Confirmed_Output_folder_location) {
  file_names <- list.files(Events_Files_To_Process_folder_location,
                           pattern = "*Events.csv",
                           recursive = TRUE)

  for (i in file_names) {
    id <- unlist(strsplit(i, "-"))[1]

    print(id)

    ex.times.list <- list.files(Confirmed_Output_folder_location,
                                pattern = "*_ex_times_confirmed.csv",
                                recursive = TRUE)
    ex.times.path <- ex.times.list[grep(id, ex.times.list)]

    if (length(ex.times.path) == 0) {
      print(id)
      print("No Exercise Times")
    }

    if (length(ex.times.path) > 0) {
      events_file <- read.csv(paste(Events_Files_To_Process_folder_location,
                                    i, sep = ""),
                              row.names = NULL,
                              sep = ",",
                              stringsAsFactors = FALSE)

      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400,
                                     origin = "1899-12-30",
                                     tz = "UTC")
      events_file$Date <- as.Date(events_file$Time)

      events_file$date <- as.Date(events_file$Time)

      events_file$exercise <- 0

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

      ex.times <- read.csv(paste(Confirmed_Output_folder_location,
                                 id, "/",
                                 id, "_ex_times_confirmed.csv",
                                 sep = ""))

      ex.times$Ex_Start <- strptime(ex.times$Ex_Start, format = "%m/%d/%Y %H:%M", tz = "UTC")
      ex.times$Ex_End <- strptime(ex.times$Ex_End, format = "%m/%d/%Y %H:%M", tz = "UTC")

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
        temp.day.ex <- temp.day[which(temp.day$exercise == 1), ]
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

        temp.summary <- data.frame("Participant ID" = id, "Day" = date.counter, "Date Worn" = unique(temp.day$date), "Total Steps" = tot.steps, "Exercise Steps" = ex.steps, "Steps w/o Exercise" = non.ex.num.step, "Sleep (min)" = sleep.min, "Wake (min)" = wake.min, "Time Standing (min)" = stand.min, "Time Walking (min)" = tot.step.min, "Time Sedentary (min)" = sed.min, "Total Time (min)" = tot.min, "Total Other (min)" = tot.cycling.min, check.names = FALSE)

        if (date.counter == 1) {
          summary <- temp.summary
        }
        if (date.counter > 1) {
          summary <- rbind(summary, temp.summary)
        }

        date.counter <- date.counter + 1
      }
      write.table(summary, file = paste(Confirmed_Output_folder_location,
                                        # Wed Mar 20 10:29:24 2024 -------------
                                        ## change the separator to / for mac----
                                        id, "/", id,
                                        "_summary_confirmed.csv",
                                        sep = ""),
                  row.names = FALSE,
                  sep = ",")
    }
  }
}
