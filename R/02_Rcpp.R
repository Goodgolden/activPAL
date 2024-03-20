## package Rcpp ---------------------------------------------------------------


## 2.1 activpal.stepping.process.file ------------------------------------------------
activpal.stepping.process.file <-
  function(events_file, window_size = 2700,
           lower_bout_size = 0.1,
           max_bout_size = 86400,
           wear_time_minimum = 72000,
           daily_summary = TRUE) {

    if (nrow(events_file) > 0) {
      # valid_days <- events_file[,c(1,4,5)]
      # valid_days$date <- as.Date(valid_days$Time)
      # valid_days$diff <- as.numeric(valid_days$Time) %% 86400 +
      #                     valid_days$Duration..s.
      # split <- valid_days[which(valid_days$diff > 86400),]
      #
      # if(nrow(split) > 0){
      #   after_split <- split
      #   after_split$date <- after_split$date + 1
      #   after_split$Time <- as.POSIXct(after_split$date)
      #   after_split$Duration..s. <- round(after_split$diff - 86400,1)
      #   split$Duration..s. <- split$Duration..s. -
      #                         round(after_split$diff - 86400,1)
      #   split$diff <- 0
      #   after_split$diff <- 0
      #   valid_days <- bind_rows(valid_days,split,after_split)
      #   valid_days <- valid_days[-which(valid_days$diff > 86400),]
      # }
      #
      # valid_days <- valid_days %>%
      #   group_by(date) %>%
      #   filter(Event.Type != 4) %>%
      #   summarise(valid_time = sum(Duration..s.))
      # valid_days <- valid_days %>% filter(valid_time >= wear_time_minimum)
      #
      # events_file <-
      #       events_file[which(as.Date(events_file$Time) %in% valid_days$date),]

      colnames(events_file)[1:4] <- c("Time", "counts", "Duration..s.", "Event.Type")

      events_file <- activpal.remove.longer.bouts(events_file,
                                                  lower_bout_size,
                                                  max_bout_size)
      file.stepping.summary <- activpal.stepping.test.file(events_file,
                                                           window_size)
      file.stepping.summary <- file.stepping.summary %>% filter(steps > 0)


      if (nrow(file.stepping.summary) > 0) {
        if (daily_summary) {
          file.stepping.summary <- file.stepping.summary %>%
            group_by(date) %>%
            filter(steps == max(steps)) %>%
            filter(duration == min(duration)) %>%
            filter(Time == min(Time))
        } else {
          file.stepping.summary <- file.stepping.summary %>%
            filter(steps == max(steps)) %>%
            filter(duration == min(duration)) %>%
            filter(Time == min(Time))
        }
      }
    }

    end_time <- file.stepping.summary$Time + window_size
    file.stepping.summary <- data.frame(file.stepping.summary[, c(1:2)],
                                        end_time = end_time,
                                        file.stepping.summary[, c(3:5)])

    # names(file.stepping.summary) <- c("Date","Ex_Start",
    #                                   "Ex_End","Num_Step",
    #                                   "Step_Minutes","Cadence")
    # file.stepping.summary$Step_Minutes <- file.stepping.summary$Step_Minutes/60

    return(file.stepping.summary)
  }


## 2.2 activpal.remove.longer.bouts ------------------------------------------------
activpal.remove.longer.bouts <-
  function(file_data, lower_bout_length, upper_bout_length) {
    rownames(file_data) <- 1:nrow(file_data)

    # file_data[which(file_data$Event.Type == 2 & file_data$Duration..s. > 4),]$Event.Type <-
    # rep(1,length(which(file_data$Event.Type == 2 & file_data$Duration..s. > 4)))

    one <- c(-1, file_data$Event.Type)
    two <- c(file_data$Event.Type, -1)

    stepping_start <- which(one != 2 & two == 2)
    stepping_end <- which(one == 2 & two != 2) - 1

    file_data$group <- 0
    group_id <- 1:length(stepping_start)
    group_val <- rep(0, nrow(file_data))

    for (i in (1:length(stepping_start))) {
      # Tag each bout of stepping
      group_val[(stepping_start[i]:stepping_end[i])] <- i
    }

    file_data$group <- group_val
    bouts_to_exclude <- file_data %>%
      filter(group > 0) %>%
      group_by(group) %>%
      summarise(time = sum(Duration..s.)) %>%
      filter(time > upper_bout_length | time < lower_bout_length)

    if (nrow(bouts_to_exclude) > 0) {
      file_data[which(file_data$group %in% bouts_to_exclude$group), ]$Event.Type <- 1
    }

    file_data <- file_data[, -ncol(file_data)]
    return(file_data)
  }


## 2.3 activpal.stepping.test.file ------------------------------------------------
##
activpal.stepping.test.file <-
  function(file.data, window.size) {
    library(Rcpp)
    # extract the activpal code from the filename.
    # Assumes the filename is stored in the format AP

    # process the event files using the list of valid dates
    # Split events that cross two days
    file.data <- file.data[order(file.data$Time), ]
    rownames(file.data) <- 1:nrow(file.data)
    file.data$date <- as.Date(file.data$Time)
    file.data$diff <- (difftime(file.data$Time, file.data$date, tz = "UTC", units = "secs") + file.data$Duration..s.) - 86400
    cross.days <- which(file.data$diff > 0)
    file.data <- rbind(file.data, file.data[cross.days, ])
    file.data[cross.days, ]$Duration..s. <- round(file.data[cross.days, ]$Duration..s. - file.data[cross.days, ]$diff, 1)
    file.data[(nrow(file.data) - length(cross.days) + 1):nrow(file.data), ]$Duration..s. <- round(file.data[(nrow(file.data) - length(cross.days) + 1):nrow(file.data), ]$diff, 1)
    file.data[(nrow(file.data) - length(cross.days) + 1):nrow(file.data), ]$Time <- file.data[(nrow(file.data) - length(cross.days) + 1):nrow(file.data), ]$date + 1
    file.data <- file.data[order(file.data$Time), ]
    rownames(file.data) <- 1:nrow(file.data)
    file.data$date <- as.Date(file.data$Time)

    # Format the events file
    file.data$seq <- (1:nrow(file.data))
    colnames(file.data)[3:4] <- c("interval", "activity")
    file.data <- file.data[, c(10, 12, 1, 3, 4)]

    ans <- bout_end(file.data$seq, file.data$activity, file.data$interval, nrow(file.data), window.size)

    stepping.summary <- file.data[, c(1, 3)]
    stepping.summary$steps <- as.integer(ans[(nrow(file.data) + 1):(nrow(file.data) * 2)])
    stepping.summary$duration <- ans[((2 * nrow(file.data)) + 1):(nrow(file.data) * 3)]
    stepping.summary$cadence <- stepping.summary$steps / (stepping.summary$duration / 60)
    return(stepping.summary)
  }

## 2.4 activpal.stepping.test.day  ------------------------------------------------

activpal.stepping.test.day <-
  function(file.data, window.size) {
    # library(Rcpp)
    file.data$seconds <- round(as.numeric(difftime(file.data$Time, file.data$date, units = "secs")), 1)
    stepping <- which(file.data$activity == 2)
    stepping.time <- file.data[stepping, ]$seconds
    file.seconds <- file.data$seconds

    ans <- bout_end(file.data$seq, file.data$activity, file.data$interval, nrow(file.data), window.size)

    stepping.summary <- file.data[, c(1, 3)]
    stepping.summary$steps <- as.integer(ans[(nrow(file.data) + 1):(nrow(file.data) * 2)])
    stepping.summary$duration <- ans[((2 * nrow(file.data)) + 1):(nrow(file.data) * 3)]
    stepping.summary$cadence <- stepping.summary$steps / (stepping.summary$duration / 60)
    return(stepping.summary)
  }


## 2.5 activpal.calculate.cadence.change --------------------------------------------
activpal.calculate.cadence.change <-
  function(data) {
    data <- data[which(data$window_size %% 10 == 0), ]
    data <- data[order(data$uid, data$window_size), ]
    data$cadence_change <- c(0, -diff(data$cadence))
    data[which(data$window_size == 10), ]$cadence_change <- 0
    return(data)
  }


## 2.6 activpal.compare.days -----------------------------------------------------
activpal.compare.days <-
  function(data) {
    library(dplyr)

    last_value <- data %>%
      filter(window_size == 600) %>%
      select(uid, steps, duration) %>%
      mutate(level_off = TRUE)
    data <- left_join(data, last_value)
    data <- data[-which(data$level_off & data$window_size != 600), ]
    uid_that_level_off <- data %>%
      group_by(uid) %>%
      summarise(values = n()) %>%
      filter(values != 164)
    data <- data[-which(data$uid %in% uid_that_level_off$uid & data$window_size == 600), ]

    compare_data <- data[which(data$window_size == 30), ]
    compare_data <- compare_data %>%
      group_by(id) %>%
      summarise(events = n()) %>%
      filter(events == 2)

    matched_entries <- data %>%
      group_by(id, window_size) %>%
      summarise(entries = n())
    data <- inner_join(data, matched_entries)
    data <- data[which(data$entries == 2), ]

    data <- data[which(data$id %in% compare_data$id), ]
    data <- data[which(data$window_size %% 5 == 0 & data$window_size > 5), ]
    data <- data %>% arrange(id, window_size, file_date)

    data$steps_diff <- c(0, diff(data$steps))
    data$cadence_diff <- c(0, diff(data$cadence))
    data <- data[seq(2, nrow(data), 2), ]
    data$steps_2 <- data$steps - data$steps_diff
    data$cadence_2 <- data$cadence - data$cadence_diff
    data$duration_2 <- round(data$steps_2 / (data$cadence_2 / 60), 2)
    data <- data[, c(8, 6, 3, 4, 5, 14, 15, 16, 12, 13)]
    data$steps_diff <- abs(data$steps_diff)
    data$cadence_diff <- abs(data$cadence_diff)

    return(data)
  }


## 2.7 split.days ---------------------------------------------------------------
split.days <-
  function(data, v = 72000) {
    library(dplyr)

    #    data <- data[,c(1,3,4)]
    #    colnames(data) <- c("time","interval","activity")
    data$end_time <- as.numeric(data$Time) %% 86400 + data$Interval..s.

    span <- which(data$end_time > 86400)
    if (length(span) > 0) {
      to_split_pre <- data[which(data$end_time > 86400), ]
      to_split_post <- data[which(data$end_time > 86400), ]

      to_split_post$Interval..s. <- to_split_post$end_time - 86400
      to_split_pre$Interval..s. <- to_split_pre$Interval..s. - to_split_post$Interval..s.
      to_split_post$Time <- to_split_post$Time + to_split_pre$Interval..s.

      data <- data[-span, ]
      data <- rbind(data, to_split_pre)
      data <- rbind(data, to_split_post)
    }
    return(data)
  }

## 2.8 apSummary ---------------------------------------------------------------

apSummary <- function(Events_Files_To_Process_folder_location, Confirmed_Output_folder_location) {
  file_names <- list.files(Events_Files_To_Process_folder_location, pattern = "*Events.csv", recursive = TRUE)

  for (i in file_names) {
    id <- unlist(strsplit(i, "-"))[1]

    print(id)

    ex.times.list <- list.files(Confirmed_Output_folder_location, pattern = "*_ex_times_confirmed.csv", recursive = TRUE)
    ex.times.path <- ex.times.list[grep(id, ex.times.list)]

    if (length(ex.times.path) == 0) {
      print(id)
      print("No Exercise Times")
    }

    if (length(ex.times.path) > 0) {
      events_file <- read.csv(paste(Events_Files_To_Process_folder_location, i, sep = ""), row.names = NULL, sep = ",", stringsAsFactors = FALSE)

      events_file$Time <- as.POSIXct(as.numeric(events_file$Time) * 86400, origin = "1899-12-30", tz = "UTC")
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
      events_file$diff <- (difftime(events_file$Time, events_file$date, tz = "UTC", units = "secs") + events_file$Duration..s.) - 86400
      cross.days <- which(events_file$diff > 0)
      events_file <- rbind(events_file, events_file[cross.days, ])
      events_file[cross.days, ]$Duration..s. <- round(events_file[cross.days, ]$Duration..s. - events_file[cross.days, ]$diff, 1)
      events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$Duration..s. <- round(events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$diff, 1)
      events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$Time <- events_file[(nrow(events_file) - length(cross.days) + 1):nrow(events_file), ]$date + 1
      events_file <- events_file[order(events_file$Time), ]
      rownames(events_file) <- 1:nrow(events_file)
      events_file$date <- as.Date(events_file$Time)

      ### 	a valid day is set for 20hrs of wear	###

      valid_day_list <- valid.days(events_file)
      events_file <- events_file[which(events_file$date %in% valid_day_list), ]


      #  	id <- unlist(strsplit(i,"-"))[1]

      ex.times <- read.csv(paste(Confirmed_Output_folder_location, id, "\\", id, "_ex_times_confirmed.csv", sep = ""))

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
        temp.num.steps <- events_file$CumulativeStepCount[f + 1] - events_file$CumulativeStepCount[f]
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
      write.table(summary, file = paste(Confirmed_Output_folder_location, id, "\\", id, "_summary_confirmed.csv", sep = ""), row.names = FALSE, sep = ",")
    }
  }
}


##	functions for overlay charts ------------------------------------------------

##	2.9 activity.with.overlay.chart.folder_drive --------------------------------
#' Combines events file data with observational data across multiple events files
#' to produce and save charts
#' @description Reads in a csv file containing a list of events file locations and
#'     overlay file locations, generating and saving spiral and linear charts for
#'     each events file aligning the events file data with the corresponding overlay
#'     data.
#'     Where the overlay data is non-continuous data (such as sleeps diaries) the
#'     csv file should have the columns start_time (dd-mm-YYYY HH:MM),
#'     end_time (dd-mm-YYYY HH:MM) and category (text).
#'     Where the overlay data is continuous data (such as continuous glucose monitoring)
#'     the csv file should have the columns start_time (dd-mm-YYYY HH:MM) and
#'     category (text).  In this case the end time coincides with the start time of
#'     the subsequent observation.
#'     The charts are saved as png images with two images generated for each events
#'     file / overlay file pair (one spiral chart and one linear chart). \cr
#'     \strong{Note}: Spiral plots are generated using ggplot2 and coord_polar() and can take
#'     45 - 60 seconds to generate each spiral plot.
#' @param index_file_location The filepath for the csv file with the following column names
#'     file_id, events_file, overlay_file
#' @param output_folder The filepath of the folder where the generated chart are to be saved to
#' @export

activity.with.overlay.chart.folder_drive <-
  function(index_file_location, output_folder) {

    # browser()
    if (!valid.file.path(index_file_location)) {
      stop("A valid file name for index_file_location has not been provided.")
    }
    if (!valid.folder.path(output_folder)) {
      stop("A valid folder to save the generated output has not been provided.")
    }

    index_file <- read.csv(paste(index_file_location, sep = ""))

    for (i in (1:nrow(index_file))) {
      overlay_file_folder <- substr(
        as.character(index_file[i, ]$overlay_file), 1,
        gregexpr("\\\\", index_file[i, ]$overlay_file)[[1]][length(gregexpr("\\\\", index_file[i, ]$overlay_file)[[1]])]
      )
      overlay_file_name <- substr(
        as.character(index_file[i, ]$overlay_file),
        gregexpr("\\\\", index_file[i, ]$overlay_file)[[1]][length(gregexpr("\\\\", index_file[i, ]$overlay_file)[[1]])] + 1,
        nchar(as.character(index_file[i, ]$overlay_file))
      )

      activity.with.overlay.chart_drive(as.character(index_file[i, ]$events_file),
                                        as.character(index_file[i, ]$overlay_file),
                                        output_folder)
    }
  }



## 2.10 activity.with.overlay.chart_drive ----------------------------------------

#' Combines events file data with observational data for a single events file
#' @description Reads in an events file and overlay file, generating spiral and linear
#'     charts aligning the events file data with the corresponding overlay data.
#'     Where the overlay data is non-continuous data (such as sleeps diaries) the csv
#'     file should have the columns start_time (dd-mm-YYYY HH:MM),
#'     end_time (dd-mm-YYYY HH:MM) and category (text).
#'     Where the overlay data is continuous data (such as continuous glucose monitoring)
#'     the csv file should have the columns start_time(dd-mm-YYYY HH:MM) and category (text).
#'     In this case the end time coincides with the start time of the subsequent observation.
#'     The charts are saved as png images with two images generated
#'     (one spiral chart and one linear chart). \cr
#'     \strong{Note}: Spiral plots are generated using ggplot2 and coord_polar() and can take
#'     45 - 60 seconds to generate each spiral plot.
#' @param events_file The filepath of the events file. Must be a valid activPAL events csv file
#' @param overlay_file The filepath of the csv file containing the overlay data csv file
#' @param output_folder The filepath of the folder where the generated chart are to be saved to

#' @export
#' @examples events_file <- system.file("extdata", "Test_Events.csv", package = "activPAL")
#' sleep_file <- system.file("extdata", "Sleep.csv", package = "activPAL")
#' output_folder <- paste(tempdir(),"/",sep="")
#'
#' \donttest{activPAL::activity.with.overlay.chart(events_file,sleep_file,output_folder)}

activity.with.overlay.chart_drive <-
  function(events_file, overlay_file, output_folder) {
    # browser()

    if (!valid.file.path(events_file)) {
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if (!valid.file.path(overlay_file)) {
      stop("The location of the required input file has not been provided / does not exist.")
    }
    if (!valid.folder.path(output_folder)) {
      stop("A valid folder to save the generated output has not been provided.")
    }

    events_file_folder <- substr(
      events_file, 1,
      gregexpr("\\\\", events_file)[[1]][length(gregexpr("\\\\", events_file)[[1]])])

    events_file_name <-
      substr(events_file,
             gregexpr("\\\\",
                      events_file)[[1]][length(gregexpr("\\\\",
                                                        events_file)[[1]])] + 1,
             nchar(events_file))

    pre.process.events.file <-
      function(file_name,folder = "", minimum_valid_wear = 20){
        events_file <- load.events.file(folder,file_name)
        events_file <- activpal.file.process(events_file,
                                             wear.time.minimum = minimum_valid_wear * 3600)
        return(events_file)
      }

    activpal.convert.events.extended.file <-
      function(data){
        if(length(which(colnames(data) == "Time.approx.")) >= 1){
          if(rownames(data)[1] != 1){
            data$time <- as.numeric(rownames(data))
            data <- data[,c(ncol(data),2,4,3,6:10)]
          }else{
            data <- data[,c(1,3,5,4,7:11)]
          }
          rownames(data) <- 1:nrow(data)
          return(data)
        }else{
          return(data)
        }
      }

    # activpal.file.process<-
    #   function(data, valid.days = NULL,wear.time.minimum = 72000){
    #     # takes in an unprocessed activpal file, formatting and processing the file to allow further analysis
    #     # data = an unprocessed activpal event file
    #     # wear.time.minimum = minimum wear time required for a day to be considered valid
    #     process.data<-data
    #     if(ncol(process.data)==6){
    #       process.data$abs.sum <- 0
    #     }
    #     process.data<-activpal.file.process.rename.row(process.data)
    #
    #     process.data$time <- as.POSIXct(process.data$time*86400,origin="1899-12-30",tz="UTC")
    #     process.data<-process.data[,1:7]
    #     process.data<-process.data[which(process.data$interval>0),]
    #
    #     process.data<-activpal.file.process.merge.stepping(process.data)
    #     #process.data$steps<-0
    #     if(!is.null(valid.days)){
    #       process.data<-activpal.file.process.split.day(process.data,c(6,7,8))
    #       process.data <- process.data[which(as.Date(process.data$time) %in% valid.days),]
    #     }else{
    #       process.data <- process.data[which(process.data$interval<72000),]
    #       process.data<-activpal.file.process.split.day(process.data,c(6,7,8))
    #       process.data<-activpal.file.process.exclude.days(process.data,(86400-wear.time.minimum))
    #     }
    #     return(process.data)
    #   }

    events_data <- pre.process.events.file(events_file_name,
                                           events_file_folder)

    load.events.file <-
      function(folder,file_name){
        # Load cell A1 to test if the events file contains a header
        events_file <- read.csv(paste(folder,file_name,sep=""), nrows=1, header = FALSE)
        if(events_file[1,1] == "**header**"){
          events_file <- read.csv(paste(folder,file_name,sep=""), header = FALSE)
          data_start <- grep("**data**",events_file$V1,fixed=TRUE)
          if(length(data_start) == 0){
            return(NULL)
          }
          events_file <- read.csv(paste(folder,file_name,sep=""), skip = data_start[1]+1)
        }else {
          events_file <- read.csv(paste(folder,file_name,sep=""))
        }
        # Loads an activPAL events file and processes the file
        if(colnames(events_file)[1] == "row.names"){
          events_file <- events_file[,-c(1)]
        }
        if(ncol(events_file) == 1){
          # Is not a csv file.  Load the file to see if it is semi-colon delimited
          if(events_file[1,1] == "**header**"){
            events_file <- read.csv(paste(folder,file_name,sep=""), header = FALSE)
            data_start <- grep("**data**",events_file$V1,fixed=TRUE)
            if(length(data_start) == 0){
              return(NULL)
            }
            events_file <- read.csv(paste(folder,file_name,sep=""), sep=";", skip = data_start[1]+1)
          }else {
            events_file <- read.csv(paste(folder,file_name,sep=""), sep=";", skip=1)
          }
          if(ncol(events_file) == 1){
            return(NULL)
          }
        }
        events_file <- activpal.convert.events.extended.file(events_file)

        return(events_file)
      }


    overlay_data <- read.csv(paste(overlay_file, sep = ""))
    blank_rows <- which(overlay_data[, 1] != "")


    if (length(blank_rows) > 0) { overlay_data <- overlay_data[which(overlay_data[, 1] != ""), ] }


    if (ncol(overlay_data) == 2) {
      colnames(overlay_data)[1] <- "start_time"
      overlay_data$start_time <- as.POSIXct(overlay_data$start_time,
                                            tryFormat = c("%d/%m/%Y %H:%M"),
                                            tz = "UTC")
      overlay_data$end_time <- as.POSIXct(c(tail(overlay_data$start_time, -1),
                                            tail(overlay_data$start_time, 1)),
                                          tz = "UTC")
      overlay_data$end_time <- overlay_data$start_time +
        as.numeric(difftime(overlay_data$end_time, overlay_data$start_time,
                            units = "secs"))
      overlay_data <- overlay_data[, c(1, 3, 2)]
    } else {
      colnames(overlay_data)[1:2] <- c("start_time", "end_time")
      overlay_data$start_time <- as.POSIXct(overlay_data$start_time,
                                            tryFormat = c("%d/%m/%Y %H:%M",
                                                          "%d/%m/%Y %H:%M:%S"),
                                            tz = "UTC")
      overlay_data$end_time <- as.POSIXct(overlay_data$end_time,
                                          tryFormat = c("%d/%m/%Y %H:%M",
                                                        "%d/%m/%Y %H:%M:%S"),
                                          tz = "UTC")
    }

    activity.with.overlay.single.chart(events_data,
                                       overlay_data,
                                       events_file_name,
                                       output_folder, "single")
  }



## 2.11 cppFunction  ------------------------------------------------------------
Rcpp::cppFunction("std::vector<double> bout_end(std::vector<int> seq, std::vector<double> activity, std::vector<double> interval, int size, int window_size) {
  std::vector<double> end_pos(size * 3);
  for(int i=0; i < size; i++){
    int step_count = 0;
    int inner_pos = i;
    double bout_duration = 0;
    double stepping_duration = 0;
    if(activity[i] == 0 || activity[i] == 1 || activity[i] == 2 || activity[i] == 2.1 || activity[i] == 3.1 || activity[i] == 3.2 || activity[i] == 5){
      while((activity[inner_pos] == 0 || activity[inner_pos] == 1 || activity[inner_pos] == 2 || activity[inner_pos] == 2.1 || activity[inner_pos] == 3.1 || activity[inner_pos] == 3.2 || activity[inner_pos] == 5) && (bout_duration + 0.001) < window_size && inner_pos < size){
    // if(activity[i] == 0 || activity[i] == 1 || activity[i] == 2 || activity[i] == 2.1 || activity[i] == 3.1 || activity[i] == 3.2 || activity[i] == 5){
      // while((activity[inner_pos] == 0 || activity[inner_pos] == 1 || activity[inner_pos] == 2 || activity[inner_pos] == 2.1 || activity[inner_pos] == 3.1 || activity[inner_pos] == 3.2 || activity[inner_pos] == 5) && (bout_duration + interval[inner_pos] + 0.001) < window_size && inner_pos < size){
        // (bout_duration + 0.001) needed to correctly account for comparison between double and integer
        // as the smallest unit of time measured is 0.1 seconds
        // Next activity is standing or stepping and current bout is shorter than the window.
        // Add the next event to the bout
        bout_duration += interval[inner_pos];
        if(activity[inner_pos] == 2){
          step_count += 2;
          stepping_duration += interval[inner_pos];
        }
        inner_pos += 1;
      }
      if(activity[inner_pos-1] == 1 && bout_duration > window_size){
        // Last event of interest is a period of quiet standing.
        // If the quiet standing makes the bout longer than the window then set the total bout duration to equal the window size
        bout_duration = window_size;
      }
    } else {
      inner_pos = -1;
    }
    end_pos[i] = inner_pos;
    end_pos[size + i] = step_count;
    end_pos[(2 * size) + i] = stepping_duration;
  }
  return end_pos;
}")

