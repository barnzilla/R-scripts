# Set working directory
setwd(choose.dir())

# User-defined functions
# Wrapper function for sum()
get_sum <- function(x) {
	sum(x, na.rm = TRUE)
}

# Main function to reduce raw actical data
reduce_awc <- function(x, days_to_extract, downsample = FALSE) {
	# Import awc file
	awc <- trimws(readLines(con <- file(x)))

	# Close the connection
	close(con)

	# Find blank line (after which comes activity data)
	blank_line <- match("", awc)

	# Isolate metadata
	meta <- awc[1:(blank_line - 1)]

	# Set meta variables
	pid <- as.character(meta[1])
	start_date <- as.Date(meta[2], format = "%d-%b-%Y")
	start_time <- paste(meta[3], ":00", sep = "")
	start <- as.POSIXct(trimws(paste(start_date, start_time, sep = " ")), format = "%Y-%m-%d %H:%M:%S")
	epoch <- as.numeric(as.character(meta[4]))	
	age <- as.numeric(as.character(meta[5]))
	did <- as.character(meta[6])
	sex <- as.character(meta[7])
	height <- as.numeric(as.character(meta[8]))
	weight <- as.numeric(as.character(meta[9]))
	battery <- as.numeric(as.character(meta[10]))

	# Split activity data into columns
	data <- trimws(unlist(strsplit(awc[(blank_line + 1):length(awc)], ",")))

	# Remove letters first (e.g., "7 M", "41 M") then convert to numeric, otherwise NAs introduced
	data <- gsub("[^0-9.]", "", data)
	data <- as.numeric(as.character(data))

	# Structure the activity data as a data frame
	rows <- length(awc[(blank_line + 1):length(awc)])
	if(length(data) / 2 == rows) {	
		df <- as.data.frame(matrix(data, ncol = 2, byrow = TRUE))
		colnames(df) <- c("counts", "steps")
	} else {
		df <- as.data.frame("counts" = data)
	}

	# Set variables based on epoch rate
	if(epoch == 4) {
		epoch_rate <- 60
		epoch_rate_label <- "60-second"
		multiplier <- 1
	} else if(epoch == 1) {
		if(isTRUE(downsample)) {
			# Sum 4-sample chunks
			downsampled_counts <- unname(tapply(df$counts, (seq_along(df$counts) - 1) %/% 4, get_sum))
			downsampled_steps <- unname(tapply(df$steps, (seq_along(df$steps) - 1) %/% 4, get_sum))
			# Recreate data frame
			df <- data.frame(counts = downsampled_counts, steps = downsampled_steps)
			# Treat data as sampled at 60-second rate
			epoch_rate <- 60
			epoch_rate_label <- "60-second (downsampled from 15-second)"
			multiplier <- 1
		} else {
			epoch_rate <- 15
			epoch_rate_label <- "15-second"
			multiplier <- 4
		}
	} else if(epoch == 2) {
		if(isTRUE(downsample)) {
			# Sum 2-sample chunks
			downsampled_counts <- unname(tapply(df$counts, (seq_along(df$counts) - 1) %/% 2, get_sum))
			downsampled_steps <- unname(tapply(df$steps, (seq_along(df$steps) - 1) %/% 2, get_sum))
			# Recreate data frame
			df <- data.frame(counts = downsampled_counts, steps = downsampled_steps)
			# Treat data as sampled at 60-second rate
			epoch_rate <- 60
			epoch_rate_label <- "60-second (downsampled from 30-second)"
			multiplier <- 1
		} else {
			epoch_rate <- 30
			epoch_rate_label <- "30-second"
			multiplier <- 2
		}
	} else { 
		# Do nothing 
	}
	spurious_count <- 20000 / multiplier
	spurious_step <- 253  / multiplier
	
	# Set intensity cut-points
	if(epoch_rate == 60) {
		if(age <= 6) {
			# StatsCan cut-points for preschoolers
			vcut <- 9999999
			mcut <- 1150
			lcut <- 100
		} else if(age >= 18) {
			# StatsCan cut-points for adults
			vcut <- 3962
			mcut <- 1535
			lcut <- 100
		} else {
			# StatsCan cut-points for children/youth
			vcut <- 6500
			mcut <- 1500
			lcut <- 100
		}
	} else if(epoch_rate == 15) {
		if(age <= 6) {
			# StatsCan cut-points for preschoolers
			vcut <- 9999999
			mcut <- 288
			lcut <- 25
		} else if(age >= 18) {
			# StatsCan cut-points for adults
			vcut <- 3962 / 4
			mcut <- 1535 / 4
			lcut <- 100 / 4
		} else {
			# StatsCan cut-points for children/youth
			vcut <- 991
			mcut <- 384
			lcut <- 25
		}
	} else {
		# Do thing
	}

	# Set hours for valid day
	if(age <= 6) {
		valid_hours <- 5
	} else {
		valid_hours <- 10
	}

	# Remove impartial days (make sure data start at midnight)
	start_time_split <- as.numeric(unlist(strsplit(start_time, ":")))
	minutes_from_midnight <- ((start_time_split[1] * 60) + start_time_split[2]) * multiplier
	if(minutes_from_midnight > (1 * multiplier)) {
		df <- tail(df, -minutes_from_midnight)
		start_date <- start_date + 1
		start_time <- "00:00:00"
		start <- as.POSIXct(trimws(paste(start_date, start_time, sep = " ")), format = "%Y-%m-%d %H:%M:%S")
	}

	# Calculate timestamps; note: March 12 will be missing one hour due to the start of Daylight Savings
	time_stamp <- sapply(row(df)[,1], function(x, y = start, z = epoch_rate) y + ((x - 1) * z))
	time_stamp <- as.POSIXct(time_stamp, origin = "1970-01-01 00:00:00")

	# Calculate day of the week (string)
	day_string <- weekdays(time_stamp)

	# Calculate day of the week (number)
	v <- vector()
	for(i in 1:length(day_string)) {
		if(i == 1) {
			day_number <- 1
			v[i] <- day_number
		} else {
			if(day_string[i] != day_string[(i - 1)]) {
				day_number <- day_number + 1
				v[i] <- day_number
			} else { v[i] <- day_number }
		}
	}

	# Add to data frame
	df <- cbind("date_time" = time_stamp, day_string, "day_number" = v, df)

	# Subset data frame in accordance with days_to_extract value
	df <- df[df$day_number <= days_to_extract,]

	# Set any spurious counts/steps to zero
	df$counts_cleaned <- sapply(df$counts, function(x, y = spurious_count) if(is.na(x) | x >= y) NA else x )
	df$steps_cleaned <- sapply(df$steps, function(x, y = spurious_step) if(is.na(x) | x >= y) NA else x )
	
	# Create wear time column
	i_ref <- 1
	zero_sum <- 0
	exception_sum <- 0
	df$wear_time <- rep(NA, nrow(df))
	for(i in 1:nrow(df)) {
		if(is.na(df$counts_cleaned[i])) df$counts_cleaned[i] <- 0
		if(df$counts_cleaned[i] >= lcut) {
			df$wear_time[i] <- 1
			if((zero_sum + exception_sum - 1) >= (60 * multiplier)) df$wear_time[i_ref:(i - 1)] <- 0 else df$wear_time[i_ref:(i - 1)] <- 1
			i_ref <- i + 1
			zero_sum <- 0
			exception_sum <- 0
		} else {
			if(df$counts_cleaned[i] == 0) {
				zero_sum <- zero_sum + 1
			} else {
				exception_sum <- exception_sum + 1
				if(exception_sum > 2) {
					df$wear_time[i] <- 1
					if((zero_sum + exception_sum - 1) >= (60 * multiplier)) df$wear_time[i_ref:(i - 1)] <- 0 else df$wear_time[i_ref:(i - 1)] <- 1
					if(i != nrow(df)) i_ref <- i + 1
					zero_sum <- 0
					exception_sum <- 0
				}
			}
			if(i == nrow(df)) {
				if((zero_sum + exception_sum - 1) >= (60 * multiplier)) df$wear_time[i_ref:i] <- 0 else df$wear_time[i_ref:i] <- 1
			}
		}
	}

	# Create intensity column
	df$intensity <- rep(NA, nrow(df))
	for(i in 1:length(df$intensity)) {
		if(df$wear_time[i] == 1) {
			if(is.na(df$counts_cleaned[i])) df$intensity[i] <- NA  else if(df$counts_cleaned[i] >= vcut) df$intensity[i] <- "VPA" else if(df$counts_cleaned[i] >= mcut) df$intensity[i] <- "MPA" else if(df$counts_cleaned[i] >= lcut) df$intensity[i] <- "LPA" else df$intensity[i] <- "SB"
		} else { df$intensity[i] <- NA }
	}

	# Create a summary file by day
	max_days <- max(df$day_number, na.rm = TRUE)
	data_collection_days <- sapply(0:(max_days - 1), function(x, y = start) y + (x * 86400))
	data_collection_days <- as.POSIXct(data_collection_days, origin = "1970-01-01 00:00:00")
	summary_df <- data.frame(
		type = rep("daily", max_days),
		pid = rep(pid, max_days), 
		did = rep(did, max_days), 
		epoch = rep(epoch_rate_label, max_days),
		age = rep(age, max_days),
		sex = rep(sex, max_days), 
		height = rep(height, max_days),
		weight = rep(weight, max_days), 
		date_time = data_collection_days,
		day_number = 1:max_days, 
		valid_day = rep(NA, max_days), 
		wear_time = rep(NA, max_days), 
		non_wear_time = rep(NA, max_days), 
		total_time = rep(NA, max_days), 
		counts = rep(NA, max_days), 
		steps = rep(NA, max_days), 
		counts_weighted = rep(NA, max_days),
		steps_weighted = rep(NA, max_days),
		sb = rep(NA, max_days), 
		lpa = rep(NA, max_days), 
		mpa = rep(NA, max_days), 
		vpa = rep(NA, max_days),
		mvpa = rep(NA, max_days),
		sb_weighted = rep(NA, max_days),
		lpa_weighted = rep(NA, max_days),
		mpa_weighted = rep(NA, max_days),
		vpa_weighted = rep(NA, max_days),
		mvpa_weighted = rep(NA, max_days)
	)
	for(i in 1:max(df$day_number)) {
		summary_df$wear_time[i] <- sum(df$wear_time[df$day_number == i], na.rm = TRUE) / multiplier
		summary_df$non_wear_time[i] <- sum(df$wear_time[df$day_number == i] == 0, na.rm = TRUE) / multiplier
		summary_df$total_time[i] <- sum(c(summary_df$wear_time[i], summary_df$non_wear_time[i]), na.rm = TRUE)
		if(summary_df$wear_time[i] >= (60 * valid_hours)) summary_df$valid_day[i] <- 1 else summary_df$valid_day[i] <- 0
		if(summary_df$wear_time[i] > 0) summary_df$counts[i] <- sum(df$counts_cleaned[df$day_number == i & df$wear_time == 1], na.rm = TRUE) else NA
		if(summary_df$wear_time[i] > 0) summary_df$steps[i] <- sum(df$steps_cleaned[df$day_number == i & df$wear_time == 1], na.rm = TRUE) else NA
		if(summary_df$valid_day[i] == 1) summary_df$counts_weighted[i] <- summary_df$counts[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$valid_day[i] == 1) summary_df$steps_weighted[i] <- summary_df$steps[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$wear_time[i] > 0) summary_df$sb[i] <- sum(df$intensity[df$day_number == i] == "SB", na.rm = TRUE) / multiplier else NA
		if(summary_df$wear_time[i] > 0) summary_df$lpa[i] <- sum(df$intensity[df$day_number == i] == "LPA", na.rm = TRUE) / multiplier else NA
		if(summary_df$wear_time[i] > 0) summary_df$mpa[i] <- sum(df$intensity[df$day_number == i] == "MPA", na.rm = TRUE) / multiplier else NA
		if(summary_df$wear_time[i] > 0) summary_df$vpa[i] <- sum(df$intensity[df$day_number == i] == "VPA", na.rm = TRUE) / multiplier else NA
		if(summary_df$wear_time[i] > 0) summary_df$mvpa[i] <- sum(c(summary_df$mpa[i], summary_df$vpa[i]), na.rm = TRUE) else NA
		if(summary_df$valid_day[i] == 1) summary_df$sb_weighted[i] <- summary_df$sb[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$valid_day[i] == 1) summary_df$lpa_weighted[i] <- summary_df$lpa[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$valid_day[i] == 1) summary_df$mpa_weighted[i] <- summary_df$mpa[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$valid_day[i] == 1) summary_df$vpa_weighted[i] <- summary_df$vpa[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
		if(summary_df$valid_day[i] == 1) summary_df$mvpa_weighted[i] <- summary_df$mvpa[i] * 60 * valid_hours / summary_df$wear_time[i] else NA
	}

	# Create daily data frame
	valid_day_sum <- sum(summary_df$valid_day, na.rm = TRUE)
	daily_minutes_weighted <- 60 * valid_hours
	total_wear_time_unstd <- sum(summary_df$wear_time[summary_df$valid_day == 1], na.rm = TRUE)
	daily_df <- data.frame(
		type = "summary",
		pid = unique(summary_df$pid),
		did = unique(summary_df$did),
		epoch = epoch_rate_label,
		age = unique(summary_df$age),
		sex = unique(summary_df$sex),
		height = unique(summary_df$height),
		weight = unique(summary_df$weight),
		date_time = start,
		day_number = max(summary_df$day_number),
		valid_day = valid_day_sum,
		wear_time = if(valid_day_sum > 0) mean(summary_df$wear_time[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		non_wear_time = if(valid_day_sum > 0) mean(summary_df$non_wear_time[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		total_time = if(valid_day_sum > 0) mean(summary_df$total_time[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		counts = if(valid_day_sum > 0) mean(summary_df$counts[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		steps = if(valid_day_sum > 0) mean(summary_df$steps[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		counts_weighted = if(valid_day_sum > 0) sum(summary_df$counts[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		steps_weighted = if(valid_day_sum > 0) sum(summary_df$steps[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		sb = if(valid_day_sum > 0) mean(summary_df$sb[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		lpa = if(valid_day_sum > 0) mean(summary_df$lpa[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		mpa = if(valid_day_sum > 0) mean(summary_df$mpa[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		vpa = if(valid_day_sum > 0) mean(summary_df$vpa[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		mvpa = if(valid_day_sum > 0) mean(summary_df$mvpa[summary_df$valid_day == 1], na.rm = TRUE) else NA,
		sb_weighted = if(valid_day_sum > 0) sum(summary_df$sb[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		lpa_weighted = if(valid_day_sum > 0) sum(summary_df$lpa[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		mpa_weighted = if(valid_day_sum > 0) sum(summary_df$mpa[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		vpa_weighted = if(valid_day_sum > 0) sum(summary_df$vpa[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA,
		mvpa_weighted = if(valid_day_sum > 0) sum(summary_df$mvpa[summary_df$valid_day == 1], na.rm = TRUE) * valid_day_sum * daily_minutes_weighted / total_wear_time_unstd / valid_day_sum else NA
	)
	# Add results to output file
	output <- rbind(summary_df, daily_df)
}

# Wrapper function for reduce_awc()
get_awc <- function(days = 7, downsample = FALSE) {
	# Ensure days parameter is a number and, if not, then revert to the default
	if(is.numeric(days)) floor(days) else 7
	# Return output as a data frame
	return(do.call("rbind", apply(X = data.frame(choose.files()), MARGIN = 1, FUN = function(x, days_to_extract = days, option = downsample) reduce_awc(x[1], days_to_extract, option))))
}

# Function to display reduce_awc output in R's viewer
view_awc <- function(result, type) {
	if(type == "daily") colnames(result)[9] <- "start_time" else colnames(result)[9] <- "start_date"
	View(result[result$type == type,])
}

# Function to export reduce_awc output to csv
export_awc <- function(result, type, name = type) {
	# Export results to csv
	if(type == "daily") colnames(result)[9] <- "start_time" else colnames(result)[9] <- "start_date"
	write.csv(result[result$type == type,], paste(name, ".csv", sep = ""), row.names = FALSE, na = "")
}

# Reduce actical data 
results <- get_awc(days = 7, downsample = TRUE)

# View reduced data
view_awc(results, "daily")
view_awc(results, "summary")

# Export results
export_awc(results, "daily", name = "day by day")
export_awc(results, "summary", name = "summary")
