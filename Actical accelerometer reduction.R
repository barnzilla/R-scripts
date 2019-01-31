# Notes: file(s) need to be in AWC format, not AWCF format

# Load packages to extend base R
library("pbapply")

# Set desktop as working directory (where CSV output will be stored)
setwd(file.path(Sys.getenv("USERPROFILE"), "Desktop"))

# User-defined functions
## Wrapper function for sum()
get_sum <- function(x) {
	# Ensure NA values are ignored
	sum(x, na.rm = TRUE)
}

## Reduce actical accelerometer data
reduce <- function(x, days_to_extract, summary_option) {
	# Import data from selected file(s)
	data <- trimws(readLines(con <- file(x)))

	# Close the connection
	close(con)

	# Find blank line (after which comes activity data)
	blank_line <- match("", data)

	# Isolate metadata
	meta <- data[1:(blank_line - 1)]

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

	# Accelerometer reduction settings/rules/decisions
	epoch_lookup <- data.frame(x = c(1:2, 4), y = c(15, 30, 60), z = c("15-second", "30-second", "60-second"))
	epoch_rate <- epoch_lookup[epoch_lookup$x == epoch, 2]
	start <- as.POSIXct(trimws(paste(start_date, start_time, sep = " ")), format = "%Y-%m-%d %H:%M:%S")
	spurious_count <- 20000
	spurious_step <- 253

	# Set intensity cut-points
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

	# Set hours for valid day
	if(age <= 6) {
		valid_hours <- 5
	} else {
		valid_hours <- 10
	}

	# Split activity data into columns
	data2 <- trimws(unlist(strsplit(data[(blank_line + 1):length(data)], ",")))

	# Remove letters first (e.g., "7 M", "41 M") then convert to numeric, otherwise NAs introduced
	data2 <- gsub("[^0-9.]", "", data2)
	data2 <- as.numeric(as.character(data2))

	# Structure the activity data as a data frame
	rows <- length(data[(blank_line + 1):length(data)])
	if(length(data2) / 2 == rows) {	
		df <- as.data.frame(matrix(data2, ncol = 2, byrow = TRUE))
		colnames(df) <- c("counts", "steps")
	} else {
		df <- as.data.frame("counts" = data2)
	}

	# Downsample counts and steps if the epoch rate is not 60 seconds
	if(epoch_rate == 15) {
		counts <- unname(tapply(df$counts, (seq_along(df$counts) - 1) %/% 4, get_sum))
		steps <- unname(tapply(df$steps, (seq_along(df$steps) - 1) %/% 4, get_sum))
		df <- data.frame(counts, steps)
	} else if(epoch_rate == 30) {
		counts <- unname(tapply(df$counts, (seq_along(df$counts) - 1) %/% 2, get_sum))
		steps <- unname(tapply(df$steps, (seq_along(df$steps) - 1) %/% 2, get_sum))
		df <- data.frame(counts, steps)
	} else { 
		# Do nothing 
	}

	# Calculate timestamps; note: March 12 will be missing one hour due to the start of Daylight Savings
	time_stamp <- sapply(row(df)[,1], function(x, y = start, z = 60) y + ((x - 1) * z))
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

	# Compute days sampled
	days_sampled <- max(df$day_number)

	# Subset data frame in accordance with days_to_extract value
	if(days_sampled < days_to_extract) days_to_extract <- days_sampled
	df <- df[df$day_number <= days_to_extract,]
	
	# Set any spurious counts/steps to zero
	df$counts <- sapply(df$counts, function(x, y = spurious_count) if(is.na(x) | x >= y) NA else x )
	df$steps <- sapply(df$steps, function(x, y = spurious_step) if(is.na(x) | x >= y) NA else x )

	# Create index so that, for a given count, forward/backward inspection for exceptions can be executed before classifying wear time (0 = off, 1 = on)
	index <- data.frame(lower = rep(NA, nrow(df)), upper = rep(NA, nrow(df)))	
	for(i in 1:nrow(df)) {
		index$lower[i] <- (i - 60)
		index$upper[i] <- (i + 59)
	}

	# Remove non-positive values from index
	index$lower <- sapply(index$lower, function(x) if(is.na(x)) NA else if(x < 1) 1 else x)
	index$upper <- sapply(index$upper, function(x, y = max(nrow(index))) if(is.na(x)) NA else if(x > y) y else x)

	# Classify wear time
	df$wear_time <- rep(NA, nrow(df))
	for(i in 1:nrow(df)) {
		if(is.na(df$counts[i])) {
			df$wear_time <- NA
		} else if(df$counts[i] > 100) {
			df$wear_time[i] <- 1
		} else {
			if(sum(df$counts[index$lower[i]:i] >= 1, na.rm = TRUE) > 2 | sum(df$counts[i:index$upper[i]] >= 1, na.rm = TRUE) > 2) {
				df$wear_time[i] <- 1
			} else {
				df$wear_time[i] <- 0
			}
		}
	}
	
	# Create non wear time column for use when summarizing data
	df$non_wear_time <- sapply(df$wear_time, function(x) if(is.na(x)) NA else if(x == 0) 1 else 0)

	# Classify intensity
	df$intensity <- apply(df[, c("counts", "wear_time")], 1, function(x, a = vcut, b = mcut, c = lcut) if(is.na(x[1]) | is.na(x[2]) | x[2] == 0) NA else if(x[1] >= a) "VPA" else if(x[1] >= b) "MPA" else if(x[1] >= c) "LPA" else "SB")
	
	# Create movement behaviour vectors for use when summarizing data
	df$sb <- sapply(df$intensity, function(x) if(is.na(x) | x != "SB") 0 else 1)
	df$lpa <- sapply(df$intensity, function(x) if(is.na(x) | x != "LPA") 0 else 1)
	df$mpa <- sapply(df$intensity, function(x) if(is.na(x) | x != "MPA") 0 else 1)
	df$vpa <- sapply(df$intensity, function(x) if(is.na(x) | x != "VPA") 0 else 1)
	df$mvpa <- apply(df[, c("mpa", "vpa")], 1, sum)

	# Create other data structures for use when summarizing data
	time_stamp2 <- unname(sapply(as.character(df$date_time), function(x) unlist(strsplit(x, " "))[1]))
	df2 <- data.frame(date_time = unique(time_stamp2)[1:days_to_extract])
	df2$day <- rep(unique(df$day_string), 100)[1:days_to_extract]
	df2$pid <- rep(pid, days_to_extract) 
	df2$did <- rep(did, days_to_extract) 
	df2$epoch <- rep(epoch_lookup[epoch_lookup$x == epoch, 3], days_to_extract) 
	df2$age <- rep(age, days_to_extract) 
	df2$sex <- rep(sex, days_to_extract) 
	df2$height <- rep(height, days_to_extract) 
	df2$weight <- rep(weight, days_to_extract) 
	df2$wear_time <- aggregate(df$wear_time, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$non_wear_time <- aggregate(df$non_wear_time, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$valid_day <- sapply(df2$wear_time, function(x) if(is.na(x) | x < (valid_hours * 60)) 0 else 1)
	df2$total_wear_time <- apply(df2[, c("wear_time", "non_wear_time")], 1, function(x) get_sum(x[1:2]))
	df2$counts <- aggregate(df$counts, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$steps <- aggregate(df$steps, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$sb <- aggregate(df$sb, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$lpa <- aggregate(df$lpa, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$mpa <- aggregate(df$mpa, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$vpa <- aggregate(df$vpa, by = list(Day = df$day_number), FUN = get_sum)$x
	df2$mvpa <- aggregate(df$mvpa, by = list(Day = df$day_number), FUN = get_sum)$x
		
	# Create summary data frame
	summary <- data.frame(
		pid = pid, 
		did = did, 
		epoch = epoch_lookup[epoch_lookup$x == epoch, 3], 
		age, 
		sex, 
		height, 
		weight, 
		days_sampled, 
		days_extracted = days_to_extract,
		valid_days = get_sum(df2$valid_day), 
		start_date,
		start_time,
		wear_time = mean(df2$wear_time[df2$valid_day == 1], na.rm = TRUE), 
		non_wear_time = mean(df2$non_wear_time[df2$valid_day == 1], na.rm = TRUE), 
		total_wear_time = mean(df2$total_wear_time[df2$valid_day == 1], na.rm = TRUE),
		counts = mean(df2$counts[df2$valid_day == 1], na.rm = TRUE),
		steps = mean(df2$steps[df2$valid_day == 1], na.rm = TRUE),
		sb = mean(df2$sb[df2$valid_day == 1], na.rm = TRUE),
		lpa = mean(df2$lpa[df2$valid_day == 1], na.rm = TRUE),
		mpa = mean(df2$mpa[df2$valid_day == 1], na.rm = TRUE),
		vpa = mean(df2$vpa[df2$valid_day == 1], na.rm = TRUE),
		mvpa = mean(df2$mvpa[df2$valid_day == 1], na.rm = TRUE)
	)
	
	# Output the summary data frame 
	if(summary_option == "overall") output <- summary else output <- df2

	# Return output
	return(output)
}

## Wrapper function for reduce()
reduce_files <- function(days_to_extract = 7, summary = "overall") {
	# Ensure days parameter is a number and, if not, then revert to the default
	if(is.numeric(days_to_extract)) days_to_extract <- floor(days_to_extract) else days_to_extract <- 7
	
	# Return output as a data frame
	return(do.call("rbind", pbapply(data.frame(choose.files()), 1, function(x, y = days_to_extract, z = summary) reduce(x[1], y, z))))
}

# Reduce accelerometer file(s) (days_to_extract = integer, summary = "overall" or "by_day")
output <- reduce_files(days_to_extract = 14, summary = "overall")

# View output
View(output)

# Export output to CSV file
write.csv(output, "output.csv", na = "", row.names = FALSE)