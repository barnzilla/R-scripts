# Notes: file(s) need to be in AWC format, not AWCF format

# Load packages to extend base R
library( "pbapply" )

# Set desktop as working directory (where CSV output will be stored)
setwd( file.path( Sys.getenv( "USERPROFILE" ), "Desktop" ) )

# User-defined functions
## Wrapper function for sum()
get_sum <- function( x ) {
	# Ensure NA values are ignored
	sum( x, na.rm = TRUE )
}

## Reduce accelerometer data
reduce <- function( x, days_to_extract, summary_option, remove_first_day, remove_last_day, consecutive_zeros ) {
	# Validate parameters
	consecutive_zeros <- ifelse( ! is.numeric( consecutive_zeros ), 60, as.integer( consecutive_zeros ) )
	
	# Get file extension
	fe <- tolower( tools::file_ext( x ) )
	
	# Actical file
	if( fe == "awc" ) {
		# Import data from selected file(s)
		data <- trimws( readLines( con <- file( x ) ) )

		# Close the connection
		on.exit( close( con ) )

		# Find blank line (after which comes activity data)
		blank_line <- match( "", data )

		# Isolate metadata
		meta <- data[1:( blank_line - 1 )]

		# Set meta variables
		pid <- as.character( meta[1] )
		start_date <- as.Date( meta[2], format = "%d-%b-%Y" )
		start_time <- paste( meta[3], ":00", sep = "" )
		start <- as.POSIXct( trimws( paste( start_date, start_time, sep = " " ) ), format = "%Y-%m-%d %H:%M:%S" )
		epoch <- as.numeric( as.character( meta[4] ) )	
		age <- as.numeric( as.character( meta[5] ) )
		did <- as.character( meta[6] )
		sex <- as.character( meta[7] )
		height <- as.numeric( as.character( meta[8] ) )
		weight <- as.numeric( as.character( meta[9] ) )
		battery <- as.numeric( as.character( meta[10] ) )
		
		# Split activity data into columns
		data2 <- trimws( unlist( strsplit( data[( blank_line + 1 ):length( data )], "," ) ) )

		# Remove letters first (e.g., "7 M", "41 M") then convert to numeric, otherwise NAs introduced
		data2 <- gsub( "[^0-9.]", "", data2 )
		data2 <- as.numeric( as.character( data2 ) )

		# Structure the activity data as a data frame
		rows <- length( data[( blank_line + 1 ):length( data )] )
		if( length( data2 ) / 2 == rows ) {	
			df <- as.data.frame( matrix( data2, ncol = 2, byrow = TRUE ) )
			colnames( df ) <- c( "counts", "steps" )
		} else {
			df <- data.frame( "counts" = data2, "steps" = rep( NA, rows ) )
		}
		
		# Set age-related variables
		if( age <= 6 ) {
			# StatsCan intensity cut-points for preschoolers
			vcut <- 9999999
			mcut <- 1150
			lcut <- 100
		} else if( age >= 18 ) {
			# StatsCan intensity cut-points for adults
			vcut <- 3962
			mcut <- 1535
			lcut <- 100
		} else {
			# StatsCan intensity cut-points for children/youth
			vcut <- 6500
			mcut <- 1500
			lcut <- 100
		} 
	}
	# CSV file with TimeStamp, steps and vm column names
	else if( fe == "csv" ) {
		# Import data from selected file(s)
		data <- read.csv( x )
		
		# Set meta variables
		pid <- as.character( tools::file_path_sans_ext( basename( x ) ) )
		start_date <- as.Date( data$TimeStamp[1] )
		start_time <- strsplit( as.character( data$TimeStamp[1] ), " " )[[1]][2]
		start <- as.POSIXct( trimws( paste( start_date, start_time, sep = " " ) ), format = "%Y-%m-%d %H:%M:%S" )
		epoch <- strptime( as.character( data$TimeStamp[2] ), format = "%Y-%m-%d %H:%M:%S") - strptime( as.character( data$TimeStamp[1] ), format = "%Y-%m-%d %H:%M:%S" )
		epoch <- if( epoch == 15 ) 1 else if( epoch == 30 ) 2 else 4
		age <- 12
		did <- pid
		sex <- NA
		height <- NA
		weight <- NA
		battery <- NA
		
		# Structure the activity data as a data frame
		df <- data.frame( "counts" = data$vm, "steps" = data$steps )
		
		# Set age-related variables
		if( age <= 6 ) {
			# Butte cut-points for preschoolers
			vcut <- 6112
			mcut <- 3908
			lcut <- 820
		} else if( age >= 18 ) {
			# Freedson cut-points for adults
			vcut <- 6167
			mcut <- 2691
			lcut <- 100
		} else {
			# Evenson cut-points for children
			vcut <- 4012
			mcut <- 2296
			lcut <- 101
		} 
	} else {}

	# Accelerometer reduction settings/rules/decisions
	epoch_lookup <- data.frame( x = c( 1:2, 4 ), y = c( 15, 30, 60 ), z = c( "15-second", "30-second", "60-second" ) )
	epoch_rate <- epoch_lookup[epoch_lookup$x == epoch, 2]
	start <- as.POSIXct( trimws( paste( start_date, start_time, sep = " " ) ), format = "%Y-%m-%d %H:%M:%S" )
	valid_hours <- ifelse(age <= 6, 5, 10)
	spurious_count <- 20000
	spurious_step <- 253

	# Downsample counts and steps if the epoch rate is not 60 seconds
	if( epoch_rate == 15 ) {
		counts <- unname( tapply( df$counts, ( seq_along( df$counts ) - 1 ) %/% 4, get_sum ) )
		steps <- unname( tapply( df$steps, ( seq_along( df$steps ) - 1 ) %/% 4, get_sum ) )
		df <- data.frame( counts, steps )
	} else if( epoch_rate == 30 ) {
		counts <- unname( tapply( df$counts, ( seq_along( df$counts ) - 1 ) %/% 2, get_sum ) )
		steps <- unname( tapply( df$steps, ( seq_along( df$steps ) - 1 ) %/% 2, get_sum ) )
		df <- data.frame( counts, steps )
	} else { }

	# Calculate timestamps; note: March 12 will be missing one hour due to the start of Daylight Savings
	time_stamp <- sapply( row( df )[,1], function( x, y = start, z = 60 ) y + ( ( x - 1 ) * z ) )
	time_stamp <- as.POSIXct( time_stamp, origin = "1970-01-01 00:00:00" )

	# Calculate day of the week (string)
	day_string <- weekdays( time_stamp )

	# Calculate day of the week (number)
	v <- vector()
	for( i in 1:length( day_string ) ) {
		if( i == 1 ) {
			day_number <- 1
			v[i] <- day_number
		} else {
			if( day_string[i] != day_string[( i - 1 )] ) {
				day_number <- day_number + 1
				v[i] <- day_number
			} else { v[i] <- day_number }
		}
	}

	# Add to data frame
	df <- cbind( "date_time" = time_stamp, day_string, "day_number" = v, df )

	# Compute days sampled
	days_sampled <- max( df$day_number )

	# Subset data frame in accordance with days_to_extract value
	if( days_sampled < days_to_extract ) days_to_extract <- days_sampled
	df <- df[df$day_number <= days_to_extract,]

	# Subset data frame again if remove_first_day set to TRUE or to a time stamp
	if( isTRUE( remove_first_day ) ) {
		df <- df[df$day_number > 1,]
		days_to_extract <- days_to_extract - 1
	} else if( is.character( remove_first_day ) ) {
		lower_bound <- paste( start_date, remove_first_day, sep = " " )
		adjusted_steps <- as.numeric( unlist( apply( df[,c( "date_time", "steps" )], 1, function( x, y = lower_bound ) if( x[1] < y ) NA else x[2] ) ) )
		adjusted_counts <- as.numeric( unlist( apply( df[,c( "date_time", "counts" )], 1, function( x, y = lower_bound ) if( x[1] < y ) NA else x[2] ) ) )
		df$steps <- adjusted_steps
		df$counts <- adjusted_counts
	} else {
		# Leave df as is
	}
	
	# Subset data frame again if remove_last_day set to TRUE or to a time stamp
	if( isTRUE( remove_last_day ) ) {
		df <- df[df$day_number < max(df$day_number),]
		days_to_extract <- days_to_extract - 1
	} else if( is.character( remove_last_day ) ) {
		end_date <- strsplit( as.character( df$date_time[length( df$date_time )] ), " ")[[1]][1]
		upper_bound <- paste( end_date, remove_last_day, sep = " " )
		adjusted_steps <- as.numeric( unlist( apply( df[,c( "date_time", "steps" )], 1, function( x, y = upper_bound ) if( x[1] > y ) NA else x[2] ) ) )
		adjusted_counts <- as.numeric( unlist( apply( df[,c( "date_time", "counts" )], 1, function( x, y = upper_bound ) if( x[1] > y ) NA else x[2] ) ) )
		df$steps <- adjusted_steps
		df$counts <- adjusted_counts
	} else {
		# Leave df as is
	}
	
	# Set any spurious counts/steps to zero
	df$counts <- sapply( df$counts, function( x, y = spurious_count ) if( is.na( x ) | x >= y ) NA else x )
	df$steps <- sapply( df$steps, function( x, y = spurious_step ) if( is.na( x ) | x >= y ) NA else x )

	# Classify wear time
	df$wear_time <- rep( NA, nrow( df ) )
	for( i in 1:nrow( df ) ) {
		# Create index so that, for a given count, forward/backward inspection for exceptions can be executed before classifying wear time (0 = off, 1 = on)
		il <- ifelse( i - consecutive_zeros < 1, 1, i - consecutive_zeros )
		iu <- ifelse( i + consecutive_zeros > nrow( df ), nrow( df ), i + ( consecutive_zeros - 1 ) )
		if( is.na( df$counts[i] ) ) {
			df$wear_time[i] <- 0
		} else if( df$counts[i] > 100 ) {
			df$wear_time[i] <- 1
		} else {
			if( sum( df$counts[il:i] >= 1, na.rm = TRUE ) > 2 | sum( df$counts[i:iu] >= 1, na.rm = TRUE ) > 2 ) {
				df$wear_time[i] <- 1
			} else {
				df$wear_time[i] <- 0
			}
		}
	}
	
	# Create non wear time column for use when summarizing data
	df$non_wear_time <- sapply( df$wear_time, function( x ) if( is.na( x ) ) 1 else if( x == 0 ) 1 else 0 )

	# Classify intensity
	df$intensity <- apply( df[, c( "counts", "wear_time" )], 1, function( x, a = vcut, b = mcut, d = lcut ) if( is.na( x[1] ) | is.na( x[2] ) | x[2] == 0 ) NA else if( x[1] >= a ) "VPA" else if( x[1] >= b ) "MPA" else if( x[1] >= d ) "LPA" else "SB")
	
	# Create movement behaviour vectors for use when summarizing data
	df$sb <- sapply( df$intensity, function( x ) if( is.na( x ) | x != "SB" ) 0 else 1 )
	df$lpa <- sapply( df$intensity, function( x ) if( is.na( x ) | x != "LPA" ) 0 else 1 )
	df$mpa <- sapply( df$intensity, function( x ) if( is.na( x ) | x != "MPA" ) 0 else 1 )
	df$vpa <- sapply( df$intensity, function( x ) if( is.na( x ) | x != "VPA" ) 0 else 1 )
	df$mvpa <- apply( df[, c( "mpa", "vpa" )], 1, sum )

	# Create other data structures for use when summarizing data
	time_stamp2 <- unname( sapply( as.character( df$date_time ), function( x ) unlist( strsplit( x, " " ) )[1] ) )
	df2 <- data.frame( date_time = unique( time_stamp2 )[1:days_to_extract] )
	df2$day <- rep( unique( df$day_string ), 100 )[1:days_to_extract]
	df2$pid <- rep( pid, days_to_extract ) 
	df2$did <- rep( did, days_to_extract ) 
	df2$epoch <- rep( epoch_lookup[epoch_lookup$x == epoch, 3], days_to_extract ) 
	df2$age <- rep( age, days_to_extract ) 
	df2$sex <- rep( sex, days_to_extract ) 
	df2$height <- rep( height, days_to_extract ) 
	df2$weight <- rep( weight, days_to_extract ) 
	df2$wear_time <- aggregate( df$wear_time, by = list( Day = df$day_number ), FUN = sum )$x
	df2$non_wear_time <- aggregate( df$non_wear_time, by = list( Day = df$day_number ), FUN = sum )$x
	df2$valid_day <- sapply( df2$wear_time, function( x ) if( is.na( x ) | x < ( valid_hours * 60 ) ) 0 else 1 )
	df2$total_wear_time <- apply( df2[, c( "wear_time", "non_wear_time" )], 1, function( x ) get_sum( x[1:2] ) )
	df2$counts <- aggregate( df$counts, by = list( Day = df$day_number ), FUN = get_sum )$x
	df2$steps <- aggregate( df$steps, by = list( Day = df$day_number ), FUN = get_sum )$x
	df2$sb <- aggregate( df$sb, by = list( Day = df$day_number ), FUN = sum )$x
	df2$lpa <- aggregate( df$lpa, by = list( Day = df$day_number ), FUN = sum )$x
	df2$mpa <- aggregate( df$mpa, by = list( Day = df$day_number ), FUN = sum )$x
	df2$vpa <- aggregate( df$vpa, by = list( Day = df$day_number ), FUN = sum )$x
	df2$mvpa <- aggregate( df$mvpa, by = list( Day = df$day_number ), FUN = sum )$x
		
	# Create summary data frame
	summary <- data.frame(
		pid = pid, 
		did = did, 
		epoch = epoch_lookup[epoch_lookup$x == epoch,3], 
		age, 
		sex, 
		height, 
		weight, 
		days_sampled, 
		days_extracted = days_to_extract,
		valid_days = get_sum( df2$valid_day ), 
		start_date,
		start_time,
		wear_time = mean( df2$wear_time[df2$valid_day == 1], na.rm = TRUE ), 
		non_wear_time = mean( df2$non_wear_time[df2$valid_day == 1], na.rm = TRUE ), 
		total_wear_time = mean( df2$total_wear_time[df2$valid_day == 1], na.rm = TRUE ),
		counts = mean( df2$counts[df2$valid_day == 1], na.rm = TRUE ),
		steps = mean( df2$steps[df2$valid_day == 1], na.rm = TRUE ),
		sb = mean( df2$sb[df2$valid_day == 1], na.rm = TRUE ),
		lpa = mean( df2$lpa[df2$valid_day == 1], na.rm = TRUE ),
		mpa = mean( df2$mpa[df2$valid_day == 1], na.rm = TRUE ),
		vpa = mean( df2$vpa[df2$valid_day == 1], na.rm = TRUE ),
		mvpa = mean( df2$mvpa[df2$valid_day == 1], na.rm = TRUE )
	)
	
	# Output the summary data frame 
	if( summary_option == "overall" ) output <- summary else output <- df2

	# Return output
	return( output )
}

## Wrapper function for reduce()
reduce_files <- function( days_to_extract = 7, summary = "overall", remove_first_day = FALSE, remove_last_day = FALSE, consecutive_zeros = 60 ) {
	# Ensure days parameter is a number and, if not, then revert to the default
	if( is.numeric( days_to_extract ) ) days_to_extract <- floor( days_to_extract ) else days_to_extract <- 7
	
	# Return output as a data frame
	return( do.call( "rbind", pbapply( data.frame( choose.files() ), 1, function( x, y = days_to_extract, z = summary, a = remove_first_day, b = remove_last_day, d = consecutive_zeros ) reduce( x[1], y, z, a, b, d ) ) ) )
}

# Reduce accelerometer file(s)
## days_to_extract = integer
## summary = "overall" or "by_day"
## remove_first_day = TRUE for the entire first day, a timestamp (e.g., "13:30:00") for the part of the day that is >= the timestamp, or FALSE for none of the first day)
## remove_last_day = TRUE for the entire last day, a timestamp (e.g., "13:30:00") for the part of the day that is <= the timestamp, or FALSE for none of the last day)
## consecutive_zeros = integer representing minutes of consecutive zeroes; defaults to 60
output <- reduce_files( days_to_extract = 7, summary = "by_day", remove_first_day = FALSE, remove_last_day = FALSE, consecutive_zeros = 60 )

# View output
View( output )

# Export output to CSV file
write.csv( output, "output.csv", na = "", row.names = FALSE )