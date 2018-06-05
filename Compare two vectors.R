# Load packages to extend base R
library("ggplot2")
library("effsize")

# Import data files
# First dataset
d1 <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)
# Second dataset
d2 <- read.csv(file.choose(), header = TRUE, sep = ",", stringsAsFactors = FALSE)

# User-defined functions
compare <- function(var_name, x = d1[, var_name], y = d2[, var_name]) {
  if(is.character(x) | is.character(y)) {
    if(nlevels(as.factor(x)) <= 5) {
      df <- as.data.frame(table(as.factor(as.vector(as.matrix(x)))))
      df$d <- rep("Original data", nrow(df))
      colnames(df) <- c("level", "count", "d")
      df2 <- as.data.frame(table(as.factor(as.vector(as.matrix(y)))))
      df2$d <- rep("New data", nrow(df))
      colnames(df2) <- c("level", "count", "d")
      df <- rbind(df, df2)
      df$d <- as.factor(df$d)
      df$d <- relevel(df$d, ref = "Original data")
      ggplot(data = df, aes(x = level, y = count, fill = d)) +
        geom_bar(stat = "identity", position = position_dodge()) +
        theme_minimal() + labs(title = paste("Comparison of", var_name, "data", sep = " "), x = "", y = "Count", subtitle = paste(format(as.numeric(round(sum(x != y, na.rm = TRUE) / length(x) * 100, 2)), nsmall = 2), "%", " difference", sep = "")) + 
        theme(legend.position="top") + 
        scale_fill_manual("", values = c("Original data" = "#0C244A", "New data" = "#C5CDDD")) +
        geom_text(data = df, aes(x = level, y = count, label = count), position = position_dodge(width = 0.9), vjust = -0.75) 
    } else {
      paste(format(as.numeric(round(sum(x != y, na.rm = TRUE) / length(x) * 100, 2)), nsmall = 2), "%", " difference", sep = "")
    }
  } else {
    x_stats <- paste(format(round(mean(x, na.rm = TRUE), 1), nsmall = 1), "\u00b1", format(round(sd(x, na.rm = TRUE), 1), nsmall = 1), sep = " ")
    # Convert NAs to zeroes so residuals can be calculated
    x[is.na(x)] <- 0
    y_stats <- paste(format(round(mean(y, na.rm = TRUE), 1), nsmall = 1), "\u00b1", format(round(sd(y, na.rm = TRUE), 1), nsmall = 1), sep = " ")
    # Convert NAs to zeroes so residuals can be calculated
    y[is.na(y)] <- 0
    df <- data.frame(x, y)
    df$residuals <- apply(data.frame(x, y), 1, function(x) if(is.na(x[1]) & is.na(x[2])) 0 else x[2] - x[1]  )
    p <- ggplot(df, aes(x, residuals, colour = x))
    p + geom_count(stat = "sum", position = "identity") + theme_minimal() + labs(title = paste("Comparison of", var_name, "data", sep = " "), subtitle = paste(round(sum(df$residuals != 0, na.rm = TRUE) / length(df$residuals) * 100, 2), "%", " disagreement", " (", x_stats, " vs.", y_stats, "; Cohen's d: ", format(round(unname(cohen.d(x, y)$estimate), 2), nsmall = 2),")", sep = ""), x = "Fitted values (original data)", y = "Residuals (new data - original data)") +
      geom_hline(yintercept = 0) + theme(legend.position = "none") +
      annotate("text", 0, 0, vjust = -1, hjust = -0.25, label = "No difference", colour = "#242424")
  }
}

difference_table <- function(d1, d2) {
  var_names <- names(d1)
  df <- data.frame(var = NA, difference = NA)
  for(i in 1:length(var_names)) {
    if(var_names[i] %in% colnames(d1) & var_names[i] %in% colnames(d2)) {
      diff <- as.numeric(round(sum(d1[,var_names[i]] != d2[,var_names[i]], na.rm = TRUE) / length(d1[,var_names[i]]) * 100, 2))
      if(diff > 0.00) {
        df[i,"var"] <- var_names[i]
        df[i, "difference"] <- diff
      }
    }
  }
  df <- df[!is.na(df$difference),]
  colnames(df) <- c("Variable", "% difference")
  View(df)
}

# Data analysis
## Calculate differences between all matching vectors between two data frames and generate a table with rows where disagreement > 0%
difference_table(d1, d2)

## Compare individiual vectors between two data frames
compare("var1")