library(dygraphs)

# Path to root directory where the dataset resides.
ROOT_DIR = "C://bigdata/SSAQS-Dataset/"

# List of users ids.
users <- list.dirs(ROOT_DIR, recursive = F, full.names = F)


# Compute the percent of answered days.
percents <- NULL
counts <- NULL

for(u in users){
  
  df <- read.csv(paste0(ROOT_DIR,u,"/daily_questions.csv"))
  
  first.timestamp <- min(df$timeStampStop)
  first.datetime <- as.POSIXct(first.timestamp, origin="1970-01-01", tz="UTC")
  
  last.timestamp <- max(df$timeStampStop)
  last.datetime <- as.POSIXct(last.timestamp, origin="1970-01-01", tz="UTC")
  
  days_difference <- floor(as.numeric(difftime(last.datetime, first.datetime, units = "days"))) + 1
  pct <- (nrow(df) / days_difference) * 100
  
  percents <- c(percents, pct)
  counts <- c(counts, nrow(df))
}

mean(percents)

summary(counts)


# Validate data ranges.

all <- NULL

for(u in users){
  df <- read.csv(paste0(ROOT_DIR,u,"/daily_questions.csv"))
  all <- rbind(all, df)
}

pdf(file = "stress-all.pdf", width = 5, height = 4)
hist(all$stress, main="Histogram of daily self-reported stress levels", xlab = "stress level")
dev.off()

summary(all)

# Plot stress over time.

u <- "1" # Select user.

df <- read.csv(paste0(ROOT_DIR,u,"/daily_questions.csv"))

df$timeStampStop <- as.POSIXct(df$timeStampStop,
                                    origin="1970-01-01", tz="UTC")

time_series <- df[, c("timeStampStop","stress")]

dygraph(time_series) %>% 
  dyAxis("y", label = "stress levels") %>%
  dyOptions(fillGraph = F, drawGrid = F) %>%
  dySeries("stress", label = "Stress Level") %>%
  dyEvent(as.Date("2025-04-12"), "Start Easter", labelLoc = "bottom", color = "red", strokePattern = "dashed") %>%
  dyEvent(as.Date("2025-04-20"), "End Easter", labelLoc = "bottom", color = "red", strokePattern = "dashed")


