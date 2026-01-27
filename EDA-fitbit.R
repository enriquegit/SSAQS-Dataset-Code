library(ggplot2)
library(gridExtra)
library(cowplot)

# Path to root directory where the dataset resides.
ROOT_DIR = "C://bigdata/SSAQS-Dataset/"

# List of users ids.
users <- list.dirs(ROOT_DIR, recursive = F, full.names = F)


df.activity <- NULL
df.hr <- NULL
df.oxygen <- NULL
df.sleep <- NULL
df.steps <- NULL
df.stress <- NULL

# Concatenate all files.
for(u in users){
  
  f <- paste0(ROOT_DIR,u,"/activity_level.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.activity <- rbind(df.activity, df)
  }
  
  f <- paste0(ROOT_DIR,u,"/hr.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.hr <- rbind(df.hr, df)
  }
  
  f <- paste0(ROOT_DIR,u,"/oxygen.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.oxygen <- rbind(df.oxygen, df)
  }
  
  f <- paste0(ROOT_DIR,u,"/sleep.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.sleep <- rbind(df.sleep, df)
  }
  
  f <- paste0(ROOT_DIR,u,"/steps.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.steps <- rbind(df.steps, df)
  }
  
  f <- paste0(ROOT_DIR,u,"/stress.csv")
  if(file.exists(f)){
    df <- read.csv(f)
    df.stress <- rbind(df.stress, df)
  }
  
}

# Plot histograms

p1 <- ggplot(df.oxygen, aes(x = value)) +
  geom_histogram() +
  ggtitle("Oxygen saturation")+
  theme(plot.title = element_text(hjust = 0.5))


p2 <- ggplot(df.hr, aes(x = rmssd)) +
  geom_histogram() +
  ggtitle("Heart rate (rmssd)")+
  theme(plot.title = element_text(hjust = 0.5))


p3 <- ggplot(df.sleep, aes(x = deep_sleep_in_minutes)) +
  geom_histogram() +
  ggtitle("Deep sleep time (minutes)")+
  theme(plot.title = element_text(hjust = 0.5))

p32 <- ggplot(df.sleep, aes(x = overall_score)) +
  geom_histogram() +
  ggtitle("Sleep (overall score)")+
  theme(plot.title = element_text(hjust = 0.5))


p4 <- ggplot(df.steps, aes(x = steps)) +
  geom_histogram() +
  ggtitle("Steps")+
  theme(plot.title = element_text(hjust = 0.5))


p5 <- ggplot(df.stress, aes(x = STRESS_SCORE)) +
  geom_histogram() +
  ggtitle("Stress score (Fitbit)")+
  theme(plot.title = element_text(hjust = 0.5))


df <- as.data.frame(table(df.activity$level))
names(df) <- c("level","count")

p6 <- ggplot(df, aes(x = level, y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  ggtitle("Activty level")+
  theme(plot.title = element_text(hjust = 0.5))

#grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3, nrow = 3)

pdf(file = "allplots.pdf", width = 10, height = 12)
plot_grid(p1, p2, p3, p32, p4, p5, p6,
          ncol = 3, nrow = 4,
          labels = c("a)", "b)", "c)", "d)", "e)", "f)", "g)"))
dev.off()
