###import data
library(readr)
space_mean_speed_1 <- read_csv("space mean speed_1.csv", 
                               col_types = cols(no. = col_skip(), full = col_time(format = "%M:%S"), 
                                                lab = col_time(format = "%M:%S"), 
                                                `License plate` = col_character()))
space_mean_speed_2 <- read_csv("space mean speed_2.csv", 
                               col_types = cols(no. = col_skip(), full = col_time(format = "%M:%S"), 
                                                lab = col_time(format = "%M:%S"), 
                                                `License plate` = col_character()))

###Data Preprocessing
space_mean_speed <- cbind(space_mean_speed_1, space_mean_speed_2)
space_mean_speed <- space_mean_speed[,-c(2, 3, 5)]
space_mean_speed$time_diff <- (space_mean_speed$full.1 - space_mean_speed$full)

###Violin plot
space_mean_speed$TMS <- round(50*3.6 / as.double(space_mean_speed$time_diff), 2)
space_mean_speed$loc <- as.integer(rep(1, nrow(space_mean_speed)))
library(ggplot2)
ggplot(data = space_mean_speed, aes(x = loc, y = TMS)) +
  geom_violin(fill = 'grey90') +
  scale_y_continuous(limits = c(0, 70), n.breaks = 10) +
  scale_x_continuous(limits = c(0,2)) +
  geom_jitter(width = 0.28) +
  theme_bw() +
  labs(y ="Time Mean Speed [m/s]", x= 'Prob.')

###Box plot : result no outliers
boxplot(space_mean_speed$TMS)
rug(space_mean_speed$TMS, side = 4, col = "black", ticksize = 0.03)

###Space mean speed
nrow(space_mean_speed)*50*3.6 / as.double(sum(space_mean_speed$time_diff))

###Time mean speed
mean(space_mean_speed$TMS)

###Use if there are outliers
###outlier <- boxplot(as.double(space_mean_speed$time_diff))$stats
###space_mean_speed$time_diff <- ifelse(space_mean_speed$time_diff < outlier[1,] | space_mean_speed$time_diff > outlier[5,], NA, space_mean_speed$time_diff)
###is.na(space_mean_speed$time_diff == 0)
###space_mean_speed[!is.na(space_mean_speed$time_diff == 0), ]
###nrow(space_mean_speed[!is.na(space_mean_speed$time_diff == 0),])
###nrow(space_mean_speed[!is.na(space_mean_speed$time_diff == 0),])*50*3.6 / as.double(sum(space_mean_speed[!is.na(space_mean_speed$time_diff == 0),]$time_diff)) ### result increased because of removal of Low speed vehicle

###Plot time-space diagram
library(ggplot2)
op <- par(no.readonly = TRUE)
par(mar = (c(2,2,2,2)))

###datarange = c(as.POSIXct(min(space_mean_speed$full), format = "%S"), as.POSIXct(max(space_mean_speed$full.1), format = "%S"))
datarange = c(space_mean_speed$full[1], space_mean_speed$full.1[38])
datarange
?as.Date
?axis.Date
axis.Date(1, at = seq(datarange[1], datarange[2], by = "minute"), format = "%M %S")

plot(c(0, 15*60), c(0, 50), type ="n")

for(i in seq_len(nrow(space_mean_speed))) {
  segments(as.double(space_mean_speed[i, 1]) - 3*60, 0, 
           x1 = as.double(space_mean_speed[i, 2]) - 3*60, y1 = 50, 
           lwd = 1)
}

abline(h = c(0, 50), col = "gray", lty = 3)

###gray grid
###abline(h = , v = , col = "Gray", lty=3)