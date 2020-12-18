library(readr)
library(ggplot2)
Group_1 <- read_csv("Group_1.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_1) <- "Speed_1"
Group_1$loc <- rep(1, nrow(Group_1))

Group_2_1 <- read_csv("Group_2_1.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_2_1) <- "Speed_2_1"
Group_2_1$loc <- rep(2, nrow(Group_2_1))

Group_3 <- read_csv("Group_3.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_3) <- "Speed_3"
Group_3$loc <- rep(3, nrow(Group_3))

Group_4_1 <- read_csv("Group_4_1.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_4_1) <- "Speed_4_1"
Group_4_1$loc <- rep(4, nrow(Group_4_1))

Group_5 <- read_csv("Group_5.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_5) <- "Speed_5"
Group_5$loc <- rep(5, nrow(Group_5))

Group_6 <- read_csv("Group_6.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_6) <- "Speed_6"
Group_6$loc <- rep(6, nrow(Group_6))

Group_7 <- read_csv("Group_7.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_7) <- "Speed_7"
Group_7$loc <- rep(7, nrow(Group_7))

Group_2_2 <- read_csv("Group_2_2.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_2_2) <- "Speed_2_2"
Group_2_2$loc <- rep(2, nrow(Group_2_2))

Group_4_2 <- read_csv("Group_4_2.csv", 
                          col_types = cols(DATANO = col_skip(), 
                                           DATETIME = col_skip(), SYSID = col_skip(), 
                                           DEVID = col_skip(), COUNT = col_skip()))

#colnames(Group_4_2) <- "Speed_4_2"
Group_4_2$loc <- rep(4, nrow(Group_4_2))

#########################################################################################

###data frame all speed from Group 1 to Group 7
Speed <- rbind(Group_1,
               Group_2_1,
               Group_2_2,
               Group_3,
               Group_4_1,
               Group_4_2,
               Group_5,
               Group_6,
               Group_7
               )

###Speed only Group 7
Speed <- Group_7

###rm unnecessary
rm(Group_1, Group_2_1, Group_2_2, Group_3, Group_4_1, Group_4_2, Group_5, Group_6, Group_7)

###boxplot before removal of outliers
op <- par(no.readonly = TRUE)
par(mar = (c(2,2,2,2)))
boxplot(Speed$SPEED, outpch=8)
rug(Speed$SPEED, side = 2, col = "black", ticksize = 0.03)

###countif() outliers above then, below
outlier <- boxplot(Speed$SPEED)$stats
table(Speed$SPEED > outlier[5,])
table(Speed$SPEED < outlier[1,])

###boxplot after removal of outliers
Speed$SPEED <- ifelse(Speed$SPEED < outlier[1,] | Speed$SPEED > outlier[5,], NA, Speed$SPEED)
boxplot(Speed$SPEED)

###mean & sd
speed.mean <- mean(Speed$SPEED, na.rm = T) ####remove NA while calculating mean value
speed.sd <- sd(Speed$SPEED, na.rm = T)
speed.mean
speed.sd

###Violinplot before removal of outliers
ggplot(data = Speed, aes(x = loc, y = SPEED, group = loc)) +
  geom_violin(fill = 'grey90') +
  scale_y_continuous(limits = c(0, 70), n.breaks = 10) +
  scale_x_continuous(limits = c(0,8), n.breaks = 10) +
  theme_bw() +
  labs(y ="Spot Speed [km/h]", x= 'Period') +
  geom_hline(yintercept = speed.mean, lwd = 0.7, lty = 2, color = "red")

###countif() NA
table(is.na(Speed))

###histogram & density plot : alpha for transparency, fill for filling color
###facet_wrap(~as.factor(loc)) : array as loc, identify position = same position
ggplot(data = Speed, aes(x = SPEED)) +
  geom_histogram(aes(y = ..density..), color = "black", fill='gray85') +
  geom_density(alpha = .2, fill = 'red') +
  labs(y ="Density", x= 'Speed [km/h]') +
  geom_vline(xintercept = speed.mean, lwd = 0.7, lty = 2, color = "red") +
  theme_bw()

###Data 2nd Processing
Speed$loc <- "Equipment"
Spot_speed_Studies$loc <- "Collectors"
table(Speed$loc)
Speed <- rbind(Speed, Spot_speed_Studies)

###removal std : outlier of all
###outlier <- boxplot(Speed[Speed$loc == 8, ]$SPEED)$stats
###Speed$SPEED <- ifelse(Speed$SPEED < outlier[1,] | Speed$SPEED > outlier[5,], NA, Speed$SPEED)

###density plot comparison : All & Group7 : theme_bw() then, theme()
ggplot(data=Speed, aes(x=SPEED, fill= as.factor(loc)))+
  geom_density(alpha=.6, position='identity') +
  labs(y ="Density", x= 'Speed [km/h]') +
  theme_bw() +
  theme(legend.background = element_rect(linetype = 1, color = "#333333",fill = "#eeeeee")) +
  scale_fill_manual(values=c("#15BFC4", "#F8766C")) +
  geom_vline(aes(xintercept = speed.mean, linetype = "Equipment"), colour= 'red') +
  geom_vline(aes(xintercept = studies_speed.mean, linetype = "Collectors"), colour= 'blue') +
  scale_linetype_manual(name = "Mean", values = c(2, 2), 
                        guide = guide_legend(override.aes = list(color = c("blue", "red"))))

###search

# Use custom color palettes
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
# Use grey scale
p + scale_fill_grey() + theme_classic()

p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend