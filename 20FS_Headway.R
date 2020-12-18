###font
####install.packages(c("tidyverse", "showtext"))
library(tidyverse)
library(showtext)
font_add_google("Noto Serif")
showtext.auto()
windows()
dev.off()


##import data

##remove outlier from file = "headway"
outlier <- function() {
  outlier <- boxplot(headway$headway)$stats
  headway$headway <- ifelse(headway$headway < outlier[1,] | headway$headway > outlier[5,], NA, headway$headway)

  outlier_1 <- boxplot(headway$headway_1)$stats
  headway$headway_1 <- ifelse(headway$headway_1 < outlier_1[1,] | headway$headway_1 > outlier_1[5,], NA, headway$headway_1)

  outlier_2 <- boxplot(headway$headway_2)$stats
  headway$headway_2 <- ifelse(headway$headway_2 < outlier_2[1,] | headway$headway_2 > outlier_2[5,], NA, headway$headway_2)

  outlier_3 <- boxplot(headway$headway_3)$stats
  headway$headway_3 <- ifelse(headway$headway_3 < outlier_3[1,] | headway$headway_3 > outlier_3[5,], NA, headway$headway_3)

  outlier_4 <- boxplot(headway$headway_4)$stats
  headway$headway_4 <- ifelse(headway$headway_4 < outlier_4[1,] | headway$headway_4 > outlier_4[5,], NA, headway$headway_4)

  outlier_5 <- boxplot(headway$headway_5)$stats
  headway$headway_5 <- ifelse(headway$headway_5 < outlier_5[1,] | headway$headway_5 > outlier_5[5,], NA, headway$headway_5)

  outlier_6 <- boxplot(headway$headway_6)$stats
  headway$headway_6 <- ifelse(headway$headway_6 < outlier_6[1,] | headway$headway_6 > outlier_6[5,], NA, headway$headway_6)

  outlier_7 <- boxplot(headway$headway_7)$stats
  headway$headway_7 <- ifelse(headway$headway_7 < outlier_7[1,] | headway$headway_7 > outlier_7[5,], NA, headway$headway_7)

  outlier_8 <- boxplot(headway$headway_8)$stats
  headway$headway_8 <- ifelse(headway$headway_8 < outlier_8[1,] | headway$headway_8 > outlier_8[5,], NA, headway$headway_8)

  outlier_9 <- boxplot(headway$headway_9)$stats
  headway$headway_9 <- ifelse(headway$headway_9 < outlier_9[1,] | headway$headway_9 > outlier_9[5,], NA, headway$headway_9)

  headway
}

h <- outlier()

##remove outlier from file = "headway2"
outlier <- function() {
  outlier <- boxplot(headway2$Headway)$stats
  headway2$Headway <- ifelse(headway2$Headway < outlier[1,] | headway2$Headway > outlier[5,], NA, headway2$Headway)

  headway2
}

trimmed_headway2 <- outlier()

##barplot
##op <- par(no.readonly = TRUE)
##par(mfrow = c(2,2))
##
##h1 <- as.vector(h$headway)
##h1 <- head(h1[!is.na(h1)], 8)
##
##number <- c(1:28)
##barplot(h1, names.arg = number[1 : length(h1)])
##title(main = "C1")
##
##h2 <- as.vector(h$headway_1)
##h2 <- head(h2[!is.na(h2)], 8)
##barplot(h2, names.arg = number[1 : length(h2)])
##title(main = "C2")
##
##h3 <- as.vector(h$headway_2)
##h3 <- head(h3[!is.na(h3)], 8)
##barplot(h3, names.arg = number[1 : length(h3)])
##title(main = "C3")
##
##h4 <- as.vector(h$headway_3)
##h4 <- head(h4[!is.na(h4)], 8)
##barplot(h4, names.arg = number[1 : length(h4)])
##title(main = "C4")
##
##h5 <- as.vector(h$headway_4)
##h5 <- head(h5[!is.na(h5)], 8)
##barplot(h5, names.arg = number[1 : length(h5)])
##title(main = "C5")
##
##h6 <- as.vector(h$headway_5)
##h6 <- head(h6[!is.na(h6)], 8)
##barplot(h6, names.arg = number[1 : length(h6)])
##title(main = "C6")
##
##h7 <- as.vector(h$headway_6)
##h7 <- head(h7[!is.na(h7)], 8)
##barplot(h7, names.arg = number[1 : length(h7)])
##title(main = "C7")
##
##h8 <- as.vector(h$headway_7)
##h8 <- head(h8[!is.na(h8)], 8)
##barplot(h8, names.arg = number[1 : length(h8)])
##title(main = "C8")
##
##h9 <- as.vector(h$headway_8)
##h9 <- head(h9[!is.na(h9)], 8)
##barplot(h9, names.arg = number[1 : length(h9)])
##title(main = "C9")
##
##h10 <- as.vector(h$headway_9)
##h10 <- head(h10[!is.na(h10)], 8)
##barplot(h10, names.arg = number[1 : length(h10)])
##title(main = "C10")

##################################################################
##outlier graph
boxplot(Headway~Cycle,
        data=headway2,
        main= list("Headways boxplot by Cycle", cex = 2, font = 2),
        xlab= list("Cycle number", cex = 1.6),
        ylab= list("Headway [s]", cex = 1.6),
        col="lightgray",
        border="black",
        family="Noto Serif",
        outpch=8
        )
title()
title()
title()
?title

##################################################################
###Kernel Density Plot
hist(trimmed_headway2$Headway, breaks = 15, freq = F,
     main = list("Kernel Density Plot", cex = 2, font = 2),
     xlab = list('Headway[s]', cex = 1.6),
     ylim = c(0, 0.4),
     ylab = list('Prob.', cex = 1.6),
     family = "Noto Serif")
box(which = "plot")
max.trimmed_headway <- max(trimmed_headway2$Headway, na.rm = T)
lines(density(trimmed_headway2$Headway, na.rm = T, from = 0, to = max.trimmed_headway),
      lwd = 2.5, col = "blue")
abline(v = 2.0, lty = 8 , lwd = 1.5, col = "red")
dev.off()

###Gamma Density Curve
hist(trimmed_headway2$Headway, breaks = 15, freq = F, 
     xlab = 'Headway[s]', 
     ylim = c(0, 0.4), 
     ylab = 'Probability')
mean.trimmed_headway <- mean(trimmed_headway2$Headway, na.rm = T)
var.trimmed_headway <- var(trimmed_headway2$Headway, na.rm = T)
curve(dgamma(x, shape = mean.trimmed_headway^2/var.trimmed_headway, scale = var.trimmed_headway/mean.trimmed_headway), add = T)
abline(v = 2.0, lty = 1 , lwd = 1.5, col = "red")
dev.off()

###https://chemicalstatistician.wordpress.com/2013/07/29/exploratory-data-analysis-combining-histograms-and-density-plots-to-examine-the-distribution-of-the-ozone-pollution-data-from-new-york-in-r/

###calculating MLE
mle(headway2$Headway, start = list(lambda = 5), nobs = NROW(y))

##출처: https://issactoast.com/97 [Anyone Can Learn Anything]

###violin Plot
library(ggplot2)
ggplot(data=airline_stats, aes(airline, pct_carrier_delay)) +
  ylim(0, 50) +
  geom_violin() +
  labs(x = list('Installed Point', cex = 1.6),
       y = list('Speed', cex = 1.6))

###+points
p_violin <- ggplot(data.frame(y), aes(x = 1, y = y)) + geom_violin(fill = 'grey90') +
  geom_text(data = df, aes(x, y, label = label), hjust = 0,
            size = 14/.pt, family = dviz_font_family) +
  scale_x_continuous(limits = c(0, 3.5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2.55, 3.5), expand = c(0, 0)) +
  theme_nothing()

p_points <- ggplot(data.frame(y), aes(x = 0, y = y)) + 
  geom_point(position = position_jitter(width = .4, height = 0, seed = 320)) +
  scale_x_continuous(limits = c(-1.8, .4), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-2.55, 3.5), expand = c(0, 0)) +
  theme_nothing()

plot_grid(p_points, p_violin, rel_widths = c(.65, 1), nrow = 1)

###NC200
quants <- sapply(split(NC200$Cal_Speed, list(NC200$Group)), quantile, probs = 0.85, na.rm = TRUE)
quants
q <- unname(quants)

boxplot(Cal_Speed ~ Group, data = NC200,
#        main= list("NC200 Speed Studies", cex = 2, font = 2),
        xlab= list("Installed Point", cex = 1.6),
        ylab= list("Speed [km/h]", cex = 1.6),
        col="lightgray",
        border="black",
        family="Noto Serif",
        outpch=8,
        ylim = c(0,50))

abline(h = 30, lty = 8 , lwd = 1.8, col = "red")

xlocs <- 1:7
##tickl <- 0.15

##segments(xlocs[2] - 0.4, q[2], 
##         x1 = xlocs[2] + 0.4, y1 = q[2], 
##         col = "blue", lwd = 1)

for(i in seq_len(length(xlocs))) {
  segments(xlocs[i] - 0.4, q[i], 
           x1 = xlocs[i] + 0.4, y1 = q[i], 
           col = "blue", lwd = 1.8)
}

legend("bottomleft", legend = c("Speed limit", "0.85 quantile"), bty = "n", lty = c(8, 1), lwd = 1.8, col = c("red", "blue"))

###varians
sd_speed <- sapply(split(NC200$Cal_Speed, list(NC200$Group)), sd, na.rm = TRUE)
sd_speed
