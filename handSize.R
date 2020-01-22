library(data.table)
library(ggplot2)
library(magrittr)
library(dplyr)
library(gridExtra)

## try-out values
my_height = 175
my_hand   = 209
my_gender = "F"
my_race = "4"

men = fread("ansur_men.txt", header = T)
women = fread("ansur_women.txt", header = T, fill = T)

if(my_gender == "M") {
        ss = men %>%
                filter(EST_HT_FEET_ANSUR88 != -9) %>%
                mutate(height_ft_cm = EST_HT_FEET_ANSUR88 * 30.48,
                       height_in_cm = EST_HT_INCH_ANSUR88 * 2.54,
                       HEIGHT_CM = height_ft_cm + height_in_cm) %>%
                select(HEIGHT_CM, HAND_LNTH)
} else {
        ss = women %>%
                filter(EST_HT_FEET_ANSUR88 != -9) %>%
                mutate(height_ft_cm = EST_HT_FEET_ANSUR88 * 30.48,
                       height_in_cm = EST_HT_INCH_ANSUR88 * 2.54,
                       HEIGHT_CM = height_ft_cm + height_in_cm) %>%
                select(HEIGHT_CM, HAND_LNTH)
}

meanHeight = mean(ss$HEIGHT_CM)
sdHeight = sd(ss$HEIGHT_CM)
meanHand = mean(ss$HAND_LNTH)
sdHand = sd(ss$HAND_LNTH)

taller = round(100 - pnorm(my_height, meanHeight, sdHeight) * 100, 0)
bigger = round(100 - pnorm(my_hand, meanHand, sdHand) * 100, 0)

g1 = ggplot(ss, aes(x = HEIGHT_CM)) +
        geom_density(fill = "steelblue4", color = "steelblue4") +
        geom_vline(aes(xintercept = my_height), color = "red", size = 1.2) +
        labs(x = "Body Height (cm)", title = "Body height distribution") +
        annotate("text", x = my_height - 2, y = 0, angle = 90, hjust = 0, color = "red", label = "You") + 
        scale_y_continuous(limits = c(0, .06)) +
        annotate("text", x = meanHeight, y = .025, label = paste0(taller, "%"), color = "white", size = 10)


g2 = ggplot(ss, aes(x = HAND_LNTH)) +
        geom_density(fill = "steelblue4", color = "steelblue4") +
        geom_vline(aes(xintercept = my_hand), color = "red", size = 1.2) +
        labs(x = "Hand length (mm)", title = "Hand length distribution") +
        annotate("text", x = my_hand - 2, y = 0, hjust = 0, angle = 90, color = "red", label = "You") + 
        scale_y_continuous(limits = c(0, .05)) +
        annotate("text", x = meanHand, y = .020, label = paste0(bigger, "%"), color = "white", size = 10)


g3 = ggplot(ss, aes(x = HEIGHT_CM, y = HAND_LNTH)) +
        geom_point() + geom_jitter() + 
        geom_smooth(method = "lm", se = FALSE, size = 1.5, linetype = "dashed", color = "blue") +
        geom_point(aes(x = my_height, y = my_hand), color = "red",  cex = 2.5) +
        geom_vline(xintercept = my_height, color = "red", linetype = "dashed", alpha = 1/2) +
        geom_hline(yintercept = my_hand, color = "red", linetype = "dashed", alpha = 1/2) +
        labs(x = "Body Height (cm)", y = "Hand Length (mm)", title = "Relationship body height and hand length")

fit1 = lm(HAND_LNTH ~ HEIGHT_CM, data = ss)
deviations = summary(fit1)$residuals

ss$deviations = deviations

meanHand_predict = summary(fit1)$coef[1] + summary(fit1)$coef[2] * my_height
my_deviation = my_hand - meanHand_predict

sdHand = sd(deviations)

g4 = ggplot(ss, aes(x = HEIGHT_CM, y = deviations)) +
        geom_point() + geom_jitter() +
        labs(x = "Body Height (cm)", y = "Deviations (mm)", title = "Deviations from the mean") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "blue", size = 1.2) +
        geom_hline(yintercept = 0 - sdHand, linetype = "dotted", color = "blue", size = 1) +
        geom_hline(yintercept = 0 + sdHand, linetype = "dotted", color = "blue", size = 1) +
        geom_point(aes(x = my_height, y = my_deviation), color = "red", cex = 2.5) +
        geom_vline(xintercept = my_height - 2, linetype = "dashed", color = "red", alpha = 1/2) +
        geom_vline(xintercept = my_height + 2, linetype = "dashed", color = "red", alpha = 1/2)

subpopulation = ss$HAND_LNTH[which(ss$HEIGHT_CM >= my_height - 1 & ss$HEIGHT_CM < my_height + 1)]


subpopulation = as.data.frame(ss$HAND_LNTH[which(ss$HEIGHT_CM >= my_height - 2 & ss$HEIGHT_CM < my_height + 2)])
names(subpopulation) = "handSize"

mean_subpopulation = mean(subpopulation$handSize)
sd_subpopulation = sd(subpopulation$handSize)
simul_data = as.data.frame(rnorm(10000, mean_subpopulation, sd_subpopulation))
names(simul_data) = "size"

biggerGivenHeight = round(100 - pnorm(my_hand, mean_subpopulation, sd_subpopulation) * 100, 0)

g5 = ggplot(subpopulation, aes(handSize)) +
        geom_density(fill = "steelblue4", color = "steelblue4") +
        geom_vline(aes(xintercept = my_hand), color = "red", size = 1.2) +
        labs(x = "Hand length (mm)", title = paste0("Subdata: P(X > ", my_hand, " | Height)")) +
        annotate("text", x = my_hand - 2, y = 0, hjust = 0, angle = 90, color = "red", label = "You") + 
        scale_y_continuous(limits = c(0, .06)) +
        annotate("text", x = mean_subpopulation, y = .025, label = paste0(biggerGivenHeight, "%"), color = "white", size = 10)

g6 = ggplot(simul_data, aes(x = size)) +
        geom_density(fill = "steelblue4", color = "steelblue4") +
        geom_vline(aes(xintercept = my_hand), color = "red", size = 1.2) +
        labs(x = "Hand length (mm)", title = paste0("Simulated data: P(X > ", my_hand, " | Height)")) +
        annotate("text", x = my_hand - 2, y = 0, angle = 90, hjust = 0, color = "red", label = "You") + 
        scale_y_continuous(limits = c(0, .055)) +
        annotate("text", x = mean_subpopulation, y = .025, label = paste0(biggerGivenHeight, "%"), color = "white", size = 10)

grid.arrange(g1, g2, g3, g4, g5, nrow = 3, ncol = 2)

