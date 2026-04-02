library(readxl)
library(MASS)
library(lme4)
library(car)
library(ggplot2)
install.packages('nflreadr')
library(nflreadr)

combine_data_pfr <- load_combine()

max(combine_data_pfr$draft_year)

seasons <- c(2000:2024)

dim(combine_data_pfr)

combine_data_pfr$drafted[combine_data_pfr$draft_year > 0] <- 1
combine_data_pfr$drafted[is.na(combine_data_pfr$draft_year)] <- 0

draft_round <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = combine_data_pfr)

summary(draft_round)

plot(draft_round)


data <- combine_data_pfr

## Getting Graphs for Exploratory Analysis


## Number of Observations per year


library(dplyr)
library(ggplot2)


by_year <- data %>%
  group_by(season, pos) %>%
  summarise(n = n())


ggplot(by_year, aes(x = season, y = n, group = pos, colour = pos)) +
  geom_line(size = 0.5) +
  geom_point() +
  labs(title = "Trend of Participation in NFL Draft Combine Over Time (by position)", x = "Year", y = "Total Observations") +
  theme_bw() +  ylim(0,100)



par(mfrow = c(3,2))
# 40 Yard Dash Time

hist(data$forty, xlab = "Forty Time (in seconds)", main = "Histogram of 40-Yard Dash Times")

# Bench Press

hist(data$bench, xlab = "Bench Press Reps", main = "Histogram of Bench Press Max Reps")


# Vertical

hist(data$vertical, xlab = "Vertical (in In)", main = "Histogram of Max Verticals")

# Broad Jump

hist(data$broad_jump, xlab = "Broad Jump", main = "Histogram of Broad Jump")

# 40 Yard Dash Time

hist(data$cone, xlab = "Cone Drill Time", main = "Histogram of Cone Drill Time")

# Bench Press

hist(data$shuttle, xlab = "Shuttle Run Time", main = "Histogram of Shuttle Run Time")


# Adding Log Transformation
## Getting Graphs for Exploratory Analysis
par(mfrow = c(3,2))
# 40 Yard Dash Time

hist(log(data$forty), xlab = "Forty Time (in seconds)", main = "Histogram of 40-Yard Dash Times")

# Bench Press

hist(log(data$bench), xlab = "Bench Press Reps", main = "Histogram of Bench Press Max Reps")


# Vertical

hist(log(data$vertical), xlab = "Vertical (in In)", main = "Histogram of Max Verticals")

# Broad Jump

hist(log(data$broad_jump), xlab = "Broad Jump", main = "Histogram of Broad Jump")

# 40 Yard Dash Time

hist(log(data$cone), xlab = "Cone Drill Time", main = "Histogram of Cone Drill Time")

# Bench Press

hist(log(data$shuttle), xlab = "Shuttle Run Time", main = "Histogram of Shuttle Run Time")


## Modeling

combine_data_pfr$drafted[combine_data_pfr$draft_year > 0] <- 1
combine_data_pfr$drafted[is.na(combine_data_pfr$draft_year)] <- 0

draft_round <- glm(drafted~bench+vertical + cone+shuttle, family = binomial(link = "logit"), data = combine_data_pfr)

summary(draft_round)



vif(draft_round)


probability <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
classification_table

## Subsetting the data

## 2000-2004

subset_data0 <- combine_data_pfr %>% filter(2000 <= season & season < 2005)

draft_round0 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data0)
summary(draft_round0)

probability  <- predict(draft_round0, newdata = subset_data0, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)



classification_table <- table(Actual = subset_data0$drafted, Predicted = preds)
classification_table

## 2005-2009

subset_data1 <- combine_data_pfr %>% filter(2005 <= season & season < 2010)

draft_round1 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data1)
summary(draft_round1)

probability  <- predict(draft_round1, newdata = subset_data1, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = subset_data1$drafted, Predicted = preds)
classification_table

## 2010 - 2014

subset_data2 <- combine_data_pfr %>% filter(2010 <= season & season < 2015)

draft_round2 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data2)
summary(draft_round2)

probability  <- predict(draft_round2, newdata = subset_data2, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = subset_data2$drafted, Predicted = preds)
classification_table

## 2015 - 2019

subset_data3 <- combine_data_pfr %>% filter(2015 <= season & season < 2020)

draft_round3 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data3)
summary(draft_round3)


probability  <- predict(draft_round3, newdata = subset_data3, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = subset_data3$drafted, Predicted = preds)
classification_table

## 2020-2025

subset_data4 <- combine_data_pfr %>% filter(2020 <= season)

draft_round4 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data4)
summary(draft_round4)

probability  <- predict(draft_round4, newdata = subset_data4, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = subset_data4$drafted, Predicted = preds)
classification_table

## Only  Linemen


all_line <- combine_data_pfr %>% filter(pos %in% c("OL", "DL","OT", "DT", "EDGE", "DE"))


draft_round <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = all_line)

summary(draft_round)



probability  <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
classification_table


## No Linemen


no_line <- combine_data_pfr %>% filter(!(pos %in% c("OL", "DL","OT", "DT", "EDGE", "DE")))


draft_round <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = no_line)

summary(draft_round)



probability  <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
classification_table


#################################################
## Did Not End Up Using these in Report
#################################################


## Predicting Draft Position

draft_round <- glm(draft_ovr~bench+vertical+cone+shuttle, data = combine_data_pfr)
plot(draft_round)
summary(draft_round)


par(mfrow = c(2,2))

## Looking at histogram of data

hist((combine_data_pfr$draft_ovr), xlab = "Draft Pick", main = "Histogram of Draft Picks")

## log of data

hist(exp(combine_data_pfr$draft_ovr), xlab = "Draft Pick", main = "Histogram of Draft Picks")

## Adding a cubic term to try and fix the model

draft_round <- polr(as.factor(draft_ovr)~bench + vertical+cone+shuttle, data = combine_data_pfr)



summary(draft_round)

vif(draft_round)

par(mfrow = c(2,2))
plot(draft_round)


## Mixed Model ## Did not end up using

library(lme4)


random_model <- glmer(drafted~bench+vertical+cone+shuttle+(1|pos), family = binomial, data = combine_data_pfr)



summary(random_model)


probability  <- predict(random_model, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probability  > 0.5, 1, 0)


classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
classification_table



