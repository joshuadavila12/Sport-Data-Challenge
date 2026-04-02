library(readxl)
library(MASS)
library(lme4)


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

# Step 1: Count observations per year
yearly_counts <- data %>%
  group_by(season, pos) %>%
  summarise(n = n())

# Step 2: Plot the results
ggplot(yearly_counts, aes(x = season, y = n, group = pos, colour = pos)) +
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


# Adding Log
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

library(car)

vif(draft_round)


probs <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
print(classification_table)

## Subsetting the data

## 2000-2004

subset_data0 <- combine_data_pfr %>% filter(2000 <= season & season < 2005)

draft_round0 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data0)
summary(draft_round0)

probs <- predict(draft_round0, newdata = subset_data0, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = subset_data0$drafted, Predicted = preds)
print(classification_table)

## 2005-2009

subset_data1 <- combine_data_pfr %>% filter(2005 <= season & season < 2010)

draft_round1 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data1)
summary(draft_round1)

probs <- predict(draft_round1, newdata = subset_data1, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = subset_data1$drafted, Predicted = preds)
print(classification_table)

## 2010 - 2014

subset_data2 <- combine_data_pfr %>% filter(2010 <= season & season < 2015)

draft_round2 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data2)
summary(draft_round2)

probs <- predict(draft_round2, newdata = subset_data2, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = subset_data2$drafted, Predicted = preds)
print(classification_table)

## 2015 - 2019

subset_data3 <- combine_data_pfr %>% filter(2015 <= season & season < 2020)

draft_round3 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data3)
summary(draft_round3)


probs <- predict(draft_round3, newdata = subset_data3, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = subset_data3$drafted, Predicted = preds)
print(classification_table)

## 2020-2025

subset_data4 <- combine_data_pfr %>% filter(2020 <= season)

draft_round4 <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = subset_data4)
summary(draft_round4)

probs <- predict(draft_round4, newdata = subset_data4, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = subset_data4$drafted, Predicted = preds)
print(classification_table)

## Only  Linemen


all_line <- combine_data_pfr %>% filter(pos %in% c("OL", "DL","OT", "DT", "EDGE", "DE"))


draft_round <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = all_line)

summary(draft_round)



probs <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
print(classification_table)


## No Linemen


no_line <- combine_data_pfr %>% filter(!(pos %in% c("OL", "DL","OT", "DT", "EDGE", "DE")))


draft_round <- glm(drafted~bench+vertical+cone+shuttle, family = binomial(link = "logit"), data = no_line)

summary(draft_round)



probs <- predict(draft_round, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
print(classification_table)


## Predicting Draft Position

draft_round <- glm(draft_ovr~bench+vertical+cone+shuttle, data = combine_data_pfr)
plot(draft_round)
summary(draft_round)


par(mfrow = c(2,2))
plot(draft_round)

## Mixed Model

library(lme4)


random_model <- glmer(drafted~bench+vertical+cone+shuttle+(1|pos), family = binomial, data = combine_data_pfr)



summary(random_model)


probs <- predict(random_model, newdata = combine_data_pfr, type = "response")
preds <- ifelse(probs > 0.5, 1, 0)

# Method 1: Base R table
classification_table <- table(Actual = combine_data_pfr$drafted, Predicted = preds)
print(classification_table)


## Adding College Stats 

library(cfbfastR)

cfb_recruiting <- cfbd_stats_season_player(year = seasons)


for (i in 2001:2025){
  cfb_recruiting_add <- cfbd_stats_season_player(year = i)
  cfb_recruiting <- bind_rows(cfb_recruiting, cfb_recruiting_add)
}

min(cfb_recruiting$year)

combine_cfb <- left_join(combine_data_pfr, cfb_recruiting, by = c("player", "school", "season"))


names(combine_data_pfr)[names(combine_data_pfr) == "player_name"] <- "player"


names(cfb_recruiting)[names(cfb_recruiting) == "team"] <- "school"
names(cfb_recruiting)[names(cfb_recruiting) == "year"] <- "season"
names(cfb_recruiting)[names(cfb_recruiting) == "season"] <- "old_season"
cfb_recruiting$season <- cfb_recruiting$old_season + 1

## For QBs



combine_cfb <- left_join(combine_data_pfr, cfb_recruiting, by = c("player", "school", "season"))

test <- combine_cfb %>% filter(season == 2015)
View(test)



## For QBs

quarterbacks <- combine_cfb %>% filter(position == "QB")
colnames(quarterbacks)

qb_full <- lm(draft_ovr~forty+bench+vertical+broad_jump+cone+shuttle+passing_completions+passing_att+passing_pct+passing_yds+passing_td+passing_int+passing_ypa+rushing_car+rushing_yds+rushing_td+rushing_ypc+rushing_long, data = quarterbacks)
summary(qb_linear)

library(MASS)
# Fit the full model


# Fit the intercept-only model
null_model <- lm(draft_ovr ~ 1, data = quarterbacks)

# Perform stepwise selection
final_model <- stepAIC(null_model, 
                       direction = "both", 
                       scope = list(lower = null_model, upper = qb_full))



## team stats


team_stats <- load_team_stats(seasons= TRUE, summary_level = "reg+post")

team_stats$


team_combine <- left_join(combine_data_pfr, team_stats)


