library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)

d <- read.csv("./csv/RoomData.csv") %>%
  filter(Condition %in% c("control", "personal", "relational-static"))

d1 <- d %>%
  group_by(Condition, RoundID) %>%
  summarise(
    n = dplyr::n(),
    mean_count = mean(NumMsgs,  na.rm = TRUE),
    mean_vader = mean(VADERMean,  na.rm = TRUE),
    sd_count   = sd(NumMsgs,  na.rm = TRUE),
    se_count   = sd_count / sqrt(n),
    sd_vader   = sd(VADERMean,  na.rm = TRUE),
    se_vader   = sd_vader / sqrt(n),
    .groups = "drop"
  )

d1$Condition2 <- factor(d1$Condition, levels=c("control", "personal", "relational-static"), labels = c("None", "Personalized", "Relational" ))
color <- c("None"="#8b8b83", "Personalized"="#1496BB", "Relational"="#E85125")

d2 <- subset(d1, RoundID != 5)

# Figure 5C

ggplot(d2, aes(x = RoundID, y = mean_count, color = Condition2)) +
  geom_line(position = position_dodge(width = 0.2))+
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_count - se_count, ymax = mean_count + se_count),
                width = 0.0, position = position_dodge(width = 0.2)) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5), limits = c(1, 5.2)) +
  #scale_y_continuous(limits = c(0, 27)) +
  scale_color_manual(values = color) +
  labs(x = "Round", y = "Messages per group", color = "") +
  theme_classic()

# Figure 5D

ggplot(d2, aes(x = RoundID, y = mean_vader, color = Condition2)) +
  geom_line(position = position_dodge(width = 0.2))+
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_vader - se_vader, ymax = mean_vader + se_vader),
                width = 0.0, position = position_dodge(width = 0.2)) +
  scale_y_continuous(breaks = c(0.1, 0.2, 0.3)) +
  scale_color_manual(values = color) +
  labs(x = "Round", y = "Sentiment per group", color = "") +
  theme_classic()



d3 <- d2 %>%
  group_by(Condition) %>%
  summarise(
    mean_count2 = mean(mean_count),
    mean_vader2 = mean(mean_vader)  
  )

d4 <- subset(d, RoundID != 5)
fit1 <- lmer(NumMsgs ~ Condition + (1 | SessionID/RoomID), data = d4)
summary(fit1)

fit2 <- lmer(NumMsgs ~ RoundID*Condition + (1 | SessionID/RoomID), data = d4)
summary(fit2)


fit3 <- lmer(VADERMean ~ Condition + (1 | SessionID/RoomID), data = d4)
summary(fit3)

fit4 <- lmer(VADERMean ~ RoundID*Condition + (1 | SessionID/RoomID), data = d4)
summary(fit4)

