library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(paletteer)
library(ggside)

d <- read.csv("./csv/ParticipantData.csv") %>%
  filter(Condition %in% c("control", "personal", "relational-static"))


d1 <- d %>%
  arrange(SessionID, ParticipantID, RoundID) %>%
  group_by(SessionID, ParticipantID) %>%
  mutate(
    Next_GroupSize = lead(GroupSize),
    Next_OpinionDifference = lead(OpinionDifference),
    Delta_GroupSize = Next_GroupSize - GroupSize,
    Delta_OpinionDifference = Next_OpinionDifference - OpinionDifference,
    NextRoomID = lead(RoomID),                    
    ChangedRoom = ifelse(!is.na(NextRoomID) & RoomID != NextRoomID, 1, 0)
  ) %>%
  ungroup()

d1 <- d1 %>%
  filter(!is.na(Next_GroupSize), !is.na(Next_OpinionDifference))

fit1 <- lmer(
  Next_OpinionDifference ~ OpinionDifference + VADERMean * NumMsgs + (1|RoomID/ParticipantID),
  data = d1
)
summary(fit1)


d2 <- d1 %>%
  group_by(ParticipantID) %>%
  mutate(
    change_rate = mean(ChangedRoom),
  )
mean(d2$change_rate)

aggregate(change_rate ~ Condition, data = d2, mean)
anova_result <- aov(change_rate ~ Condition, data = d2)
summary(anova_result)

fit_condition <- glmer(ChangedRoom ~ Condition + RoundID + (1 | ParticipantID), family = binomial,data = d1)
summary(fit_condition)



anova_result2 <- aov(change_rate ~ Condition * RoundID, data = d2)
summary(anova_result2)

d1$Round_c <- d1$RoundID - min(d1$RoundID)
fit2 <- glmer(ChangedRoom ~ Condition * Round_c + (1 | ParticipantID), family = binomial, data = d1)
summary(fit2)
exp(fixef(fit2, parm = "Round_c"))
exp(confint(fit2, parm = "Round_c"))
exp(confint(fit2, parm = c("Conditionpersonal", "Conditionrelational-static"), method = "Wald"))


plot_data <- d1 %>%
  group_by(Condition, RoundID) %>%
  summarise(
    switch_rate = mean(ChangedRoom),
    n = n(),
    se = sqrt(switch_rate * (1 - switch_rate) / n),
    ci = 1.96 * se,
    .groups = "drop"
  )

plot_data$Condition2 <- factor(plot_data$Condition, levels = c("control", "personal", "relational-static"), labels = c("None", "Individual", "Relational"))

color2 <- c("None"="#8b8b83", "Individual"="#1496BB", "Relational"="#E85125")

ggplot(plot_data, aes(x = RoundID, y = switch_rate, color = Condition2, group = Condition2)) +
  geom_line(linewidth = 0.8, position = position_dodge(width = 0.2)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = pmax(0, switch_rate - ci), ymax = pmin(1, switch_rate + ci)), width = 0.0, position = position_dodge(width = 0.2)) +
  #scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = color2) + 
  labs(x = "Round", y = "Participants switching groups", color = "AI assistance") +
  theme_classic(base_size = 12)