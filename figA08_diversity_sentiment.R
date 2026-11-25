library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(paletteer)
library(ggside)
library(ggeffects)
library(emmeans)

d <- read.csv("./csv/ParticipantData.csv") %>%
  filter(Condition %in% c("control", "personal", "relational-static"))


d2 <- subset(d, RoundID != 5)

fit1 <- lmer(VADERMean ~ Condition * OpinionDifference + GroupSize + RoundID + (1|SessionID/RoomID) + (1|ParticipantID), data = d2)
summary(fit1)
confint(fit1)


pred1 <- ggpredict(fit1,terms = c("OpinionDifference [all]","Condition"))

pred1_df <- as.data.frame(pred1)

# Relabel
pred1_df$group <- factor(
  pred1_df$group,
  levels = c("control", "personal", "relational-static"),
  labels = c("None", "Individual", "Relational")
)

d2$Condition_plot <- factor(
  d2$Condition,
  levels = c("control", "personal", "relational-static"),
  labels = c("None", "Individual", "Relational")
)

# Plot
color <- c("None"="#8b8b83", "Individual"="#1496BB", "Relational"="#E85125")
ggplot(pred1_df, aes(x = x, y = predicted, color = group, fill = group, group = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1, color=NA) +
  geom_line(linewidth = 1.0) +
  labs(x = "Mean stance distance\nfrom group members", y = "Sentiment per participant", color= "AI assistance", fill="AI assistance") +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color) +
  theme_classic() +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    legend.title = element_text(size = 10)
  )
