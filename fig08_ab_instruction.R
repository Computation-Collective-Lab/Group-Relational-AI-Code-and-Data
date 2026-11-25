library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)
library(effectsize)


d <- read.csv("./csv/SessionData.csv")

d %>%
  group_by(Condition) %>%
  summarize(total = sum(NumPlayers, na.rm = TRUE))


d <- read.csv("./csv/SessionRoundData.csv")

d0 <- subset(d, RoundID==1)
fit <- aov(Assortativity ~ Condition, data = d0)
summary(fit)
eta_squared(fit)

pd <- d %>%
  filter(Condition %in% c("control", "relational-static", "control-instruction")) %>%
  mutate(
    RoundID = as.numeric(RoundID)
  )

pd$Condition2 <- factor(pd$Condition, levels=c("control", "control-instruction", "relational-static"), labels = c("None",  "Instruction", "AI suggestions"))
color <- c("None"="#8b8b83", "AI suggestions"="#E85125", "Instruction"="#59A14F")

ggplot(pd, aes(x = RoundID, y = AssortativityDelta, color = Condition2, group = Condition2)) +
  geom_hline(yintercept = 0, lty=2) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.2)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.0, position = position_dodge(width = 0.2)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.2)) +
  scale_color_manual(values = color) +
  labs(
    x = "Round",
    y = "Stance Assortativity change",
    color = "Relational framing"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),   # shrink x-axis label
    axis.title.y = element_text(size = 10),   # shrink y-axis label
  )


pd$control <- as.integer(pd$Condition == "control")
pd$ai  <- as.integer(pd$Condition == "relational-static")
fit1 <- lm(AssortativityDelta ~ 0 + RoundID + RoundID:control + RoundID:ai, data = pd)
summary(fit1)
confint(fit1)

pd$control <- as.integer(pd$Condition == "control")
pd$ai  <- as.integer(pd$Condition == "relational-static")
fit1 <- lm(AssortativityDelta ~ 0 + RoundID + RoundID:control + RoundID:ai, data = pd)
summary(fit1)
confint(fit1)


# -----------------------------
# Figure 8B: Sentiment per participant
# -----------------------------
d_sent <- read.csv("./csv/ParticipantData.csv") %>%
  filter(RoundID != 5,
         Condition %in% c("control", "control-instruction", "relational-static")) %>%
  group_by(SessionID, Condition, ParticipantID) %>%
  summarise(
    WeightedVADER = sum(VADERMean * NumMsgs, na.rm = TRUE) /
                    sum(NumMsgs, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(is.finite(WeightedVADER)) %>%
  mutate(Condition2 = factor(Condition,
    levels = c("control", "control-instruction", "relational-static"),
    labels = c("None", "Instruction", "AI")))

# Participant-level means and standard errors
sent_summary <- d_sent %>%
  group_by(Condition2) %>%
  summarise(Mean = mean(WeightedVADER),
            SE = sd(WeightedVADER) / sqrt(n()),
            n = n(), .groups = "drop")
print(sent_summary)

# Unadjusted pairwise contrasts from the participant-level regression
fit_sent <- lm(WeightedVADER ~ Condition2, data = d_sent)
summary(fit_sent)
confint(fit_sent)
d_sent$ConditionInstruction <- relevel(d_sent$Condition2, ref = "Instruction")
fit_sent_instruction <- lm(WeightedVADER ~ ConditionInstruction, data = d_sent)
summary(fit_sent_instruction)
confint(fit_sent_instruction)
p1 <- coef(summary(fit_sent))["Condition2Instruction", "Pr(>|t|)"]
p2 <- coef(summary(fit_sent_instruction))["ConditionInstructionAI", "Pr(>|t|)"]
p_label <- function(p) if (p < 0.001) "italic(p) < 0.001" else sprintf("italic(p) == %.3f", p)

sent_colors <- c("None" = "#8b8b83", "Instruction" = "#59A14F", "AI" = "#E85125")
fig08b <- ggplot(sent_summary, aes(x = Condition2, y = Mean, fill = Condition2)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0) +
  annotate("segment", x = c(1, 2), xend = c(2, 3),
           y = c(0.184, 0.289), yend = c(0.184, 0.289)) +
  annotate("segment", x = c(1, 2, 2, 3), xend = c(1, 2, 2, 3),
           y = c(0.171, 0.171, 0.276, 0.276),
           yend = c(0.184, 0.184, 0.289, 0.289)) +
  annotate("text", x = c(1.5, 2.5), y = c(0.205, 0.310),
           label = c(p_label(p1), p_label(p2)), parse = TRUE, size = 3.5) +
  scale_fill_manual(values = sent_colors) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.1),
                     limits = c(0, 0.34), expand = c(0, 0)) +
  labs(x = "Relational framing", y = "Sentiment per participant") +
  theme_classic() +
  theme(legend.position = "none")
print(fig08b)
