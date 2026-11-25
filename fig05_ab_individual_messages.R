library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)

d <- read.csv("./csv/ParticipantData.csv") %>%
  filter(Condition %in% c("control", "personal", "relational-static"))

dd <- d %>%
  filter(RoundID != 5) %>%   # exclude RoundID == 5
  group_by(SessionID, Condition, ParticipantID) %>%
  summarise(
    WeightedVADER      = sum(VADERMean * NumMsgs, na.rm = TRUE) / sum(NumMsgs, na.rm = TRUE),
    WeightedVADERHuman = sum(VADERMeanHuman * NumMsgsHuman, na.rm = TRUE) / sum(NumMsgsHuman, na.rm = TRUE),
    MMsgs              = mean(NumMsgs, na.rm = TRUE),
    MMsgsHuman         = mean(NumMsgsHuman, na.rm = TRUE),
    .groups = "drop"
  )

dd2 <- dd %>%
  group_by(Condition) %>%
  summarise(
    MeanMsg      = mean(MMsgs, na.rm = TRUE),
    SEMsg        = sd(MMsgs, na.rm = TRUE) / sqrt(n()),
    MeanMsgHuman = mean(MMsgsHuman, na.rm = TRUE),
    SEMsgHuman   = sd(MMsgsHuman, na.rm = TRUE) / sqrt(n()),
    MeanVADER      = mean(WeightedVADER, na.rm = TRUE),
    SEVADER        = sd(WeightedVADER, na.rm = TRUE) / sqrt(n()),
    MeanVADERHuman = mean(WeightedVADERHuman, na.rm = TRUE),
    SEVADERHuman   = sd(WeightedVADERHuman, na.rm = TRUE) / sqrt(n()),
    n = n(),
    .groups = "drop"
  )

dd3 <- dd2 %>%
  transmute(
    Condition,
    Mean_All   = MeanVADER,       SE_All   = SEVADER,
    Mean_Human = MeanVADERHuman,  SE_Human = SEVADERHuman
  ) %>%
  pivot_longer(cols = c(Mean_All, Mean_Human),
               names_to = "Measure", values_to = "Mean") %>%
  mutate(
    SE = ifelse(Measure == "Mean_All", SE_All, SE_Human),
    Measure = recode(Measure,
                     "Mean_All"   = "All Messages",
                     "Mean_Human" = "Human Messages Only")
  )

# Keep only Self-written for control (None)
dd4 <- dd3 %>%
  filter(!(Condition == "control" & Measure != "Human Messages Only"))

# Map to FillGroup (base) and then FillGroup2 (final labels/order)
dd5 <- dd4 %>%
  mutate(
    FillGroup = case_when(
      Condition == "control" ~ "Self-written", 
      Condition == "personal" & Measure == "Human Messages Only" ~ "Self-written",
      Condition == "relational-static" & Measure == "Human Messages Only" ~ "Self-written",
      Condition == "personal" & Measure == "All Messages" ~ "Pers.",
      Condition == "relational-static" & Measure == "All Messages" ~ "Rel."
    ),
    Condition2 = factor(Condition,
                        levels = c("control", "personal", "relational-static"),
                        labels = c("None", "Pers.", "Rel."))
  )

# FINAL fill labels **and order** (All-including first, Self-written last)
dd5$FillGroup2 <- factor(
  dd5$FillGroup,
  levels = c("Pers.", "Rel.", "Self-written"),
  labels = c("All incl. personalized-AI suggested",
             "All incl. relational-AI suggested",
             "Self-written")
)

# Single, shared palette used in BOTH plots
pal_fill <- c(
  "All incl. personalized-AI suggested" = "#1496BB",
  "All incl. relational-AI suggested"   = "#E85125",
  "Self-written"                        = "#8b8b83"
)

# -----------------------------
# PLOT A: Politeness per individual (Weighted VADER)
# -----------------------------
polite_none   <- dd5 %>% filter(Condition2 == "None")
polite_others <- dd5 %>% filter(Condition2 != "None")

# Figure 5a

ggplot() +
  # Thin bars for None
  geom_col(
    data = polite_none,
    aes(x = Condition2, y = Mean, fill = FillGroup2),
    width = 0.45
  ) +
  geom_errorbar(
    data = polite_none,
    aes(x = Condition2, ymin = Mean - SE, ymax = Mean + SE),
    width = 0.0
  ) +
  # Normal bars for Pers./Rel. with dodging
  geom_col(
    data = polite_others,
    aes(x = Condition2, y = Mean, fill = FillGroup2),
    width = 0.8,
    position = position_dodge(width = 0.8)
  ) +
  geom_errorbar(
    data = polite_others,
    aes(x = Condition2, ymin = Mean - SE, ymax = Mean + SE, group = FillGroup2),
    width = 0.0,
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(values = pal_fill) +
  labs(x = "Message suggestion condition", y = "Sentiment per participant", fill = "") +
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() +
  theme(legend.position = "bottom")

# -----------------------------
# PLOT B: Messages per individual per round
# Use SAME colors; None thinner (width 0.45), others normal (0.8)
# -----------------------------
dd3_msg <- dd2 %>%
  select(Condition, MeanMsg, SEMsg, MeanMsgHuman, SEMsgHuman) %>%
  pivot_longer(cols = starts_with("Mean"),
               names_to = "Measure", values_to = "Mean") %>%
  mutate(
    SE = ifelse(Measure=="MeanMsg", SEMsg, SEMsgHuman),
    Measure = recode(Measure,
                     "MeanMsg" = "All Messages",
                     "MeanMsgHuman" = "Human Messages Only")
  )

dd4_msg <- dd3_msg %>%
  filter(!(Condition == "control" & Measure != "Human Messages Only"))

dd5_msg <- dd4_msg %>%
  mutate(
    FillGroup2 = case_when(
      Condition == "control" ~ "Self-written",
      Condition == "personal" & Measure == "Human Messages Only" ~ "Self-written",
      Condition == "relational-static" & Measure == "Human Messages Only" ~ "Self-written",
      Condition == "personal" & Measure == "All Messages" ~ "All incl. personalized-AI suggested",
      Condition == "relational-static" & Measure == "All Messages" ~ "All incl. relational-AI suggested"
    ),
    Condition2 = factor(Condition,
                        levels=c("control","personal","relational-static"),
                        labels=c("None","Pers.","Rel."))
  ) %>%
  mutate(FillGroup2 = factor(FillGroup2, levels=levels(dd5$FillGroup2)))

# Figure 5A

msgs_none   <- dd5_msg %>% filter(Condition2=="None")
msgs_others <- dd5_msg %>% filter(Condition2!="None")

ggplot() +
  geom_col(
    data = msgs_none,
    aes(x=Condition2, y=Mean, fill=FillGroup2),
    width=0.45
  ) +
  geom_errorbar(
    data = msgs_none,
    aes(x=Condition2, ymin=Mean-SE, ymax=Mean+SE),
    width=0.0
  ) +
  geom_col(
    data = msgs_others,
    aes(x=Condition2, y=Mean, fill=FillGroup2),
    width=0.8, position=position_dodge(width=0.8)
  ) +
  geom_errorbar(
    data = msgs_others,
    aes(x=Condition2, ymin=Mean-SE, ymax=Mean+SE, group=FillGroup2),
    width=0.0, position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values=pal_fill) +
  labs(x="Message suggestion condition", y="Messages per individual / round", fill="") +
  scale_y_continuous(expand=c(0,0)) +
  theme_classic() +
  theme(legend.position="bottom")

# -----------------------------
# Regression (unchanged)
# -----------------------------
dd_long <- dd %>%
  pivot_longer(
    cols = c(MMsgs, MMsgsHuman),
    names_to = "Measure",
    values_to = "Score"
  ) %>%
  mutate(Condition = factor(Condition, 
                            levels = c("control", "personal", "relational-static")),
         Group = paste(Condition, Measure, sep = "_"))

dd_long$Group <- relevel(factor(dd_long$Group), ref = "control_MMsgs")
dd_long <- subset(dd_long, Group != "control_MMsgsHuman")

fit <- lm(Score ~ Group, data = dd_long)
summary(fit)
confint(fit)
