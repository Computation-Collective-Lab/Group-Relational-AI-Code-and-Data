library(ggplot2)
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(tidyverse)

d <- read.csv("./csv/SessionRoundData.csv") %>%
  filter(Condition %in% c("control", "personal", "relational-static"))
d <- d %>%
  mutate(
    Condition = as.factor(Condition),
    RoundID   = as.integer(RoundID)
  )

d2_diff <- d %>%
  arrange(SessionID, RoundID) %>%
  group_by(SessionID) %>%
  mutate(
    round_idx          = RoundID - 1,
    assortativity_diff = Assortativity - Assortativity[RoundID == 1][1],
    between_diff       = BetweenDistance       - BetweenDistance[RoundID == 1][1],
    within_diff        = WithinDistance        - WithinDistance[RoundID == 1][1]
  ) %>%
  ungroup() %>%
  mutate(
    personalized = as.integer(Condition == "personal"),
    relational   = as.integer(Condition == "relational-static")
  )

d3 <- d2_diff %>%
  group_by(Condition, round_idx) %>%
  summarise(
    n            = dplyr::n(),
    mean_assort  = mean(assortativity_diff, na.rm = TRUE),
    sd_assort    = sd(assortativity_diff,   na.rm = TRUE),
    se_assort    = sd_assort / sqrt(n),
    mean_within  = mean(within_diff,  na.rm = TRUE),
    sd_within    = sd(within_diff,    na.rm = TRUE),
    se_within    = sd_within / sqrt(n),
    mean_between = mean(between_diff, na.rm = TRUE),
    sd_between   = sd(between_diff,   na.rm = TRUE),
    se_between   = sd_between / sqrt(n),
    .groups = "drop"
  )

d3$Condition2 <- factor(d3$Condition, levels=c("control", "personal", "relational-static"), labels = c("None", "Personalized", "Relational" ))
color <- c("None"="#8b8b83", "Personalized"="#1496BB", "Relational"="#E85125")

# Figure 3a

ggplot(d3, aes(x = round_idx+1, y = mean_assort, color = Condition2)) +
  geom_hline(yintercept = 0, lty=2) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_assort - se_assort, ymax = mean_assort + se_assort),
                width = 0.0, position = position_dodge(width = 0.2)) +
  #scale_y_continuous(limits = c(-0.3, 0.03)) +
  scale_color_manual(values = color) +
  labs(x = "Round", y = "Δ Stance assortativity", color = "Message suggestions") +
  theme_classic() +
  theme(
    #legend.position = "none",
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),   # shrink x-axis label
    axis.title.y = element_text(size = 10),   # shrink y-axis label
  )


fit1 <- lm(assortativity_diff ~ 0 + round_idx + round_idx:personalized + round_idx:relational, data = d2_diff)
summary(fit1)

fit2 <- lm(within_diff ~ 0 + round_idx + round_idx:personalized + round_idx:relational, data = d2_diff)
summary(fit2)

fit3 <- lm(between_diff ~ 0 + round_idx + round_idx:personalized + round_idx:relational, data = d2_diff)
summary(fit3)


slopes <- data.frame(
  Condition = c("None", "Personalized", "Relational"),
  Slope = c(
    coef(fit1)["round_idx"],
    coef(fit1)["round_idx"] + coef(fit1)["round_idx:personalized"],
    coef(fit1)["round_idx"] + coef(fit1)["round_idx:relational"]
  )
)
slopes

vc <- vcov(fit1)

# Figure 3b -- stance change per round

# compute SE for each slope (linear combination of coefficients)
slopes$SE <- c(
  sqrt(vc["round_idx","round_idx"]),
  sqrt(vc["round_idx","round_idx"] + vc["round_idx:personalized","round_idx:personalized"] +
         2*vc["round_idx","round_idx:personalized"]),
  sqrt(vc["round_idx","round_idx"] + vc["round_idx:relational","round_idx:relational"] +
         2*vc["round_idx","round_idx:relational"])
)

color2 <- c("None"="#8b8b83", "Personalized"="#1496BB", "Relational"="#E85125")
color3 <- c("None"="#8b8b83", "Pers."="#1496BB", "Rel."="#E85125")
slopes$Condition2 <- factor(slopes$Condition, levels = c("None", "Personalized", "Relational"), labels = c("None", "Pers.", "Rel."))

ggplot(slopes, aes(x = Condition2, y = Slope, fill = Condition2)) +
  geom_col(width = 0.75) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = Slope - SE, ymax = Slope + SE), width = 0.0) +
  scale_fill_manual(values = color3) + # None, Personalized, Relational
  scale_y_continuous(limits = c(-0.055, 0.055)) +
  labs(
    x = "",
    y = "Estimated per-round change"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# Figure 4a

ggplot(d3, aes(x = round_idx+1, y = mean_within, color = Condition2)) +
  geom_hline(yintercept = 0, lty=2) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_within - se_within, ymax = mean_within + se_within),
                width = 0.0, position = position_dodge(width = 0.2)) +
  #scale_y_continuous(limits = c(2, 2.8)) +
  scale_color_manual(values = color) +
  labs(x = "Round", y = "Δ Within-group distance", color = "") +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )

# Figure 4b

ggplot(d3, aes(x = round_idx+1, y = mean_between, color = Condition2)) +
  geom_hline(yintercept = 0, lty=2) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_between - se_between, ymax = mean_between + se_between),
                width = 0.0, position = position_dodge(width = 0.2)) +
  #scale_y_continuous(limits = c(2, 2.8)) +
  scale_color_manual(values = color) +
  labs(x = "Round", y = "Δ Between-group distance", color = "") +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
