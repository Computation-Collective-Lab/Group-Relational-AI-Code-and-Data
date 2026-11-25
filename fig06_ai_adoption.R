library(ggplot2)
library(tidyr)
library(dplyr)
library(lemon)

d <- read.csv("./csv/ParticipantData.csv")%>%
  filter(Condition %in% c("control", "personal", "relational-static"))

d$AIRate <- (d$TotalCopiedSuggestion + d$TotalEditedSuggestion) / d$TotalMessageCount
d$RoundMessages <- d$TotalMessageCount /  d$CountofRoundParticipated
dd <- subset(d, RoundID==1 & Condition != "control")

dd$Condition2 <- factor(dd$Condition, levels=c("control", "personal", "relational-static"), labels = c("None", "Individual assistance", "Relational assistance" ))
color2 <- c("No"="#8b8b83", "Individual assistance"="#1496BB", "Relational assistance"="#E85125")

ggplot(dd, aes(x = AIRate, y = RoundMessages, color = Condition2)) +
  geom_point(size=0.7, alpha=0.8) + 
  scale_color_manual(values=color2) + 
  facet_rep_grid(~ Condition2) +
  labs(x = "Proportion of AI-generated messages", y = "Messages per\nparticipant / round") +
  scale_x_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  theme_classic() +
  theme(
    legend.position = "none",
    legend.key=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="transparent", linetype=0),
    #strip.text=element_text(face='bold'),
    strip.text=element_text(size=11),
    axis.line = element_line(color = "black")
  )


cor_results <- dd %>%
  group_by(Condition) %>%
  summarise(
    n = n(),
    cor = cor(AIRate, RoundMessages, method = "spearman", use = "complete.obs"),
    p_value = cor.test(AIRate, RoundMessages, method = "spearman")$p.value
  )




ggplot(dd, aes(x = AIRate, fill = Condition2)) +
  geom_histogram(binwidth = 0.10, boundary = 0, closed = "left", color="white") +
  facet_rep_grid(~ Condition2) +
  labs(x = "Proportion of AI-generated messages", y = "Number of participants") +
  scale_x_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values=color2) + 
  theme_classic() +
  theme(
    legend.position = "none",
    legend.key=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="transparent", linetype=0),
    #strip.text=element_text(face='bold'),
    strip.text=element_text(size=11),
    axis.line = element_line(color = "black")
  )

dd %>%
  distinct(ParticipantID, Condition2, AIRate) %>%
  group_by(Condition2) %>%
  summarise(
    N = n(),
    Percent_zero = mean(AIRate == 0, na.rm = TRUE) * 100,
    Percent_less_25 = mean(AIRate < 0.25, na.rm = TRUE) * 100,
    Percent_less_50 = mean(AIRate < 0.5, na.rm = TRUE) * 100,
    Median_AIRate = median(AIRate, na.rm = TRUE),
    Mean_AIRate = mean(AIRate, na.rm = TRUE)
  )