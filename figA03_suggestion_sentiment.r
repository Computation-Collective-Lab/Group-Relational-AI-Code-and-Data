library(ggplot2)
library(dplyr)
library(readr)

# Read the raw per-suggestion scores
df <- read_csv("./csv/VaderScoreforAISuggestions.csv", show_col_types = FALSE)

# If you want only AI-suggested conditions (usually "personal" and "relational-static"):
df2 <- df %>% filter(Condition %in% c("personal", "relational-static"))

# Compute mean and SE per condition
sumdf <- df2 %>%
  group_by(Condition) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    SE   = sd(Score,  na.rm = TRUE) / sqrt(n()),
    n    = n(),
    .groups = "drop"
  )

# Pretty labels to match your prior figures
sumdf$Condition2 <- factor(sumdf$Condition,
                           levels = c("personal", "relational-static"),
                           labels = c("Personalized", "Relational"),)

# Optional: consistent colors with your scheme
color_map <- c("Personalized" = "#1496BB", "Relational" = "#E85125")

#ggplot(dd5, aes(x = Condition2, y = Mean, fill = FillGroup)) +
  #geom_bar(stat="identity", position=position_dodge()) +
  #geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.0, position=position_dodge(0.8)) +
  #scale_fill_manual(values=color) +
  #labs(x = "Message suggestion condition", y = "Politeness per individual") +
  #theme_classic() +
  #theme(legend.position = "right")

ggplot(sumdf, aes(x = Condition2, y = Mean, fill = Condition2)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE),
                width = 0.0, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = color_map) +
  scale_y_continuous(limits = c(0, 0.45), expand = c(0, 0)) +
  labs(x = "",
       y = "Sentiment (VADER Score)") +
  theme_classic() #+
  #theme(legend.position = "right")
