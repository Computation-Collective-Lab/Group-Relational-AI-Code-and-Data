# --- Libraries ---
library(dplyr)
library(ggplot2)
library(readr)

# --- Load LONG data ---
# Expect columns: SessionID, Condition, RoundID, NetworkDegree
d <- read_csv("./csv/SessionRoundData.csv", show_col_types = FALSE) %>%
  filter(Condition %in% c("control", "personal", "relational-static")) %>%
  mutate(
    Condition = factor(Condition, levels = c("control","personal","relational-static")),
    RoundID   = as.integer(RoundID)
  )

# --- Δ vs Round 1 within each session ---
d_diff <- d %>%
  arrange(SessionID, RoundID) %>%
  group_by(SessionID) %>%
  mutate(
    round_idx   = RoundID - 1,
    degree_diff = NetworkDegree - NetworkDegree[RoundID == 1][1]
  ) %>%
  ungroup() %>%
  mutate(
    personalized = as.integer(Condition == "personal"),
    relational   = as.integer(Condition == "relational-static")
  )

# --- Summary stats by Condition × round_idx ---
d_sum <- d_diff %>%
  group_by(Condition, round_idx) %>%
  summarise(
    n = dplyr::n(),
    mean_degree = mean(degree_diff, na.rm = TRUE),
    sd_degree   = sd(degree_diff,   na.rm = TRUE),
    se_degree   = sd_degree / sqrt(n),
    .groups = "drop"
  )

d_sum$Condition2 <- factor(
  d_sum$Condition,
  levels = c("control","personal","relational-static"),
  labels = c("No Assistance","Personalized Assistance","Relational Assistance")
)
pal <- c("No Assistance"="#8b8b83","Personalized Assistance"="#1496BB","Relational Assistance"="#E85125")

# --- Plot: Δ Network degree ---
ggplot(d_sum, aes(x = round_idx + 1, y = mean_degree, color = Condition2)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = mean_degree - se_degree, ymax = mean_degree + se_degree),
                width = 0.0, position = position_dodge(width = 0.2)) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_manual(values = pal) +
  labs(x = "Round", y = "Δ Network degree", color = "") +
  theme_classic() +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10))

# --- Per-round linear trend model (degree) ---
fit_deg <- lm(degree_diff ~ 0 + round_idx + round_idx:personalized + round_idx:relational, data = d_diff)
summary(fit_deg)
