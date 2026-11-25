# ============================================================
# Setup
# ============================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================
# Load data WITH precomputed politeness score
# ============================================================
df <- read.csv("./csv/utterance_minmax_composite.csv")

# Expecting at least: condition, PID, message_type, composite_mm
stopifnot(all(c("condition","pid","message_type","composite_mm") %in% names(df)))

# ------------------------------------------------------------
# Participant-level means (HumanOnly vs AllMessages)
# ------------------------------------------------------------
# HumanOnly: only human messages
human_summary <- df %>%
  filter(message_type == "human", !is.na(composite_mm)) %>%
  group_by(condition, pid) %>%
  summarise(PolitenessMean = mean(composite_mm, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type = "HumanOnly")

# AllMessages: all message types
all_summary <- df %>%
  filter(!is.na(composite_mm)) %>%
  group_by(condition, pid) %>%
  summarise(PolitenessMean = mean(composite_mm, na.rm = TRUE), .groups = "drop") %>%
  mutate(Type = "AllMessages")

final_summary <- bind_rows(human_summary, all_summary)

# ------------------------------------------------------------
# Condition-level summary (for plot error bars)
# ------------------------------------------------------------
condition_summary <- final_summary %>%
  group_by(condition, Type) %>%
  summarise(
    AveragePoliteness = mean(PolitenessMean, na.rm = TRUE),
    SEM               = sd(PolitenessMean, na.rm = TRUE) / sqrt(n()),
    n                 = n(),
    .groups = "drop"
  )

# ------------------------------------------------------------
# Build plotting frame (None / Pers. / Rel.) in your style
# ------------------------------------------------------------
cs <- condition_summary %>%
  mutate(
    Condition2 = dplyr::recode(
      condition,
      "control"           = "None",
      "personal"          = "Pers.",
      "relational-static" = "Rel."
    )
  )

df_plot <- bind_rows(
  cs %>%
    filter(Type == "HumanOnly") %>%
    transmute(
      Condition2,
      FillGroup2 = "Self-written",
      Mean = AveragePoliteness,
      SE   = SEM
    ),
  cs %>%
    filter(Type == "AllMessages", condition %in% c("personal", "relational-static")) %>%
    mutate(
      Condition2 = dplyr::recode(condition,
                                 "personal"          = "Pers.",
                                 "relational-static" = "Rel."),
      FillGroup2 = if_else(condition == "personal",
                           "All incl. personalized-AI suggested",
                           "All incl. relational-AI suggested")
    ) %>%
    transmute(
      Condition2, FillGroup2,
      Mean = AveragePoliteness, SE = SEM
    )
) %>%
  mutate(
    Condition2 = factor(Condition2, levels = c("None","Pers.","Rel.")),
    # Show All-messages first, then Self-written (you can swap the order if you like)
    FillGroup2 = factor(FillGroup2,
                        levels = c("All incl. personalized-AI suggested",
                                   "All incl. relational-AI suggested",
                                   "Self-written"))
  )

# ------------------------------------------------------------
# Palette and plot (your reference style)
# ------------------------------------------------------------
pal_fill <- c(
  "Self-written"                        = "#8b8b83",
  "All incl. personalized-AI suggested" = "#1496BB",
  "All incl. relational-AI suggested"   = "#E85125"
)

dodge2 <- position_dodge2(width = 0.8, padding = 0, preserve = "single")

ggplot(df_plot, aes(x = Condition2, y = Mean, fill = FillGroup2)) +
  geom_col(width = 0.8, position = dodge2) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE, group = FillGroup2),
    width = 0,
    position=position_dodge(width=0.8)
  ) +
  scale_fill_manual(values = pal_fill, drop = FALSE) +
  labs(x = "Message suggestion condition",
       y = "Politeness Score (Mizil et al.)",
       fill = "") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13) +
  theme(legend.position = "bottom")

# ============================================================
# Linear model contrasts vs control_HumanOnly (from final_summary)
# ============================================================
library(dplyr)
library(tidyr)
library(broom)

# Build long frame for LM
df_long <- final_summary %>%
  rename(Condition = condition,
         Measure   = Type,
         Score     = PolitenessMean) %>%
  mutate(
    Condition = factor(Condition, levels = c("control", "personal", "relational-static")),
    Group = paste(Condition, Measure, sep = "_")
  )

# Reference group: control_HumanOnly
df_long$Group <- relevel(factor(df_long$Group), ref = "control_HumanOnly")

# Fit LM
fit <- lm(Score ~ Group, data = df_long)

# Print model summary and CIs (optional)
print(summary(fit))
print(confint(fit))

# ---- Significance stars ----
sig_label <- function(p) {
  if (is.na(p)) "NA"
  else if (p < 1e-4) "****"
  else if (p < 1e-3) "***"
  else if (p < 1e-2) "**"
  else if (p < 0.05) "*" else "ns"
}

# Tidy coefficients and map to your four named contrasts
lm_tidy <- broom::tidy(fit, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    contrast = dplyr::recode(
      term,
      "Grouppersonal_HumanOnly"            = "personal_HumanOnly vs control_HumanOnly",
      "Grouprelational-static_HumanOnly"   = "relational-static_HumanOnly vs control_HumanOnly",
      "Grouppersonal_AllMessages"          = "personal_AllMessages vs control_HumanOnly",
      "Grouprelational-static_AllMessages" = "relational-static_AllMessages vs control_HumanOnly",
      .default = term
    ),
    sig = vapply(p.value, sig_label, character(1))
  ) %>%
  select(contrast, estimate, conf.low, conf.high, p.value, sig)

print(lm_tidy)

# (Optional) A compact means+SE table for the paper
summary_table <- df_long %>%
  group_by(Condition, Measure) %>%
  summarise(
    Mean = mean(Score, na.rm = TRUE),
    SE   = sd(Score, na.rm = TRUE) / sqrt(n()),
    n    = n(),
    .groups = "drop"
  ) %>%
  arrange(Condition, Measure)

print(summary_table)

# (Optional pretty labels for display)
summary_table_pretty <- summary_table %>%
  mutate(
    Condition = recode(Condition,
                       "control" = "None",
                       "personal" = "Pers.",
                       "relational-static" = "Rel."),
    Measure = recode(Measure,
                     "HumanOnly"   = "Self-written",
                     "AllMessages" = "All incl. AI-suggested")
  )
print(summary_table_pretty)