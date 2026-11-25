# AI-Mediated Communication Can Reduce Opinion-Based Network Segregation

Analysis code and processed data accompanying the article by Faria Huq, Elijah L. Claggett, and Hirokazu Shirado, accepted in *Computers in Human Behavior*.

This repository contains R scripts for statistical analyses and figure panels from the study, together with the processed CSV inputs required by those scripts. The study includes 711 participants across 80 experimental sessions (20 sessions per condition). The archive is intended to support inspection and reproduction of the included analyses, not to reproduce the original experimental platform or text-scoring pipelines.

## Getting started

1. Download and extract the archive.
2. Open R or RStudio and set the working directory to the extracted `Group-Relational-AI-Analysis-R/` directory (the directory containing the R scripts and `csv/`).
3. Install the required packages once:

```r
packages <- c(
  "ggplot2", "dplyr", "tidyr", "readr", "tidyverse",
  "lme4", "lmerTest", "emmeans", "effectsize", "broom",
  "lemon", "paletteer", "ggside", "ggeffects"
)
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
```

4. Run the script for the figure of interest. For example, in RStudio open `fig07_b_group_diversity.R` and run it, or use:

```r
source("fig07_b_group_diversity.R", print.eval = TRUE)
```

Run each script in a fresh R session when possible: the scripts use common variable names. Plots appear in the graphics device (the Plots pane in RStudio); model summaries and other numerical outputs are printed to the console. Some scripts generate multiple plots, so use the plot-history arrows to inspect earlier panels. The supplied scripts do not automatically save all figures to disk. To save a multi-panel script's successive plots to a multipage PDF, for example:

```r
pdf("figure05_panels.pdf", width = 6, height = 4)
source("fig05_ab_individual_messages.R", print.eval = TRUE)
dev.off()
```

For troubleshooting, record `sessionInfo()` along with the output. Exact original R and package versions are not available in this archive; minor graphical and statistical differences can occur across versions.

## Figure-to-script index

| Figure | Description | Script | Input data (in `csv/`) |
|---|---|---|---|
| 3, 4 | Stance assortativity, estimated per-round change, within- and between-group stance distance | `fig03_04_assortativity.R` | `SessionRoundData.csv` |
| 5A, 5B | Individual message volume and sentiment | `fig05_ab_individual_messages.R` | `ParticipantData.csv` |
| 5C, 5D | Group message volume and sentiment | `fig05_cd_group_messages.R` | `RoomData.csv` |
| 6A, 6B | AI adoption distribution and association with message production | `fig06_ai_adoption.R` | `ParticipantData.csv` |
| 7B | Predicted change in within-group stance distance by message volume and sentiment | `fig07_b_group_diversity.R` | `RoomData.csv` |
| 8A, 8B | Direct instruction versus relational AI: assortativity and sentiment | `fig08_ab_instruction.R` | `SessionData.csv`, `SessionRoundData.csv`, `ParticipantData.csv` |
| A2A | Number of conversation partners (network degree) | `figA02_a_network_degree.R` | `SessionRoundData.csv` |
| A2B | Number of conversation groups | `figA02_b_group_count.R` | `RoomData.csv` |
| A3 | Sentiment of AI-generated suggestions | `figA03_suggestion_sentiment.r` | `VaderScoreforAISuggestions.csv` |
| A4A | Politeness of messages | `figA04_a_politeness.r` | `utterance_minmax_composite.csv` |
| A4B, A4C | Toxicity and respectfulness | `figA04_bc_perspective.R` | `PerspectiveScores.csv` |
| A5 | Group-switching rates | `figA05_group_switching.R` | `ParticipantData.csv` |
| A6 | AI adoption and change in individual stance distance | `figA06_ai_usage_diversity.R` | `ParticipantData.csv` |
| A8 | Individual sentiment as a function of stance distance | `figA08_diversity_sentiment.R` | `ParticipantData.csv` |

Figures 1, 2, and 7A were prepared in PowerPoint rather than R. Figure A1 contains experimental interface illustrations. The Figure A7 path-analysis script is not included in this archive. The index describes the supplied analyses; it is not a claim that every manuscript table, annotation, or panel is exactly regenerated.

In `fig05_ab_individual_messages.R`, sentiment (5B) is plotted before message volume (5A). In `fig06_ai_adoption.R`, the scatterplot (6B) is plotted before the histogram (6A). Some older labels use “Personalized” or “Pers.” for the individual-assistance condition.

## Processed data

Only variables referenced by the included scripts are distributed. Each retained file preserves its original rows, row ordering, and retained field values. Raw message content, AI suggestion text, timestamps, unused survey fields, and intermediate text-scoring features are excluded. `AllRawMessages.csv` and `ProvidedSuggestions.csv` are not needed by the included R scripts and are omitted entirely. The original Perspective data file is reduced to score and grouping fields and renamed `PerspectiveScores.csv`.

| File | Unit | Rows | Columns | Contents |
|---|---|---:|---:|---|
| `SessionData.csv` | Session | 80 | 2 | Condition and participant count |
| `SessionRoundData.csv` | Session × round | 400 | 8 | Assortativity, stance distances, network degree |
| `RoomData.csv` | Discussion room × round | 1,185 | 8 | Group size, within-group distance, volume and sentiment |
| `ParticipantData.csv` | Participant × round | 3,546 | 15 | Group membership, message counts, sentiment, AI adoption and stance distance |
| `VaderScoreforAISuggestions.csv` | AI suggestion score | 28,681 | 2 | Condition and precomputed VADER sentiment score |
| `utterance_minmax_composite.csv` | Scored utterance | 7,601 | 4 | Participant identifier, condition, message type and composite politeness score |
| `PerspectiveScores.csv` | Scored message | 12,364 | 7 | Session/participant identifiers, condition, round, message type, toxicity and respectfulness scores |

### Condition and round coding

| Stored condition | Manuscript terminology |
|---|---|
| `control` | No assistance |
| `personal` | Individual AI assistance |
| `relational-static` | Relational AI assistance |
| `control-instruction` | Direct relational instruction |

Rounds 1–4 are discussion rounds. Round 5 represents the final group configuration following the fourth discussion, not an additional discussion round. Missing values are retained; refer to the individual scripts for analysis-specific exclusions.

### Key variables

- `SessionID` / `GameID`, `ParticipantID` / `PID` / `pid`, `RoomID`: grouping and matching identifiers retained where required by the analyses.
- `Condition` / `condition`, `RoundID`: experimental condition and round.
- `NumPlayers`, `GroupSize`: session participant count and room membership count, respectively.
- `Assortativity`, `AssortativityDelta`: stance assortativity and its stored change from the initial round.
- `WithinDistance`, `BetweenDistance`, `WithinDistanceAcrossRoomParticipant`, `OpinionDifference`: stance-distance measures at the session, room, and participant levels; see the manuscript for definitions.
- `NetworkDegree`: network degree measure used for the conversation-partner analysis.
- `NumMsgs`, `NumMsgsHuman`, `VADERMean`, `VADERMeanHuman`: message counts and mean precomputed VADER sentiment scores for all and self-written messages, respectively.
- `TotalMessageCount`, `TotalCopiedSuggestion`, `TotalEditedSuggestion`, `CountofRoundParticipated`: participant-level totals used to compute AI adoption and message production rates.
- `message_type` / `MessageType`: message-origin category used to distinguish self-written messages from AI-assisted messages.
- `Score`, `composite_mm`, `TOXICITY`, `RESPECT_EXPERIMENTAL`: precomputed suggestion sentiment, composite politeness, toxicity, and respectfulness scores, respectively.

No API access, original message text, or language model credentials are required for the included analyses. The archive does not include the original text-scoring pipeline. Identifiers and individual score observations are retained because the scripts require grouping, matching and repeated-observation models; the data should not be treated as fully anonymous merely because text was removed.

## Reproducibility notes

- Scripts are organized by manuscript figure number and include both original and reconstructed analysis code. Some final figure formatting and significance annotations were assembled separately from the R plots.
- The supplied Figure 3–4 and Figure 5A/B scripts do not explicitly filter out `control-instruction` at input, although the corresponding main-experiment figures concern only the three original conditions. To restrict these analyses to the main experiment, immediately after the relevant CSV load use `d <- d %>% filter(Condition %in% c("control", "personal", "relational-static"))`. This archive's data cleanup does not change the supplied statistical code.
- The Figure 7B reconstruction has previously produced 398 complete group transitions, whereas Table A8 reports 402. Its numerical reproduction should therefore be treated as approximate pending reconciliation.
- The included scripts have not been executed as a complete suite after the column cleanup. The retained CSV contents were checked against the inputs and all file references were checked for existence.

## Citation

Please cite the associated article: Huq, F., Claggett, E. L., & Shirado, H. *AI-Mediated Communication Can Reduce Opinion-Based Network Segregation*. Computers in Human Behavior. When available, also cite the version-specific DOI assigned to this archive by Zenodo.
