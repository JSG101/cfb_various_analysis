# Header ----
#
# Script name: 02_01_cfb_red_zone_success.R
#
# Purpose of script: Answer the question: for teams that make the redzone, how successful are they at scoring?
#
# Location of script:
#
# Author: Japbir Gill, MBA MSEd
#
# Date Created: 2026-09-17
#
# Copyright (c) Japbir Gill, 2026
# Email: Japbir.Gill@gmail.com
#
# Notes ----
# https://cloud.r-project.org/web/packages/cfbfastR/refman/cfbfastR.html#cfbfastR-package
#   
#

rm(list=ls())

# Working Directories, Libraries, Functions, Values ----

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00_00_dirs_libs_funs_vals.R")

# Formatting and Global Options ----

# Datasets ----

## Game Drives ----
raw_game_drives <- cfbd_drives(2026, season_type = "regular", division = "fbs")


# Processing ----

## Game Drives ----
prod_game_drives <- 
  # start with game drives
  raw_game_drives |> 
  # filter for those that made the red zone
  # filter(end_yards_to_goal <= 20) |> 
  # create fields
  mutate(
    # conference or non-conference game
    game_type = ifelse(offense_conference == defense_conference, "In-Conference", "Out of Conference"),
    # drive made the end zone
    drive_end_category = ifelse(end_yardline >= 80, "Red Zone", "Non-Red Zone"),
    # scoring drive numeric
    scoring_numeric = ifelse(scoring, 1, 0),
    scoring_numeric_offense = ifelse(end_offense_score > start_offense_score, 1, 0),
    scoring_numeric_defense = ifelse(end_defense_score > start_defense_score, 1, 0),
    # points scored in drive
    scoring_points_offense = end_offense_score - start_offense_score,
    scoring_points_defense = end_defense_score - start_defense_score,
    # QA - comparing start and end yeardlines
    qa_end_yardline = (start_yardline + yards) - end_yardline
  ) |> 
  filter(between(qa_end_yardline, -10, 10))

print(
  prod_game_drives |> 
    group_by(offense_conference) |>
    summarise(scoring_numeric = mean(scoring_numeric)) |> 
    arrange(desc(scoring_numeric)),
  n = 50
  )

print(
  prod_game_drives |> 
    group_by(offense_conference, drive_end_category) |>
    summarise(scoring_numeric = mean(scoring_numeric)) |> 
    arrange(desc(scoring_numeric)),
  n = 50
)

print(
  prod_game_drives |> 
    group_by(defense_conference) |> 
    summarise(scoring_numeric = mean(scoring_numeric)) |> 
    arrange(scoring_numeric),
  n = 50
)
  
print(
  prod_game_drives |> 
    group_by(defense_conference, drive_end_category) |> 
    summarise(scoring_numeric = mean(scoring_numeric)) |> 
    arrange(scoring_numeric),
  n = 50
)

print(
  prod_game_drives |> 
    group_by(drive_end_category) |> 
    summarise(scoring_numeric = mean(scoring_numeric)) |> 
    arrange(desc(scoring_numeric)),
  n = 50
)

prod_game_drives |> 
  mutate(bucket = cut_width(end_yards_to_goal, width = 5, boundary = 0)) |> 
  group_by(bucket) |> 
  summarize(
    min_yard = min(end_yards_to_goal, na.rm = TRUE),
    max_yard = max(end_yards_to_goal, na.rm = TRUE),
    mean_scoring = mean(scoring_numeric_offense, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    # Create label strings
    raw_range = paste0(round(min_yard), "–", round(max_yard), " yds"),
    # Disambiguate duplicate labels by attaching the bucket index if needed
    yard_range = factor(raw_range, levels = unique(raw_range))
  ) |> 
  ggplot(aes(x = yard_range, y = mean_scoring, fill = mean_scoring)) +
  geom_col(width = 0.7, show.legend = FALSE, color = "white") +
  geom_text(
    aes(label = scales::percent(mean_scoring, accuracy = 0.1)), 
    vjust = -0.5, 
    fontface = "bold", 
    size = 3.5
  ) +
  scale_fill_viridis_c(option = "magma", begin = 0.25, end = 0.85) +
  scale_y_continuous(
    labels = scales::percent_format(), 
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "Average Scoring Rate by Drive Field Position",
    subtitle = "Field divided into 10 equal-volume observation deciles",
    x = "Yards to Goal Range",
    y = "Mean Scoring Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "gray30", margin = margin(b = 10)),
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, face = "bold"),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
