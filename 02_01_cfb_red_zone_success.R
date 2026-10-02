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

## Team information ----
raw_team_info <- cfbd_team_info()


# Processing ----

## Game Drives ----
prod_game_drives <- 
  # start with game drives
  raw_game_drives |> 
  # filter for those that made the red zone
  filter(end_yards_to_goal <= 20) |>
  # create fields
  mutate(
    # conference or non-conference game
    game_type = ifelse(offense_conference == defense_conference, "In-Conference", "Out of Conference"),
    # points scored on drive
    drive_points = case_when(
      drive_result %in% c("TD") ~ 7,
      drive_result %in% c("FG") ~ 3,
      drive_result %in% c("FUMBLE RETURN TD", "FUMBLE TD", "INT TD", "MISSED FG TD") ~ -7,
      TRUE ~ 0
    )
  ) 

# offsense mean and total scored by team
print(
  prod_game_drives |> 
    summarize(
      total_drives = n(),
      total_points = sum(drive_points),
      avg_points = mean(drive_points, na.rm = TRUE),
      .by = offense) |> 
    arrange(desc(avg_points)),
  n = 50
)

# defenses average scored by team 
print(
  prod_game_drives |> 
    summarize(
      total_drives = n(),
      total_points = sum(drive_points),
      avg_points = mean(drive_points, na.rm = TRUE),
      .by = defense) |> 
    arrange(avg_points),
  n = 50
)

# Plot 1: Average Points Scored ----
plot_01_data <-
  full_join(
    prod_game_drives |> 
      filter(offense_conference %in% conferences_p5) |> 
      summarise(average_points_offense = mean(drive_points), .by = offense) |> 
      rename(team = offense),
    prod_game_drives |> 
      filter(defense_conference %in% conferences_p5) |> 
      summarise(average_points_defense = mean(drive_points), .by = defense) |> 
      rename(team = defense),
    by = join_by(team)
  ) |> 
  # add team information 
  left_join(
    raw_team_info |> 
      select(school, logo),
    by = join_by(team == school)
  ) |> 
  # add fields
  mutate(
    net_team_points = average_points_offense - average_points_defense,
    net_team_points_normalized = net_team_points - mean(net_team_points)
  )

plot_01_mean_offense <- mean(plot_01_data$average_points_offense)
plot_01_mean_defense <- mean(plot_01_data$average_points_defense)
placement_factors <- c(0.6, 1.40)

## Scatterplot of average points scored and allowed by team ----
ggplot(plot_01_data, aes(x = average_points_offense, y = average_points_defense)) +
  # add reference lines
  geom_vline(aes(xintercept = plot_01_mean_offense)) +
  geom_hline(aes(yintercept = plot_01_mean_defense)) +
  # add scatterplot from data
  geom_from_path(aes(path = logo), width = 0.03) +
  # add watermark
  geom_from_path(data = watermark_path, 
                         aes(x = 6.5, y = 6.5, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  # quadrant labels 
  # Top-Right: Good Offense + Good Defense
  annotate("text", 
           x = plot_01_mean_offense*placement_factors[2], 
           y = plot_01_mean_defense*placement_factors[1], 
           label = "Stress-Free\nTailgating", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#1b5e20", size = 4) +
  
  # Top-Left: Bad Offense + Good Defense
  annotate("text", 
           x = plot_01_mean_offense*placement_factors[1], 
           y = plot_01_mean_defense*placement_factors[1], 
           label = "Throwing Pillows\nat the TV", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#e65100", size = 4) +
  
  # Bottom-Right: Good Offense + Bad Defense
  annotate("text", 
           x = plot_01_mean_offense*placement_factors[2], 
           y = plot_01_mean_defense*placement_factors[2], 
           label = "Cardiac\nArrest\nFootball", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#b71c1c", size = 4) +
  
  # Bottom-Left: Bad Offense + Bad Defense
  annotate("text", 
           x = plot_01_mean_offense*placement_factors[1], 
           y = plot_01_mean_defense*placement_factors[2], 
           label = "Changing to the\nNature Channel", hjust = 0.5, vjust = 0, 
           fontface = "bold", color = "#4a148c", size = 4) +
  # labels
  labs(title = "Average Red Zone Points Scored and Allowed by Team", 
       subtitle = "For Teams in FBS Power 5 Conferences, 2026 Season To Date", 
       caption = "cfbfastR, ggplot2, ggpath") +
  ylab("Average Red Zone Points Allowed on Defense") + xlab("Average Red Zone Points Scored on Offense") +
  # formatting and other
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_y_reverse() 
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_sores.png", 
       width = 11.5, height = 7)

## bar chart of mean-centered net team score ----
ggplot(plot_01_data, aes(x = reorder(team, net_team_points_normalized), y = net_team_points_normalized, 
                         fill = net_team_points_normalized)) +
  geom_col(show.legend = FALSE, alpha = 0.65) +
  geom_from_path(
    aes(path = logo,
        vjust = ifelse(net_team_points_normalized >= 0, -0.2, 1.2)), # Fixed 'team' (removed extra 's')
    height = 0.025
  ) +
  # add watermark
  geom_from_path(
    data = watermark_path,
    aes(x = 75, y = -2.5, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  # labels
  labs(title = "Mean-Centered Net Red Zone Points, by Team", 
       subtitle = "For Teams in FBS Power 5 Conferences, 2026 Season To Date",
       caption = "cfbfastR, ggplot2, ggpath\nCalculated as a team's net red zone points (Offensive Points Scored − Defensive Points Allowed) minus the Power 5 conference average") +
  ylab("Mean-Centered Net Team Points") +
  # formatting and other
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  ) +
  scale_fill_viridis_c(option = "turbo")
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_column_net_team_points_meancentered.png", 
       width = 13, height = 6)

# Plot 2: Percent TDs in Red Zone ----
plot_02_data <- 
  full_join(
    # offense
    prod_game_drives |> 
      filter(offense_conference %in% conferences_p5) |> 
      summarise(
        # number of drives
        count_drive_offense = n(),
        # number of scoring drives
        scoring_drive_offense = sum(drive_result %in% c("TD", "FG")),
        # number of TD scoring drives
        td_drive_offense = sum(drive_result %in% c("TD")),
        .by = offense
      ) |> 
      rename(team = offense),
    # defense
    prod_game_drives |> 
      filter(defense_conference %in% conferences_p5) |> 
      summarise(
        # number of drives
        count_drive_defense = n(),
        # number of scoring drives
        scoring_drive_defense = sum(drive_result %in% c("TD", "FG")),
        # number of TD scoring drives
        td_drive_defense = sum(drive_result %in% c("TD")),
        .by = defense
      ) |> 
      rename(team = defense),
    by = join_by(team)
  ) |> 
  # additional fields
  mutate(
    # ratio of offense:defense drives
    ratio_drives = round(count_drive_offense/count_drive_defense, 5),
    # ratio of offense:defense scoring drives
    ratio_scoring_drives = round(scoring_drive_offense/scoring_drive_defense, 5),
    # ratio of offense: defense TD drives
    ratio_td_drives = round(td_drive_offense/td_drive_defense, 5),
    # proportion offense scoring drive
    pct_scoring_drives_offense = round(scoring_drive_offense/count_drive_offense, 5),
    # proportion offense TD drives
    pct_td_drives_offense = round(td_drive_offense/count_drive_offense, 5),
    # proportion defense scoring drives
    pct_scoring_drives_defense = round(scoring_drive_defense/count_drive_defense, 5),
    # proportion defense TD drives 
    pct_td_drives_defense = round(td_drive_defense/count_drive_defense, 5)
  )|> 
  # add team information 
  left_join(
    raw_team_info |> 
      select(school, logo_url = logo),
    by = join_by(team == school)
  ) 

## Four Quadrant Offense vs Defense Plot ----

# Calculate league averages for quadrant dividers
avg_off_td <- mean(plot_02_data$pct_td_drives_offense, na.rm = TRUE)
avg_def_td <- mean(plot_02_data$pct_td_drives_defense, na.rm = TRUE)

ggplot(plot_02_data, aes(x = pct_td_drives_offense, y = pct_td_drives_defense)) +
  # Quadrant reference lines
  geom_vline(xintercept = avg_off_td, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = avg_def_td, linetype = "dashed", color = "gray60") +
  # Plot team logos
  geom_from_path(aes(path = logo_url), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = 0.87, y = 0.82, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  # Invert Y-axis so lower defensive TD % allowed is at the top
  scale_y_reverse(labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Quadrant Annotations
  annotate("text", x = avg_off_td + 0.20, y = avg_def_td - 0.25, label = "Elite Teams", fontface = "bold", color = "forestgreen", alpha = 0.6) +
  annotate("text", x = avg_off_td - 0.20, y = avg_def_td + 0.20, label = "Struggling Red Zone Teams", fontface = "bold", color = "firebrick", alpha = 0.6) +
  labs(
    title = "Power 5 Red Zone Efficiency Matrix",
    subtitle = "Offensive RZ Touchdown % Scored vs. Defensive RZ Touchdown % Allowed",
    x = "Offensive RZ Touchdown Rate Scored",
    y = "Defensive RZ Touchdown Rate Allowed (Inverted)",
    caption = "Data: cfbfastR, ggplot2, ggpath"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
    )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_offense_defense_td_pct.png", 
       width = 11.5, height = 7)

## Settling for 3 vs Going For 7 ----
plot_02_data |>
  # Top 25 teams by RZ scoring rate
  slice_max(pct_scoring_drives_offense, n = 30) |>
  mutate(team = reorder(team, pct_scoring_drives_offense)) |>
  ggplot() +
  # Segment representing FG gap
  geom_segment(
    aes(x = pct_td_drives_offense, xend = pct_scoring_drives_offense, y = team, yend = team),
    color = "gray70", linewidth = 1.2
  ) +
  # Point for Total Scoring %
  geom_point(aes(x = pct_scoring_drives_offense, y = team), color = "#336699", size = 4) +
  # Logo placed on Touchdown %
  geom_from_path(aes(x = pct_td_drives_offense, y = team, path = logo_url), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = 0.91, y = 3, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Top 30 Red Zone Finishers vs. Field Goal Settlers",
    subtitle = "Logo shows RZ Touchdown % | Blue dot shows Total RZ Scoring % (TD + FG)",
    x = "Conversion Rate",
    y = NULL
  ) +
  # format and others
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    axis.text.y = element_blank()
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_dumbell_red_zone_finishing.png", 
       width = 13, height = 8)

## Red Zone Volume vs Efficiency ----
ggplot(plot_02_data, aes(x = count_drive_offense, y = pct_td_drives_offense)) +
  # add the causal line
  geom_smooth(method = "lm", se = FALSE, color = "gray80", linetype = "dotted") +
  # add the logos
  geom_from_path(aes(path = logo_url), width = 0.025) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = 37, y = 0.33, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  # labels
  labs(
    title = "Red Zone Opportunity vs. Finishing Ability",
    subtitle = "Total Offensive Red Zone Trips vs. Touchdown Conversion Rate",
    x = "Total RZ Drives",
    y = "RZ Touchdown Rate"
  ) +
  # formatting and others
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5),
    plot.caption = element_text(hjust = 0)
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_scatter_volume_efficiency.png", 
       width = 13, height = 8)

## Drive Outcome Breakdown ----
plot_02_data |>
  mutate(
    pct_fg_drives_offense = pct_scoring_drives_offense - pct_td_drives_offense,
    pct_no_score = 1 - pct_scoring_drives_offense
  ) |>
  # slice_max(pct_scoring_drives_offense, n = 30) |> 
  arrange(desc(pct_scoring_drives_offense)) |>
  select(team, pct_td_drives_offense, pct_fg_drives_offense, pct_no_score) |> 
  # pivot the data longer
  pivot_longer(
    cols = c(pct_td_drives_offense, pct_fg_drives_offense, pct_no_score),
    names_to = "outcome",
    values_to = "pct"
  ) |>
  mutate(
    outcome = factor(outcome, levels = c("pct_no_score", "pct_fg_drives_offense", "pct_td_drives_offense"),
                     labels = c("No Points", "Field Goal", "Touchdown"))
  ) |>
  ggplot(aes(x = pct, y = reorder(team, pct), fill = outcome)) +
  geom_col(position = "fill", width = 0.7) +
  # add watermark
  geom_from_path(data = watermark_path, 
                 aes(x = 0.95, y = 0.76, path = image_path), width = 0.04, alpha = 0.95, inherit.aes = FALSE) +
  labs(
    title = "Red Zone Drive Outcome Distribution",
    x = "Proportion of Red Zone Drives",
    y = NULL,
    fill = "Drive Result"
  ) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Touchdown" = "#2e7d32", "Field Goal" = "#fbc02d", "No Points" = "#c62828")) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5)
    )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_columns_outcome_distribution.png", 
       width = 7, height = 16)

## Net Red Zone Ratio Lollipop Chart ----
plot_02_data |>
  filter(!is.na(ratio_td_drives)) |>
  # slice_max(ratio_td_drives, n = 15) |>
  ggplot(aes(x = ratio_td_drives, y = reorder(team, ratio_td_drives))) +
  geom_segment(aes(x = 1, xend = ratio_td_drives, y = team, yend = team), color = "gray50", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black") +
  geom_from_path(aes(path = logo_url), width = 0.02) +
  scale_x_continuous(breaks = seq(0, 13, 1)) +
  labs(
    title = "Top Power 5 Net Red Zone Touchdown Ratios",
    subtitle = "Ratio of Offensive RZ TDs Scored to Defensive RZ TDs Allowed (Baseline = 1.0)",
    x = "Offensive RZ TDs / Defensive RZ TDs Allowed",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "italic", hjust = 0.5)
  )
ggsave(plot = get_last_plot(), path = DIR_OUTPUTS, filename = "proj_012_02_01_lollipop_redzone_ratio.png", 
       width = 14, height = 16)
