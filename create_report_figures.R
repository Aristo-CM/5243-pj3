# Create report figures for the DataPath default-option A/B test.
# Run from the project root:
#   Rscript create_report_figures.R

library(dplyr)
library(ggplot2)

data_file <- file.path("datapath-shinyapp", "default_option_ab_test_data.csv")
fig_dir <- "figures"

if (!dir.exists(fig_dir)) {
  dir.create(fig_dir, recursive = TRUE)
}

ab_data <- read.csv(data_file, stringsAsFactors = FALSE)

ab_data <- ab_data %>%
  mutate(
    group_label = ifelse(
      assigned_group == "A",
      "Group A: Default",
      "Group B: No Default"
    ),
    group_label = factor(
      group_label,
      levels = c("Group A: Default", "Group B: No Default")
    ),
    completion_status = as.integer(completion_status),
    selected_path = factor(
      selected_path,
      levels = c("beginner", "standard", "advanced"),
      labels = c("Beginner", "Standard", "Advanced")
    )
  )

theme_datapath <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 17, color = "#102033"),
      plot.subtitle = element_text(size = 12, color = "#5f6b7a"),
      axis.title = element_text(face = "bold", color = "#26364a"),
      axis.text = element_text(color = "#3f4f63"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.caption = element_text(color = "#6b7280", hjust = 0),
      plot.margin = margin(12, 18, 12, 12)
    )
}

palette_groups <- c(
  "Group A: Default" = "#177E89",
  "Group B: No Default" = "#2F5597"
)

# Figure 1: Completion rate with 95% CI.
completion_summary <- ab_data %>%
  group_by(group_label) %>%
  summarise(
    visitors = n(),
    completed = sum(completion_status == 1),
    completion_rate = completed / visitors,
    se = sqrt(completion_rate * (1 - completion_rate) / visitors),
    ci_low = pmax(0, completion_rate - 1.96 * se),
    ci_high = pmin(1, completion_rate + 1.96 * se),
    .groups = "drop"
  )

p_completion <- ggplot(
  completion_summary,
  aes(x = group_label, y = completion_rate, fill = group_label)
) +
  geom_col(width = 0.58, color = "white", linewidth = 0.8) +
  geom_errorbar(
    aes(ymin = ci_low, ymax = ci_high),
    width = 0.16,
    linewidth = 0.9,
    color = "#102033"
  ) +
  geom_text(
    aes(label = paste0(round(completion_rate * 100, 1), "%")),
    vjust = -0.7,
    fontface = "bold",
    size = 5,
    color = "#102033"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100), "%"),
    limits = c(0, 0.95),
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_fill_manual(values = palette_groups) +
  labs(
    title = "Completion Rate with 95% Confidence Intervals",
    subtitle = "Default selections increased completion from 61.5% to 79.0%.",
    x = NULL,
    y = "Completion rate",
    caption = "Source: Simulated DataPath A/B test dataset, n = 400 users."
  ) +
  theme_datapath() +
  theme(legend.position = "none")

ggsave(
  filename = file.path(fig_dir, "completion_rate_ci.png"),
  plot = p_completion,
  width = 8.5,
  height = 5.4,
  dpi = 300
)

# Figure 2: Time-to-completion distribution.
completed_data <- ab_data %>%
  filter(completion_status == 1)

time_means <- completed_data %>%
  group_by(group_label) %>%
  summarise(mean_time = mean(time_to_completion_seconds), .groups = "drop")

p_time <- ggplot(
  completed_data,
  aes(x = time_to_completion_seconds, fill = group_label)
) +
  geom_histogram(
    bins = 22,
    alpha = 0.72,
    position = "identity",
    color = "white",
    linewidth = 0.25
  ) +
  geom_vline(
    data = time_means,
    aes(xintercept = mean_time, color = group_label),
    linetype = "dashed",
    linewidth = 1.1
  ) +
  scale_fill_manual(values = palette_groups) +
  scale_color_manual(values = palette_groups) +
  labs(
    title = "Time-to-Completion Distribution",
    subtitle = "The default group completed the flow faster on average.",
    x = "Seconds from entry to completion",
    y = "Completed users",
    caption = "Dashed lines show group means: Group A = 82.7 seconds, Group B = 125.7 seconds."
  ) +
  theme_datapath()

ggsave(
  filename = file.path(fig_dir, "time_to_completion_distribution.png"),
  plot = p_time,
  width = 8.5,
  height = 5.4,
  dpi = 300
)

# Figure 3: Selected path distribution by group.
path_summary <- completed_data %>%
  group_by(group_label, selected_path) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(percent = count / sum(count)) %>%
  ungroup()

p_path <- ggplot(
  path_summary,
  aes(x = selected_path, y = percent, fill = group_label)
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.62,
    color = "white",
    linewidth = 0.5
  ) +
  geom_text(
    aes(label = paste0(round(percent * 100), "%")),
    position = position_dodge(width = 0.7),
    vjust = -0.45,
    fontface = "bold",
    size = 4,
    color = "#102033"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(round(x * 100), "%"),
    limits = c(0, 0.46),
    expand = expansion(mult = c(0, 0.03))
  ) +
  scale_fill_manual(values = palette_groups) +
  labs(
    title = "Selected Learning Path Distribution by Group",
    subtitle = "Defaults changed the pattern of completed users' final learning-path choices.",
    x = "Selected learning path",
    y = "Share of completed users",
    caption = "Percentages are calculated among users who completed the flow."
  ) +
  theme_datapath()

ggsave(
  filename = file.path(fig_dir, "selected_path_distribution.png"),
  plot = p_path,
  width = 8.5,
  height = 5.4,
  dpi = 300
)

message("Created report figures in: ", normalizePath(fig_dir))
