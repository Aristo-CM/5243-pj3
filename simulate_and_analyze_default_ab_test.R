# Simulate and analyze an A/B test for default options in the DataPath Shiny app.
# Group A = default condition. Group B = non-default condition.
#
# Outputs:
#   - default_option_ab_test_data.csv
#   - default_option_ab_test_analysis_summary.txt

set.seed(5205)

n_per_group <- 200
data_file <- file.path(getwd(), "default_option_ab_test_data.csv")
summary_file <- file.path(getwd(), "default_option_ab_test_analysis_summary.txt")

csv_columns <- c(
  "session_id",
  "assigned_group",
  "entry_timestamp",
  "primary_goal",
  "experience_level",
  "recommended_plan",
  "selected_path",
  "selected_plan",
  "default_option_shown",
  "selected_plan_is_default",
  "changed_away_from_default",
  "completion_status",
  "completion_timestamp",
  "time_to_completion_seconds",
  "confidence_rating"
)

goal_levels <- c("job", "switch", "academic", "personal")
goal_probs <- c(0.36, 0.22, 0.26, 0.16)

experience_levels <- c("none", "basic", "intermediate", "advanced")
experience_probs <- c(0.20, 0.35, 0.30, 0.15)

weighted_plan_for_experience <- function(experience_id) {
  probabilities <- switch(
    experience_id,
    none = c(beginner = 0.82, standard = 0.15, advanced = 0.03),
    basic = c(beginner = 0.50, standard = 0.45, advanced = 0.05),
    intermediate = c(beginner = 0.08, standard = 0.77, advanced = 0.15),
    advanced = c(beginner = 0.03, standard = 0.17, advanced = 0.80),
    c(beginner = 1 / 3, standard = 1 / 3, advanced = 1 / 3)
  )

  sample(names(probabilities), size = 1, prob = probabilities)
}

choose_alternative_plan <- function(recommended_plan) {
  all_plans <- c("beginner", "standard", "advanced")
  sample(all_plans[all_plans != recommended_plan], size = 1)
}

completion_probability <- function(group, primary_goal, experience_level) {
  base_probability <- if (group == "A") 0.78 else 0.60

  goal_adjustment <- switch(
    primary_goal,
    job = 0.04,
    switch = 0.02,
    academic = 0.00,
    personal = -0.05,
    0.00
  )

  experience_adjustment <- switch(
    experience_level,
    none = -0.05,
    basic = 0.00,
    intermediate = 0.03,
    advanced = 0.04,
    0.00
  )

  probability <- base_probability + goal_adjustment + experience_adjustment
  min(0.95, max(0.05, probability))
}

simulate_group <- function(group, n_users) {
  default_shown <- group == "A"
  entry_start <- as.POSIXct("2026-04-17 09:00:00", tz = "America/New_York")
  rows <- vector("list", n_users)

  for (i in seq_len(n_users)) {
    entry_time <- entry_start + sample(0:(60 * 60 * 10), 1)

    primary_goal <- if (default_shown && runif(1) < 0.58) {
      "academic"
    } else {
      sample(goal_levels, size = 1, prob = goal_probs)
    }

    experience_level <- if (default_shown && runif(1) < 0.56) {
      "basic"
    } else {
      sample(experience_levels, size = 1, prob = experience_probs)
    }

    recommended_plan <- weighted_plan_for_experience(experience_level)
    completed <- as.integer(runif(1) < completion_probability(group, primary_goal, experience_level))

    if (completed == 1L) {
      retention_probability <- if (default_shown) 0.76 else 0.57
      retained_recommended_path <- runif(1) < retention_probability

      selected_path <- if (retained_recommended_path) {
        recommended_plan
      } else {
        choose_alternative_plan(recommended_plan)
      }

      time_mean <- if (default_shown) 82 else 122
      time_sd <- if (default_shown) 24 else 38
      time_to_completion <- max(18, round(rnorm(1, mean = time_mean, sd = time_sd), 1))
      completion_time <- entry_time + time_to_completion

      confidence_mean <- if (selected_path == recommended_plan) 4.05 else 3.35
      confidence_rating <- min(5L, max(1L, round(rnorm(1, mean = confidence_mean, sd = 0.75))))

      selected_plan_is_default <- if (default_shown) {
        as.integer(selected_path == recommended_plan)
      } else {
        NA_integer_
      }

      changed_away_from_default <- if (default_shown) {
        as.integer(selected_path != recommended_plan)
      } else {
        NA_integer_
      }
    } else {
      selected_path <- ""
      time_to_completion <- NA_real_
      completion_time <- NA
      confidence_rating <- NA_integer_
      selected_plan_is_default <- NA_integer_
      changed_away_from_default <- NA_integer_
    }

    rows[[i]] <- data.frame(
      session_id = sprintf("sim_%s_%03d", group, i),
      assigned_group = group,
      entry_timestamp = format(entry_time, "%Y-%m-%d %H:%M:%S %Z"),
      primary_goal = primary_goal,
      experience_level = experience_level,
      recommended_plan = recommended_plan,
      selected_path = selected_path,
      selected_plan = selected_path,
      default_option_shown = as.integer(default_shown),
      selected_plan_is_default = selected_plan_is_default,
      changed_away_from_default = changed_away_from_default,
      completion_status = completed,
      completion_timestamp = if (completed == 1L) {
        format(completion_time, "%Y-%m-%d %H:%M:%S %Z")
      } else {
        ""
      },
      time_to_completion_seconds = time_to_completion,
      confidence_rating = confidence_rating,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows)
}

raw_data <- rbind(
  simulate_group("A", n_per_group),
  simulate_group("B", n_per_group)
)

# Shuffle rows so the output resembles raw data collected over time.
raw_data <- raw_data[sample(seq_len(nrow(raw_data))), ]
row.names(raw_data) <- NULL
raw_data <- raw_data[csv_columns]

write.csv(raw_data, data_file, row.names = FALSE, na = "")

completion_summary <- aggregate(
  completion_status ~ assigned_group,
  raw_data,
  function(x) c(users = length(x), completions = sum(x), completion_rate = mean(x))
)

completion_summary <- data.frame(
  assigned_group = completion_summary$assigned_group,
  users = completion_summary$completion_status[, "users"],
  completions = completion_summary$completion_status[, "completions"],
  completion_rate = completion_summary$completion_status[, "completion_rate"],
  row.names = NULL
)

completed_data <- raw_data[raw_data$completion_status == 1, ]

retention_summary <- aggregate(
  selected_path == recommended_plan ~ assigned_group,
  completed_data,
  mean
)
names(retention_summary) <- c("assigned_group", "retention_rate")

retention_counts <- aggregate(
  selected_path == recommended_plan ~ assigned_group,
  completed_data,
  sum
)
names(retention_counts) <- c("assigned_group", "retained_count")

retention_summary <- merge(retention_summary, retention_counts, by = "assigned_group")
retention_summary <- merge(
  retention_summary,
  completion_summary[c("assigned_group", "completions")],
  by = "assigned_group"
)

time_summary <- aggregate(time_to_completion_seconds ~ assigned_group, completed_data, mean)
confidence_summary <- aggregate(confidence_rating ~ assigned_group, completed_data, mean)
path_table <- with(completed_data, table(assigned_group, selected_path))

completion_test <- prop.test(
  x = completion_summary$completions,
  n = completion_summary$users,
  correct = FALSE
)

retention_test <- prop.test(
  x = retention_summary$retained_count,
  n = retention_summary$completions,
  correct = FALSE
)

completion_diff <- completion_summary$completion_rate[completion_summary$assigned_group == "A"] -
  completion_summary$completion_rate[completion_summary$assigned_group == "B"]

retention_diff <- retention_summary$retention_rate[retention_summary$assigned_group == "A"] -
  retention_summary$retention_rate[retention_summary$assigned_group == "B"]

summary_lines <- c(
  "Simulated Default Option A/B Test Analysis",
  "==========================================",
  "",
  "Sample size:",
  sprintf("- Group A default condition: %d users", completion_summary$users[completion_summary$assigned_group == "A"]),
  sprintf("- Group B non-default condition: %d users", completion_summary$users[completion_summary$assigned_group == "B"]),
  sprintf("- Total simulated users: %d", nrow(raw_data)),
  "",
  "Primary metric: completion rate",
  sprintf(
    "- Group A: %d/%d completed (%.1f%%)",
    completion_summary$completions[completion_summary$assigned_group == "A"],
    completion_summary$users[completion_summary$assigned_group == "A"],
    100 * completion_summary$completion_rate[completion_summary$assigned_group == "A"]
  ),
  sprintf(
    "- Group B: %d/%d completed (%.1f%%)",
    completion_summary$completions[completion_summary$assigned_group == "B"],
    completion_summary$users[completion_summary$assigned_group == "B"],
    100 * completion_summary$completion_rate[completion_summary$assigned_group == "B"]
  ),
  sprintf("- Difference, A minus B: %.1f percentage points", 100 * completion_diff),
  sprintf("- Two-proportion test p-value: %.5f", completion_test$p.value),
  "",
  "Secondary metric: retention rate",
  "Retention means the completed user kept the recommended/default path.",
  sprintf(
    "- Group A: %d/%d retained (%.1f%%)",
    retention_summary$retained_count[retention_summary$assigned_group == "A"],
    retention_summary$completions[retention_summary$assigned_group == "A"],
    100 * retention_summary$retention_rate[retention_summary$assigned_group == "A"]
  ),
  sprintf(
    "- Group B: %d/%d retained (%.1f%%)",
    retention_summary$retained_count[retention_summary$assigned_group == "B"],
    retention_summary$completions[retention_summary$assigned_group == "B"],
    100 * retention_summary$retention_rate[retention_summary$assigned_group == "B"]
  ),
  sprintf("- Difference, A minus B: %.1f percentage points", 100 * retention_diff),
  sprintf("- Two-proportion test p-value: %.5f", retention_test$p.value),
  "",
  "Additional secondary metrics:",
  sprintf(
    "- Average time to completion, A: %.1f seconds",
    time_summary$time_to_completion_seconds[time_summary$assigned_group == "A"]
  ),
  sprintf(
    "- Average time to completion, B: %.1f seconds",
    time_summary$time_to_completion_seconds[time_summary$assigned_group == "B"]
  ),
  sprintf(
    "- Average confidence, A: %.2f out of 5",
    confidence_summary$confidence_rating[confidence_summary$assigned_group == "A"]
  ),
  sprintf(
    "- Average confidence, B: %.2f out of 5",
    confidence_summary$confidence_rating[confidence_summary$assigned_group == "B"]
  ),
  "",
  "Selected path among completed users:",
  paste(capture.output(print(path_table)), collapse = "\n"),
  "",
  "Decision:",
  if (completion_diff > 0 && completion_test$p.value < 0.05) {
    "The simulated data favors the default-option feature. Group A had a higher completion rate than Group B, and the difference is statistically significant at the 5% level."
  } else {
    "The simulated data does not provide strong enough evidence to prefer the default-option feature at the 5% level."
  },
  "",
  paste("Raw blended CSV written to:", data_file),
  paste("Analysis summary written to:", summary_file)
)

writeLines(summary_lines, summary_file)
writeLines(summary_lines)
