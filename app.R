library(shiny)

# Dataset for this default-option A/B test.
# It is saved in the same folder as app.R for easy local use and deployment.
data_file <- file.path(getwd(), "default_option_ab_test_data.csv")

default_goal <- "academic"
default_experience <- "basic"
data_download_key <- "data"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

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

empty_ab_data <- function() {
  data.frame(
    session_id = character(),
    assigned_group = character(),
    entry_timestamp = character(),
    primary_goal = character(),
    experience_level = character(),
    recommended_plan = character(),
    selected_path = character(),
    selected_plan = character(),
    default_option_shown = integer(),
    selected_plan_is_default = integer(),
    changed_away_from_default = integer(),
    completion_status = integer(),
    completion_timestamp = character(),
    time_to_completion_seconds = numeric(),
    confidence_rating = integer(),
    stringsAsFactors = FALSE
  )
}

read_ab_data <- function() {
  if (!file.exists(data_file)) {
    return(empty_ab_data())
  }

  data <- tryCatch(
    read.csv(data_file, stringsAsFactors = FALSE),
    error = function(e) empty_ab_data()
  )

  for (column in csv_columns) {
    if (!column %in% names(data)) {
      data[[column]] <- NA
    }
  }

  data <- data[csv_columns]
  missing_selected_path <- is.na(data$selected_path) | data$selected_path == ""
  data$selected_path[missing_selected_path] <- data$selected_plan[missing_selected_path]
  data$default_option_shown <- as.integer(data$default_option_shown)
  data$selected_plan_is_default <- as.integer(data$selected_plan_is_default)
  data$changed_away_from_default <- as.integer(data$changed_away_from_default)
  data$completion_status <- ifelse(is.na(data$completion_status), 0L, as.integer(data$completion_status))
  data$time_to_completion_seconds <- suppressWarnings(as.numeric(data$time_to_completion_seconds))
  data$confidence_rating <- suppressWarnings(as.integer(data$confidence_rating))
  data
}

write_ab_data <- function(data) {
  write.csv(data[csv_columns], data_file, row.names = FALSE, na = "")
}

create_session_id <- function(session_token) {
  if (requireNamespace("uuid", quietly = TRUE)) {
    return(uuid::UUIDgenerate())
  }

  paste0(
    "session_",
    session_token,
    "_",
    format(Sys.time(), "%Y%m%d%H%M%OS3"),
    "_",
    sample(100000:999999, 1)
  )
}

format_timestamp <- function(time) {
  format(time, "%Y-%m-%d %H:%M:%S %Z")
}

resolve_group <- function(url_search) {
  query <- parseQueryString(url_search %||% "")
  requested_group <- toupper(query$group %||% "")

  if (requested_group %in% c("A", "B")) {
    return(requested_group)
  }

  sample(c("A", "B"), size = 1, prob = c(0.5, 0.5))
}

is_data_download_mode <- function(url_search) {
  query <- parseQueryString(url_search %||% "")
  identical(query$admin %||% "", data_download_key)
}

plan_label <- function(plan_id) {
  switch(
    plan_id,
    beginner = "Beginner / Foundation",
    standard = "Standard / Career Track",
    advanced = "Advanced / Intensive",
    "Not selected"
  )
}

experience_label <- function(experience_id) {
  switch(
    experience_id,
    none = "No experience",
    basic = "Basic familiarity (e.g., Excel, beginner Python)",
    intermediate = "Intermediate (coursework, small projects)",
    advanced = "Advanced (strong coding, internships, or work experience)",
    "Not provided"
  )
}

goal_label <- function(goal_id) {
  switch(
    goal_id,
    job = "Get a job or internship",
    switch = "Career switch",
    academic = "Academic learning",
    personal = "Personal interest / exploration",
    "Not provided"
  )
}

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

plan_modules <- function(plan_id) {
  switch(
    plan_id,
    beginner = c(
      "Week 1: Python fundamentals, notebooks, and basic problem solving",
      "Week 2: Data cleaning with spreadsheets and pandas",
      "Week 3: Introductory statistics and simple visualizations",
      "Week 4: Mini project: analyze a small real-world dataset"
    ),
    standard = c(
      "Week 1: Python refresh, pandas, and reproducible notebooks",
      "Week 2: SQL querying, joins, and data preparation workflows",
      "Week 3: Statistics, experiment thinking, and data visualization",
      "Week 4: Machine learning basics with a portfolio-ready project"
    ),
    advanced = c(
      "Week 1: Feature engineering, validation strategy, and model selection",
      "Week 2: Model evaluation, tuning, and error analysis",
      "Week 3: Deployment basics, dashboards, and communicating results",
      "Week 4: Capstone portfolio build with documentation and presentation"
    ),
    character()
  )
}

log_visit <- function(session_id, assigned_group, entry_time, default_option_shown) {
  data <- read_ab_data()

  visit <- data.frame(
    session_id = session_id,
    assigned_group = assigned_group,
    entry_timestamp = format_timestamp(entry_time),
    primary_goal = "",
    experience_level = "",
    recommended_plan = "",
    selected_path = "",
    selected_plan = "",
    default_option_shown = as.integer(default_option_shown),
    selected_plan_is_default = NA_integer_,
    changed_away_from_default = NA_integer_,
    completion_status = 0L,
    completion_timestamp = "",
    time_to_completion_seconds = NA_real_,
    confidence_rating = NA_integer_,
    stringsAsFactors = FALSE
  )

  write_ab_data(rbind(data, visit))
}

log_completion <- function(
  session_id,
  assigned_group,
  entry_time,
  primary_goal,
  experience_level,
  recommended_plan,
  selected_plan,
  default_option_shown,
  confidence_rating
) {
  data <- read_ab_data()
  row_number <- tail(which(data$session_id == session_id), 1)
  completion_time <- Sys.time()

  if (length(row_number) == 0) {
    data <- rbind(
      data,
      data.frame(
        session_id = session_id,
        assigned_group = assigned_group,
        entry_timestamp = format_timestamp(entry_time),
        primary_goal = "",
        experience_level = "",
        recommended_plan = "",
        selected_path = "",
        selected_plan = "",
        default_option_shown = as.integer(default_option_shown),
        selected_plan_is_default = NA_integer_,
        changed_away_from_default = NA_integer_,
        completion_status = 0L,
        completion_timestamp = "",
        time_to_completion_seconds = NA_real_,
        confidence_rating = NA_integer_,
        stringsAsFactors = FALSE
      )
    )
    row_number <- nrow(data)
  }

  selected_is_default <- default_option_shown && selected_plan == recommended_plan
  changed_from_default <- default_option_shown && selected_plan != recommended_plan

  data[row_number, "primary_goal"] <- primary_goal
  data[row_number, "experience_level"] <- experience_level
  data[row_number, "recommended_plan"] <- recommended_plan
  data[row_number, "selected_path"] <- selected_plan
  data[row_number, "selected_plan"] <- selected_plan
  data[row_number, "default_option_shown"] <- as.integer(default_option_shown)
  data[row_number, "selected_plan_is_default"] <- if (default_option_shown) as.integer(selected_is_default) else NA_integer_
  data[row_number, "changed_away_from_default"] <- if (default_option_shown) as.integer(changed_from_default) else NA_integer_
  data[row_number, "completion_status"] <- 1L
  data[row_number, "completion_timestamp"] <- format_timestamp(completion_time)
  data[row_number, "time_to_completion_seconds"] <- round(
    as.numeric(difftime(completion_time, entry_time, units = "secs")),
    2
  )
  data[row_number, "confidence_rating"] <- as.integer(confidence_rating)

  write_ab_data(data)
}

plan_option_card <- function(
  plan_id,
  name,
  fit,
  description,
  topics,
  selected,
  show_default_badge = FALSE,
  show_suggested_badge = FALSE
) {
  div(
    class = paste("plan-option", if (selected) "selected" else ""),
    role = "radio",
    `aria-checked` = if (selected) "true" else "false",
    tabindex = "0",
    onclick = sprintf("Shiny.setInputValue('selected_plan_click', '%s', {priority: 'event'});", plan_id),
    onkeydown = sprintf(
      "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); Shiny.setInputValue('selected_plan_click', '%s', {priority: 'event'}); }",
      plan_id
    ),
    div(
      class = "plan-option-header",
      span(class = "plan-radio-dot"),
      div(
        class = "plan-title-row",
        div(class = "plan-name", name),
        if (show_default_badge) span(class = "default-badge", "Default"),
        if (show_suggested_badge) span(class = "suggested-badge", "Suggested")
      )
    ),
    div(class = "plan-fit", fit),
    p(class = "plan-desc", description),
    tags$ul(
      class = "topic-list",
      lapply(topics, tags$li)
    )
  )
}

choice_option_card <- function(input_name, option_id, title, description, selected, show_default_badge = FALSE) {
  div(
    class = paste("choice-option", if (selected) "selected" else ""),
    role = "radio",
    `aria-checked` = if (selected) "true" else "false",
    tabindex = "0",
    onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'});", input_name, option_id),
    onkeydown = sprintf(
      "if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); Shiny.setInputValue('%s', '%s', {priority: 'event'}); }",
      input_name,
      option_id
    ),
    div(
      class = "choice-option-header",
      span(class = "choice-radio-dot"),
      div(
        class = "plan-title-row",
        div(class = "choice-title", title),
        if (show_default_badge) span(class = "default-badge", "Default")
      )
    ),
    p(class = "choice-desc", description)
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("DataPath Learning Plan"),
    tags$style(HTML("
      :root {
        --navy: #102033;
        --ink: #182536;
        --muted: #607184;
        --line: #dce5ef;
        --blue: #2563eb;
        --blue-dark: #1e40af;
        --teal: #0f766e;
        --teal-soft: #e6fffb;
        --gold: #c9891d;
        --surface: #ffffff;
      }

      html, body {
        min-height: 100%;
      }

      body {
        background:
          radial-gradient(circle at 8% 8%, rgba(37, 99, 235, 0.10), transparent 28rem),
          radial-gradient(circle at 86% 16%, rgba(15, 118, 110, 0.10), transparent 24rem),
          linear-gradient(135deg, #f8fbff 0%, #eef7f4 52%, #f7f4ec 100%);
        color: var(--ink);
        font-family: Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        letter-spacing: 0;
      }

      .topbar {
        align-items: center;
        background: rgba(255, 255, 255, 0.88);
        border-bottom: 1px solid rgba(220, 229, 239, 0.95);
        display: flex;
        justify-content: space-between;
        padding: 16px 28px;
      }

      .brand {
        align-items: center;
        color: var(--navy);
        display: flex;
        font-size: 19px;
        font-weight: 800;
        gap: 10px;
      }

      .brand-mark {
        align-items: center;
        background: linear-gradient(145deg, var(--blue), var(--teal));
        border-radius: 8px;
        color: #fff;
        display: inline-flex;
        height: 32px;
        justify-content: center;
        width: 32px;
      }

      .nav-note {
        color: var(--muted);
        font-size: 14px;
        font-weight: 650;
      }

      .page-shell {
        margin: 0 auto;
        max-width: 1180px;
        padding: 44px 22px 46px;
      }

      .hero {
        align-items: center;
        display: grid;
        gap: 34px;
        grid-template-columns: minmax(0, 1fr) minmax(360px, 0.72fr);
        margin-bottom: 34px;
      }

      .eyebrow {
        align-items: center;
        color: var(--teal);
        display: flex;
        font-size: 13px;
        font-weight: 800;
        gap: 8px;
        margin-bottom: 16px;
        text-transform: uppercase;
      }

      .eyebrow-dot {
        background: var(--gold);
        border-radius: 999px;
        display: inline-block;
        height: 9px;
        width: 9px;
      }

      .hero h1 {
        color: var(--navy);
        font-size: 56px;
        font-weight: 850;
        line-height: 1.03;
        margin: 0 0 18px;
      }

      .hero-copy {
        color: var(--muted);
        font-size: 19px;
        line-height: 1.65;
        margin: 0;
        max-width: 660px;
      }

      .hero-card {
        background: rgba(255, 255, 255, 0.92);
        border: 1px solid rgba(220, 229, 239, 0.95);
        border-radius: 8px;
        box-shadow: 0 22px 54px rgba(16, 32, 51, 0.13);
        padding: 26px;
      }

      .hero-card h2 {
        color: var(--navy);
        font-size: 24px;
        font-weight: 820;
        line-height: 1.25;
        margin: 0 0 8px;
      }

      .hero-card p {
        color: var(--muted);
        line-height: 1.55;
        margin: 0 0 18px;
      }

      .signal-list {
        display: grid;
        gap: 12px;
      }

      .signal-item {
        align-items: center;
        background: #f8fafc;
        border: 1px solid var(--line);
        border-radius: 8px;
        display: flex;
        gap: 12px;
        padding: 13px;
      }

      .signal-icon {
        align-items: center;
        background: var(--teal-soft);
        border-radius: 8px;
        color: var(--teal);
        display: inline-flex;
        font-weight: 850;
        height: 32px;
        justify-content: center;
        width: 32px;
      }

      .main-card {
        background: rgba(255, 255, 255, 0.96);
        border: 1px solid rgba(220, 229, 239, 0.95);
        border-radius: 8px;
        box-shadow: 0 18px 44px rgba(16, 32, 51, 0.10);
        padding: 30px;
      }

      .section-header {
        align-items: flex-start;
        display: flex;
        gap: 22px;
        justify-content: space-between;
        margin-bottom: 22px;
      }

      .section-kicker {
        color: var(--teal);
        font-size: 13px;
        font-weight: 800;
        margin-bottom: 7px;
        text-transform: uppercase;
      }

      .section-title {
        color: var(--navy);
        font-size: 29px;
        font-weight: 850;
        margin: 0 0 7px;
      }

      .section-copy {
        color: var(--muted);
        line-height: 1.55;
        margin: 0;
        max-width: 680px;
      }

      .background-card {
        background: #f8fafc;
        border: 1px solid var(--line);
        border-radius: 8px;
        margin-bottom: 22px;
        padding: 18px;
      }

      .background-title {
        color: var(--navy);
        font-size: 17px;
        font-weight: 820;
        margin-bottom: 6px;
      }

      .background-copy {
        color: var(--muted);
        line-height: 1.45;
        margin: 0 0 14px;
      }

      .background-card .form-group {
        margin-bottom: 0;
      }

      .background-card label {
        color: #405066;
        font-size: 13px;
        font-weight: 750;
      }

      .background-divider {
        border: 0;
        border-top: 1px solid var(--line);
        margin: 18px 0;
      }

      .choice-grid {
        display: grid;
        gap: 14px;
        margin-top: 22px;
      }

      .choice-option {
        background: #fff;
        border: 1px solid var(--line);
        border-radius: 8px;
        box-shadow: 0 10px 24px rgba(16, 32, 51, 0.06);
        box-sizing: border-box;
        cursor: pointer;
        padding: 18px 20px;
        transition: border-color 160ms ease, box-shadow 160ms ease, transform 160ms ease;
        width: 100%;
      }

      .choice-option:hover {
        border-color: #9bb7ee;
        box-shadow: 0 14px 30px rgba(16, 32, 51, 0.09);
        transform: translateY(-1px);
      }

      .choice-option.selected {
        border-color: var(--blue);
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.13), 0 16px 34px rgba(16, 32, 51, 0.10);
      }

      .choice-option-header {
        align-items: center;
        display: flex;
        gap: 13px;
      }

      .choice-radio-dot {
        border: 2px solid #6b7280;
        border-radius: 999px;
        box-sizing: border-box;
        flex: 0 0 auto;
        height: 22px;
        width: 22px;
      }

      .choice-option.selected .choice-radio-dot {
        border: 6px solid var(--blue);
      }

      .choice-title {
        color: var(--navy);
        font-size: 18px;
        font-weight: 830;
        line-height: 1.3;
      }

      .choice-desc {
        color: var(--muted);
        line-height: 1.5;
        margin: 9px 0 0 35px;
      }

      .page-actions {
        align-items: center;
        display: flex;
        gap: 12px;
        justify-content: flex-end;
        margin-top: 24px;
      }

      .step-pill {
        background: #edf4ff;
        border: 1px solid #c8dcff;
        border-radius: 999px;
        color: var(--blue);
        flex: 0 0 auto;
        font-size: 13px;
        font-weight: 800;
        padding: 8px 12px;
      }

      .plan-grid .shiny-options-group {
        display: grid;
        gap: 14px;
        grid-template-columns: 1fr;
      }

      .plan-grid {
        display: grid;
        gap: 14px;
        width: 100%;
      }

      .plan-option {
        background: #fff;
        border: 1px solid var(--line);
        border-radius: 8px;
        box-sizing: border-box;
        box-shadow: 0 10px 24px rgba(16, 32, 51, 0.06);
        cursor: pointer;
        display: block;
        margin: 0;
        min-height: 0;
        padding: 22px 24px;
        position: relative;
        transition: border-color 160ms ease, box-shadow 160ms ease, transform 160ms ease;
        width: 100%;
      }

      .plan-option:hover {
        border-color: #9bb7ee;
        box-shadow: 0 14px 30px rgba(16, 32, 51, 0.09);
        transform: translateY(-1px);
      }

      .plan-option.selected {
        border-color: var(--blue);
        box-shadow: 0 0 0 3px rgba(37, 99, 235, 0.13), 0 16px 34px rgba(16, 32, 51, 0.10);
      }

      .plan-option-header {
        align-items: flex-start;
        display: flex;
        gap: 14px;
        margin-bottom: 12px;
      }

      .plan-title-row {
        align-items: center;
        display: flex;
        flex-wrap: wrap;
        gap: 10px;
      }

      .plan-radio-dot {
        border: 2px solid #6b7280;
        border-radius: 999px;
        box-sizing: border-box;
        flex: 0 0 auto;
        height: 22px;
        margin-top: 2px;
        width: 22px;
      }

      .plan-option.selected .plan-radio-dot {
        border: 6px solid var(--blue);
      }

      .plan-name {
        color: var(--navy);
        font-size: 22px;
        font-weight: 850;
        line-height: 1.3;
        margin-bottom: 0;
      }

      .default-badge {
        background: #edf4ff;
        border: 1px solid #b8d0ff;
        border-radius: 999px;
        color: var(--blue);
        display: inline-flex;
        font-size: 12px;
        font-weight: 850;
        line-height: 1;
        padding: 7px 10px;
        text-transform: uppercase;
      }

      .suggested-badge {
        background: #e6fffb;
        border: 1px solid #99f6e4;
        border-radius: 999px;
        color: var(--teal);
        display: inline-flex;
        font-size: 12px;
        font-weight: 850;
        line-height: 1;
        padding: 7px 10px;
        text-transform: uppercase;
      }

      .plan-fit {
        color: var(--teal);
        font-size: 13px;
        font-weight: 800;
        margin: 0 0 10px 36px;
        text-transform: uppercase;
      }

      .plan-desc {
        color: var(--muted);
        font-size: 15px;
        line-height: 1.6;
        margin: 0 0 12px 36px;
        max-width: 900px;
      }

      .topic-list {
        color: #405066;
        display: grid;
        font-size: 13px;
        gap: 8px 18px;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        line-height: 1.4;
        margin: 0 0 0 36px;
        padding-left: 18px;
      }

      .topic-list li {
        padding-right: 8px;
        white-space: normal;
        word-break: normal;
      }

      .confidence-row {
        align-items: center;
        background: #f8fafc;
        border: 1px solid var(--line);
        border-radius: 8px;
        display: grid;
        gap: 18px;
        grid-template-columns: minmax(0, 1fr) 260px;
        margin-top: 22px;
        padding: 18px;
      }

      .confidence-title {
        color: var(--navy);
        font-size: 17px;
        font-weight: 820;
        margin-bottom: 4px;
      }

      .confidence-copy {
        color: var(--muted);
        line-height: 1.45;
        margin: 0;
      }

      .confidence-row .form-group {
        margin: 0;
      }

      .confidence-row label {
        color: #405066;
        font-size: 13px;
        font-weight: 750;
      }

      .btn-primary,
      .btn-primary:focus {
        background: var(--blue);
        border: 0;
        border-radius: 7px;
        box-shadow: 0 10px 22px rgba(37, 99, 235, 0.22);
        color: #fff;
        font-size: 16px;
        font-weight: 800;
        min-height: 48px;
        padding: 12px 18px;
      }

      .btn-primary:hover {
        background: var(--blue-dark);
        color: #fff;
      }

      .btn-secondary,
      .btn-secondary:focus {
        background: #ffffff;
        border: 1px solid #cbd5e1;
        border-radius: 7px;
        color: var(--navy);
        font-size: 16px;
        font-weight: 800;
        min-height: 48px;
        padding: 12px 18px;
      }

      .btn-secondary:hover {
        background: #f8fafc;
        border-color: #9fb1c7;
        color: var(--navy);
      }

      .action-row {
        align-items: center;
        display: flex;
        gap: 16px;
        justify-content: space-between;
        margin-top: 24px;
      }

      .privacy-note {
        color: #718196;
        font-size: 13px;
        line-height: 1.45;
        margin: 0;
      }

      .result-card {
        background: #fff;
        border: 1px solid rgba(220, 229, 239, 0.95);
        border-radius: 8px;
        box-shadow: 0 18px 44px rgba(16, 32, 51, 0.10);
        padding: 30px;
      }

      .success-pill {
        background: #e6fffb;
        border: 1px solid #99f6e4;
        border-radius: 999px;
        color: var(--teal);
        display: inline-block;
        font-size: 13px;
        font-weight: 850;
        margin-bottom: 14px;
        padding: 8px 12px;
      }

      .result-card h2 {
        color: var(--navy);
        font-size: 32px;
        font-weight: 850;
        margin: 0 0 10px;
      }

      .result-card p {
        color: var(--muted);
        line-height: 1.6;
        margin-bottom: 20px;
      }

      .module-list {
        display: grid;
        gap: 12px;
        margin-top: 18px;
      }

      .module-item {
        align-items: flex-start;
        background: #f8fafc;
        border: 1px solid var(--line);
        border-radius: 8px;
        display: flex;
        gap: 12px;
        padding: 14px;
      }

      .module-number {
        align-items: center;
        background: var(--navy);
        border-radius: 8px;
        color: #fff;
        display: inline-flex;
        flex: 0 0 auto;
        font-size: 13px;
        font-weight: 850;
        height: 30px;
        justify-content: center;
        width: 30px;
      }

      @media (max-width: 930px) {
        .hero {
          grid-template-columns: 1fr;
        }

        .hero h1 {
          font-size: 42px;
        }

        .topic-list {
          grid-template-columns: 1fr;
        }

        .confidence-row {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 560px) {
        .topbar {
          align-items: flex-start;
          flex-direction: column;
          gap: 8px;
        }

        .page-shell {
          padding: 30px 16px;
        }

        .hero h1 {
          font-size: 34px;
        }

        .main-card,
        .result-card,
        .hero-card {
          padding: 22px;
        }

        .section-header,
        .action-row {
          align-items: flex-start;
          flex-direction: column;
        }

        .btn-primary {
          width: 100%;
        }
      }
    "))
  ),
  div(
    class = "topbar",
    div(class = "brand", span(class = "brand-mark", "D"), span("DataPath")),
    div(class = "nav-note", "Personalized data science learning plans")
  ),
  div(
    class = "page-shell",
    uiOutput("page_content")
  )
)

server <- function(input, output, session) {
  # URL query values are reactive in Shiny; isolate reads the value once so the
  # assigned group remains fixed for the full session.
  url_search <- isolate(session$clientData$url_search)
  data_download_mode <- is_data_download_mode(url_search)
  assigned_group <- resolve_group(url_search)
  default_option_shown <- assigned_group == "A"

  entry_time <- Sys.time()
  session_id <- create_session_id(session$token)
  completed <- reactiveVal(FALSE)
  final_plan <- reactiveVal(NULL)
  final_confidence <- reactiveVal(NULL)
  final_goal <- reactiveVal(NULL)
  final_experience <- reactiveVal(NULL)
  final_recommended_plan <- reactiveVal(NULL)
  current_step <- reactiveVal(1)
  initial_recommended_plan <- if (default_option_shown) weighted_plan_for_experience(default_experience) else ""
  primary_goal_state <- reactiveVal(if (default_option_shown) default_goal else "")
  experience_state <- reactiveVal(if (default_option_shown) default_experience else "")
  recommended_plan_state <- reactiveVal(initial_recommended_plan)
  selected_plan_state <- reactiveVal(if (default_option_shown) initial_recommended_plan else "")

  if (!data_download_mode) {
    log_visit(session_id, assigned_group, entry_time, default_option_shown)
  }

  output$download_data <- downloadHandler(
    filename = function() {
      paste0("default_option_ab_test_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(read_ab_data(), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )

  observeEvent(input$selected_plan_click, {
    selected_plan_state(input$selected_plan_click)
  }, ignoreInit = TRUE)

  observeEvent(input$primary_goal_click, {
    primary_goal_state(input$primary_goal_click)
  }, ignoreInit = TRUE)

  observeEvent(input$experience_level_click, {
    experience_state(input$experience_level_click)

    if (input$experience_level_click != "") {
      recommended_plan <- weighted_plan_for_experience(input$experience_level_click)
      recommended_plan_state(recommended_plan)
      if (default_option_shown) {
        selected_plan_state(recommended_plan)
      }
    } else {
      recommended_plan_state("")
      selected_plan_state("")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$next_from_goal, {
    if (primary_goal_state() == "") {
      showNotification("Please select your primary goal for learning data science.", type = "error")
      return()
    }

    current_step(2)
  })

  observeEvent(input$back_to_goal, {
    current_step(1)
  })

  observeEvent(input$next_from_experience, {
    if (experience_state() == "") {
      showNotification("Please select your current experience level.", type = "error")
      return()
    }

    recommended_plan <- weighted_plan_for_experience(experience_state())
    recommended_plan_state(recommended_plan)
    if (default_option_shown) {
      selected_plan_state(recommended_plan)
    }
    current_step(3)
  })

  observeEvent(input$back_to_experience, {
    current_step(2)
  })

  output$page_content <- renderUI({
    if (data_download_mode) {
      data <- read_ab_data()

      return(div(
        class = "result-card",
        div(class = "success-pill", "Data export"),
        h2("Download Experiment Data"),
        p(
          "This hidden export page is for the project owner. It is not shown in the normal participant flow."
        ),
        p(
          "Current rows available: ",
          strong(nrow(data))
        ),
        div(
          class = "action-row",
          p(class = "privacy-note", paste("CSV path on this server:", data_file)),
          downloadButton("download_data", "Download CSV", class = "btn-primary")
        )
      ))
    }

    if (completed()) {
      selected_plan <- final_plan()
      modules <- plan_modules(selected_plan)

      return(tagList(
        div(
          class = "result-card",
          div(class = "success-pill", "Roadmap generated"),
          h2("Your recommended Data Science Roadmap"),
          p(
            "Based on your selection, your starting path is ",
            strong(plan_label(selected_plan)),
            ". Use this four-week roadmap as a focused first step, then expand into deeper projects."
          ),
          p(
            "Background: ",
            strong(goal_label(final_goal())),
            " goal with ",
            strong(experience_label(final_experience())),
            ". Suggested path from background: ",
            strong(plan_label(final_recommended_plan())),
            "."
          ),
          div(
            class = "module-list",
            lapply(seq_along(modules), function(i) {
              div(
                class = "module-item",
                span(class = "module-number", i),
                div(modules[[i]])
              )
            })
          ),
          div(
            class = "action-row",
            p(
              class = "privacy-note",
              paste0("Confidence rating recorded: ", final_confidence(), " out of 5.")
            ),
            actionButton("back_to_selection", "Back to Plan Selection", class = "btn-secondary")
          )
        )
      ))
    }

    selected_now <- selected_plan_state()
    primary_goal_now <- primary_goal_state()
    experience_now <- experience_state()
    recommended_now <- recommended_plan_state()
    step_now <- current_step()

    hero <- div(
      class = "hero",
      div(
        div(class = "eyebrow", span(class = "eyebrow-dot"), "Data science roadmap builder"),
        h1("Build Your Data Science Learning Plan"),
        p(
          class = "hero-copy",
          "Answer three quick questions and receive a concise roadmap covering Python, SQL, statistics, machine learning, visualization, and project work."
        )
      ),
      div(
        class = "hero-card",
        h2("Plan with a practical roadmap"),
        p("DataPath helps learners turn broad career goals into a focused sequence of topics and projects."),
        div(
          class = "signal-list",
          div(class = "signal-item", span(class = "signal-icon", "1"), div("Clarify your learning goal.")),
          div(class = "signal-item", span(class = "signal-icon", "2"), div("Match the recommendation to your current experience.")),
          div(class = "signal-item", span(class = "signal-icon", "3"), div("Choose or adjust your path before generating the roadmap."))
        )
      )
    )

    if (step_now == 1) {
      return(tagList(
        hero,
        div(
          class = "main-card",
          div(
            class = "section-header",
            div(
              div(class = "section-kicker", "Learning goal"),
              h2(class = "section-title", "What is your primary goal for learning data science?"),
              p(
                class = "section-copy",
                "Choose the goal that best describes why you are building a data science roadmap."
              )
            ),
            div(class = "step-pill", "Step 1 of 3")
          ),
          div(
            class = "choice-grid",
            choice_option_card(
              input_name = "primary_goal_click",
              option_id = "job",
              title = "Get a job or internship",
              description = "Focus on portfolio projects, interview-ready skills, and tools used in entry-level roles.",
              selected = primary_goal_now == "job",
              show_default_badge = default_option_shown && default_goal == "job"
            ),
            choice_option_card(
              input_name = "primary_goal_click",
              option_id = "switch",
              title = "Career switch",
              description = "Build a structured bridge from your current background into data science work.",
              selected = primary_goal_now == "switch",
              show_default_badge = default_option_shown && default_goal == "switch"
            ),
            choice_option_card(
              input_name = "primary_goal_click",
              option_id = "academic",
              title = "Academic learning",
              description = "Strengthen coursework, research readiness, and conceptual foundations.",
              selected = primary_goal_now == "academic",
              show_default_badge = default_option_shown && default_goal == "academic"
            ),
            choice_option_card(
              input_name = "primary_goal_click",
              option_id = "personal",
              title = "Personal interest / exploration",
              description = "Explore data tools and projects at a comfortable pace without a strict career deadline.",
              selected = primary_goal_now == "personal",
              show_default_badge = default_option_shown && default_goal == "personal"
            )
          ),
          div(
            class = "page-actions",
            actionButton("next_from_goal", "Next", class = "btn-primary")
          )
        )
      ))
    }

    if (step_now == 2) {
      return(tagList(
        hero,
        div(
          class = "main-card",
          div(
            class = "section-header",
            div(
              div(class = "section-kicker", "Current experience"),
              h2(class = "section-title", "What is your current experience with data science or programming?"),
              p(
                class = "section-copy",
                "Your answer strongly influences the suggested path, but the recommendation still includes randomness and you can change it on the next page."
              )
            ),
            div(class = "step-pill", "Step 2 of 3")
          ),
          div(
            class = "choice-grid",
            choice_option_card(
              input_name = "experience_level_click",
              option_id = "none",
              title = "No experience",
              description = "You are new to programming, data tools, or statistics.",
              selected = experience_now == "none",
              show_default_badge = default_option_shown && default_experience == "none"
            ),
            choice_option_card(
              input_name = "experience_level_click",
              option_id = "basic",
              title = "Basic familiarity (e.g., Excel, beginner Python)",
              description = "You have tried basic tools or introductory coding but want more structure.",
              selected = experience_now == "basic",
              show_default_badge = default_option_shown && default_experience == "basic"
            ),
            choice_option_card(
              input_name = "experience_level_click",
              option_id = "intermediate",
              title = "Intermediate (coursework, small projects)",
              description = "You have completed coursework, notebooks, or small projects and want a stronger roadmap.",
              selected = experience_now == "intermediate",
              show_default_badge = default_option_shown && default_experience == "intermediate"
            ),
            choice_option_card(
              input_name = "experience_level_click",
              option_id = "advanced",
              title = "Advanced (strong coding, internships, or work experience)",
              description = "You are ready for deeper modeling, deployment, portfolio polish, and advanced projects.",
              selected = experience_now == "advanced",
              show_default_badge = default_option_shown && default_experience == "advanced"
            )
          ),
          div(
            class = "page-actions",
            actionButton("back_to_goal", "Back", class = "btn-secondary"),
            actionButton("next_from_experience", "Next", class = "btn-primary")
          )
        )
      ))
    }

    tagList(
      hero,
      div(
        class = "main-card",
        div(
          class = "section-header",
          div(
            div(class = "section-kicker", "Choose your learning path"),
            h2(class = "section-title", "Select the data science plan that fits your goals"),
            p(
              class = "section-copy",
              if (default_option_shown) {
                "Based on your background, DataPath has preselected a default path. You can keep it or choose a different one before generating your roadmap."
              } else {
                "Based on your background, review the available paths and actively choose the one that best fits your goals before generating your roadmap."
              }
            )
          ),
          div(class = "step-pill", "Step 3 of 3")
        ),
        div(
          class = "plan-grid",
          plan_option_card(
            plan_id = "beginner",
            name = "Beginner / Foundation",
            fit = "For newcomers",
            description = "Learn Python basics, data cleaning, simple visualization, and introductory statistics.",
            topics = c("Python fundamentals", "Data cleaning and charts", "Statistics foundations"),
            selected = selected_now == "beginner",
            show_default_badge = default_option_shown && recommended_now == "beginner"
          ),
          plan_option_card(
            plan_id = "standard",
            name = "Standard / Career Track",
            fit = "For internship and job goals",
            description = "Learn Python, SQL, statistics, machine learning basics, and real-world projects.",
            topics = c("Python and SQL workflows", "Machine learning basics", "Portfolio project practice"),
            selected = selected_now == "standard",
            show_default_badge = default_option_shown && recommended_now == "standard"
          ),
          plan_option_card(
            plan_id = "advanced",
            name = "Advanced / Intensive",
            fit = "For prior coding experience",
            description = "Build skills in model evaluation, feature engineering, deployment, and portfolio presentation.",
            topics = c("Feature engineering", "Model evaluation", "Deployment and portfolio polish"),
            selected = selected_now == "advanced",
            show_default_badge = default_option_shown && recommended_now == "advanced"
          )
        ),
        div(
          class = "confidence-row",
          div(
            div(class = "confidence-title", "How confident are you in this choice?"),
            p(class = "confidence-copy", "Use a quick 1-5 rating so the roadmap can reflect how certain you feel about your path.")
          ),
          sliderInput(
            inputId = "confidence_rating",
            label = "Confidence",
            min = 1,
            max = 5,
            value = 3,
            step = 1,
            ticks = TRUE
          )
        ),
        div(
          class = "page-actions",
          actionButton("back_to_experience", "Back", class = "btn-secondary"),
          actionButton("complete_flow", "Generate My Plan", class = "btn-primary")
        )
      )
    )
  })

  observeEvent(input$complete_flow, {
    if (completed()) {
      return()
    }

    primary_goal <- primary_goal_state() %||% ""
    experience_level <- experience_state() %||% ""
    recommended_plan <- recommended_plan_state() %||% ""
    selected_plan <- selected_plan_state() %||% ""

    if (primary_goal == "") {
      showNotification("Please select your primary goal for learning data science.", type = "error")
      return()
    }

    if (experience_level == "") {
      showNotification("Please select your current experience level.", type = "error")
      return()
    }

    if (selected_plan == "") {
      showNotification("Please choose a learning path before generating your plan.", type = "error")
      return()
    }

    confidence_rating <- input$confidence_rating %||% NA_integer_

    log_completion(
      session_id = session_id,
      assigned_group = assigned_group,
      entry_time = entry_time,
      primary_goal = primary_goal,
      experience_level = experience_level,
      recommended_plan = recommended_plan,
      selected_plan = selected_plan,
      default_option_shown = default_option_shown,
      confidence_rating = confidence_rating
    )

    final_plan(selected_plan)
    final_confidence(confidence_rating)
    final_goal(primary_goal)
    final_experience(experience_level)
    final_recommended_plan(recommended_plan)
    completed(TRUE)
  })

  observeEvent(input$back_to_selection, {
    current_step(3)
    completed(FALSE)
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)
