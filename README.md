# DataPath Default Option A/B Test

This repository contains an R Shiny web app and a simulated A/B test analysis for a class project on how default options influence user behavior.

The app simulates a data science learning-plan platform called **DataPath**. Users answer background questions, choose a learning path, and generate a short personalized roadmap. The experiment tests whether showing default-selected options increases completion behavior compared with requiring users to actively choose each option.

## Live App

Random assignment:

https://datascience-path.shinyapps.io/datapath-ab-test/

Manual testing links:

- Group A, default condition: https://datascience-path.shinyapps.io/datapath-ab-test/?group=A
- Group B, non-default condition: https://datascience-path.shinyapps.io/datapath-ab-test/?group=B
- Hidden data export page: https://datascience-path.shinyapps.io/datapath-ab-test/?admin=data

## Research Question

Does providing default-selected options in a data science learning-plan tool increase users' likelihood of completing the plan-generation flow?

## Hypotheses

**Null hypothesis:** There is no difference in completion rate between users shown default-selected options and users not shown default-selected options.

**Alternative hypothesis:** Users shown default-selected options have a higher completion rate than users not shown default-selected options.

In notation:

```text
H0: p_default = p_no_default
H1: p_default > p_no_default
```

where `p_default` is the completion rate for Group A and `p_no_default` is the completion rate for Group B.

## Experimental Design

The app randomly assigns users to one of two groups:

| Group | Condition | Description |
|---|---|---|
| A | Default condition | Default options are pre-selected and labeled `Default` |
| B | Non-default condition | No options are pre-selected; users must actively choose |

The participant flow has three pages:

1. **Primary goal**
   - Get a job or internship
   - Career switch
   - Academic learning
   - Personal interest / exploration

2. **Current experience**
   - No experience
   - Basic familiarity
   - Intermediate
   - Advanced

3. **Learning path**
   - Beginner / Foundation
   - Standard / Career Track
   - Advanced / Intensive

In Group A, the primary goal and experience questions have defaults. The learning-path default is sampled using weighted probabilities based on the selected experience level. In Group B, no defaults are shown.

## Behavioral Assumptions

The simulation assumes that experience level strongly influences the recommended path, while still allowing randomness:

| Experience Level | Beginner | Standard | Advanced |
|---|---:|---:|---:|
| No experience | 82% | 15% | 3% |
| Basic familiarity | 50% | 45% | 5% |
| Intermediate | 8% | 77% | 15% |
| Advanced | 3% | 17% | 80% |

The simulation also assumes that defaults reduce decision effort, increasing completion rate and increasing the probability that users keep the recommended/default path.

## Primary and Secondary Metrics

Primary metric:

- Completion rate: whether the user completes the plan-generation flow

Secondary metrics:

- Retention rate: among completed users, whether the user kept the recommended/default path
- Time to completion
- Confidence rating
- Selected learning path

## Files

Recommended files to upload to GitHub:

| File | Purpose |
|---|---|
| `app.R` | Main R Shiny app code |
| `simulate_and_analyze_default_ab_test.R` | Code for simulating data and running the A/B test analysis |
| `default_option_ab_test_data.csv` | Simulated raw blended dataset with 400 users |
| `default_option_ab_test_analysis_summary.txt` | Plain-text analysis summary |
| `README.md` | Project documentation |

The folder `datapath-shinyapp/` contains the deployment copy used for shinyapps.io. It includes the app code and the CSV used by the hidden data-export page.

## How to Run the Shiny App Locally

From the project folder:

```bash
Rscript -e "shiny::runApp('.', host = '127.0.0.1', port = 3838)"
```

Then open:

```text
http://127.0.0.1:3838/
```

Manual local testing:

```text
http://127.0.0.1:3838/?group=A
http://127.0.0.1:3838/?group=B
http://127.0.0.1:3838/?admin=data
```

## How to Reproduce the Simulated Data and Analysis

Run:

```bash
Rscript simulate_and_analyze_default_ab_test.R
```

This script creates:

```text
default_option_ab_test_data.csv
default_option_ab_test_analysis_summary.txt
```

The script simulates:

- 200 users in Group A
- 200 users in Group B
- 400 total users
- blended row order, so the raw data looks like a realistic mixed collection order

## Simulated Results

The current simulation produced:

```text
Group A default condition: 200 users
Group B non-default condition: 200 users
Total simulated users: 400
```

Completion rate:

```text
Group A: 158/200 completed = 79.0%
Group B: 123/200 completed = 61.5%
Difference: +17.5 percentage points for Group A
Two-proportion test p-value: 0.00013
```

Retention rate:

```text
Group A: 118/158 retained = 74.7%
Group B: 81/123 retained = 65.9%
Difference: +8.8 percentage points for Group A
Two-proportion test p-value: 0.10624
```

Additional metrics:

```text
Average time to completion, Group A: 82.7 seconds
Average time to completion, Group B: 125.7 seconds
Average confidence, Group A: 3.80 / 5
Average confidence, Group B: 3.85 / 5
```

## Interpretation

The simulated data supports the default-option feature for the primary metric. Group A, which saw default-selected options, completed the flow at a higher rate than Group B. The completion-rate difference was statistically significant in the simulated dataset.

Retention was also higher in Group A, but the retention-rate difference was not statistically significant at the 5% level in this simulated sample.

## Deployment Notes

The app was deployed to shinyapps.io using `rsconnect`.

Example deployment command:

```r
rsconnect::deployApp(
  appDir = "datapath-shinyapp",
  appName = "datapath-ab-test",
  account = "data-analysis-tools",
  launch.browser = TRUE,
  forceUpdate = TRUE
)
```

The public URL uses the renamed shinyapps.io account:

```text
https://datascience-path.shinyapps.io/datapath-ab-test/
```

Do not commit shinyapps.io tokens or secrets to GitHub.

## Data Dictionary

| Column | Description |
|---|---|
| `session_id` | Unique simulated or live session identifier |
| `assigned_group` | A = default condition, B = non-default condition |
| `entry_timestamp` | Time when the user entered the app |
| `primary_goal` | User's stated goal for learning data science |
| `experience_level` | User's current experience level |
| `recommended_plan` | Path recommended by the weighted experience-based logic |
| `selected_path` | Final path selected by the user |
| `selected_plan` | Duplicate of selected path, kept for app compatibility |
| `default_option_shown` | 1 if defaults were shown, 0 otherwise |
| `selected_plan_is_default` | For Group A, 1 if user kept the default path |
| `changed_away_from_default` | For Group A, 1 if user changed away from the default path |
| `completion_status` | 1 if the user completed the flow, 0 otherwise |
| `completion_timestamp` | Time when the user completed the flow |
| `time_to_completion_seconds` | Seconds from entry to completion |
| `confidence_rating` | User confidence rating from 1 to 5 |

## Required R Packages

The Shiny app uses:

```r
shiny
```

The app optionally uses `uuid` if installed; otherwise it falls back to a generated session ID.

The simulation and analysis script uses base R only.
