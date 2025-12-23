# ==============================================================================
# IPL Statistical Computing Project
# Match: SRH vs RCB (IPL 2024)
# Data Source: Kaggle - IPL Dataset (2008–2025)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CLEAN SESSION
# ------------------------------------------------------------------------------
rm(list = ls(all.names = TRUE))

# ------------------------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------------------------
file_path <- "data/IPL.csv"
ipl_data <- read.csv(file_path)

# ------------------------------------------------------------------------------
# 2. COLUMN SELECTION
# ------------------------------------------------------------------------------
excluded_col_names <- c(
  "balls_faced","balls_per_over","bat_pos","batting_partners",
  "city","date","day","event_match_no","event_name","extra_type",
  "fielders","gender","match_number","match_type","method","month",
  "new_batter","next_batter","non_striker","non_striker_pos","overs",
  "player_of_match","player_out","result_type","review_batter",
  "review_decision","runs_bowler","runs_not_boundary","season","stage",
  "striker_out","superover_winner","team_balls","team_reviewed",
  "team_type","team_wicket","valid_ball","wicket_kind","win_outcome",
  "umpire","umpires_call","X"
)

ipl_data <- ipl_data[, !(names(ipl_data) %in% excluded_col_names)]
ipl_data <- ipl_data[, !(names(ipl_data) %in% "year")]

# ------------------------------------------------------------------------------
# 3. DERIVED VARIABLES
# ------------------------------------------------------------------------------
ipl_data$current_run_rate <- round(ipl_data$team_runs / ipl_data$ball_no, 2)

# ------------------------------------------------------------------------------
# 4. SUBSET: SRH vs RCB (2024)
# ------------------------------------------------------------------------------
specific_match <- subset(ipl_data, match_id == 1426268)

# ------------------------------------------------------------------------------
# UNIVARIATE ANALYSIS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# U1: Runs per Ball
# ------------------------------------------------------------------------------
runs_freq <- table(specific_match$runs_batter)
prop.table(runs_freq)

mean(specific_match$runs_batter)
median(specific_match$runs_batter)
sd(specific_match$runs_batter)

hist(specific_match$runs_batter,
     main = "Runs per Ball (SRH vs RCB 2024)",
     xlab = "Runs per Ball",
     col = "lightgray")

# ------------------------------------------------------------------------------
# U2: Extras per Ball
# ------------------------------------------------------------------------------
extras_freq <- table(specific_match$runs_extras)
prop.table(extras_freq)

mean(specific_match$runs_extras)
median(specific_match$runs_extras)

barplot(extras_freq,
        main = "Extras per Ball (SRH vs RCB 2024)",
        xlab = "Extras",
        ylab = "Frequency",
        col = "lightgray")

# ------------------------------------------------------------------------------
# U3: Current Run Rate
# ------------------------------------------------------------------------------
summary(specific_match$current_run_rate)
quantile(specific_match$current_run_rate)
sd(specific_match$current_run_rate)
IQR(specific_match$current_run_rate)

boxplot(specific_match$current_run_rate,
        main = "Current Run Rate (SRH vs RCB 2024)",
        ylab = "Current Run Rate",
        col = "lightgray")

plot(ecdf(specific_match$current_run_rate),
     main = "ECDF of Current Run Rate (SRH vs RCB 2024)",
     xlab = "Current Run Rate",
     ylab = "F(x)")

# ------------------------------------------------------------------------------
# U4: Over Number (Ordinal)
# ------------------------------------------------------------------------------
over_freq <- table(specific_match$over)
prop.table(over_freq)
cumsum(prop.table(over_freq))

# ------------------------------------------------------------------------------
# BIVARIATE ANALYSIS
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# BI-1: Over × Runs per Ball (by Innings)
# ------------------------------------------------------------------------------
mean_runs_over_inn1 <- tapply(
  specific_match$runs_batter[specific_match$innings == 1],
  specific_match$over[specific_match$innings == 1],
  mean
)

mean_runs_over_inn2 <- tapply(
  specific_match$runs_batter[specific_match$innings == 2],
  specific_match$over[specific_match$innings == 2],
  mean
)

ylim_range <- range(c(mean_runs_over_inn1, mean_runs_over_inn2), na.rm = TRUE)

plot(mean_runs_over_inn1,
     type = "b",
     ylim = ylim_range,
     main = "Avg Runs per Ball by Over (Innings 1)",
     xlab = "Over",
     ylab = "Avg Runs per Ball")

plot(mean_runs_over_inn2,
     type = "b",
     ylim = ylim_range,
     main = "Avg Runs per Ball by Over (Innings 2)",
     xlab = "Over",
     ylab = "Avg Runs per Ball")

# ------------------------------------------------------------------------------
# BI-2: Innings × Current Run Rate
# ------------------------------------------------------------------------------
boxplot(current_run_rate ~ innings,
        data = specific_match,
        main = "Current Run Rate by Innings",
        xlab = "Innings",
        ylab = "Current Run Rate",
        col = "lightgray")

tapply(specific_match$current_run_rate,
       specific_match$innings,
       summary)

# ------------------------------------------------------------------------------
# BI-3: Ball Number × Current Run Rate
# ------------------------------------------------------------------------------
plot(specific_match$ball_no,
     specific_match$current_run_rate,
     main = "Ball Number vs Current Run Rate",
     xlab = "Ball Number",
     ylab = "Current Run Rate")

cor(specific_match$ball_no,
    specific_match$current_run_rate,
    use = "complete.obs")

