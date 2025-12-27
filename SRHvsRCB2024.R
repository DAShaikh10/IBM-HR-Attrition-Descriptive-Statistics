# ==============================================================================
# INITIAL SETUP
# ==============================================================================

# Clear the workspace to ensure a clean environment.
# This avoids accidental reuse of objects from previous sessions.
rm(list = ls(all.names = TRUE))

# Load the IPL ball-by-ball dataset.
# Assumes the CSV is locally available and correctly formatted.
file_path <- "data/IPL.csv"
ipl_data <- read.csv(file_path)

# Create a derived variable: current run rate (CRR).
# Defined as cumulative team runs divided by balls faced.
# NOTE:
# - This variable is mathematically unstable early in the innings
#   because the denominator (balls faced) is small.
# - It is still valid for descriptive analysis if this limitation is stated.
ipl_data$current_run_rate <- round(ipl_data$team_runs / ipl_data$ball_no, 2)

# ------------------------------------------------------------------------------
# Subset the dataset to a single match (SRH vs RCB, match_id = 1426268).
# Only variables relevant for univariate and bivariate analysis are retained.
# This ensures the analysis is focused and avoids unnecessary columns.
# ------------------------------------------------------------------------------

included_columns <- c(
  "batter",            # Nominal: batter facing the delivery
  "bowler",            # Nominal: bowler delivering the ball
  "batter_balls",      # Discrete (cumulative): balls faced by the batter
  "batter_runs",       # Discrete (cumulative): runs scored by the batter
  "batting_team",      # Nominal: batting team
  "current_run_rate",  # Continuous (derived): run rate at each delivery
  "over",              # Ordinal: over number (0–19)
  "runs_total",        # Discrete: runs scored on a single delivery
  "team_runs"          # Discrete (cumulative): team score
)

match <- subset(
  x = ipl_data,
  subset = match_id == 1426268,
  select = included_columns
)

# ==============================================================================
# UNIVARIATE ANALYSIS
# Purpose:
#   - Validate data quality and distributions
#   - Check plausibility of values
#   - NOT to explain performance, skill, or match outcome
# ==============================================================================

# ------------------------------------------------------------------------------
# Nominal variables: frequency-based summaries
# These make sense for describing exposure, not performance.
# ------------------------------------------------------------------------------

# Batter: number of balls faced by each batter.
# MAKES SENSE:
# - Shows distribution of exposure across batters.
# DOES NOT MAKE SENSE:
# - Drawing conclusions about batting quality or effectiveness.
table(match$batter)
round(prop.table(table(match$batter)), 2) * 100

# Bowler: number of balls bowled by each bowler.
# MAKES SENSE:
# - Describes workload distribution.
# DOES NOT MAKE SENSE:
# - Inferring bowling quality from frequencies.
table(match$bowler)
round(prop.table(table(match$bowler)), 2) * 100

# ------------------------------------------------------------------------------
# Cumulative variables (exploratory only)
# These are INCLUDED for completeness but are NOT meaningful univariately.
# ------------------------------------------------------------------------------

# Batter balls faced.
# DOES NOT MAKE SENSE UNIVARIATELY:
# - Values are cumulative and strictly increasing.
# - Frequency tables have no meaningful interpretation.
table(match$batter_balls)

# Batter runs.
# DOES NOT MAKE SENSE UNIVARIATELY:
# - Cumulative by construction.
# - Better analysed in a bivariate context (e.g., runs vs balls).
table(match$batter_runs)

# Team runs.
# DOES NOT MAKE SENSE UNIVARIATELY:
# - Strictly increasing cumulative variable.
# - Frequency distribution is meaningless.
table(match$team_runs)

# ------------------------------------------------------------------------------
# Batting team: nominal validation.
# MAKES SENSE:
# - Confirms near-equal number of deliveries for both teams.
# - Validates completeness of innings.
# ------------------------------------------------------------------------------

table(match$batting_team)
prop.table(table(match$batting_team))

# ------------------------------------------------------------------------------
# Over number: ordinal variable.
# MAKES SENSE:
# - Used only to confirm full coverage (overs 0–19).
# DOES NOT MAKE SENSE:
# - Deep interpretation of over frequencies.
# ------------------------------------------------------------------------------

table(match$over)
round(prop.table(table(match$over)), 2) * 100

# ------------------------------------------------------------------------------
# Runs per ball: core discrete variable.
# THIS IS THE MOST IMPORTANT UNIVARIATE VARIABLE.
# MAKES SENSE:
# - Frequency, proportions, and distributional analysis.
# ------------------------------------------------------------------------------

table(match$runs_total)
round(prop.table(table(match$runs_total)), 2) * 100

# ==============================================================================
# CUMULATIVE BOUNDARY ANALYSIS (DESCRIPTIVE)
# Purpose:
#   - Analyse boundary events via a binary per-ball indicator
#   - Describe how boundaries accumulate over time
# ==============================================================================

# Create a binary boundary indicator.
# 1 = boundary (4 or 6 runs), 0 = non-boundary.
# MAKES SENSE:
# - Converts scoring into a well-defined binary event.
match$is_boundary <- ifelse(match$runs_total %in% c(4, 6), 1, 0)

# Frequency of boundary vs non-boundary balls.
# MAKES SENSE:
# - Interpretable proportion of boundary deliveries.
table(match$is_boundary)
prop.table(table(match$is_boundary))

# Cumulative boundary count.
# MAKES SENSE DESCRIPTIVELY:
# - Shows accumulation over the match.
# DOES NOT IMPLY:
# - Momentum or causality.
match$cum_boundaries <- cumsum(match$is_boundary)

# Plot cumulative boundary progression.
# This is a descriptive trend plot only.
plot(match$cum_boundaries,
     type = "l",
     xlab = "Ball Number",
     ylab = "Cumulative Boundaries",
     main = "Cumulative Number of Boundary Events Over the Match")

# ==============================================================================
# BIVARIATE ANALYSIS
# Purpose:
#   - Explore relationships between pairs of variables
#   - Describe co-variation without causal interpretation
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Qualitative × Numeric
# Runs per ball by batting team.
# ------------------------------------------------------------------------------

# Summary statistics by team.
# MAKES SENSE:
# - Compares central tendency and variability between teams.
tapply(match$runs_total, match$batting_team, mean)
tapply(match$runs_total, match$batting_team, median)
tapply(match$runs_total, match$batting_team, sd)

# Boxplot comparison.
# MAKES SENSE:
# - Visual comparison of distributions.
boxplot(runs_total ~ batting_team,
        data = match,
        main = "Runs per Ball by Batting Team",
        xlab = "Batting Team",
        ylab = "Runs per Ball",
        varwidth = TRUE,
        col = "lightgray")

# Overlay group means.
# NOTE:
# - This is a visual aid only.
# - Mean vs median differences must be interpreted carefully.
points(1:length(levels(match$batting_team)),
       tapply(match$runs_total, match$batting_team, mean),
       pch = 19,
       col = "red")


# ------------------------------------------------------------------------------
# 2. Numeric × Numeric
# Cumulative team runs over overs.
# ------------------------------------------------------------------------------

# Scatterplot of cumulative runs against overs.
# MAKES SENSE:
# - Visualises scoring progression.
plot(match$over,
     match$team_runs,
     pch = 16,
     xlab = "Over",
     ylab = "Cumulative Team Runs",
     main = "Cumulative Team Runs vs Over")

# Correlation analysis.
# IMPORTANT:
# - Very high correlation is EXPECTED due to cumulative structure.
# - This is a descriptive validation, not a discovery.
cor(match$over, match$team_runs, use = "complete.obs")
cor(match$over, match$team_runs, method = "spearman", use = "complete.obs")

# Simple linear regression.
# MAKES SENSE DESCRIPTIVELY:
# - Quantifies average run accumulation per over.
# DOES NOT IMPLY:
# - Causality or strategy.
lm_runs_over <- lm(team_runs ~ over, data = match)
summary(lm_runs_over)

# Add regression line for visual guidance.
abline(lm_runs_over, col = "red", lwd = 2)

# ------------------------------------------------------------------------------
# 3. Numeric × Ordinal
# Current run rate over overs.
# ------------------------------------------------------------------------------

# Scatterplot of CRR.
# MAKES SENSE:
# - Shows instability early and stabilisation later.
plot(match$over,
     match$current_run_rate,
     pch = 16,
     xlab = "Over",
     ylab = "Current Run Rate",
     main = "Current Run Rate over Overs")

# LOWESS smoothing.
# MAKES SENSE:
# - Highlights overall trend without assuming linearity.
lines(lowess(match$over, match$current_run_rate),
      col = "red", lwd = 2)

# ------------------------------------------------------------------------------
# 4. Qualitative × Binary
# Boundary frequency by batting team.
# ------------------------------------------------------------------------------

# Contingency table of boundary events.
# MAKES SENSE:
# - Compares boundary proportions between teams.
table(match$batting_team, match$is_boundary)

# Conditional proportions (row-wise).
# MAKES SENSE:
# - Interpretable boundary rates per team.
prop.table(table(match$batting_team, match$is_boundary), margin = 1)

# Barplot of boundary proportions.
# DESCRIPTIVE ONLY:
# - Does not explain match outcome.
barplot(prop.table(table(match$batting_team, match$is_boundary), margin = 1)[,2],
        main = "Proportion of Boundary Balls by Batting Team",
        xlab = "Batting Team",
        ylab = "Proportion of Boundaries",
        col = "lightgray")

