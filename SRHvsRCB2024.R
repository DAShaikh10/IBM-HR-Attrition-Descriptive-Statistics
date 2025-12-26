rm(list = ls(all.names = TRUE))

# Load the dataset.
file_path <- "data/IPL.csv"
ipl_data <- read.csv(file_path)

ipl_data$current_run_rate <- round(ipl_data$team_runs / ipl_data$ball_no, 2)

# ball_no orbowler# ball_no or ball
names(match)
included_columns <- c(
  "batter", # 1 - Nominal
  "bowler", # 2 - Nominal
  "batter_balls", # 3 - Discrete
  "batter_runs", # 4 - Discrete
  "batting_team", # 5 - Nominal
  "current_run_rate", # 6 - Continuous
  # 7 - Ordinal - Argument for it to be discrete would be around whether
  # the distance between over be calculated?
  "over", 
  "runs_total", # 8 - Discrete
  "team_runs" # 9 - Discrete
) 

match <- subset(
  x = ipl_data,
  subset = match_id == 1426268,
  select = included_columns
)

# ==============================================================================
#                                UNIVARIATE
# ==============================================================================
# ------------------------------ FREQUENCIES -----------------------------------
# 1 - Batter 
# Absolute Frequency shows how many balls each batter faced.  
table(match$batter) 

# Relative frequency can help build story around how travis only faced 17% balls
# and made 103 or whatever.
round(prop.table(table(match$batter)), 2) * 100

# 2 - Bowler
table(match$bowler)

round(prop.table(table(match$bowler)), 2) * 100

# 3 - Batter Balls
# Won't make sense. But we can explain one example if space permits.
table(match$batter_balls) 

# 4 - Batter Runs
table(match$batter_runs) #  Same as above.

# 5 - Batting Team 
# This simply shows that RCB faced one ball extra than SRH.
table(match$batting_team)

# Relative freq won't suggest better.
prop.table(table(match$batting_team))

# 6 - CRR
# No sense to try to check freq. here.
table(match$current_run_rate)

# 7 - Over
# This will show which over had the most extras.
table(match$over)

# Not a very strong decriptive measure. 
round(prop.table(table(match$over)), 2) * 100

# 8 - Runs Total / Ball
# Good for absolute frequency again.
table(match$runs_total)

round(prop.table(table(match$runs_total)), 2) * 100

# 9 - Team runs.
# Makes no sense.
table(match$team_runs)
