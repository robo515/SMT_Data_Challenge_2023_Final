library(dplyr)
library(DBI)
library(gifski)
library(ggplot2)
library(gganimate)
library(dbplyr)
library(plotly)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(scales)
library(gridExtra)
library(grid)
set.seed(123)
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/')

# This file repeats the exact same analysis as in our main submission (Smt-Data-Challenge-Submission.R)
# 

# To upload every file into R, I looped through the files in each folder and created
# Dataframes for every csv using the pathnames. To get this to run, you'll need
# to change the pathname to your own directory's pathname to the correct csv

# game events
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/game_events")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
game_events <- dplyr::bind_rows(dataframes)

# game info
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/game_info')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/game_info")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
game_info <- dplyr::bind_rows(dataframes)

# ball pos
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/ball_pos')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/ball_pos")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
ball_pos <- dplyr::bind_rows(dataframes)

# team info:
team_info <- read.csv('/Users/zacambrose/Downloads/smt_data_challenge_2023/team_info.csv')

# Team A1 1903 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1903_TeamA1')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1903_TeamA1")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1903A1 <- dplyr::bind_rows(dataframes)

# Team A1 1902 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1902_TeamA1')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA1/player_pos-1902_TeamA1")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1902A1 <- dplyr::bind_rows(dataframes)

# Team A2 1903 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1903_TeamA2')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1903_TeamA2")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1903A2 <- dplyr::bind_rows(dataframes)

# Team A2 1902 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1902_TeamA2')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA2/player_pos-1902_TeamA2")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1902A2 <- dplyr::bind_rows(dataframes)

# Team A3 1903 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1903_TeamA3')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1903_TeamA3")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1903A3 <- dplyr::bind_rows(dataframes)

# Team A3 1902 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1902_TeamA3')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1902_TeamA3")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1902A3 <- dplyr::bind_rows(dataframes)

# Team A3 1901 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1901_TeamA3')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamA3/player_pos-1901_TeamA3")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1901A3 <- dplyr::bind_rows(dataframes)

# Team B 1903 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1903_TeamB')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1903_TeamB")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1903B <- dplyr::bind_rows(dataframes)

# Team B 1902 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1902_TeamB')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1902_TeamB")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1902B <- dplyr::bind_rows(dataframes)

# Team B 1901 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1901_TeamB')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1901_TeamB")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1901B <- dplyr::bind_rows(dataframes)

# Team B 1900 player_pos:
setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1900_TeamB')
folder <- c("/Users/zacambrose/Downloads/smt_data_challenge_2023/player_pos/TeamB/player_pos-1900_TeamB")

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
player_pos_1900B <- dplyr::bind_rows(dataframes)

# Create a vector of databases
dataframes <- c(game_events, game_info, ball_pos, team_info, player_pos_1903A1, player_pos_1902A1, player_pos_1903A2, player_pos_1902A2, player_pos_1903A3, player_pos_1902A3, player_pos_1901A3,player_pos_1903B,player_pos_1902B,player_pos_1901B,player_pos_1900B)

# Create lists of player position tables total and for each year
player_pos_total_list <- list(player_pos_1903A1, player_pos_1902A1, player_pos_1903A2, player_pos_1902A2, player_pos_1903A3, player_pos_1902A3, player_pos_1901A3,player_pos_1903B,player_pos_1902B,player_pos_1901B,player_pos_1900B)
player_pos_total <- dplyr::bind_rows(player_pos_total_list)
player_pos_total

player_pos_1903_list <- list(player_pos_1903A1, player_pos_1903A2, player_pos_1903A3,player_pos_1903B)
player_pos_1903 <- dplyr::bind_rows(player_pos_1903_list)
player_pos_1902_list <- list(player_pos_1902A1, player_pos_1902A2, player_pos_1902A3,player_pos_1902B)
player_pos_1902 <- dplyr::bind_rows(player_pos_1902_list)
player_pos_1901_list <- list(player_pos_1901A3,player_pos_1901B)
player_pos_1901 <- dplyr::bind_rows(player_pos_1901_list)

setwd('/Users/zacambrose/Downloads/smt_data_challenge_2023/')

# Connect to RSQLite
mydb <- dbConnect(RSQLite::SQLite(), "")

# Create all the tables
dbWriteTable(mydb, "game_events", game_events)
dbWriteTable(mydb, "game_info", game_info)
dbWriteTable(mydb, "ball_pos", ball_pos)
dbWriteTable(mydb, "team_info", team_info)
dbWriteTable(mydb, "player_pos", player_pos_total)
dbWriteTable(mydb, "player_pos_1903A1", player_pos_1903A1)
dbWriteTable(mydb, "player_pos_1902A1", player_pos_1902A1)
dbWriteTable(mydb, "player_pos_1903A2", player_pos_1903A2)
dbWriteTable(mydb, "player_pos_1902A2", player_pos_1902A2)
dbWriteTable(mydb, "player_pos_1903A3", player_pos_1903A3)
dbWriteTable(mydb, "player_pos_1902A3", player_pos_1902A3)
dbWriteTable(mydb, "player_pos_1901A3", player_pos_1901A3)
dbWriteTable(mydb, "player_pos_1903B", player_pos_1903B)
dbWriteTable(mydb, "player_pos_1902B", player_pos_1902B)
dbWriteTable(mydb, "player_pos_1901B", player_pos_1901B)
dbWriteTable(mydb, "player_pos_1900B", player_pos_1900B)
dbWriteTable(mydb, "player_pos_1903", player_pos_1903)
dbWriteTable(mydb, "player_pos_1902", player_pos_1902)
dbWriteTable(mydb, "player_pos_1901", player_pos_1901)
# Define function to get the batter's handedness from a table
get_batter_cluster_kmeans <- function(table_name) {
  
  # SQL query string
  query_string <- paste0("SELECT DISTINCT game_info.batter, min_timestamps.game_str, min_timestamps.play_id, game_info.at_bat, min_timestamps.min_timestamp, ", table_name, ".field_x, ", table_name, ".field_y
    FROM game_info
    LEFT JOIN (
        SELECT game_str, play_id, MIN(timestamp) as min_timestamp
        FROM ", table_name, "
        GROUP BY game_str, play_id
    ) as min_timestamps ON game_info.play_per_game = min_timestamps.play_id
    LEFT JOIN ", table_name, " ON min_timestamps.play_id = ", table_name, ".play_id
    WHERE ", table_name, ".player_position = 10 AND ", table_name, ".game_str = game_info.game_str AND ", table_name, ".timestamp = min_timestamps.min_timestamp AND ", table_name, ".field_x < 6 AND ", table_name, ".field_x > -6 AND game_info.batter != 0 AND ", table_name, ".field_y > -4 AND ", table_name, ".field_y < 4
    ORDER BY game_info.batter")
  
  # Get data from database
  all_batters <- dbGetQuery(mydb, query_string)
  
  # Perform the slicing, kmeans clustering, and calculations
  field_x_mean <- all_batters %>%
    group_by(game_str, at_bat) %>%
    slice(1) %>%
    ungroup() %>%
    group_by(batter) %>%
    summarise(mean_field_x = mean(field_x, na.rm = TRUE), var_field_x = var(field_x, na.rm = TRUE)) %>%
    na.omit() %>%
    mutate(cluster = kmeans(.[, "mean_field_x", drop = FALSE], centers = 3)$cluster)
  
  # Calculate means for each cluster and order them
  cluster_means_df <- field_x_mean %>%
    group_by(cluster) %>%
    summarise(cluster_mean = mean(mean_field_x)) %>%
    arrange(cluster_mean) %>%
    mutate(cluster_names = factor(1:3, labels = c("R","S","L")))
  
  # Generate the final batter_cluster dataframe
  batter_cluster <- field_x_mean %>%
    select(batter, cluster, mean_field_x, var_field_x) %>%
    left_join(cluster_means_df, by = 'cluster') %>%
    select(batter, cluster = cluster_names,mean_field_x, var_field_x) %>%
    na.omit()
  
  all_at_bats <- all_batters %>%
    group_by(game_str, at_bat) %>%
    left_join(batter_cluster, by = 'batter') %>%
    na.omit()
  
  print(ggplot(data.frame(all_at_bats), aes(x = field_x, y = field_y, color = as.factor(cluster))) +
          geom_point(alpha = 0.7) +
          geom_jitter() +
          theme_minimal() +
          labs(title = "Predicted batter handedness 1902", x = "field_x", y= "field_y", color="cluster"))
  # Return the final dataframe
  return(batter_cluster)
}
### Save batter cluster to variable
batter_cluster_kmeans_1902 <- get_batter_cluster_kmeans("player_pos_1902")
batter_cluster_kmeans_1902
value_counts_1902 <- table(batter_cluster_kmeans_1902$cluster)
value_counts_1902
batter_cluster_kmeans_1902 <- get_batter_cluster_kmeans("player_pos_1902")

color_palette <- c("#FA92FB","#A653F5","#65B8BF")

# Every possible play for our analysis in outfield (any play with ball hit into a play + a play acquires it or it bounces)
usable_game_events <- game_events %>%
  group_by(game_str, play_id) %>%
  filter(any(event_code == 4) & any(event_code == 2)) %>%
  select(!X)

# Merge the usable game events (events with a ball in play, ball acquired, or a hit) with game_info
hits_bounces_outfield <- usable_game_events %>%
  inner_join(game_info, by = c("game_str", "play_per_game"))

# Find all of the BIP with initial bounce, and all BIP initially caught
hits_bounces_outfield <- hits_bounces_outfield %>%
  group_by(game_str, play_id) %>%  # for each game and play
  mutate(
    event_shifted = lag(event_code),  # shift event codes down
    bounce_after_bip = event_code == 16 & event_shifted == 4,  # find bounces after ball in play
    acquired_after_bip = (event_code == 2 & event_shifted == 4) | (event_code == 7 & event_shifted == 4)  # find acquisitions after ball in play
  )
bounce_plays <- hits_bounces_outfield %>%
  filter(bounce_after_bip)
catch_plays <- hits_bounces_outfield %>%
  filter(acquired_after_bip)

# Now join bounce plays and catch plays with the ball position at the same timestamp, play id, game string as bounce plays and catch plays and 
# find balls hit into outfield and add a radius column for the ball position, then filter with radius > 180
bounce_ball_pos <- inner_join(bounce_plays, ball_pos, by = c("game_str", "play_id", "timestamp")) %>%
  mutate(radius = sqrt(ball_position_x**2 + ball_position_y**2)) %>%
  filter(radius > 180)
caught_ball_pos <- inner_join(catch_plays, ball_pos, by = c("game_str", "play_id", "timestamp")) %>%
  mutate(radius = sqrt(ball_position_x**2 + ball_position_y**2)) %>%
  filter(radius > 180)

caught_ball_pos %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y), color = "#65B8BF") +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Catch locations")

bounce_ball_pos %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y), color = "#A653F5") +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Bounce locations")
bounce_ball_pos <- bounce_ball_pos %>%
  rename('timestamp_bounce' = 'timestamp') 
# We're finding player positions at the same play_id, game_str where event code = 4 for all plays in bounce_ball_pos


# Now we also want to find ALL balls both bounced and hit into play
outfield_BIP <- bind_rows(caught_ball_pos, bounce_ball_pos)
outfield_BIP %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y), color = "#65B8BF") +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Outfield BIP locations")

# THIS PART TAKEN OUT BECAUSE WE WANT 1903 KMEANS
# Kmeans analysis of all outfield balls in play (not L vs R)
# ball_positions <- outfield_BIP[,c('ball_position_x', 'ball_position_y')]
# kmeans_result <- kmeans(ball_positions, centers=3, nstart=25)
# Create a data frame for the TOTAL centroids (IMPORTANT)
# centroids <- as.data.frame(kmeans_result$centers)
# names(centroids) <- c("ball_position_x", "ball_position_y")

# Create the base plot
p <- ggplot(ball_positions, aes(x=ball_position_x, y=ball_position_y)) +
  geom_point(aes(color=factor(kmeans_result$cluster))) + 
  scale_color_discrete(name  ="Clusters") +
  theme_minimal()

# Add the centroids to the plot
p + geom_point(data=centroids, aes(x=ball_position_x, y=ball_position_y), size=6, shape=4, color="black")

### The following code block is repeating the outfield Balls In Play analysis for
### Lefties/Righties, this time only in 1902 so we can merge it with the 1902 kmeans results
###

setwd('/Users/zacambrose/Desktop/game_events1902')
folder <- c('/Users/zacambrose/Desktop/game_events1902')

files <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
dataframes <- list()
for (file in files) {
  data <- read.csv(file)
  dataframes <- append(dataframes, list(data))
}
game_events1902 <- dplyr::bind_rows(dataframes)

usable_game_events1902 <- game_events1902 %>%
  group_by(game_str, play_id) %>%
  filter(any(event_code == 4) & any(event_code == 2)) %>%
  select(!X)

# Merge the usable game events (events with a ball in play, ball acquired, or a hit) with game_info
hits_bounces_outfield1902 <- usable_game_events1902 %>%
  inner_join(game_info, by = c("game_str", "play_per_game"))

# Find all of the BIP with initial bounce, and all BIP initially caught
hits_bounces_outfield1902 <- hits_bounces_outfield1902 %>%
  group_by(game_str, play_id) %>%  # for each game and play
  mutate(
    event_shifted = lag(event_code),  # shift event codes down
    bounce_after_bip = event_code == 16 & event_shifted == 4,  # find bounces after ball in play
    acquired_after_bip = (event_code == 2 & event_shifted == 4) | (event_code == 7 & event_shifted == 4)  # find acquisitions after ball in play
  )

bounce_plays1902 <- hits_bounces_outfield1902 %>%
  filter(bounce_after_bip)

catch_plays1902 <- hits_bounces_outfield1902 %>%
  filter(acquired_after_bip)
# Now join bounce plays and catch plays with the ball position at the same timestamp, play id, game string as bounce plays and catch plays and 
# find balls hit into outfield
bounce_ball_pos1902 <- inner_join(bounce_plays1902, ball_pos, by = c("game_str", "play_id", "timestamp")) %>%
  mutate(radius = sqrt(ball_position_x**2 + ball_position_y**2)) %>%
  filter(radius > 180)
caught_ball_pos1902 <- inner_join(catch_plays1902, ball_pos, by = c("game_str", "play_id", "timestamp")) %>%
  mutate(radius = sqrt(ball_position_x**2 + ball_position_y**2)) %>%
  filter(radius > 180)

caught_ball_pos1902 %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y, color=as.factor(player_position))) +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Catch locations") +
  scale_color_discrete(name = 'Player Position')

bounce_ball_pos1902 %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y), color = "#A653F5") +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Bounce locations")

# Now we also want to find ALL balls hit into play in outfield in 1902
outfield_BIP1902 <- bind_rows(caught_ball_pos1902, bounce_ball_pos1902)

outfield_BIP1902 %>%
  ggplot(aes(x = ball_position_x)) +
  geom_point(aes(y = ball_position_y, color = as.factor(player_position))) +
  labs(y = "y-pos", 
       x = "x-pos", 
       title = "Outfield BIP 1902 locations") +
  scale_color_discrete(name = "Player Position")

merged_data <- merge(outfield_BIP1902, batter_cluster_kmeans_1902, by = "batter")

# Find all the plays where the batter is a Lefty
lefty_BIP_1902 <- subset(merged_data, cluster == "L")

# Find all the plays where the batter is a Righty
righty_BIP_1902 <- subset(merged_data, cluster == "R")

### Now: I want to find the clusters across lefties, then righties within 1902

# Lefties:
ball_positions_1902L <- lefty_BIP_1902[,c('ball_position_x', 'ball_position_y')]
kmeans_result_1902L <- kmeans(ball_positions_1902L, centers=3, nstart=25)
# Create a data frame for the centroids
centroids_1902L <- as.data.frame(kmeans_result_1902L$centers)
names(centroids_1902L) <- c("ball_position_x", "ball_position_y")

lefties_cluster1902 <- ggplot(ball_positions_1902L, aes(x=ball_position_x, y=ball_position_y)) +
  geom_point(aes(color=factor(kmeans_result_1902L$cluster))) + 
  labs(title = 'Outfield BIP clusters, L 1902') +
  scale_color_discrete(name  ="Clusters") +
  theme_minimal()

# Add the centroids to the plot
lefties_cluster1902 + geom_point(data=centroids_1902L, aes(x=ball_position_x, y=ball_position_y), size=6, shape=4, color="black")

# Righties:
ball_positions_1902R <- righty_BIP_1902[,c('ball_position_x', 'ball_position_y')]
kmeans_result_1902R <- kmeans(ball_positions_1902R, centers=3, nstart=25)
# Create a data frame for the centroids
centroids_1902R <- as.data.frame(kmeans_result_1902R$centers)
names(centroids_1902R) <- c("ball_position_x", "ball_position_y")

# Create the base plot
righties_cluster1902 <- ggplot(ball_positions_1902R, aes(x=ball_position_x, y=ball_position_y)) +
  geom_point(aes(color=factor(kmeans_result_1902R$cluster))) + 
  labs(title = 'Outfield BIP clusters, R 1902') +
  scale_color_discrete(name  ="Clusters") +
  theme_minimal()

# Add the centroids to the plot
righties_cluster1902 + geom_point(data=centroids_1902R, aes(x=ball_position_x, y=ball_position_y), size=6, shape=4, color="black")

print(centroids_1902R)
print(centroids_1902L)

setwd('/Users/zacambrose/Downloads/SMT_Data_2023')


# Outfield parameters

outfield_center_x <- 0
outfield_center_y <- 60
outfield_radius <- 100
val <- 15


# Filter plays where the flyball is outside the outfield circle
# We need to find all outfield_plays that are also in outfield range official hitbounce
outfield_plays <- ball_pos %>%
  group_by(game_str, play_id) %>%
  filter(max(ball_position_z) > val) %>%
  filter((ball_position_x - outfield_center_x)^2 + (ball_position_y - outfield_center_y)^2 > outfield_radius^2) %>%
  mutate(max_z = max(ball_position_z))

# The "outfield_plays" dataframe now contains all the plays where a flyball was hit outside the outfield circle with center (0, 60) and radius 95 feet.
plot(outfield_plays$ball_position_x, outfield_plays$ball_position_y)


#Find all game events where player_pos = 7, 8 0r 9, & where event code = 2
outfield_received_gameevents <- game_events %>%
  filter(player_position %in% c(7, 8, 9) & event_code == 2)
#This has the timestamp for when they caught the ball
ball_hit_in_play <- game_events %>%
  filter(event_code == 4) %>% group_by(game_str, play_id)

#Ball_bounce = the first ball bounce of every play
ball_bounce <- game_events %>%
  group_by(game_str,play_id) %>% 
  mutate(event_last = lag(event_code)) %>% 
  arrange(timestamp) %>% 
  filter(event_code == 16 & event_last == 4) %>%
  slice(1) %>% 
  rename("timestamp_bounce" = "timestamp")

# Used in hits_bounces_outfield to find all play's hit into play and received
usable_game_events1902 <- game_events1902 %>%
  group_by(game_str, play_id) %>%
  filter(any(event_code == 4) & any(event_code == 2)) %>%
  select(!X)

# Merge the usable game events (events with a ball in play, ball acquired, or a hit) with game_info
hits_bounces_outfield1902 <- usable_game_events1902 %>%
  inner_join(game_info, by = c("game_str", "play_per_game"))

hits_bounces_outfield1902 <- game_events1902 %>%
  group_by(game_str, play_id) %>%  # for each game and play
  mutate(
    event_last = lag(event_code), # shift event codes down
    bounce_after_bip = (event_code == 16 & event_last == 4),  # find bounces after ball in play
    acquired_after_bip = (event_code == 2 & event_last == 4) | (event_code == 7 & event_last == 4)  # find acquisitions after ball in play
  ) %>%
  filter(any(bounce_after_bip == TRUE | acquired_after_bip == TRUE))


### Adding ball position data to hits bounces with distance from center of infield. Only consider balls that are either caught
### by an outfielder or bounced in the outfield then caught by an outfielder

ballpos_bounces_hits <- merge(hits_bounces_outfield1902, ball_pos, by = c("game_str", "play_id", "timestamp")) %>%
  mutate(ball_dist_center = sqrt((ball_position_x - outfield_center_x)^2 + (ball_position_y - outfield_center_y)^2))

first_bounces <- ballpos_bounces_hits %>%
  arrange(timestamp) %>%
  filter(event_code == 16) %>%
  group_by(game_str, play_id) %>%
  summarise(first_bounce_time = first(timestamp), first_bounce_distance = first(ball_dist_center), .groups = "keep") %>%
  filter(first_bounce_distance > 100)

# Join the first bounce times with the original data
outfield_bounces <- left_join(first_bounces, ballpos_bounces_hits, by = c("game_str", "play_id"))

bounce_ball_pos_cleaned <- subset(bounce_ball_pos, select = c(game_str, play_id, timestamp_bounce, ball_position_x, ball_position_y))

hit_game_events <- usable_game_events %>% filter(event_code == 4)

# Merge hit_game_events with player_pos1902
hit_time_when_ball_bounced1902 <- left_join(hit_game_events, player_pos_1902, by=c("game_str","play_id","timestamp"))
hit_time_when_ball_bounced1902 <- hit_time_when_ball_bounced1902[hit_time_when_ball_bounced1902$player_position.y == 7 | hit_time_when_ball_bounced1902$player_position.y == 8 |hit_time_when_ball_bounced1902$player_position.y == 9,]
outfield_hit_time_when_ball_bounced1902 <- subset(hit_time_when_ball_bounced1902, substr(game_str,1,4) == "1902")
# outfield_hit_time_when_ball_bounced1902 is the game, play id where the ball bounced with the timestamp when the ball was hit and the player positions when the ball was hit
# we will use this in order to create official_outfield_range_hitbounce

bounce_ball_pos_cleaned
bounce_ball_pos_cleaned <- rename(bounce_ball_pos_cleaned, bounce_position_x = ball_position_x, bounce_position_y = ball_position_y)
filtered_events <- game_events %>%
  filter(player_position %in% c(7, 8, 9) & event_code == 2)

# acquired_after_bounce1902 is the game events where the ball was acquired after the ball was bounced
acquired_after_bounce1902 <- game_events1902 %>%
  group_by(game_str, play_id) %>%  # for each game and play
  mutate(
    event_last = lag(event_code), # shift event codes down
    acquired_after_bounce = (event_code == 2 & event_last == 16)  # find acquisitions after ball in play
  ) %>%
  filter(any(acquired_after_bounce == TRUE))

acquired_after_bounce1902 <- acquired_after_bounce1902 %>%
  group_by(game_str, play_id) %>%                              # Group by play_id
  filter((event_code == 4 & lead(event_code) == 16) | # Look for rows where event_code is 4 and the next is 16
           (lag(event_code) == 4 & event_code == 16)) %>% # Look for rows where event_code is 16 and the previous is 4
  ungroup()

bounces <- acquired_after_bounce1902[acquired_after_bounce1902$event_code == 16,] %>%
  group_by(game_str, play_id)
bounces <- rename(bounces, timestamp_bounce = timestamp)
hits <- acquired_after_bounce1902[acquired_after_bounce1902$event_code == 4,] %>%
  group_by(game_str, play_id)
hits <- rename(hits, timestamp_hit = timestamp)
keeps <- c("game_str","play_id","timestamp_hit")
hits <- hits[keeps]
hits_and_bounces <- hits %>%
  left_join(bounces, by = c("game_str","play_id"))

kept <- c("game_str","play_id","player_position")

filtered_events <- filtered_events[kept]
hits_and_bounces
newdf <- hits_and_bounces %>%
  left_join(filtered_events, by = c("game_str","play_id")) %>%
  drop_na('player_position.y')

bounce_ball_df <- newdf %>%
  left_join(bounce_ball_pos_cleaned, by = c("game_str","play_id")) %>%
  drop_na('bounce_position_x') 


bounce_ball_df <- left_join(bounce_ball_df, player_pos_1902, by = c("game_str" = "game_str", "play_id" = "play_id", "player_position.y" = "player_position", "timestamp_hit" = "timestamp"))
bounce_ball_df <- bounce_ball_df %>%
  select(-c(X.x, player_position.x, event_last, timestamp_bounce.x, X.y,event_code,acquired_after_bounce))
bounce_ball_df <- rename(bounce_ball_df, timestamp_bounce = timestamp_bounce.y)
bounce_ball_df <- rename(bounce_ball_df, player_position = player_position.y)

bounce_ball_df$timestamp_hit <- bounce_ball_df$timestamp_hit / 1000
bounce_ball_df$timestamp_bounce <- bounce_ball_df$timestamp_bounce / 1000
bounce_ball_df$hangtime <- bounce_ball_df$timestamp_bounce - bounce_ball_df$timestamp_hit

player_1902_outfield_pos_rf <- player_pos_1902[player_pos_1902$player_position == 9,]
player_1902_outfield_pos_cf <- player_pos_1902[player_pos_1902$player_position == 8,]
player_1902_outfield_pos_lf <- player_pos_1902[player_pos_1902$player_position == 7,]

player_1902_outfield_pos <- rbind(player_1902_outfield_pos_rf, player_1902_outfield_pos_cf, player_1902_outfield_pos_lf)

### Now, find average starting positions for each position, LF, CF, RF
rf_starting_positions <- player_1902_outfield_pos_rf %>%
  group_by(game_str, play_id) %>%
  filter(timestamp == min(timestamp)) %>%
  ungroup()

cf_starting_positions <- player_1902_outfield_pos_cf %>%
  group_by(game_str, play_id) %>%
  filter(timestamp == min(timestamp)) %>%
  ungroup()

lf_starting_positions <- player_1902_outfield_pos_lf %>%
  group_by(game_str, play_id) %>%
  filter(timestamp == min(timestamp)) %>%
  ungroup()

#cf
mean(cf_starting_positions$field_x)
mean(cf_starting_positions$field_y)
cf_avg_pos <- c(mean(cf_starting_positions$field_x),mean(cf_starting_positions$field_y))

#rf
mean(rf_starting_positions$field_x)
mean(rf_starting_positions$field_y)
rf_avg_pos <- c(mean(rf_starting_positions$field_x),mean(rf_starting_positions$field_y))


#lf
mean(lf_starting_positions$field_x)
mean(lf_starting_positions$field_y)
lf_avg_pos <- c(mean(lf_starting_positions$field_x),mean(lf_starting_positions$field_y))

lf_bounces <- bounce_ball_df[bounce_ball_df$player_position == 7,]
cf_bounces <- bounce_ball_df[bounce_ball_df$player_position == 8,]
rf_bounces <- bounce_ball_df[bounce_ball_df$player_position == 9,]



# lf outs:
lf_bounces$bounce_dist_from_avgstart <- sqrt((lf_bounces$bounce_position_x - lf_avg_pos[1])^2 + (lf_bounces$bounce_position_y - lf_avg_pos[2])^2)
lf_bounces$out <- ifelse(splits_mean * lf_bounces$hangtime > lf_bounces$bounce_dist_from_avgstart, 1, 0)
table(lf_bounces$out)
# cf outs:
cf_bounces$bounce_dist_from_avgstart <- sqrt((cf_bounces$bounce_position_x - cf_avg_pos[1])^2 + (cf_bounces$bounce_position_y - cf_avg_pos[2])^2)
cf_bounces$out <- ifelse(splits_mean * cf_bounces$hangtime > cf_bounces$bounce_dist_from_avgstart, 1, 0)
plot <- ggplot(cf_bounces, aes(x=cf_bounces$bounce_position_x,y=cf_bounces$bounce_position_y,color=as.factor(cf_bounces$out))) +
  geom_point() +
  geom_point(aes(x=cf_avg_pos[1],y=cf_avg_pos[2]), color = "goldenrod",size=4)
plotcf <- plot + ggtitle("Cf Outs generated with 75th percentile speeds at average starting pos")

table(cf_bounces$out)
# rf outs:
rf_bounces$bounce_dist_from_avgstart <- sqrt((rf_bounces$bounce_position_x - rf_avg_pos[1])^2 + (rf_bounces$bounce_position_y - rf_avg_pos[2])^2)
plotrf <- ggplot(rf_bounces, aes(x=rf_bounces$bounce_position_x,y=rf_bounces$bounce_position_y,color=as.factor(rf_bounces$out))) +
  geom_point() +
  geom_point(aes(x=rf_avg_pos[1],y=rf_avg_pos[2]), color = "goldenrod",size=4) +
  ggtitle("Rf Outs generated with 75th percentile speeds at average starting pos")
table(rf_bounces$out)
rf_bounces$out <- ifelse(splits_mean * rf_bounces$hangtime > rf_bounces$bounce_dist_from_avgstart, 1, 0)
table(rf_bounces$out)

plotlf <- ggplot(lf_bounces, aes(x=lf_bounces$bounce_position_x,y=lf_bounces$bounce_position_y,color=as.factor(lf_bounces$out))) +
  geom_point()
print(plotlf)
plotcf <- ggplot(cf_bounces, aes(x=cf_bounces$bounce_position_x,y=cf_bounces$bounce_position_y)) +
  geom_point(color="#65B8BF") +
  labs(x='X pos bounces',y='Y pos bounces') +
  ggtitle('Bounces in Center')
plotcf
### END 8/23 IMPORTANT CODE 


### With kmeans positions:
centroids
rf_kmeans_pos <- c(centroids[1,])
cf_kmeans_pos <- c(centroids[2,])
lf_kmeans_pos <- c(centroids[3,])

# RED = HITS, BLUE = OUTS, GOLD = KMEANS, PURPLE = AVG. START POS

# Source: https://baseballsavant.mlb.com/running_splits?type=raw&bats=&year=2023&position=&team=&min=5
running_splits <- read.csv('/Users/zacambrose/Downloads/running_splits.csv')
running_splits$zero_sixty <- 60 / running_splits$seconds_since_hit_060
splits_mean <- mean(running_splits$zero_sixty)

# Rf 75th percentile:
rf_bounces$bounce_dist_from_kmeanstart <- sqrt((rf_bounces$bounce_position_x - rf_kmeans_pos[[1]])^2 + (rf_bounces$bounce_position_y - rf_kmeans_pos[[2]])^2)
rf_bounces$out <- ifelse(splits_mean * rf_bounces$hangtime > rf_bounces$bounce_dist_from_kmeanstart, 1, 0)
rf_kmeans_pos
plotrf <- ggplot(rf_bounces, aes(x=rf_bounces$bounce_position_x, y=rf_bounces$bounce_position_y, color=as.factor(rf_bounces$out))) +
  geom_point() +
  geom_point(aes(x=rf_kmeans_pos[[1]], y=rf_kmeans_pos[[2]]), color = "goldenrod", size=6, shape=4) +
  geom_point(aes(x=rf_avg_pos[1], y=rf_avg_pos[2]), color = "violet", size=6, shape=4) +
  
  # Arrow from violet point to goldenrod point for plotrf
  geom_segment(aes(x = rf_avg_pos[1], y = rf_avg_pos[2], 
                   xend = rf_kmeans_pos[[1]], yend = rf_kmeans_pos[[2]]),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "black", size = 0.3) +
  
  ggtitle("Rf Outs") + 
  theme(legend.position = "none") +
  labs(x = 'X-axis position', y = 'Y-axis position')
plotrf

table(rf_bounces$out)

# From statcast: The Major League average on a "competitive" play is 27 ft/sec, and the competitive range is roughly from 23 ft/sec (poor) to 30 ft/sec (elite). A player must have at least 10 competitive runs to qualify for this leaderboard.
# Lf 75th percentile:

# Should point from 1902 avg_pos to kmeans_pos
# centroids is from 1903 data, manually hardcoded in
centroids <- data.frame(ball_position_x = c(110.554146, -1.231608, -110.175482), ball_position_y = c(232.5459, 310.3384, 225.8532))

lf_bounces$bounce_dist_from_kmeanstart <- sqrt((lf_bounces$bounce_position_x - lf_kmeans_pos[[1]])^2 + (lf_bounces$bounce_position_y - lf_kmeans_pos[[2]])^2)
lf_bounces$out <- ifelse(splits_mean * lf_bounces$hangtime > lf_bounces$bounce_dist_from_kmeanstart, 1, 0)
plotlf <- ggplot(lf_bounces, aes(x=lf_bounces$bounce_position_x, y=lf_bounces$bounce_position_y, color=as.factor(lf_bounces$out))) +
  geom_point() +
  geom_point(aes(x=lf_kmeans_pos[[1]], y=lf_kmeans_pos[[2]]), color = "goldenrod", size=6, shape=4) +
  geom_point(aes(x=lf_avg_pos[1], y=lf_avg_pos[2]), color = "violet", size=6, shape=4) +
  
  # Arrow from violet point to goldenrod point for plotlf
  geom_segment(aes(x = lf_avg_pos[1], y = lf_avg_pos[2], 
                   xend = lf_kmeans_pos[[1]], yend = lf_kmeans_pos[[2]]),
               arrow = arrow(type = "closed", length = unit(0.15, "inches")), color = "black", size = 0.3) +
  
  ggtitle("Lf Outs") + 
  theme(legend.position = "none") +
  labs(x = 'X-axis position', y = 'Y-axis position')
table(lf_bounces$out)
plotlf

# Cf 75th percentile:
cf_bounces
cf_kmeans_pos
cf_bounces$bounce_dist_from_kmeanstart <- sqrt((cf_bounces$bounce_position_x - cf_kmeans_pos[[1]])^2 + (cf_bounces$bounce_position_y - cf_kmeans_pos[[2]])^2)
cf_bounces$out <- ifelse(splits_mean * cf_bounces$hangtime > cf_bounces$bounce_dist_from_kmeanstart, 1, 0)
plotcf <- ggplot(cf_bounces, aes(x=cf_bounces$bounce_position_x,y=cf_bounces$bounce_position_y,color=as.factor(cf_bounces$out))) +
  geom_point() +
  geom_point(aes(x=cf_kmeans_pos[[1]],y=cf_kmeans_pos[[2]]), color = "goldenrod",size=6,shape=4) +
  geom_point(aes(x=cf_avg_pos[1],y=cf_avg_pos[2]), color = "violet",size=6,shape=4) +
  ggtitle("Cf Outs") + 
  theme(legend.position = "none") +
  labs(x = 'X-axis position',y = 'Y-axis position') +
  ylim(150,350)
plotcf
grid.arrange(plotlf, plotcf, plotrf, ncol = 3)

print(plotcf)
table(cf_bounces$out)



#Find all game events where player_pos = 7, 8 0r 9, & where event code = 2
filtered_events <- game_events %>%
  filter(player_position %in% c(7, 8, 9) & event_code == 2)
#This has the timestamp for when they caught the ball
ball_hit_in_play <- game_events %>%
  filter(event_code == 4) %>% group_by(game_str, play_id)

#ONLY USING LEFT FIELD RN
ball_hit_in_play_test <- merge(player_1902_outfield_pos, ball_hit_in_play, by = c("game_str", "play_id", "timestamp"))

#This is all of the timestamps for when ball was acquired along with positions.
ranges_1902 <- inner_join(filtered_events, player_1902_outfield_pos, by = c("game_str", "play_id", "player_position", "timestamp"))

#ball caught
ranges_1902 <- ranges_1902 %>%
  rename("timestamp_catch" = "timestamp",
         "field_x_catch" = "field_x",
         "field_y_catch" = "field_y")
#ball hit
ball_hit_in_play_real <- ball_hit_in_play_test %>%
  rename("timestamp_hit" = "timestamp",
         "field_x_hit" = "field_x",
         "field_y_hit" = "field_y",
         "player_position" = "player_position.x")

official_outfield_range_data <- merge(ranges_1902, ball_hit_in_play_real, by = c("game_str", "play_id", "player_position"))

# Select the desired columns
official_outfield_range_data <- official_outfield_range_data[, c("game_str", "play_id", "timestamp_catch", "timestamp_hit",
                                                                 "player_position", "field_x_catch", "field_y_catch",
                                                                 "field_x_hit", "field_y_hit")]


official_outfield_range_data$straight_distance <- sqrt((official_outfield_range_data$field_x_hit - official_outfield_range_data$field_x_catch)**2 + (official_outfield_range_data$field_y_hit - official_outfield_range_data$field_y_catch)**2)

official_outfield_range_data$hit_timestamp_sec <- official_outfield_range_data$timestamp_hit / 1000
official_outfield_range_data$catch_timestamp_sec <- official_outfield_range_data$timestamp_catch / 1000

# Calculate time difference in seconds
official_outfield_range_data$time_difference_sec <- official_outfield_range_data$catch_timestamp_sec - official_outfield_range_data$hit_timestamp_sec

# Calculate average speed in feet per second
official_outfield_range_data$average_speed_fps <- official_outfield_range_data$straight_distance / official_outfield_range_data$time_difference_sec

#max(official_outfield_range_data$average_speed_fps)
#Trying to plot the movement until ball acquired
official_outfield_range_data$move_x <- official_outfield_range_data$field_x_catch - official_outfield_range_data$field_x_hit
official_outfield_range_data$move_y <- official_outfield_range_data$field_y_catch - official_outfield_range_data$field_y_hit


kmeans_rf <- c(110, 232)
kmeans_lf <- c(-110, 225)
kmeans_cf <- c(-1, 310)

#rf
right_fielder_ranges <- official_outfield_range_data[official_outfield_range_data$player_position == 9,]
right_fielder_ranges$bounce_dist_from_kmeanstart <- sqrt((right_fielder_ranges$field_x_catch - kmeans_rf[1])^2 + (right_fielder_ranges$field_y_catch - kmeans_rf[2])^2)
right_fielder_ranges$out <- ifelse(splits_mean * right_fielder_ranges$time_difference_sec > right_fielder_ranges$bounce_dist_from_kmeanstart, 1, 0)

rf <- ggplot(right_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(col = ifelse(right_fielder_ranges$out == 1, "darkgreen", "red"))

table(right_fielder_ranges$out)

#cf
center_fielder_ranges <- official_outfield_range_data[official_outfield_range_data$player_position == 8,]
center_fielder_ranges$bounce_dist_from_kmeanstart <- sqrt((center_fielder_ranges$field_x_catch - kmeans_cf[1])^2 + (center_fielder_ranges$field_y_catch - kmeans_cf[2])^2)
center_fielder_ranges$out <- ifelse(splits_mean * center_fielder_ranges$time_difference_sec > center_fielder_ranges$bounce_dist_from_kmeanstart, 1, 0)

cf <- ggplot(center_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(col = ifelse(center_fielder_ranges$out == 1, "darkgreen", "red"))

table(center_fielder_ranges$out)

#lf
left_fielder_ranges <- official_outfield_range_data[official_outfield_range_data$player_position == 7,]
left_fielder_ranges$bounce_dist_from_kmeanstart <- sqrt((left_fielder_ranges$field_x_catch - kmeans_lf[1])^2 + (left_fielder_ranges$field_y_catch - kmeans_lf[2])^2)
left_fielder_ranges$out <- ifelse(splits_mean * left_fielder_ranges$time_difference_sec > left_fielder_ranges$bounce_dist_from_kmeanstart, 1, 0)

lf <- ggplot(left_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(col = ifelse(left_fielder_ranges$out == 1, "darkgreen", "red"))

table(left_fielder_ranges$out)

#Limitations Graphs - show the
lf_limitations <- ggplot(left_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(aes(color = ifelse(out == 1, "Out",
                                ifelse(field_y_catch > 280, "XBH", "Single"))), size = 3) +
  scale_color_manual(
    values = c("Out" = "darkgreen", "XBH" = "orange", "Single" = "red"),
    labels = c("Out", "Single", "XBH")
  ) +
  labs(color = "Outcome") +
  theme_minimal()

cf_limitations <- ggplot(center_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(aes(color = ifelse(out == 1, "Out",
                                ifelse(field_y_catch > 300, "XBH", "Single"))), size = 3) +
  scale_color_manual(
    values = c("Out" = "darkgreen", "XBH" = "orange", "Single" = "red"),
    labels = c("Out", "Single", "XBH")
  ) +
  labs(color = "Outcome") +
  theme_minimal()

rf_limitations <- ggplot(right_fielder_ranges, aes(x = field_x_catch, y = field_y_catch)) +
  geom_point(aes(color = ifelse(out == 1, "Out",
                                ifelse(field_y_catch > 280, "XBH", "Single"))), size = 3) +
  scale_color_manual(
    values = c("Out" = "darkgreen", "XBH" = "orange", "Single" = "red"),
    labels = c("Out", "Single", "XBH")
  ) +
  labs(color = "Outcome") +
  theme_minimal()

grid.arrange(lf_limitations, cf_limitations, rf_limitations, ncol = 3)
