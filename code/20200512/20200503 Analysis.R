
# 20200503 Kobe - Looking Back at a Laker Legend Analysis

# Shot Charts and Gifs

# Load libraries and source code to pull shot data and draw court from ballr package (by Todd Schneider)

library(shiny)
library(ggplot2)
library(plotly)
library(gganimate)
library(hexbin)
library(dplyr)
library(httr)
library(jsonlite)
library(scales)

source("court_themes.R")
source("plot_court.R")
source("players_data.R")
source("fetch_shots.R")

# Get Kobe shots

# kobe <- find_player_by_name("Kobe Bryant")
# seasons <- season_strings[as.character(1996:2015)]
#
# kobe_shots <- bind_rows(lapply(seasons, function(season) {
#   fetch_shots_by_player_id_and_season(kobe$person_id, season)$player %>%
#     mutate(season = season)
# }))
#
# kobe_shots
#
# write.csv(kobe_shots, "kobe_shots.csv", row.names = F)


# Read CSV of shot data

kobe_shots <- read.csv("./data/20200426/kobe_shots.csv")

# Rank games by game date

add_ranks <- function(shot_data){

  game_dates <- shot_data %>% select(game_date) %>%
    mutate(game_date = as.Date(game_date)) %>%
    distinct() %>%
    mutate(game_number = rank(game_date, ties.method = "min")) %>%
    arrange(game_date)

  shot_data <- shot_data %>% mutate(game_date = as.Date(game_date)) %>%
    inner_join(game_dates, by = "game_date") %>%
    mutate(shot_number = rank(order(game_date, period, desc(minutes_remaining), desc(seconds_remaining)),
                              ties.method = "min"),
           season_year = as.numeric(substr(season, 1, 4)))

  return(shot_data)
}

# Create Shot Chart Function

create_shot_chart <- function(shot_data, start_year, end_year = start_year, subtitle = ""){

  shot_data <- add_ranks(shot_data)

  # Create court background

  court_theme <- court_themes$black
  court <- plot_court(court_theme = court_theme, use_short_three = F)

  # Filter data to include shots between start_year and end_year

  shot_data <- shot_data %>% filter(season_year >= start_year & season_year <= end_year)

  # Create player variable

  player <- shot_data %>% mutate(player_name = as.character(player_name)) %>%
    select(player_name) %>% distinct()

  # Create shot chart

  shot_chart <- court +
    geom_point(data = shot_data, aes(x = loc_x, y = loc_y, color = shot_made_flag, shape = shot_made_flag),
      alpha = 0.5, size = 2) +
    scale_color_manual("", values = c(made = court_theme$made, missed = court_theme$missed)) +
    scale_shape_manual("", values = c(made = 1, missed = 4)) +
    ggtitle(paste(player, "Career Shots"),
            subtitle = paste(start_year, "-", end_year + 1, "\n", subtitle)) +
    labs(caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", colour = "white", size = 16, hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 9, hjust = 1))

  return(shot_chart)
}

full_shot_chart <- create_shot_chart(kobe_shots, 1996, 2015)


# Function to create gif of shots

create_shot_gif <- function(shot_data, start_year, end_year = start_year, gif_duration){

  shot_data <- add_ranks(shot_data)

  # Create court background

  court_theme <- court_themes$black

  if(as.numeric(end_year) < 1997){
    court <- plot_court(court_theme = court_theme, use_short_three = T)
  }else
    court <- plot_court(court_theme = court_theme, use_short_three = F)

  # Filter data to include shots between start_year and end_year

  shot_data <- shot_data %>% filter(season_year >= start_year & season_year <= end_year)

  # Get player and num_shots variables

  player <- shot_data %>% mutate(player_name = as.character(player_name)) %>%
    select(player_name) %>% distinct()

  num_shots <- nrow(shot_data)

  # Create shot chart gif

  shot_gif <- court +
    geom_point(data = shot_data, aes(x = loc_x, y = loc_y, color = shot_made_flag, shape = shot_made_flag),
               alpha = 0.75, size = 2.5) +
    scale_color_manual("", values = c(made = court_theme$made, missed = court_theme$missed)) +
    scale_shape_manual("", values = c(made = 1, missed = 4)) +
    ggtitle(paste(player, "Shots:", start_year, "-", end_year + 1)) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", colour = "white", size = 16, hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 9, hjust = 1)) +
    transition_states(states = shot_number, transition_length = 1, state_length = 2, wrap = F) +
    labs(subtitle = "Through Shot {closest_state}",
         caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
    shadow_mark()

  shot_animation <- animate(shot_gif, duration = gif_duration, nframes = num_shots*2)
  anim_save(paste0(player, " Shots ", start_year, "-", end_year + 1, ".gif"), animation = shot_animation)

  return(shot_animation)

}

# system.time(create_shot_gif(kobe_shots, start_year = 1996, gif_duration = 90))
# system.time(create_shot_gif(kobe_shots, start_year = 2000, gif_duration = 90))
# system.time(create_shot_gif(kobe_shots, start_year = 2007, gif_duration = 90))
# system.time(create_shot_gif(kobe_shots, start_year = 2015, gif_duration = 90))


# Heat Maps

# Create Heat Map Function

create_heat_map <- function(shot_data, start_year, end_year = start_year, subtitle = ""){

  shot_data <- add_ranks(shot_data)

  # Create court background

  court_theme <- court_themes$black
  court <- plot_court(court_theme = court_theme, use_short_three = F)

  # Filter data to include shots between start_year and end_year

  shot_data <- shot_data %>% filter(season_year >= start_year & season_year <= end_year)

  # Create player variable

  player <- shot_data %>% mutate(player_name = as.character(player_name)) %>%
    select(player_name) %>% distinct()

  # Create heat map

  heat_map <- court +
    stat_density_2d(data = shot_data, aes(x = loc_x, y = loc_y, fill = stat(density / max(density))),
                    geom = "raster", contour = F, interpolate = T, n = 250) +
    geom_path(data = court_points, aes(x = x, y = y, group = desc), color = court_theme$lines) +
    scale_fill_viridis_c("Shot Frequency", limits = c(0, 1), breaks = c(0, 1), option = "inferno") +
    ggtitle(paste(player, "Career Shots Heat Map"),
            subtitle = paste(start_year, "-", end_year + 1, "\n", subtitle)) +
    labs(caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", colour = "white", size = 16, hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 9, hjust = 1))

  return(heat_map)
}


# Create Heat Map Outside Restricted Area Function

create_heat_map_outside_restricted <- function(shot_data, start_year, end_year = start_year, subtitle = ""){

  shot_data <- add_ranks(shot_data)

  shot_data <- shot_data %>% filter(shot_zone_basic != "Restricted Area")

  # Create court background

  court_theme <- court_themes$black
  court <- plot_court(court_theme = court_theme, use_short_three = F)

  # Filter data to include shots between start_year and end_year

  shot_data <- shot_data %>% filter(season_year >= start_year & season_year <= end_year)

  # Create player variable

  player <- shot_data %>% mutate(player_name = as.character(player_name)) %>%
    select(player_name) %>% distinct()

  # Create heat map

  heat_map <- court +
    stat_density_2d(data = shot_data, aes(x = loc_x, y = loc_y, fill = stat(density / max(density))),
                    geom = "raster", contour = F, interpolate = T, n = 250) +
    geom_path(data = court_points, aes(x = x, y = y, group = desc), color = court_theme$lines) +
    scale_fill_viridis_c("Shot Frequency", limits = c(0, 1), breaks = c(0, 1), option = "inferno") +
    ggtitle(paste(player, "Career Shots Heat Map \nOutside Restricted Area"),
            subtitle = paste(start_year, "-", end_year + 1, "\n", subtitle)) +
    labs(caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", colour = "white", size = 16, hjust = 0.5),
          plot.subtitle = element_text(colour = "white", size = 12, hjust = 0.5),
          plot.caption = element_text(colour = "white", size = 9, hjust = 1))

  return(heat_map)
}


full_heat_map <- create_heat_map(kobe_shots, 1996, 2015)

heat_map_outside_restricted <- create_heat_map_outside_restricted(kobe_shots, 1996, 2015)


# Shot Breakdown - FG% vs EFG%

overall_pcts <- kobe_shots %>% mutate(flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag))

fg_pct_number <- overall_pcts %>% select(fg_pct) %>% as.numeric()
efg_pct_number <- overall_pcts %>% select(efg_pct) %>% as.numeric()

kobe_fg_pct <- kobe_shots %>% mutate(flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  group_by(shot_distance) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_total = round(total_shots / sum(total_shots), 4)) %>%
  mutate(pct_of_total = scales::percent(pct_of_total, accuracy = 0.01)) %>%
  filter(shot_distance <= 29) %>%
  arrange(shot_distance)

kobe_fg_pct_line <- ggplot(kobe_fg_pct, aes(x = shot_distance, y = fg_pct, colour = "FG%")) +
  geom_line(size = 1.5) +
  geom_line(aes(x = shot_distance, y = efg_pct, colour = "EFG%"), size = 1.5) +
  scale_color_manual(values = c("purple", "black")) +
  scale_y_continuous(limits = c(0, 0.8), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Kobe Bryant Career Field Goal/Effective Field Goal \n Percentage by Shot Distance",
          subtitle = "1996 - 2016") +
  labs(x = 'Shot Distance (in feet)', y = 'Percentage', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5)) +
  guides(col = guide_legend(title = "Shooting Metric"))

kobe_pps_line <- ggplot(kobe_fg_pct, aes(x = shot_distance, y = pps)) +
  geom_line(col = 'purple', size = 1.5) +
  scale_color_manual(values = c("purple")) +
  scale_y_continuous(limits = c(0, 1.6)) +
  ggtitle("Kobe Bryant Career Points per Shot \n by Shot Distance",
          subtitle = "1996 - 2016") +
  labs(x = 'Shot Distance (in feet)', y = 'Points per Shot', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5))

kobe_fg_pct_table <- kobe_fg_pct %>% rename(`Shot Distance (ft)` = shot_distance, `FG%` = fg_pct,
                                                   `EFG%` = efg_pct, `Points per Shot` = pps,
                                                   `Total Shots` = total_shots,
                                                   `Percent of Total Shots` = pct_of_total) %>% as.data.frame()


# Shots by Zone

kobe_by_zone <- kobe_shots %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  group_by(shot_zone_basic) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_shots = round(prop.table(total_shots), 4)) %>%
  arrange(shot_zone_basic)

kobe_shots_by_zone_table <- kobe_by_zone %>%
  mutate(pct_of_shots = scales::percent(pct_of_shots, accuracy = 0.01)) %>%
  rename(`Shot Zone` = shot_zone_basic, `FG%` = fg_pct, `EFG%` = efg_pct,
         `Points per Shot` = pps, `Total Shots` = total_shots, `Percent of Total Shots` = pct_of_shots) %>%
  as.data.frame()


# Shots by Zone and Year

kobe_by_zone_year <- kobe_shots %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  group_by(season_year, shot_zone_basic) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_season = round(prop.table(total_shots), 4)) %>%
  arrange(shot_zone_basic, season_year) %>%
  filter(shot_zone_basic %in% c("Above the Break 3", "In The Paint (Non-RA)", "Mid-Range", "Restricted Area"))

# Percentage of Shots Line

kobe_by_zone_year_pct_line <- ggplot(kobe_by_zone_year, aes(x = season_year, y = pct_of_season,
                                                            col = shot_zone_basic)) +
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent_format(accuracy = 1)) +
  ggtitle("Kobe Bryant Percentage of Shots \n by Zone in Each Season",
          subtitle = "1996 - 2016") +
  labs(x = 'Season', y = 'Percentage of Season Shots', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5)) +
  guides(col = guide_legend(title = "Shot Zone"))

# PPS Line

kobe_by_zone_year_pps_line <- ggplot(kobe_by_zone_year, aes(x = season_year, y = pps,
                                                            col = shot_zone_basic)) +
  geom_line(size = 1.5) +
  scale_y_continuous(limits = c(0, 1.6)) +
  ggtitle("Kobe Bryant Points per Shot \n by Zone in Each Season",
          subtitle = "1996 - 2016") +
  labs(x = 'Season', y = 'Points per Shot', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5)) +
  guides(col = guide_legend(title = "Shot Zone"))


# Shots by Quarter

# By Zone and Quarter

kobe_by_zone_qtr <- kobe_shots %>%
  mutate(flag3 = ifelse(shot_value == 3, 1, 0), quarter = ifelse(period <= 4, as.factor(period), "OT")) %>%
  group_by(quarter, shot_zone_basic) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_qtr = round(prop.table(total_shots), 4)) %>%
  arrange(quarter, shot_zone_basic) %>%
  filter(shot_zone_basic %in% c("Above the Break 3", "In The Paint (Non-RA)", "Mid-Range", "Restricted Area"))

# Percent of Shots Bar

kobe_by_zone_qtr_pct_bar <- ggplot(kobe_by_zone_qtr, aes(x = quarter, y = pct_of_qtr)) +
  geom_col(fill = 'purple') +
  scale_y_continuous(limits = c(0, 0.5), labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~shot_zone_basic, ncol = 4) +
  ggtitle("Kobe Bryant Percentage of Shots \n by Zone in Each Quarter",
          subtitle = "1996 - 2016") +
  labs(x = 'Quarter', y = 'Percentage of Quarter Shots', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5))

# PPS Bar

kobe_by_zone_qtr_pps_bar <- ggplot(kobe_by_zone_qtr, aes(x = quarter, y = pps)) +
  geom_col(fill = 'purple') +
  scale_y_continuous(limits = c(0, 1.6)) +
  facet_wrap(~shot_zone_basic, ncol = 4) +
  ggtitle("Kobe Bryant Points per Shot \n by Zone in Each Quarter",
          subtitle = "1996 - 2016") +
  labs(x = 'Quarter', y = 'Points per Shot', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'yellow'),
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5))


# Shots by Opponent

# By Opponent

kobe_by_opponent <- kobe_shots %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0),
         opponent = as.character(ifelse(as.character(htm) == "LAL", as.character(vtm), as.character(htm)))) %>%
  mutate(opponent = ifelse(opponent %in% c("BKN", "NJN"), "BKN",
                           ifelse(opponent %in% c("CHH", "NOH", "NOK", "NOP"), "NO",
                                  ifelse(opponent %in% c("SEA", "OKC"), "OKC",
                                         ifelse(opponent %in% c("MEM", "VAN"), "MEM", opponent))))) %>%
  group_by(opponent) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_career = round(prop.table(total_shots), 4)) %>%
  arrange(opponent)

# Color by Opponent

opponent_colors <- c("ATL" = "red", "BKN" = "black", "BOS" = "green", "CHA" = "turquoise", "CHI" = "red",
                     "CLE" = "darkred", "DAL" = "dodgerblue", "DEN" = "gold", "DET" = "darkblue", "GSW" = "gold",
                     "HOU" = "red", "IND" = "gold", "LAC" = "red", "MEM" = "cyan", "MIA" = "firebrick",
                     "MIL" = "chartreuse4", "MIN" = "slateblue", "NO" = "steelblue", "NYK" = "orange",
                     "OKC" = "deepskyblue", "ORL" = "steelblue1", "PHI" = "red", "PHX" = "purple", "POR" = "red4",
                     "SAC" = "purple", "SAS" = "grey40", "TOR" = "purple", "UTA" = "slateblue4", "WAS" = "blue")

# Overall PPS Bar

kobe_by_opponent_pps_bar <- ggplot(kobe_by_opponent, aes(x = reorder(opponent, -pps), y = pps, fill = opponent)) +
  geom_col() +
  scale_fill_manual(values = opponent_colors) +
  ggtitle("Kobe Bryant Points per Shot by Opponent",
          subtitle = "1996 - 2016") +
  labs(x = 'Opponent', y = 'Points per Shot', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none",
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))


# Shots By Opponent and Year

kobe_by_opponent_year <- kobe_shots %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0),
         opponent = as.character(ifelse(as.character(htm) == "LAL", as.character(vtm), as.character(htm)))) %>%
  mutate(opponent = ifelse(opponent %in% c("BKN", "NJN"), "BKN",
                           ifelse(opponent %in% c("CHH", "NOH", "NOK", "NOP"), "NO",
                                  ifelse(opponent %in% c("SEA", "OKC"), "OKC",
                                         ifelse(opponent %in% c("MEM", "VAN"), "MEM", opponent))))) %>%
  group_by(season_year, opponent) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_season = round(prop.table(total_shots), 4)) %>%
  arrange(opponent, season_year)

# Yearly PPS Line

kobe_by_opponent_year_pps_line <- ggplot(kobe_by_opponent_year, aes(x = season_year, y = pps, col = opponent)) +
  geom_line() +
  scale_y_continuous(limits = c(0.4, 1.6)) +
  facet_wrap(~opponent, ncol = 6) +
  scale_color_manual(values = opponent_colors) +
  ggtitle("Kobe Bryant Points per Shot by Opponent in Each Season",
          subtitle = "1996 - 2016") +
  labs(x = 'Season', y = 'Points per Shot', caption = "Data: stats.nba.com \n Created by: Bryant Molloy") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = 'white'),
        legend.position = "none",
        plot.title = element_text(face = "bold", colour = "black", size = 16, hjust = 0.5),
        plot.subtitle = element_text(colour = "black", size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 90, hjust = 1))


# Winning Time

winning_time <- kobe_shots %>%
  filter(period >= 4 & ((minutes_remaining == 5 & seconds_remaining == 0) | minutes_remaining < 5)) %>%
  mutate(opponent = ifelse(htm == "LAL", as.character(vtm), as.character(htm)),
         game_date = as.Date(game_date))

# Winning Time Heat Map

winning_time_heat_map <- create_heat_map_outside_restricted(winning_time, start_year = 1996, end_year = 2015,
                                   subtitle = "Last 5 Minutes of 4th Quarter / Overtime")

# Winning Time Percentages

winning_time_pcts <- winning_time %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            fg3_pct = mean(ifelse(flag3 == 1, shot_made_numeric, NA), na.rm = T),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_shots = round(prop.table(total_shots), 4)) %>%
  as.data.frame()

winning_time_table <- winning_time_pcts %>%
  mutate(pct_of_shots = scales::percent(pct_of_shots, accuracy = 0.01)) %>%
  rename(`FG%` = fg_pct, `3FG%` = fg3_pct, `EFG%` = efg_pct,
         `Points per Shot` = pps, `Total Shots` = total_shots) %>%
  as.data.frame()

# Winning Time Percentages by Zone

winning_time_by_zone <- winning_time %>%
  mutate(season_year = as.numeric(substr(season, 1, 4)), flag3 = ifelse(shot_value == 3, 1, 0)) %>%
  group_by(shot_zone_basic) %>%
  summarize(fg_pct = mean(shot_made_numeric),
            efg_pct = mean((shot_made_numeric + 0.5 * flag3 * shot_made_numeric) / shot_attempted_flag),
            pps = mean(shot_made_numeric * shot_value),
            total_shots = n()) %>%
  mutate(fg_pct = round(fg_pct, 4), efg_pct = round(efg_pct, 4), pps = round(pps, 4),
         pct_of_shots = round(prop.table(total_shots), 4)) %>%
  arrange(shot_zone_basic) %>%
  as.data.frame()

winning_time_by_zone_table <- winning_time_by_zone %>%
  mutate(pct_of_shots = scales::percent(pct_of_shots, accuracy = 0.01)) %>%
  rename(`Shot Zone` = shot_zone_basic, `FG%` = fg_pct, `EFG%` = efg_pct,
         `Points per Shot` = pps, `Total Shots` = total_shots, `Percent of Total Shots` = pct_of_shots) %>%
  as.data.frame()



