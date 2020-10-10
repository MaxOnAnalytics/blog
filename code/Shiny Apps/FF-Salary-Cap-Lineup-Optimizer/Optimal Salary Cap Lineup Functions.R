
# title: "Fantasy Football Salary Cap Lineup Optimizer Functions"
# author: "Bryant Molloy"
# date: "September 5, 2020"



## Load Libraries and Source Code

library(ffanalytics)
library(tidyverse)
library(lpSolveAPI)
library(reactable)


## Function to scrape 2020 projection data from FantasyPros

# FantasyPros Scoring - Default is Standard (0 PPR)

fp_projections <- function(scoring = c("Standard", "Half PPR", "PPR")){
  
  scoring_type <- scoring
  
  data_2020 <- ffanalytics::scrape_data(src = c("FantasyPros"), 
                                        pos = c("QB", "RB", "WR", "TE"), 
                                        season = 2020, 
                                        week = 0)
  
  qbs <- data_2020$QB %>% 
    mutate(pos = "QB", 
           id = as.numeric(id), 
           pt_projection = site_pts) %>%
    select(id, player, pos, team, pt_projection)
  
  rbs <- data_2020$RB %>%
    mutate(
      pos = "RB",
      id = as.numeric(id), 
      pt_projection = case_when(
        scoring_type == "Standard" ~ site_pts,
        scoring_type == "Half PPR" ~ site_pts + (0.5 * rec), 
        scoring_type == "PPR" ~ site_pts + rec
      )) %>%
    select(id, player, pos, team, pt_projection)
  
  wrs <- data_2020$WR %>%
    mutate(
      pos = "WR", 
      id = as.numeric(id), 
      pt_projection = case_when(
        scoring_type == "Standard" ~ site_pts,
        scoring_type == "Half PPR" ~ site_pts + (0.5 * rec), 
        scoring_type == "PPR" ~ site_pts + rec
      )) %>%
    select(id, player, pos, team, pt_projection)
  
  tes <- data_2020$TE %>%
    mutate(
      pos = "TE", 
      id = as.numeric(id), 
      pt_projection = case_when(
        scoring_type == "Standard" ~ site_pts,
        scoring_type == "Half PPR" ~ site_pts + (0.5 * rec), 
        scoring_type == "PPR" ~ site_pts + rec
      )) %>%
    select(id, player, pos, team, pt_projection)
  
  projections <- bind_rows(qbs, rbs, wrs, tes) %>% 
    arrange(desc(pt_projection))
  
  return(projections)
  
}

projections_standard <- fp_projections(scoring = "Standard")
projections_half_ppr <- fp_projections(scoring = "Half PPR")
projections_full_ppr <- fp_projections(scoring = "PPR")


## Scrape Average Auction Values from Yahoo

yahoo_draft_aav <- function(aav_flag = T){
  
  draft_type <- ifelse(aav_flag, "AD", "SD")
  
  draft_url <- sprintf("https://football.fantasysports.yahoo.com/f1/draftanalysis?tab=%s&pos=ALL",
                       draft_type)
  
  draft_col <- ifelse(aav_flag, "Avg Salary", "Avg Pick")
  names(draft_col) <- ifelse(aav_flag, "aav", "adp")
  
  draft_session <- html_session(draft_url)
  yahoo_adp <- data.frame()
  recode_vals <- c("jac" = "30", "bal" = "33", "lar" = "14", "phi" = "21", "det" = "8",
                   "lac" = "24", "nor" = "18", "sea" = "26", "chi" = "3",  "car" = "29",
                   "pit" = "23", "nwe" = "17",  "kan" = "12", "min" = "16", "dal" = "6",
                   "was" = "28", "den" = "7", "ari" = "22", "ten" = "10", "tam" = "27",
                   "buf" = "2", "cin" = "4", "atl" = "1", "gnb" = "9", "mia" = "15",
                   "ind" = "11", "nyg" = "19",  "hou" = "34", "sfo" = "25", "cle" = "5",
                   "nyj" = "20", "oak" = "13")
  repeat({
    
    draft_page <- read_html(draft_session)
    
    draft_tbl <- draft_page %>% html_node("#draftanalysistable") %>% html_table()
    
    names(draft_tbl) <- gsub("[^[:alnum:]]$", "", names(draft_tbl))
    
    yahoo_ids <-  draft_session %>%
      html_nodes("a[href *= 'nfl/players']:not(a[class *='playernote']), a[href *= 'nfl/teams']:not(a[class *='playernote'])") %>%
      html_attr("href") %>%
      basename()
    
    draft_tbl <- draft_tbl %>%
      rename(!!!draft_col) %>% add_column(yahoo_id = yahoo_ids, .before = 1)
    
    if(any(names(draft_tbl) == "aav"))
      draft_tbl <- draft_tbl %>% mutate(aav = suppressWarnings(as.numeric(gsub("^\\$", "", aav))))
    
    yahoo_adp <- bind_rows(yahoo_adp, draft_tbl) %>%
      mutate(yahoo_id = recode(yahoo_id, !!!recode_vals))
    
    next_url <- draft_session %>%
      html_node("a:contains('Next')") %>%
      html_attr("href")
    
    if(is.na(next_url))
      break
    
    draft_session <- next_url %>% jump_to(x=draft_session, url =.)
  })
  yahoo_adp <- yahoo_adp %>%
    extract(., Name, c("Note", "Player", "Team", "Pos", "Status/Game/Opp"),
            "\\s*(.+Note[s]*)\\s+(.+)\\s([[:alpha:]]{2,3})\\s\\-\\s([[:alpha:]]{1,3},*[[:alpha:]]*)\\s{2,}(.+)") %>%
    select(., -one_of(c("Note", "Status/Game/Opp"))) %>% clean_names() %>%
    mutate(proj_salary = suppressWarnings(as.numeric(gsub("^\\$", "", proj_salary)))) %>%
    add_column(id = as.numeric(ffanalytics:::id_col(yahoo_adp$yahoo_id, "stats_id")), .before = 1)
  
  return(yahoo_adp)
}

yahoo_aav <- yahoo_draft_aav()


## Function to create optimal lineups

get_optimal_lineup <- function(scoring = c("Standard", "Half PPR", "PPR"), 
                               cost_metric = c("Projected Salary", "AAV"), 
                               lineups = 1, 
                               budget_spent_pct = 80, qb_slots = 1, rb_slots = 2, wr_slots = 2, 
                               te_slots = 1, flex_slots = 1, 
                               long = T, reduce_player_impact = T){
  
  # Inputs for testing
  
  scoring <- scoring
  cost_metric <- cost_metric
  lineups <- lineups
  budget_spent_pct <- budget_spent_pct / 100
  qb_slots <- qb_slots
  rb_slots <- rb_slots
  wr_slots <- wr_slots
  te_slots <- te_slots
  flex_slots <- flex_slots
  long <- long
  reduce_player_impact <- reduce_player_impact
  lineup_size <- qb_slots + rb_slots + wr_slots + te_slots + flex_slots
  
  # Get projections data from FantasyPros and AAV data from Yahoo
  
  projections_data <- data.frame()
  
  if(scoring == "Standard"){
    projections_data <- projections_standard
  } else if(scoring == "Half PPR"){
    projections_data <- projections_half_ppr
  } else if(scoring == "PPR"){
    projections_data <- projections_full_ppr
  }
  
  aav_data <- yahoo_aav
  
  # Join projections and AAV to create dataframe for optimization
  
  proj_matches <- projections_data %>% 
    inner_join(aav_data, by = "id") %>%  
    filter(!is.na(aav), !is.na(id)) %>%
    rename(player = player.x, pos = pos.x, team = team.x) %>%
    select(id, player, team, pos, pt_projection, proj_salary, aav) %>%
    distinct() %>%
    arrange(desc(aav))
  
  proj_non_matches <- projections_data %>%
    left_join(aav_data, by = c("player", "pos")) %>%
    rename(id = id.x, team = team.x) %>%
    filter(!is.na(aav), is.na(id)) %>%
    distinct() %>%
    select(id, player, team, pos, pt_projection, proj_salary, aav)
  
  optimize_data <- bind_rows(proj_matches, proj_non_matches) %>% 
    distinct() %>%
    mutate(QB_Flag = ifelse(pos == "QB", 1, 0),
           RB_Flag = ifelse(pos == "RB", 1, 0), 
           WR_Flag = ifelse(pos == "WR", 1, 0), 
           TE_Flag = ifelse(pos == "TE", 1, 0), 
           Flex_Flag = ifelse(pos %in% c("RB", "WR", "TE"), 1, 0)) %>%
    arrange(desc(aav))
  
  # Optimization Problem - First Iteration
  
  ## Create optimization problem to find solution that maximizes point projection
  
  players_available <- nrow(optimize_data)
  
  lp_model <- make.lp(0, players_available)
  set.objfn(lp_model, optimize_data$pt_projection)
  
  lp.control(lp_model, sense = "max")
  set.type(lp_model, 1:players_available, "binary")
  
  ## Constraint List
  
  if(cost_metric == "Projected Salary"){
    add.constraint(lp_model, optimize_data$proj_salary, "<=", 200 * budget_spent_pct) 
  }else{
    add.constraint(lp_model, optimize_data$aav, "<=", 200 * budget_spent_pct) 
  }
  add.constraint(lp_model, optimize_data$QB_Flag, "=", qb_slots)
  add.constraint(lp_model, optimize_data$RB_Flag, "<=", rb_slots + flex_slots)
  add.constraint(lp_model, optimize_data$RB_Flag, ">=", rb_slots)
  add.constraint(lp_model, optimize_data$WR_Flag, "<=", wr_slots + flex_slots)
  add.constraint(lp_model, optimize_data$WR_Flag, ">=", wr_slots)
  add.constraint(lp_model, optimize_data$TE_Flag, "<=", te_slots + flex_slots)
  add.constraint(lp_model, optimize_data$TE_Flag, ">=", te_slots)
  add.constraint(lp_model, optimize_data$Flex_Flag, "=", rb_slots + wr_slots + te_slots + flex_slots)
  
  ## Create Output Dataframes
  
  optimal_lineup_wide_output <- data.frame()
  optimal_lineup_long_output <- data.frame()
  
  ## Solve optimization problem and return most optimal lineup
  
  solve(lp_model)
  
  optimal_lineup <- optimize_data %>%
    filter(get.variables(lp_model) == 1)
  
  optimal_lineup_long <- optimal_lineup %>%
    mutate(Lineup = 1) %>%
    rename(Player = player, Team = team, Position = pos, `Projected Points` = pt_projection, 
           `Projected Salary` = proj_salary, AAV = aav) %>%
    select(Lineup, Player:AAV)
  
  optimal_lineup_wide <- optimal_lineup %>% 
    arrange(desc(pt_projection)) %>%
    group_by(pos) %>%
    mutate(pos_num = 1:n()) %>%
    ungroup() %>%
    mutate(flex_slot_flag = case_when(
      Flex_Flag == 0 ~ 0, 
      (RB_Flag == 1 & pos_num > rb_slots) ~ 1, 
      (WR_Flag == 1 & pos_num > wr_slots) ~ 1,
      (TE_Flag == 1 & pos_num > te_slots) ~ 1, 
      TRUE ~ 0)) %>%
    group_by(flex_slot_flag) %>%
    mutate(flex_num = ifelse(flex_slot_flag == 1, 1:n(), 0)) %>%
    ungroup() %>%
    mutate(position = case_when(
      flex_slot_flag > 0 ~ paste0("FLEX", flex_num), 
      TRUE ~ paste0(pos, pos_num))) %>%
    arrange(flex_num, desc(QB_Flag), desc(RB_Flag), desc(WR_Flag), desc(TE_Flag), flex_num) %>%
    select(player, position) %>%
    pivot_wider(names_from = position, values_from = player) %>%
    mutate(`Budget Spent` = ifelse(cost_metric == "Projected Salary", sum(optimal_lineup$proj_salary), sum(optimal_lineup$aav)), 
           `Total Points Projected` = sum(optimal_lineup$pt_projection), 
           Lineup = 1) %>%
    select(Lineup, everything())
  
  total_pt_projection <<- sum(optimal_lineup_long$`Projected Points`) - 0.01
  optimal_lineup_long_output <- optimal_lineup_long
  optimal_lineup_wide_output <- optimal_lineup_wide
  
  # If reduce_player_impact == T, multiply projected points of players included in lineup by 0.95
  # to reduce likelihood of being included in subsequent optimal lineups
  
  if(reduce_player_impact == T){
    optimize_data <- optimize_data %>%
      mutate(pt_projection = ifelse(get.variables(lp_model) == 1, 0.95 * pt_projection, pt_projection))
  }
  
  if(lineups > 1){
    
    for(i in 2:lineups){
      
      # Optimization Problem - Following Iterations
      
      ## Create optimization problem to find solution that maximizes point projection
      
      lp_model_i <- make.lp(0, players_available)
      set.objfn(lp_model_i, optimize_data$pt_projection)
      
      lp.control(lp_model_i, sense = "max")
      set.type(lp_model_i, 1:players_available, "binary")
      
      ## Constraint List
      
      if(cost_metric == "Projected Salary"){
        add.constraint(lp_model_i, optimize_data$proj_salary, "<=", 200 * budget_spent_pct) 
      }else{
        add.constraint(lp_model_i, optimize_data$aav, "<=", 200 * budget_spent_pct) 
      }
      add.constraint(lp_model_i, optimize_data$QB_Flag, "=", qb_slots)
      add.constraint(lp_model_i, optimize_data$RB_Flag, "<=", rb_slots + flex_slots)
      add.constraint(lp_model_i, optimize_data$RB_Flag, ">=", rb_slots)
      add.constraint(lp_model_i, optimize_data$WR_Flag, "<=", wr_slots + flex_slots)
      add.constraint(lp_model_i, optimize_data$WR_Flag, ">=", wr_slots)
      add.constraint(lp_model_i, optimize_data$TE_Flag, "<=", te_slots + flex_slots)
      add.constraint(lp_model_i, optimize_data$TE_Flag, ">=", te_slots)
      add.constraint(lp_model_i, optimize_data$Flex_Flag, "=", rb_slots + wr_slots + te_slots + flex_slots)
      add.constraint(lp_model_i, optimize_data$pt_projection, "<=", total_pt_projection)
      
      ## Solve optimization problem and return most optimal lineup
      
      solve(lp_model_i)
      
      optimal_lineup <- optimize_data %>%
        filter(get.variables(lp_model_i) == 1)
      
      optimal_lineup_long <- optimal_lineup %>%
        mutate(Lineup = i) %>%
        rename(Player = player, Team = team, Position = pos, `Projected Points` = pt_projection, 
               `Projected Salary` = proj_salary, AAV = aav) %>%
        select(Lineup, Player:AAV)
      
      optimal_lineup_wide <- optimal_lineup %>% 
        arrange(desc(pt_projection)) %>%
        group_by(pos) %>%
        mutate(pos_num = 1:n()) %>%
        ungroup() %>%
        mutate(flex_slot_flag = case_when(
          Flex_Flag == 0 ~ 0, 
          (RB_Flag == 1 & pos_num > rb_slots) ~ 1, 
          (WR_Flag == 1 & pos_num > wr_slots) ~ 1,
          (TE_Flag == 1 & pos_num > te_slots) ~ 1, 
          TRUE ~ 0)) %>%
        group_by(flex_slot_flag) %>%
        mutate(flex_num = ifelse(flex_slot_flag == 1, 1:n(), 0)) %>%
        ungroup() %>%
        mutate(position = case_when(
          flex_slot_flag > 0 ~ paste0("FLEX", flex_num), 
          TRUE ~ paste0(pos, pos_num))) %>%
        arrange(flex_num, desc(QB_Flag), desc(RB_Flag), desc(WR_Flag), desc(TE_Flag), flex_num) %>%
        select(player, position) %>%
        pivot_wider(names_from = position, values_from = player) %>%
        mutate(`Budget Spent` = ifelse(cost_metric == "Projected Salary", sum(optimal_lineup$proj_salary), sum(optimal_lineup$aav)), 
               `Total Points Projected` = sum(optimal_lineup$pt_projection), 
               Lineup = i) %>%
        select(Lineup, everything())
      
      total_pt_projection <<- sum(optimal_lineup_long$`Projected Points`) - 0.01
      
      optimal_lineup_long_output <- bind_rows(optimal_lineup_long_output, optimal_lineup_long)
      optimal_lineup_wide_output <- bind_rows(optimal_lineup_wide_output, optimal_lineup_wide)
      
      # If reduce_player_impact == T, multiply projected points of players included in lineup by 0.95 
      # to reduce likelihood of being included in subsequent optimal lineups
      
      if(reduce_player_impact == T){
        optimize_data <- optimize_data %>%
          mutate(pt_projection = ifelse(get.variables(lp_model_i) == 1, 0.95 * pt_projection, pt_projection))
      }
      
    }
  }
  
  if(long == T){
    return(optimal_lineup_long_output)
  }else{
    return(optimal_lineup_wide_output)
  }
  
}

get_optimal_lineup(scoring = "Standard", cost_metric = "AAV", lineups = 1, budget_spent_pct = 80,
                   qb_slots = 1, rb_slots = 2, wr_slots = 2, te_slots = 1, flex_slots = 1, long = F, reduce_player_impact = F)

