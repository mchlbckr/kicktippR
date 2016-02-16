library(rvest)

compute_points <- function(bet_home, bet_away, result_home, result_away){
  if(length(bet_home) == 1){
    bet_home <- rep(bet_home, length(result_home))
    bet_away <- rep(bet_away, length(result_home))
  }
  points <- rep(0,length(bet_home))
  # Correnct tendency
  points[sign(bet_home - bet_away) == sign(result_home - result_away)] <- 2
  # Draw
  points[bet_home == bet_away & result_home == result_away] <- 3
  # Correct difference
  points[bet_home - bet_away == result_home - result_away] <- 3
  # Correct result
  points[bet_home == result_home & bet_away == result_away] <- 4
  return(points)
}



init_kt_session <- function(username, password, www = "http://www.kicktipp.de/"){
  # Start HTML session
  kt_session <- html_session("http://www.kicktipp.de/")
  # Go to login form
  login_form <- html_form(kt_session)[[1]]
  # Enter values into form
  login_form <- set_values(login_form, kennung = username, passwort = password)
  # Submit form
  kt_session <- submit_form(kt_session, login_form, submit = "submitbutton")
  # Return session
  return(kt_session)
}


get_kt_odds <- function(kt_session){
  # Go to 'Tippabgabe'
  kt_session <- jump_to(kt_session, "hollowetten/tippabgabe")
  # Get and format table
  tbl <- html_table((html_nodes(kt_session, css = "table"))[2], header = TRUE)[[1]]
  tbl <- tbl[-nrow(tbl),]
  names(tbl)[8] <- "2"
  names(tbl)[8] <- "2"
  tbl$'1' <- as.numeric(tbl$'1')
  tbl$Heimmannschaft <- repair_encoding(tbl$Heimmannschaft)
  tbl$Gastmannschaft <- repair_encoding(tbl$Gastmannschaft)
  tbl <- tbl[,-c(1,4,5)]
  # return table and updated session
  return(list(odds_table = tbl,
              kt_session = kt_session))
}


kt_session <- init_kt_session(username = "", password = "")
kt_odds <- get_kt_odds(kt_session)




wh_session <- html_session("http://sports.williamhill.com/bet/de-de/betting/g/3471/Correct-Score.html")
wh_tables <- html_nodes(wh_session, css = "table")



tbl_match <- data.frame(Home_team = NULL,
                        Away_team = NULL,
                        Home = NULL,
                        Away = NULL,
                        Res = NULL,
                        Odds = NULL,
                        Prob = NULL,
                        Exp_points = NULL)
tbl_bets <- tbl_match
for (s in 1:length(wh_tables)){
  if(nrow(tbl_match)>0){
    tbl_match <- tbl_match[-(1:nrow(tbl_match)),]
  }
  for (j in c(3,5,7)){
    if(j == 3){
      res_type <- "home"
    } else if (j == 5){
      res_type = "draw"
    } else if (j == 7){
      res_type = "away"
    }
    # Fetch odds
    odds_tmp <- as.numeric(html_table(wh_tables[s], header = FALSE)[[1]][-c(1,2),j])
    rows_excl <- is.na(odds_tmp)
    odds_tmp <- odds_tmp[!rows_excl]
    # Fetch team name
    team_tmp <- html_table(wh_tables[s], header = FALSE)[[1]][-c(1,2),j+1]
    team_tmp <- team_tmp[!rows_excl]
    # Retch results
    res_tmp <- trimws(substring(team_tmp, first = nchar(team_tmp)-3))
    team_tmp <- substring(team_tmp, first = 1, last = nchar(team_tmp)-4)
    res_part_home <- ifelse(res_type == "home", 1, 3)
    res_part_away <- ifelse(res_type == "home", 3, 1)
    home_tmp <- as.numeric(substr(res_tmp,res_part_home,res_part_home))
    away_tmp <- as.numeric(substr(res_tmp,res_part_away,res_part_away))
    res_tmp_new <- paste0(home_tmp, "-", away_tmp)
    if(j == 3){
      home_team <- team_tmp[1]
    } else if (j == 7){
      away_team <- team_tmp[1]
    }
    tbl_tmp <- data.frame(Home_team = NA,
                          Away_team = NA,
                          Home = home_tmp,
                          Away = away_tmp,
                          Res = res_tmp_new,
                          Odds = odds_tmp,
                          Prob = 1/odds_tmp)
    tbl_match <- rbind(tbl_match, tbl_tmp)
    if(j == 7){
      tbl_match$Home_team <- home_team
      tbl_match$Away_team <- away_team
      tbl_match$Prob <- tbl_match$Prob/sum(tbl_match$Prob)
    }
  }
  for (i in 1:nrow(tbl_match)){
    tbl_match$Exp_points[i] <-
      sum(compute_points(tbl_match$Home[i],
                         tbl_match$Away[i],
                         tbl_match$Home,
                         tbl_match$Away) * tbl_match$Prob)
  }
  tbl_bets <- rbind(tbl_bets, tbl_match[which.max(tbl_match$Exp_points),])
}


tbl_bets
tbl_bets$Home_team<- repair_encoding(tbl_bets$Home_team)
tbl_bets$Away_team<- repair_encoding(tbl_bets$Away_team)
tbl_bets




kt_team_names <- c(kt_odds$odds_table$Heimmannschaft, c(kt_odds$odds_table$Gastmannschaft))
kt_team_names_cleaned <- kt_team_names
bet_team_names <- c(tbl_bets$Home_team, tbl_bets$Away_team)
bet_team_names_cleaned <- bet_team_names


for (text_replace in c("FSV", "SV", "FC", "04", "VfL", "1.", "Borussia",
                       "Bor.", "96", "05", "VfB", "98", "99", "Werder", "Eintracht", "Bayer ", "M'", "Mönchen")){
  kt_team_names_cleaned <- trimws(gsub(pattern = text_replace, x = kt_team_names_cleaned, replacement = "", ignore.case = TRUE))
  bet_team_names_cleaned <- trimws(gsub(pattern = text_replace, x = bet_team_names_cleaned, replacement = "", ignore.case = TRUE))
}

match_id <- numeric()
for (i in 1:length(kt_team_names_cleaned)){
  match_id[i] <- (agrep(kt_team_names_cleaned[i], bet_team_names_cleaned, max.distance = 2))
}

mapping_table <- data.frame(kt_names = kt_team_names,
                            bet_names = bet_team_names[match_id])

match(tbl_bets$Home_team, mapping_table$bet_names[match(mapping_table$kt_names, kt_odds$odds_table$Heimmannschaft)])

kt_session <- kt_odds$kt_session
kt_submit_form <- html_form(kt_session)[[2]]

kt_submit_home <- character()
kt_submit_away <- character()
kt_submit_button <- "submitbutton"
for (i in 1:length(kt_submit_form$fields)){
  if(grepl(pattern = "heimTipp", kt_submit_form$fields[i][[1]]$name)){
    kt_submit_home <- c(kt_submit_home, kt_submit_form$fields[i][[1]]$name)
  } else if(grepl(pattern = "gastTipp", kt_submit_form$fields[i][[1]]$name)){
    kt_submit_away <- c(kt_submit_away, kt_submit_form$fields[i][[1]]$name)
  }
}


submit_list <- list()
for (i in 1:9){
  submit_list[[kt_submit_home[i]]] <- tbl_bets$Home[match(mapping_table$bet_names[match(kt_odds$odds_table$Heimmannschaft, mapping_table$kt_names)],tbl_bets$Home_team)][i]
  submit_list[[kt_submit_away[i]]] <- tbl_bets$Away[match(mapping_table$bet_names[match(kt_odds$odds_table$Gastmannschaft, mapping_table$kt_names)],tbl_bets$Away_team)][i]
}




submit_list[["form"]] <- kt_submit_form

kt_submit_form <- do.call(set_values, submit_list)

kt_submit_form <- set_values(kt_submit_form, 'spieltippForms[275540044].heimTipp' = 1)

try(kt_session <- submit_form(kt_session, kt_submit_form, submit = "submitbutton"), silent = TRUE)

sum(tbl_bets$Exp_points)
