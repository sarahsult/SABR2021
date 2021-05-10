library(baseballr)
library(tidyverse)
library(dplyr)
library(sjmisc)
pitches2019_0901 <- scrape_statcast_savant_pitcher_all("2019-09-01",
                                                       "2019-09-01")
pitches2019_0519 <- scrape_statcast_savant_pitcher_all("2019-05-19",
                                                       "2019-05-19")
pitches2019_0407 <- scrape_statcast_savant_pitcher_all("2019-04-07",
                                                       "2019-04-07")
pitches2019_0411 <- scrape_statcast_savant_pitcher_all("2019-04-11",
                                                       "2019-04-11")
pitches2019_0413 <- scrape_statcast_savant_pitcher_all("2019-04-13",
                                                       "2019-04-13")
pitches2019_0418 <- scrape_statcast_savant_pitcher_all("2019-04-18",
                                                       "2019-04-18")
pitches2019_0422 <- scrape_statcast_savant_pitcher_all("2019-04-22",
                                                       "2019-04-22")
pitches2019_0506 <- scrape_statcast_savant_pitcher_all("2019-05-06",
                                                       "2019-05-06")
pitches2019_0521 <- scrape_statcast_savant_pitcher_all("2019-05-21",
                                                       "2019-05-21")
pitches2019_052829 <- scrape_statcast_savant_pitcher_all("2019-05-28",
                                                       "2019-05-29")
pitches2019_0609 <- scrape_statcast_savant_pitcher_all("2019-06-09",
                                                       "2019-06-09")
pitches2019_061819 <- scrape_statcast_savant_pitcher_all("2019-06-18",
                                                       "2019-06-19")
pitches2019_062729 <- scrape_statcast_savant_pitcher_all("2019-06-27",
                                                         "2019-06-29")
pitches2019_0712 <- scrape_statcast_savant_pitcher_all("2019-07-12",
                                                       "2019-07-12")
pitches2019_0811 <- scrape_statcast_savant_pitcher_all("2019-08-11",
                                                       "2019-08-11")
pitches2019_0817 <- scrape_statcast_savant_pitcher_all("2019-08-17",
                                                       "2019-08-17")
pitches2019_0828 <- scrape_statcast_savant_pitcher_all("2019-08-28",
                                                       "2019-08-28")
pitches2019_0904 <- scrape_statcast_savant_pitcher_all("2019-09-04",
                                                       "2019-09-04")
pitches2019_0909 <- scrape_statcast_savant_pitcher_all("2019-09-09",
                                                       "2019-09-09")
pitches2019_0914 <- scrape_statcast_savant_pitcher_all("2019-09-14",
                                                       "2019-09-14")
pitches2019_0916 <- scrape_statcast_savant_pitcher_all("2019-09-16",
                                                       "2019-09-16")
pitches2019_0924 <- scrape_statcast_savant_pitcher_all("2019-09-24",
                                                       "2019-09-24")
pitches2019_0929 <- scrape_statcast_savant_pitcher_all("2019-09-29",
                                                       "2019-09-29")

testpitches <- rbind(pitches2019_0519, pitches2019_0901, pitches2019_0407,
                     pitches2019_0411, pitches2019_0413, pitches2019_0418,
                     pitches2019_0422, pitches2019_0506, pitches2019_0521,
                     pitches2019_052829, pitches2019_0609, pitches2019_061819,
                     pitches2019_062729, pitches2019_0712, pitches2019_0811,
                     pitches2019_0817, pitches2019_0828, pitches2019_0904,
                     pitches2019_0909, pitches2019_0914, pitches2019_0916,
                     pitches2019_0924, pitches2019_0929)

#fixing all the nulls
for(pitch in 1:length(testpitches$pitch_type)){                     #sets up events with the right info
  if(testpitches$events[pitch]=="null"){
    print(pitch)
    if(str_contains(testpitches$description[pitch], "strike")){
      testpitches$events[pitch] = "strike"
    }
    else if(str_contains(testpitches$description[pitch], "sac_bunt")){
      testpitches$events[pitch] = "out"
    }
    else if(str_contains(testpitches$description[pitch], "foul")){
      testpitches$events[pitch] = "foul"                            #need to check count when assigning value!!
    }
    else if(str_contains(testpitches$description[pitch], "bunt")){
      testpitches$events[pitch] = "strike"
    }
    else if(str_contains(testpitches$description[pitch], "ball")){
      testpitches$events[pitch] = "ball"
    }
  }
}

uniqueevents <- unique(testpitches$events)

#cleaning up events
for(pitch in 1:length(testpitches$pitch_type)){                     #sets up events with the right info
  if(str_contains(testpitches$events[pitch], "field_error")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "sac_bunt")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "grounded_into_double_play")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "double_play")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "caught_stealing")){
    testpitches$events[pitch] = testpitches$description[pitch]
  }
  else if(str_contains(testpitches$events[pitch], "fielders_choice")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "sac_fly")){
    testpitches$events[pitch] = "out"
  }
  else if(str_contains(testpitches$events[pitch], "wild_pitch")){
    testpitches$events[pitch] = "ball"
  }
  else if(str_contains(testpitches$events[pitch], "batter_interference")){
    testpitches$events[pitch] = NA
  }
  else if(str_contains(testpitches$events[pitch], "catcher_interf")){
    testpitches$events[pitch] = NA
  }
  else if(toString(testpitches$events[pitch]) == "pitchout"){
    testpitches$events[pitch] = NA
  }
  else if(str_contains(testpitches$events[pitch], "null")){
    testpitches$events[pitch] = NA
  }
}
testpitches$events[76350] = "field_out"         #logged incorrectly 
testpitches$events[72939] = "out"

uniqueevents2 <- unique(testpitches$events)

#assigning values
testpitches$value <- 1                              #just to initialize..will change
for(pitch in 1:length(testpitches$pitch_type)){         
  if(str_contains(testpitches$events[pitch], "out")){
    testpitches$value[pitch] <- -0.265641
  }
  else if(str_contains(testpitches$events[pitch], "strike")){
    testpitches$value[pitch] <- -0.104628
  }
  else if(toString(testpitches$events[pitch]) == "ball"){
    testpitches$value[pitch] <- 0.0673192
  }
  else if(toString(testpitches$events[pitch]) == "walk"){
    testpitches$value[pitch] <- 0.0673192
  }
  else if(toString(testpitches$events[pitch]) == "hit_by_pitch"){
    testpitches$value[pitch] <- 0.3312207
  }
  else if(toString(testpitches$events[pitch]) == "single"){
    testpitches$value[pitch] <- 0.4565698
  }
  else if(toString(testpitches$events[pitch]) == "double"){
    testpitches$value[pitch] <- 0.7446239
  }
  else if(toString(testpitches$events[pitch]) == "triple"){
    testpitches$value[pitch] <- 1.0036235
  }
  else if(toString(testpitches$events[pitch]) == "home_run"){
    testpitches$value[pitch] <- 1.3448057
  }
  else if(toString(testpitches$events[pitch]) == "foul"){       
    testpitches$value[pitch] <- -0.041476
  }
  else if(is.na(testpitches$events[pitch])){          #for those weird cases we are ignoring
    testpitches$value[pitch] <- NA
  }
}

view(testpitches)

#subdivide testpitches into the 168 different variations: Gameday Zone (13), Pitch Type(Fastball, Offspeed, Breaking), L/R pitcher, L/R batter

#Zone 1 ########
testpitchesZone1 <- data.frame(testpitches)
testpitchesZone1 <- testpitchesZone1[0,]
x = 1
for(pitch in 1:length(testpitches$zone)){
  if(testpitches$zone[pitch] == 1 && is.na(testpitches$zone[pitch]) == FALSE){
    testpitchesZone1[x,] <- testpitches[pitch,]
    x = x+1
}}
view(testpitchesZone1)
  #Zone 1 AND Fastball
  testpitchesZone1_fastball <- data.frame(testpitchesZone1)
  testpitchesZone1_fastball <- testpitchesZone1_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1$pitch_type)){
    if(is.na(testpitchesZone1$pitch_type[pitch]) == FALSE){
      if(testpitchesZone1$pitch_type[pitch] == "FF" || testpitchesZone1$pitch_type[pitch] == "FT" || 
            testpitchesZone1$pitch_type[pitch] == "SI" || testpitchesZone1$pitch_type[pitch] == "FC")
      {
      testpitchesZone1_fastball[x,] <- testpitchesZone1[pitch,]
      x = x+1
    }}}
  view(testpitchesZone1_fastball)
    #Zone 1 AND Fastball AND R Batter
    testpitchesZone1_fastball_R <- data.frame(testpitchesZone1_fastball)
    testpitchesZone1_fastball_R <- testpitchesZone1_fastball_R[0,]
    x = 1
    for(pitch in 1:length(testpitchesZone1_fastball$stand)){
      if(testpitchesZone1_fastball$stand[pitch] == "R" && is.na(testpitchesZone1_fastball$stand[pitch]) == FALSE){
        testpitchesZone1_fastball_R[x,] <- testpitchesZone1_fastball[pitch,]
        x = x+1
      }}
    view(testpitchesZone1_fastball_R)
      #Zone 1 AND Fastball AND L Batter AND R Pitcher
      testpitchesZone1_fastball_R_R <- data.frame(testpitchesZone1_fastball_R)
      testpitchesZone1_fastball_R_R <- testpitchesZone1_fastball_R_R[0,]
      x = 1
      for(pitch in 1:length(testpitchesZone1_fastball_R$p_throws)){
        if(testpitchesZone1_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone1_fastball_R$p_throws[pitch]) == FALSE){
          testpitchesZone1_fastball_R_R[x,] <- testpitchesZone1_fastball_R[pitch,]
          x = x+1
        }}
      view(testpitchesZone1_fastball_R_R)
      #Zone 1 AND Fastball AND R Batter AND L Pitcher
      testpitchesZone1_fastball_R_L <- data.frame(testpitchesZone1_fastball_R)
      testpitchesZone1_fastball_R_L <- testpitchesZone1_fastball_R_L[0,]
      x = 1
      for(pitch in 1:length(testpitchesZone1_fastball_R$p_throws)){
        if(testpitchesZone1_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone1_fastball_R$p_throws[pitch]) == FALSE){
          testpitchesZone1_fastball_R_L[x,] <- testpitchesZone1_fastball_R[pitch,]
          x = x+1
        }}
      view(testpitchesZone1_fastball_R_L)
    #Zone 1 AND Fastball AND L Batter
    testpitchesZone1_fastball_L <- data.frame(testpitchesZone1_fastball)
    testpitchesZone1_fastball_L <- testpitchesZone1_fastball_L[0,]
    x = 1
    for(pitch in 1:length(testpitchesZone1_fastball$stand)){
      if(testpitchesZone1_fastball$stand[pitch] == "L" && is.na(testpitchesZone1_fastball$stand[pitch]) == FALSE){
        testpitchesZone1_fastball_L[x,] <- testpitchesZone1_fastball[pitch,]
        x = x+1
      }}
    view(testpitchesZone1_fastball_L)
      #Zone 1 AND Fastball AND L Batter AND R Pitcher
      testpitchesZone1_fastball_L_R <- data.frame(testpitchesZone1_fastball_L)
      testpitchesZone1_fastball_L_R <- testpitchesZone1_fastball_L_R[0,]
      x = 1
      for(pitch in 1:length(testpitchesZone1_fastball_L$p_throws)){
        if(testpitchesZone1_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone1_fastball_L$p_throws[pitch]) == FALSE){
          testpitchesZone1_fastball_L_R[x,] <- testpitchesZone1_fastball_L[pitch,]
          x = x+1
        }}
      view(testpitchesZone1_fastball_L_R)
      #Zone 1 AND Fastball AND L Batter AND L Pitcher
      testpitchesZone1_fastball_L_L <- data.frame(testpitchesZone1_fastball_L)
      testpitchesZone1_fastball_L_L <- testpitchesZone1_fastball_L_L[0,]
      x = 1
      for(pitch in 1:length(testpitchesZone1_fastball_L$p_throws)){
        if(testpitchesZone1_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone1_fastball_L$p_throws[pitch]) == FALSE){
          testpitchesZone1_fastball_L_L[x,] <- testpitchesZone1_fastball_L[pitch,]
          x = x+1
        }}
      view(testpitchesZone1_fastball_L_L)
  #Zone 1 AND Off-speed
  testpitchesZone1_offspeed <- data.frame(testpitchesZone1)
  testpitchesZone1_offspeed <- testpitchesZone1_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1$pitch_type)){
    if(is.na(testpitchesZone1$pitch_type[pitch]) == FALSE){
      if(testpitchesZone1$pitch_type[pitch] == "CH" || testpitchesZone1$pitch_type[pitch] == "FS" || 
         testpitchesZone1$pitch_type[pitch] == "FO" || testpitchesZone1$pitch_type[pitch] == "SC")
      {
        testpitchesZone1_offspeed[x,] <- testpitchesZone1[pitch,]
        x = x+1
      }}}
  view(testpitchesZone1_offspeed)
  
  #Zone 1 AND Offspeed AND R Batter
  testpitchesZone1_offspeed_R <- data.frame(testpitchesZone1_offspeed)
  testpitchesZone1_offspeed_R <- testpitchesZone1_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed$stand)){
    if(testpitchesZone1_offspeed$stand[pitch] == "R" && is.na(testpitchesZone1_offspeed$stand[pitch]) == FALSE){
      testpitchesZone1_offspeed_R[x,] <- testpitchesZone1_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_R)
  #Zone 1 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone1_offspeed_R_R <- data.frame(testpitchesZone1_offspeed_R)
  testpitchesZone1_offspeed_R_R <- testpitchesZone1_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed_R$p_throws)){
    if(testpitchesZone1_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone1_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone1_offspeed_R_R[x,] <- testpitchesZone1_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_R_R)
  #Zone 1 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone1_offspeed_R_L <- data.frame(testpitchesZone1_offspeed_R)
  testpitchesZone1_offspeed_R_L <- testpitchesZone1_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed_R$p_throws)){
    if(testpitchesZone1_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone1_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone1_offspeed_R_L[x,] <- testpitchesZone1_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_R_L)
  #Zone 1 AND Offspeed AND L Batter
  testpitchesZone1_offspeed_L <- data.frame(testpitchesZone1_offspeed)
  testpitchesZone1_offspeed_L <- testpitchesZone1_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed$stand)){
    if(testpitchesZone1_offspeed$stand[pitch] == "L" && is.na(testpitchesZone1_offspeed$stand[pitch]) == FALSE){
      testpitchesZone1_offspeed_L[x,] <- testpitchesZone1_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_L)
  #Zone 1 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone1_offspeed_L_R <- data.frame(testpitchesZone1_offspeed_L)
  testpitchesZone1_offspeed_L_R <- testpitchesZone1_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed_L$p_throws)){
    if(testpitchesZone1_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone1_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone1_offspeed_L_R[x,] <- testpitchesZone1_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_L_R)
  #Zone 1 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone1_offspeed_L_L <- data.frame(testpitchesZone1_offspeed_L)
  testpitchesZone1_offspeed_L_L <- testpitchesZone1_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_offspeed_L$p_throws)){
    if(testpitchesZone1_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone1_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone1_offspeed_L_L[x,] <- testpitchesZone1_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_offspeed_L_L)
  
  #Zone 1 AND breaking
  testpitchesZone1_breaking <- data.frame(testpitchesZone1)
  testpitchesZone1_breaking <- testpitchesZone1_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1$pitch_type)){
    if(is.na(testpitchesZone1$pitch_type[pitch]) == FALSE){
      if(testpitchesZone1$pitch_type[pitch] == "KN" || testpitchesZone1$pitch_type[pitch] == "SL" || 
         testpitchesZone1$pitch_type[pitch] == "KC" || testpitchesZone1$pitch_type[pitch] == "CU"
         || testpitchesZone1$pitch_type[pitch] == "EP"|| testpitchesZone1$pitch_type[pitch] == "CS")
      {
        testpitchesZone1_breaking[x,] <- testpitchesZone1[pitch,]
        x = x+1
      }}}
  view(testpitchesZone1_breaking)
  
  #Zone 1 AND Breaking AND R Batter
  testpitchesZone1_breaking_R <- data.frame(testpitchesZone1_breaking)
  testpitchesZone1_breaking_R <- testpitchesZone1_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking$stand)){
    if(testpitchesZone1_breaking$stand[pitch] == "R" && is.na(testpitchesZone1_breaking$stand[pitch]) == FALSE){
      testpitchesZone1_breaking_R[x,] <- testpitchesZone1_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_R)
  #Zone 1 AND breaking AND L Batter AND R Pitcher
  testpitchesZone1_breaking_R_R <- data.frame(testpitchesZone1_breaking_R)
  testpitchesZone1_breaking_R_R <- testpitchesZone1_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking_R$p_throws)){
    if(testpitchesZone1_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone1_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone1_breaking_R_R[x,] <- testpitchesZone1_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_R_R)
  #Zone 1 AND breaking AND R Batter AND L Pitcher
  testpitchesZone1_breaking_R_L <- data.frame(testpitchesZone1_breaking_R)
  testpitchesZone1_breaking_R_L <- testpitchesZone1_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking_R$p_throws)){
    if(testpitchesZone1_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone1_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone1_breaking_R_L[x,] <- testpitchesZone1_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_R_L)
  #Zone 1 AND breaking AND L Batter
  testpitchesZone1_breaking_L <- data.frame(testpitchesZone1_breaking)
  testpitchesZone1_breaking_L <- testpitchesZone1_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking$stand)){
    if(testpitchesZone1_breaking$stand[pitch] == "L" && is.na(testpitchesZone1_breaking$stand[pitch]) == FALSE){
      testpitchesZone1_breaking_L[x,] <- testpitchesZone1_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_L)
  #Zone 1 AND breaking AND L Batter AND R Pitcher
  testpitchesZone1_breaking_L_R <- data.frame(testpitchesZone1_breaking_L)
  testpitchesZone1_breaking_L_R <- testpitchesZone1_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking_L$p_throws)){
    if(testpitchesZone1_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone1_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone1_breaking_L_R[x,] <- testpitchesZone1_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_L_R)
  #Zone 1 AND breaking AND L Batter AND L Pitcher
  testpitchesZone1_breaking_L_L <- data.frame(testpitchesZone1_breaking_L)
  testpitchesZone1_breaking_L_L <- testpitchesZone1_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone1_breaking_L$p_throws)){
    if(testpitchesZone1_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone1_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone1_breaking_L_L[x,] <- testpitchesZone1_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone1_breaking_L_L)
  
  #Zone 2 ########
  testpitchesZone2 <- data.frame(testpitches)
  testpitchesZone2 <- testpitchesZone2[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 2 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone2[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone2)
  #Zone 2 AND Fastball
  testpitchesZone2_fastball <- data.frame(testpitchesZone2)
  testpitchesZone2_fastball <- testpitchesZone2_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2$pitch_type)){
    if(is.na(testpitchesZone2$pitch_type[pitch]) == FALSE){
      if(testpitchesZone2$pitch_type[pitch] == "FF" || testpitchesZone2$pitch_type[pitch] == "FT" || 
         testpitchesZone2$pitch_type[pitch] == "SI" || testpitchesZone2$pitch_type[pitch] == "FC")
      {
        testpitchesZone2_fastball[x,] <- testpitchesZone2[pitch,]
        x = x+1
      }}}
  view(testpitchesZone2_fastball)
  #Zone 2 AND Fastball AND R Batter
  testpitchesZone2_fastball_R <- data.frame(testpitchesZone2_fastball)
  testpitchesZone2_fastball_R <- testpitchesZone2_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball$stand)){
    if(testpitchesZone2_fastball$stand[pitch] == "R" && is.na(testpitchesZone2_fastball$stand[pitch]) == FALSE){
      testpitchesZone2_fastball_R[x,] <- testpitchesZone2_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_R)
  #Zone 2 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone2_fastball_R_R <- data.frame(testpitchesZone2_fastball_R)
  testpitchesZone2_fastball_R_R <- testpitchesZone2_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball_R$p_throws)){
    if(testpitchesZone2_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone2_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_fastball_R_R[x,] <- testpitchesZone2_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_R_R)
  #Zone 2 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone2_fastball_R_L <- data.frame(testpitchesZone2_fastball_R)
  testpitchesZone2_fastball_R_L <- testpitchesZone2_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball_R$p_throws)){
    if(testpitchesZone2_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone2_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_fastball_R_L[x,] <- testpitchesZone2_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_R_L)
  #Zone 2 AND Fastball AND L Batter
  testpitchesZone2_fastball_L <- data.frame(testpitchesZone2_fastball)
  testpitchesZone2_fastball_L <- testpitchesZone2_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball$stand)){
    if(testpitchesZone2_fastball$stand[pitch] == "L" && is.na(testpitchesZone2_fastball$stand[pitch]) == FALSE){
      testpitchesZone2_fastball_L[x,] <- testpitchesZone2_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_L)
  #Zone 2 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone2_fastball_L_R <- data.frame(testpitchesZone2_fastball_L)
  testpitchesZone2_fastball_L_R <- testpitchesZone2_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball_L$p_throws)){
    if(testpitchesZone2_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone2_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_fastball_L_R[x,] <- testpitchesZone2_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_L_R)
  #Zone 2 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone2_fastball_L_L <- data.frame(testpitchesZone2_fastball_L)
  testpitchesZone2_fastball_L_L <- testpitchesZone2_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_fastball_L$p_throws)){
    if(testpitchesZone2_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone2_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_fastball_L_L[x,] <- testpitchesZone2_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_fastball_L_L)
  #Zone 2 AND Off-speed
  testpitchesZone2_offspeed <- data.frame(testpitchesZone2)
  testpitchesZone2_offspeed <- testpitchesZone2_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2$pitch_type)){
    if(is.na(testpitchesZone2$pitch_type[pitch]) == FALSE){
      if(testpitchesZone2$pitch_type[pitch] == "CH" || testpitchesZone2$pitch_type[pitch] == "FS" || 
         testpitchesZone2$pitch_type[pitch] == "FO" || testpitchesZone2$pitch_type[pitch] == "SC")
      {
        testpitchesZone2_offspeed[x,] <- testpitchesZone2[pitch,]
        x = x+1
      }}}
  view(testpitchesZone2_offspeed)
  
  #Zone 2 AND Offspeed AND R Batter
  testpitchesZone2_offspeed_R <- data.frame(testpitchesZone2_offspeed)
  testpitchesZone2_offspeed_R <- testpitchesZone2_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed$stand)){
    if(testpitchesZone2_offspeed$stand[pitch] == "R" && is.na(testpitchesZone2_offspeed$stand[pitch]) == FALSE){
      testpitchesZone2_offspeed_R[x,] <- testpitchesZone2_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_R)
  #Zone 2 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone2_offspeed_R_R <- data.frame(testpitchesZone2_offspeed_R)
  testpitchesZone2_offspeed_R_R <- testpitchesZone2_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed_R$p_throws)){
    if(testpitchesZone2_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone2_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_offspeed_R_R[x,] <- testpitchesZone2_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_R_R)
  #Zone 2 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone2_offspeed_R_L <- data.frame(testpitchesZone2_offspeed_R)
  testpitchesZone2_offspeed_R_L <- testpitchesZone2_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed_R$p_throws)){
    if(testpitchesZone2_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone2_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_offspeed_R_L[x,] <- testpitchesZone2_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_R_L)
  #Zone 2 AND Offspeed AND L Batter
  testpitchesZone2_offspeed_L <- data.frame(testpitchesZone2_offspeed)
  testpitchesZone2_offspeed_L <- testpitchesZone2_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed$stand)){
    if(testpitchesZone2_offspeed$stand[pitch] == "L" && is.na(testpitchesZone2_offspeed$stand[pitch]) == FALSE){
      testpitchesZone2_offspeed_L[x,] <- testpitchesZone2_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_L)
  #Zone 2 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone2_offspeed_L_R <- data.frame(testpitchesZone2_offspeed_L)
  testpitchesZone2_offspeed_L_R <- testpitchesZone2_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed_L$p_throws)){
    if(testpitchesZone2_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone2_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_offspeed_L_R[x,] <- testpitchesZone2_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_L_R)
  #Zone 2 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone2_offspeed_L_L <- data.frame(testpitchesZone2_offspeed_L)
  testpitchesZone2_offspeed_L_L <- testpitchesZone2_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_offspeed_L$p_throws)){
    if(testpitchesZone2_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone2_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_offspeed_L_L[x,] <- testpitchesZone2_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_offspeed_L_L)
  
  #Zone 2 AND breaking
  testpitchesZone2_breaking <- data.frame(testpitchesZone2)
  testpitchesZone2_breaking <- testpitchesZone2_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2$pitch_type)){
    if(is.na(testpitchesZone2$pitch_type[pitch]) == FALSE){
      if(testpitchesZone2$pitch_type[pitch] == "KN" || testpitchesZone2$pitch_type[pitch] == "SL" || 
         testpitchesZone2$pitch_type[pitch] == "KC" || testpitchesZone2$pitch_type[pitch] == "CU"
         || testpitchesZone2$pitch_type[pitch] == "EP"|| testpitchesZone2$pitch_type[pitch] == "CS")
      {
        testpitchesZone2_breaking[x,] <- testpitchesZone2[pitch,]
        x = x+1
      }}}
  view(testpitchesZone2_breaking)
  
  #Zone 2 AND Breaking AND R Batter
  testpitchesZone2_breaking_R <- data.frame(testpitchesZone2_breaking)
  testpitchesZone2_breaking_R <- testpitchesZone2_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking$stand)){
    if(testpitchesZone2_breaking$stand[pitch] == "R" && is.na(testpitchesZone2_breaking$stand[pitch]) == FALSE){
      testpitchesZone2_breaking_R[x,] <- testpitchesZone2_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_R)
  #Zone 2 AND breaking AND L Batter AND R Pitcher
  testpitchesZone2_breaking_R_R <- data.frame(testpitchesZone2_breaking_R)
  testpitchesZone2_breaking_R_R <- testpitchesZone2_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking_R$p_throws)){
    if(testpitchesZone2_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone2_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_breaking_R_R[x,] <- testpitchesZone2_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_R_R)
  #Zone 2 AND breaking AND R Batter AND L Pitcher
  testpitchesZone2_breaking_R_L <- data.frame(testpitchesZone2_breaking_R)
  testpitchesZone2_breaking_R_L <- testpitchesZone2_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking_R$p_throws)){
    if(testpitchesZone2_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone2_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone2_breaking_R_L[x,] <- testpitchesZone2_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_R_L)
  #Zone 2 AND breaking AND L Batter
  testpitchesZone2_breaking_L <- data.frame(testpitchesZone2_breaking)
  testpitchesZone2_breaking_L <- testpitchesZone2_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking$stand)){
    if(testpitchesZone2_breaking$stand[pitch] == "L" && is.na(testpitchesZone2_breaking$stand[pitch]) == FALSE){
      testpitchesZone2_breaking_L[x,] <- testpitchesZone2_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_L)
  #Zone 2 AND breaking AND L Batter AND R Pitcher
  testpitchesZone2_breaking_L_R <- data.frame(testpitchesZone2_breaking_L)
  testpitchesZone2_breaking_L_R <- testpitchesZone2_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking_L$p_throws)){
    if(testpitchesZone2_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone2_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_breaking_L_R[x,] <- testpitchesZone2_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_L_R)
  #Zone 2 AND breaking AND L Batter AND L Pitcher
  testpitchesZone2_breaking_L_L <- data.frame(testpitchesZone2_breaking_L)
  testpitchesZone2_breaking_L_L <- testpitchesZone2_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone2_breaking_L$p_throws)){
    if(testpitchesZone2_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone2_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone2_breaking_L_L[x,] <- testpitchesZone2_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone2_breaking_L_L)
  
  #Zone 3 ######
  testpitchesZone3 <- data.frame(testpitches)
  testpitchesZone3 <- testpitchesZone3[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 3 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone3[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone3)
  #Zone 3 AND Fastball
  testpitchesZone3_fastball <- data.frame(testpitchesZone3)
  testpitchesZone3_fastball <- testpitchesZone3_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3$pitch_type)){
    if(is.na(testpitchesZone3$pitch_type[pitch]) == FALSE){
      if(testpitchesZone3$pitch_type[pitch] == "FF" || testpitchesZone3$pitch_type[pitch] == "FT" || 
         testpitchesZone3$pitch_type[pitch] == "SI" || testpitchesZone3$pitch_type[pitch] == "FC")
      {
        testpitchesZone3_fastball[x,] <- testpitchesZone3[pitch,]
        x = x+1
      }}}
  view(testpitchesZone3_fastball)
  #Zone 3 AND Fastball AND R Batter
  testpitchesZone3_fastball_R <- data.frame(testpitchesZone3_fastball)
  testpitchesZone3_fastball_R <- testpitchesZone3_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball$stand)){
    if(testpitchesZone3_fastball$stand[pitch] == "R" && is.na(testpitchesZone3_fastball$stand[pitch]) == FALSE){
      testpitchesZone3_fastball_R[x,] <- testpitchesZone3_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_R)
  #Zone 3 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone3_fastball_R_R <- data.frame(testpitchesZone3_fastball_R)
  testpitchesZone3_fastball_R_R <- testpitchesZone3_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball_R$p_throws)){
    if(testpitchesZone3_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone3_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_fastball_R_R[x,] <- testpitchesZone3_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_R_R)
  #Zone 3 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone3_fastball_R_L <- data.frame(testpitchesZone3_fastball_R)
  testpitchesZone3_fastball_R_L <- testpitchesZone3_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball_R$p_throws)){
    if(testpitchesZone3_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone3_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_fastball_R_L[x,] <- testpitchesZone3_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_R_L)
  #Zone 3 AND Fastball AND L Batter
  testpitchesZone3_fastball_L <- data.frame(testpitchesZone3_fastball)
  testpitchesZone3_fastball_L <- testpitchesZone3_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball$stand)){
    if(testpitchesZone3_fastball$stand[pitch] == "L" && is.na(testpitchesZone3_fastball$stand[pitch]) == FALSE){
      testpitchesZone3_fastball_L[x,] <- testpitchesZone3_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_L)
  #Zone 3 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone3_fastball_L_R <- data.frame(testpitchesZone3_fastball_L)
  testpitchesZone3_fastball_L_R <- testpitchesZone3_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball_L$p_throws)){
    if(testpitchesZone3_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone3_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_fastball_L_R[x,] <- testpitchesZone3_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_L_R)
  #Zone 3 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone3_fastball_L_L <- data.frame(testpitchesZone3_fastball_L)
  testpitchesZone3_fastball_L_L <- testpitchesZone3_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_fastball_L$p_throws)){
    if(testpitchesZone3_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone3_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_fastball_L_L[x,] <- testpitchesZone3_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_fastball_L_L)
  #Zone 3 AND Off-speed
  testpitchesZone3_offspeed <- data.frame(testpitchesZone3)
  testpitchesZone3_offspeed <- testpitchesZone3_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3$pitch_type)){
    if(is.na(testpitchesZone3$pitch_type[pitch]) == FALSE){
      if(testpitchesZone3$pitch_type[pitch] == "CH" || testpitchesZone3$pitch_type[pitch] == "FS" || 
         testpitchesZone3$pitch_type[pitch] == "FO" || testpitchesZone3$pitch_type[pitch] == "SC")
      {
        testpitchesZone3_offspeed[x,] <- testpitchesZone3[pitch,]
        x = x+1
      }}}
  view(testpitchesZone3_offspeed)
  
  #Zone 3 AND Offspeed AND R Batter
  testpitchesZone3_offspeed_R <- data.frame(testpitchesZone3_offspeed)
  testpitchesZone3_offspeed_R <- testpitchesZone3_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed$stand)){
    if(testpitchesZone3_offspeed$stand[pitch] == "R" && is.na(testpitchesZone3_offspeed$stand[pitch]) == FALSE){
      testpitchesZone3_offspeed_R[x,] <- testpitchesZone3_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_R)
  #Zone 3 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone3_offspeed_R_R <- data.frame(testpitchesZone3_offspeed_R)
  testpitchesZone3_offspeed_R_R <- testpitchesZone3_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed_R$p_throws)){
    if(testpitchesZone3_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone3_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_offspeed_R_R[x,] <- testpitchesZone3_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_R_R)
  #Zone 3 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone3_offspeed_R_L <- data.frame(testpitchesZone3_offspeed_R)
  testpitchesZone3_offspeed_R_L <- testpitchesZone3_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed_R$p_throws)){
    if(testpitchesZone3_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone3_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_offspeed_R_L[x,] <- testpitchesZone3_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_R_L)
  #Zone 3 AND Offspeed AND L Batter
  testpitchesZone3_offspeed_L <- data.frame(testpitchesZone3_offspeed)
  testpitchesZone3_offspeed_L <- testpitchesZone3_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed$stand)){
    if(testpitchesZone3_offspeed$stand[pitch] == "L" && is.na(testpitchesZone3_offspeed$stand[pitch]) == FALSE){
      testpitchesZone3_offspeed_L[x,] <- testpitchesZone3_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_L)
  #Zone 3 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone3_offspeed_L_R <- data.frame(testpitchesZone3_offspeed_L)
  testpitchesZone3_offspeed_L_R <- testpitchesZone3_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed_L$p_throws)){
    if(testpitchesZone3_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone3_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_offspeed_L_R[x,] <- testpitchesZone3_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_L_R)
  #Zone 3 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone3_offspeed_L_L <- data.frame(testpitchesZone3_offspeed_L)
  testpitchesZone3_offspeed_L_L <- testpitchesZone3_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_offspeed_L$p_throws)){
    if(testpitchesZone3_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone3_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_offspeed_L_L[x,] <- testpitchesZone3_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_offspeed_L_L)
  
  #Zone 3 AND breaking
  testpitchesZone3_breaking <- data.frame(testpitchesZone3)
  testpitchesZone3_breaking <- testpitchesZone3_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3$pitch_type)){
    if(is.na(testpitchesZone3$pitch_type[pitch]) == FALSE){
      if(testpitchesZone3$pitch_type[pitch] == "KN" || testpitchesZone3$pitch_type[pitch] == "SL" || 
         testpitchesZone3$pitch_type[pitch] == "KC" || testpitchesZone3$pitch_type[pitch] == "CU"
         || testpitchesZone3$pitch_type[pitch] == "EP"|| testpitchesZone3$pitch_type[pitch] == "CS")
      {
        testpitchesZone3_breaking[x,] <- testpitchesZone3[pitch,]
        x = x+1
      }}}
  view(testpitchesZone3_breaking)
  
  #Zone 3 AND Breaking AND R Batter
  testpitchesZone3_breaking_R <- data.frame(testpitchesZone3_breaking)
  testpitchesZone3_breaking_R <- testpitchesZone3_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking$stand)){
    if(testpitchesZone3_breaking$stand[pitch] == "R" && is.na(testpitchesZone3_breaking$stand[pitch]) == FALSE){
      testpitchesZone3_breaking_R[x,] <- testpitchesZone3_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_R)
  #Zone 3 AND breaking AND L Batter AND R Pitcher
  testpitchesZone3_breaking_R_R <- data.frame(testpitchesZone3_breaking_R)
  testpitchesZone3_breaking_R_R <- testpitchesZone3_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking_R$p_throws)){
    if(testpitchesZone3_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone3_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_breaking_R_R[x,] <- testpitchesZone3_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_R_R)
  #Zone 3 AND breaking AND R Batter AND L Pitcher
  testpitchesZone3_breaking_R_L <- data.frame(testpitchesZone3_breaking_R)
  testpitchesZone3_breaking_R_L <- testpitchesZone3_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking_R$p_throws)){
    if(testpitchesZone3_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone3_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone3_breaking_R_L[x,] <- testpitchesZone3_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_R_L)
  #Zone 3 AND breaking AND L Batter
  testpitchesZone3_breaking_L <- data.frame(testpitchesZone3_breaking)
  testpitchesZone3_breaking_L <- testpitchesZone3_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking$stand)){
    if(testpitchesZone3_breaking$stand[pitch] == "L" && is.na(testpitchesZone3_breaking$stand[pitch]) == FALSE){
      testpitchesZone3_breaking_L[x,] <- testpitchesZone3_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_L)
  #Zone 3 AND breaking AND L Batter AND R Pitcher
  testpitchesZone3_breaking_L_R <- data.frame(testpitchesZone3_breaking_L)
  testpitchesZone3_breaking_L_R <- testpitchesZone3_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking_L$p_throws)){
    if(testpitchesZone3_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone3_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_breaking_L_R[x,] <- testpitchesZone3_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_L_R)
  #Zone 3 AND breaking AND L Batter AND L Pitcher
  testpitchesZone3_breaking_L_L <- data.frame(testpitchesZone3_breaking_L)
  testpitchesZone3_breaking_L_L <- testpitchesZone3_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone3_breaking_L$p_throws)){
    if(testpitchesZone3_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone3_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone3_breaking_L_L[x,] <- testpitchesZone3_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone3_breaking_L_L)
  
  #Zone 4 ######
  testpitchesZone4 <- data.frame(testpitches)
  testpitchesZone4 <- testpitchesZone4[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 4 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone4[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone4)
  #Zone 4 AND Fastball
  testpitchesZone4_fastball <- data.frame(testpitchesZone4)
  testpitchesZone4_fastball <- testpitchesZone4_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4$pitch_type)){
    if(is.na(testpitchesZone4$pitch_type[pitch]) == FALSE){
      if(testpitchesZone4$pitch_type[pitch] == "FF" || testpitchesZone4$pitch_type[pitch] == "FT" || 
         testpitchesZone4$pitch_type[pitch] == "SI" || testpitchesZone4$pitch_type[pitch] == "FC")
      {
        testpitchesZone4_fastball[x,] <- testpitchesZone4[pitch,]
        x = x+1
      }}}
  view(testpitchesZone4_fastball)
  #Zone 4 AND Fastball AND R Batter
  testpitchesZone4_fastball_R <- data.frame(testpitchesZone4_fastball)
  testpitchesZone4_fastball_R <- testpitchesZone4_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball$stand)){
    if(testpitchesZone4_fastball$stand[pitch] == "R" && is.na(testpitchesZone4_fastball$stand[pitch]) == FALSE){
      testpitchesZone4_fastball_R[x,] <- testpitchesZone4_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_R)
  #Zone 4 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone4_fastball_R_R <- data.frame(testpitchesZone4_fastball_R)
  testpitchesZone4_fastball_R_R <- testpitchesZone4_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball_R$p_throws)){
    if(testpitchesZone4_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone4_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_fastball_R_R[x,] <- testpitchesZone4_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_R_R)
  #Zone 4 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone4_fastball_R_L <- data.frame(testpitchesZone4_fastball_R)
  testpitchesZone4_fastball_R_L <- testpitchesZone4_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball_R$p_throws)){
    if(testpitchesZone4_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone4_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_fastball_R_L[x,] <- testpitchesZone4_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_R_L)
  #Zone 4 AND Fastball AND L Batter
  testpitchesZone4_fastball_L <- data.frame(testpitchesZone4_fastball)
  testpitchesZone4_fastball_L <- testpitchesZone4_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball$stand)){
    if(testpitchesZone4_fastball$stand[pitch] == "L" && is.na(testpitchesZone4_fastball$stand[pitch]) == FALSE){
      testpitchesZone4_fastball_L[x,] <- testpitchesZone4_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_L)
  #Zone 4 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone4_fastball_L_R <- data.frame(testpitchesZone4_fastball_L)
  testpitchesZone4_fastball_L_R <- testpitchesZone4_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball_L$p_throws)){
    if(testpitchesZone4_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone4_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_fastball_L_R[x,] <- testpitchesZone4_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_L_R)
  #Zone 4 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone4_fastball_L_L <- data.frame(testpitchesZone4_fastball_L)
  testpitchesZone4_fastball_L_L <- testpitchesZone4_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_fastball_L$p_throws)){
    if(testpitchesZone4_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone4_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_fastball_L_L[x,] <- testpitchesZone4_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_fastball_L_L)
  #Zone 4 AND Off-speed
  testpitchesZone4_offspeed <- data.frame(testpitchesZone4)
  testpitchesZone4_offspeed <- testpitchesZone4_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4$pitch_type)){
    if(is.na(testpitchesZone4$pitch_type[pitch]) == FALSE){
      if(testpitchesZone4$pitch_type[pitch] == "CH" || testpitchesZone4$pitch_type[pitch] == "FS" || 
         testpitchesZone4$pitch_type[pitch] == "FO" || testpitchesZone4$pitch_type[pitch] == "SC")
      {
        testpitchesZone4_offspeed[x,] <- testpitchesZone4[pitch,]
        x = x+1
      }}}
  view(testpitchesZone4_offspeed)
  
  #Zone 4 AND Offspeed AND R Batter
  testpitchesZone4_offspeed_R <- data.frame(testpitchesZone4_offspeed)
  testpitchesZone4_offspeed_R <- testpitchesZone4_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed$stand)){
    if(testpitchesZone4_offspeed$stand[pitch] == "R" && is.na(testpitchesZone4_offspeed$stand[pitch]) == FALSE){
      testpitchesZone4_offspeed_R[x,] <- testpitchesZone4_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_R)
  #Zone 4 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone4_offspeed_R_R <- data.frame(testpitchesZone4_offspeed_R)
  testpitchesZone4_offspeed_R_R <- testpitchesZone4_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed_R$p_throws)){
    if(testpitchesZone4_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone4_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_offspeed_R_R[x,] <- testpitchesZone4_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_R_R)
  #Zone 4 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone4_offspeed_R_L <- data.frame(testpitchesZone4_offspeed_R)
  testpitchesZone4_offspeed_R_L <- testpitchesZone4_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed_R$p_throws)){
    if(testpitchesZone4_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone4_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_offspeed_R_L[x,] <- testpitchesZone4_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_R_L)
  #Zone 4 AND Offspeed AND L Batter
  testpitchesZone4_offspeed_L <- data.frame(testpitchesZone4_offspeed)
  testpitchesZone4_offspeed_L <- testpitchesZone4_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed$stand)){
    if(testpitchesZone4_offspeed$stand[pitch] == "L" && is.na(testpitchesZone4_offspeed$stand[pitch]) == FALSE){
      testpitchesZone4_offspeed_L[x,] <- testpitchesZone4_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_L)
  #Zone 4 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone4_offspeed_L_R <- data.frame(testpitchesZone4_offspeed_L)
  testpitchesZone4_offspeed_L_R <- testpitchesZone4_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed_L$p_throws)){
    if(testpitchesZone4_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone4_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_offspeed_L_R[x,] <- testpitchesZone4_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_L_R)
  #Zone 4 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone4_offspeed_L_L <- data.frame(testpitchesZone4_offspeed_L)
  testpitchesZone4_offspeed_L_L <- testpitchesZone4_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_offspeed_L$p_throws)){
    if(testpitchesZone4_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone4_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_offspeed_L_L[x,] <- testpitchesZone4_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_offspeed_L_L)
  
  #Zone 4 AND breaking
  testpitchesZone4_breaking <- data.frame(testpitchesZone4)
  testpitchesZone4_breaking <- testpitchesZone4_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4$pitch_type)){
    if(is.na(testpitchesZone4$pitch_type[pitch]) == FALSE){
      if(testpitchesZone4$pitch_type[pitch] == "KN" || testpitchesZone4$pitch_type[pitch] == "SL" || 
         testpitchesZone4$pitch_type[pitch] == "KC" || testpitchesZone4$pitch_type[pitch] == "CU"
         || testpitchesZone4$pitch_type[pitch] == "EP"|| testpitchesZone4$pitch_type[pitch] == "CS")
      {
        testpitchesZone4_breaking[x,] <- testpitchesZone4[pitch,]
        x = x+1
      }}}
  view(testpitchesZone4_breaking)
  
  #Zone 4 AND Breaking AND R Batter
  testpitchesZone4_breaking_R <- data.frame(testpitchesZone4_breaking)
  testpitchesZone4_breaking_R <- testpitchesZone4_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking$stand)){
    if(testpitchesZone4_breaking$stand[pitch] == "R" && is.na(testpitchesZone4_breaking$stand[pitch]) == FALSE){
      testpitchesZone4_breaking_R[x,] <- testpitchesZone4_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_R)
  #Zone 4 AND breaking AND L Batter AND R Pitcher
  testpitchesZone4_breaking_R_R <- data.frame(testpitchesZone4_breaking_R)
  testpitchesZone4_breaking_R_R <- testpitchesZone4_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking_R$p_throws)){
    if(testpitchesZone4_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone4_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_breaking_R_R[x,] <- testpitchesZone4_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_R_R)
  #Zone 4 AND breaking AND R Batter AND L Pitcher
  testpitchesZone4_breaking_R_L <- data.frame(testpitchesZone4_breaking_R)
  testpitchesZone4_breaking_R_L <- testpitchesZone4_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking_R$p_throws)){
    if(testpitchesZone4_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone4_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone4_breaking_R_L[x,] <- testpitchesZone4_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_R_L)
  #Zone 4 AND breaking AND L Batter
  testpitchesZone4_breaking_L <- data.frame(testpitchesZone4_breaking)
  testpitchesZone4_breaking_L <- testpitchesZone4_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking$stand)){
    if(testpitchesZone4_breaking$stand[pitch] == "L" && is.na(testpitchesZone4_breaking$stand[pitch]) == FALSE){
      testpitchesZone4_breaking_L[x,] <- testpitchesZone4_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_L)
  #Zone 4 AND breaking AND L Batter AND R Pitcher
  testpitchesZone4_breaking_L_R <- data.frame(testpitchesZone4_breaking_L)
  testpitchesZone4_breaking_L_R <- testpitchesZone4_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking_L$p_throws)){
    if(testpitchesZone4_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone4_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_breaking_L_R[x,] <- testpitchesZone4_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_L_R)
  #Zone 4 AND breaking AND L Batter AND L Pitcher
  testpitchesZone4_breaking_L_L <- data.frame(testpitchesZone4_breaking_L)
  testpitchesZone4_breaking_L_L <- testpitchesZone4_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone4_breaking_L$p_throws)){
    if(testpitchesZone4_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone4_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone4_breaking_L_L[x,] <- testpitchesZone4_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone4_breaking_L_L)
  
  #Zone 5 ######
  testpitchesZone5 <- data.frame(testpitches)
  testpitchesZone5 <- testpitchesZone5[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 5 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone5[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone5)
  #Zone 5 AND Fastball
  testpitchesZone5_fastball <- data.frame(testpitchesZone5)
  testpitchesZone5_fastball <- testpitchesZone5_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5$pitch_type)){
    if(is.na(testpitchesZone5$pitch_type[pitch]) == FALSE){
      if(testpitchesZone5$pitch_type[pitch] == "FF" || testpitchesZone5$pitch_type[pitch] == "FT" || 
         testpitchesZone5$pitch_type[pitch] == "SI" || testpitchesZone5$pitch_type[pitch] == "FC")
      {
        testpitchesZone5_fastball[x,] <- testpitchesZone5[pitch,]
        x = x+1
      }}}
  view(testpitchesZone5_fastball)
  #Zone 5 AND Fastball AND R Batter
  testpitchesZone5_fastball_R <- data.frame(testpitchesZone5_fastball)
  testpitchesZone5_fastball_R <- testpitchesZone5_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball$stand)){
    if(testpitchesZone5_fastball$stand[pitch] == "R" && is.na(testpitchesZone5_fastball$stand[pitch]) == FALSE){
      testpitchesZone5_fastball_R[x,] <- testpitchesZone5_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_R)
  #Zone 5 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone5_fastball_R_R <- data.frame(testpitchesZone5_fastball_R)
  testpitchesZone5_fastball_R_R <- testpitchesZone5_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball_R$p_throws)){
    if(testpitchesZone5_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone5_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_fastball_R_R[x,] <- testpitchesZone5_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_R_R)
  #Zone 5 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone5_fastball_R_L <- data.frame(testpitchesZone5_fastball_R)
  testpitchesZone5_fastball_R_L <- testpitchesZone5_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball_R$p_throws)){
    if(testpitchesZone5_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone5_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_fastball_R_L[x,] <- testpitchesZone5_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_R_L)
  #Zone 5 AND Fastball AND L Batter
  testpitchesZone5_fastball_L <- data.frame(testpitchesZone5_fastball)
  testpitchesZone5_fastball_L <- testpitchesZone5_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball$stand)){
    if(testpitchesZone5_fastball$stand[pitch] == "L" && is.na(testpitchesZone5_fastball$stand[pitch]) == FALSE){
      testpitchesZone5_fastball_L[x,] <- testpitchesZone5_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_L)
  #Zone 5 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone5_fastball_L_R <- data.frame(testpitchesZone5_fastball_L)
  testpitchesZone5_fastball_L_R <- testpitchesZone5_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball_L$p_throws)){
    if(testpitchesZone5_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone5_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_fastball_L_R[x,] <- testpitchesZone5_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_L_R)
  #Zone 5 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone5_fastball_L_L <- data.frame(testpitchesZone5_fastball_L)
  testpitchesZone5_fastball_L_L <- testpitchesZone5_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_fastball_L$p_throws)){
    if(testpitchesZone5_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone5_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_fastball_L_L[x,] <- testpitchesZone5_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_fastball_L_L)
  #Zone 5 AND Off-speed
  testpitchesZone5_offspeed <- data.frame(testpitchesZone5)
  testpitchesZone5_offspeed <- testpitchesZone5_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5$pitch_type)){
    if(is.na(testpitchesZone5$pitch_type[pitch]) == FALSE){
      if(testpitchesZone5$pitch_type[pitch] == "CH" || testpitchesZone5$pitch_type[pitch] == "FS" || 
         testpitchesZone5$pitch_type[pitch] == "FO" || testpitchesZone5$pitch_type[pitch] == "SC")
      {
        testpitchesZone5_offspeed[x,] <- testpitchesZone5[pitch,]
        x = x+1
      }}}
  view(testpitchesZone5_offspeed)
  
  #Zone 5 AND Offspeed AND R Batter
  testpitchesZone5_offspeed_R <- data.frame(testpitchesZone5_offspeed)
  testpitchesZone5_offspeed_R <- testpitchesZone5_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed$stand)){
    if(testpitchesZone5_offspeed$stand[pitch] == "R" && is.na(testpitchesZone5_offspeed$stand[pitch]) == FALSE){
      testpitchesZone5_offspeed_R[x,] <- testpitchesZone5_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_R)
  #Zone 5 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone5_offspeed_R_R <- data.frame(testpitchesZone5_offspeed_R)
  testpitchesZone5_offspeed_R_R <- testpitchesZone5_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed_R$p_throws)){
    if(testpitchesZone5_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone5_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_offspeed_R_R[x,] <- testpitchesZone5_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_R_R)
  #Zone 5 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone5_offspeed_R_L <- data.frame(testpitchesZone5_offspeed_R)
  testpitchesZone5_offspeed_R_L <- testpitchesZone5_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed_R$p_throws)){
    if(testpitchesZone5_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone5_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_offspeed_R_L[x,] <- testpitchesZone5_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_R_L)
  #Zone 5 AND Offspeed AND L Batter
  testpitchesZone5_offspeed_L <- data.frame(testpitchesZone5_offspeed)
  testpitchesZone5_offspeed_L <- testpitchesZone5_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed$stand)){
    if(testpitchesZone5_offspeed$stand[pitch] == "L" && is.na(testpitchesZone5_offspeed$stand[pitch]) == FALSE){
      testpitchesZone5_offspeed_L[x,] <- testpitchesZone5_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_L)
  #Zone 5 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone5_offspeed_L_R <- data.frame(testpitchesZone5_offspeed_L)
  testpitchesZone5_offspeed_L_R <- testpitchesZone5_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed_L$p_throws)){
    if(testpitchesZone5_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone5_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_offspeed_L_R[x,] <- testpitchesZone5_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_L_R)
  #Zone 5 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone5_offspeed_L_L <- data.frame(testpitchesZone5_offspeed_L)
  testpitchesZone5_offspeed_L_L <- testpitchesZone5_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_offspeed_L$p_throws)){
    if(testpitchesZone5_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone5_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_offspeed_L_L[x,] <- testpitchesZone5_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_offspeed_L_L)
  
  #Zone 5 AND breaking
  testpitchesZone5_breaking <- data.frame(testpitchesZone5)
  testpitchesZone5_breaking <- testpitchesZone5_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5$pitch_type)){
    if(is.na(testpitchesZone5$pitch_type[pitch]) == FALSE){
      if(testpitchesZone5$pitch_type[pitch] == "KN" || testpitchesZone5$pitch_type[pitch] == "SL" || 
         testpitchesZone5$pitch_type[pitch] == "KC" || testpitchesZone5$pitch_type[pitch] == "CU"
         || testpitchesZone5$pitch_type[pitch] == "EP"|| testpitchesZone5$pitch_type[pitch] == "CS")
      {
        testpitchesZone5_breaking[x,] <- testpitchesZone5[pitch,]
        x = x+1
      }}}
  view(testpitchesZone5_breaking)
  
  #Zone 5 AND Breaking AND R Batter
  testpitchesZone5_breaking_R <- data.frame(testpitchesZone5_breaking)
  testpitchesZone5_breaking_R <- testpitchesZone5_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking$stand)){
    if(testpitchesZone5_breaking$stand[pitch] == "R" && is.na(testpitchesZone5_breaking$stand[pitch]) == FALSE){
      testpitchesZone5_breaking_R[x,] <- testpitchesZone5_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_R)
  #Zone 5 AND breaking AND L Batter AND R Pitcher
  testpitchesZone5_breaking_R_R <- data.frame(testpitchesZone5_breaking_R)
  testpitchesZone5_breaking_R_R <- testpitchesZone5_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking_R$p_throws)){
    if(testpitchesZone5_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone5_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_breaking_R_R[x,] <- testpitchesZone5_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_R_R)
  #Zone 5 AND breaking AND R Batter AND L Pitcher
  testpitchesZone5_breaking_R_L <- data.frame(testpitchesZone5_breaking_R)
  testpitchesZone5_breaking_R_L <- testpitchesZone5_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking_R$p_throws)){
    if(testpitchesZone5_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone5_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone5_breaking_R_L[x,] <- testpitchesZone5_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_R_L)
  #Zone 5 AND breaking AND L Batter
  testpitchesZone5_breaking_L <- data.frame(testpitchesZone5_breaking)
  testpitchesZone5_breaking_L <- testpitchesZone5_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking$stand)){
    if(testpitchesZone5_breaking$stand[pitch] == "L" && is.na(testpitchesZone5_breaking$stand[pitch]) == FALSE){
      testpitchesZone5_breaking_L[x,] <- testpitchesZone5_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_L)
  #Zone 5 AND breaking AND L Batter AND R Pitcher
  testpitchesZone5_breaking_L_R <- data.frame(testpitchesZone5_breaking_L)
  testpitchesZone5_breaking_L_R <- testpitchesZone5_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking_L$p_throws)){
    if(testpitchesZone5_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone5_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_breaking_L_R[x,] <- testpitchesZone5_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_L_R)
  #Zone 5 AND breaking AND L Batter AND L Pitcher
  testpitchesZone5_breaking_L_L <- data.frame(testpitchesZone5_breaking_L)
  testpitchesZone5_breaking_L_L <- testpitchesZone5_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone5_breaking_L$p_throws)){
    if(testpitchesZone5_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone5_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone5_breaking_L_L[x,] <- testpitchesZone5_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone5_breaking_L_L)
  
  #Zone 6 #####
  testpitchesZone6 <- data.frame(testpitches)
  testpitchesZone6 <- testpitchesZone6[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 6 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone6[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone6)
  #Zone 6 AND Fastball
  testpitchesZone6_fastball <- data.frame(testpitchesZone6)
  testpitchesZone6_fastball <- testpitchesZone6_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6$pitch_type)){
    if(is.na(testpitchesZone6$pitch_type[pitch]) == FALSE){
      if(testpitchesZone6$pitch_type[pitch] == "FF" || testpitchesZone6$pitch_type[pitch] == "FT" || 
         testpitchesZone6$pitch_type[pitch] == "SI" || testpitchesZone6$pitch_type[pitch] == "FC")
      {
        testpitchesZone6_fastball[x,] <- testpitchesZone6[pitch,]
        x = x+1
      }}}
  view(testpitchesZone6_fastball)
  #Zone 6 AND Fastball AND R Batter
  testpitchesZone6_fastball_R <- data.frame(testpitchesZone6_fastball)
  testpitchesZone6_fastball_R <- testpitchesZone6_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball$stand)){
    if(testpitchesZone6_fastball$stand[pitch] == "R" && is.na(testpitchesZone6_fastball$stand[pitch]) == FALSE){
      testpitchesZone6_fastball_R[x,] <- testpitchesZone6_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_R)
  #Zone 6 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone6_fastball_R_R <- data.frame(testpitchesZone6_fastball_R)
  testpitchesZone6_fastball_R_R <- testpitchesZone6_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball_R$p_throws)){
    if(testpitchesZone6_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone6_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_fastball_R_R[x,] <- testpitchesZone6_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_R_R)
  #Zone 6 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone6_fastball_R_L <- data.frame(testpitchesZone6_fastball_R)
  testpitchesZone6_fastball_R_L <- testpitchesZone6_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball_R$p_throws)){
    if(testpitchesZone6_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone6_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_fastball_R_L[x,] <- testpitchesZone6_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_R_L)
  #Zone 6 AND Fastball AND L Batter
  testpitchesZone6_fastball_L <- data.frame(testpitchesZone6_fastball)
  testpitchesZone6_fastball_L <- testpitchesZone6_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball$stand)){
    if(testpitchesZone6_fastball$stand[pitch] == "L" && is.na(testpitchesZone6_fastball$stand[pitch]) == FALSE){
      testpitchesZone6_fastball_L[x,] <- testpitchesZone6_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_L)
  #Zone 6 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone6_fastball_L_R <- data.frame(testpitchesZone6_fastball_L)
  testpitchesZone6_fastball_L_R <- testpitchesZone6_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball_L$p_throws)){
    if(testpitchesZone6_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone6_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_fastball_L_R[x,] <- testpitchesZone6_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_L_R)
  #Zone 6 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone6_fastball_L_L <- data.frame(testpitchesZone6_fastball_L)
  testpitchesZone6_fastball_L_L <- testpitchesZone6_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_fastball_L$p_throws)){
    if(testpitchesZone6_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone6_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_fastball_L_L[x,] <- testpitchesZone6_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_fastball_L_L)
  #Zone 6 AND Off-speed
  testpitchesZone6_offspeed <- data.frame(testpitchesZone6)
  testpitchesZone6_offspeed <- testpitchesZone6_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6$pitch_type)){
    if(is.na(testpitchesZone6$pitch_type[pitch]) == FALSE){
      if(testpitchesZone6$pitch_type[pitch] == "CH" || testpitchesZone6$pitch_type[pitch] == "FS" || 
         testpitchesZone6$pitch_type[pitch] == "FO" || testpitchesZone6$pitch_type[pitch] == "SC")
      {
        testpitchesZone6_offspeed[x,] <- testpitchesZone6[pitch,]
        x = x+1
      }}}
  view(testpitchesZone6_offspeed)
  
  #Zone 6 AND Offspeed AND R Batter
  testpitchesZone6_offspeed_R <- data.frame(testpitchesZone6_offspeed)
  testpitchesZone6_offspeed_R <- testpitchesZone6_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed$stand)){
    if(testpitchesZone6_offspeed$stand[pitch] == "R" && is.na(testpitchesZone6_offspeed$stand[pitch]) == FALSE){
      testpitchesZone6_offspeed_R[x,] <- testpitchesZone6_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_R)
  #Zone 6 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone6_offspeed_R_R <- data.frame(testpitchesZone6_offspeed_R)
  testpitchesZone6_offspeed_R_R <- testpitchesZone6_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed_R$p_throws)){
    if(testpitchesZone6_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone6_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_offspeed_R_R[x,] <- testpitchesZone6_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_R_R)
  #Zone 6 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone6_offspeed_R_L <- data.frame(testpitchesZone6_offspeed_R)
  testpitchesZone6_offspeed_R_L <- testpitchesZone6_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed_R$p_throws)){
    if(testpitchesZone6_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone6_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_offspeed_R_L[x,] <- testpitchesZone6_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_R_L)
  #Zone 6 AND Offspeed AND L Batter
  testpitchesZone6_offspeed_L <- data.frame(testpitchesZone6_offspeed)
  testpitchesZone6_offspeed_L <- testpitchesZone6_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed$stand)){
    if(testpitchesZone6_offspeed$stand[pitch] == "L" && is.na(testpitchesZone6_offspeed$stand[pitch]) == FALSE){
      testpitchesZone6_offspeed_L[x,] <- testpitchesZone6_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_L)
  #Zone 6 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone6_offspeed_L_R <- data.frame(testpitchesZone6_offspeed_L)
  testpitchesZone6_offspeed_L_R <- testpitchesZone6_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed_L$p_throws)){
    if(testpitchesZone6_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone6_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_offspeed_L_R[x,] <- testpitchesZone6_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_L_R)
  #Zone 6 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone6_offspeed_L_L <- data.frame(testpitchesZone6_offspeed_L)
  testpitchesZone6_offspeed_L_L <- testpitchesZone6_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_offspeed_L$p_throws)){
    if(testpitchesZone6_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone6_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_offspeed_L_L[x,] <- testpitchesZone6_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_offspeed_L_L)
  
  #Zone 6 AND breaking
  testpitchesZone6_breaking <- data.frame(testpitchesZone6)
  testpitchesZone6_breaking <- testpitchesZone6_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6$pitch_type)){
    if(is.na(testpitchesZone6$pitch_type[pitch]) == FALSE){
      if(testpitchesZone6$pitch_type[pitch] == "KN" || testpitchesZone6$pitch_type[pitch] == "SL" || 
         testpitchesZone6$pitch_type[pitch] == "KC" || testpitchesZone6$pitch_type[pitch] == "CU"
         || testpitchesZone6$pitch_type[pitch] == "EP"|| testpitchesZone6$pitch_type[pitch] == "CS")
      {
        testpitchesZone6_breaking[x,] <- testpitchesZone6[pitch,]
        x = x+1
      }}}
  view(testpitchesZone6_breaking)
  
  #Zone 6 AND Breaking AND R Batter
  testpitchesZone6_breaking_R <- data.frame(testpitchesZone6_breaking)
  testpitchesZone6_breaking_R <- testpitchesZone6_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking$stand)){
    if(testpitchesZone6_breaking$stand[pitch] == "R" && is.na(testpitchesZone6_breaking$stand[pitch]) == FALSE){
      testpitchesZone6_breaking_R[x,] <- testpitchesZone6_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_R)
  #Zone 6 AND breaking AND L Batter AND R Pitcher
  testpitchesZone6_breaking_R_R <- data.frame(testpitchesZone6_breaking_R)
  testpitchesZone6_breaking_R_R <- testpitchesZone6_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking_R$p_throws)){
    if(testpitchesZone6_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone6_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_breaking_R_R[x,] <- testpitchesZone6_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_R_R)
  #Zone 6 AND breaking AND R Batter AND L Pitcher
  testpitchesZone6_breaking_R_L <- data.frame(testpitchesZone6_breaking_R)
  testpitchesZone6_breaking_R_L <- testpitchesZone6_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking_R$p_throws)){
    if(testpitchesZone6_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone6_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone6_breaking_R_L[x,] <- testpitchesZone6_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_R_L)
  #Zone 6 AND breaking AND L Batter
  testpitchesZone6_breaking_L <- data.frame(testpitchesZone6_breaking)
  testpitchesZone6_breaking_L <- testpitchesZone6_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking$stand)){
    if(testpitchesZone6_breaking$stand[pitch] == "L" && is.na(testpitchesZone6_breaking$stand[pitch]) == FALSE){
      testpitchesZone6_breaking_L[x,] <- testpitchesZone6_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_L)
  #Zone 6 AND breaking AND L Batter AND R Pitcher
  testpitchesZone6_breaking_L_R <- data.frame(testpitchesZone6_breaking_L)
  testpitchesZone6_breaking_L_R <- testpitchesZone6_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking_L$p_throws)){
    if(testpitchesZone6_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone6_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_breaking_L_R[x,] <- testpitchesZone6_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_L_R)
  #Zone 6 AND breaking AND L Batter AND L Pitcher
  testpitchesZone6_breaking_L_L <- data.frame(testpitchesZone6_breaking_L)
  testpitchesZone6_breaking_L_L <- testpitchesZone6_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone6_breaking_L$p_throws)){
    if(testpitchesZone6_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone6_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone6_breaking_L_L[x,] <- testpitchesZone6_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone6_breaking_L_L)
  
  #Zone 7 #####
  testpitchesZone7 <- data.frame(testpitches)
  testpitchesZone7 <- testpitchesZone7[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 7 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone7[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone7)
  #Zone 7 AND Fastball
  testpitchesZone7_fastball <- data.frame(testpitchesZone7)
  testpitchesZone7_fastball <- testpitchesZone7_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7$pitch_type)){
    if(is.na(testpitchesZone7$pitch_type[pitch]) == FALSE){
      if(testpitchesZone7$pitch_type[pitch] == "FF" || testpitchesZone7$pitch_type[pitch] == "FT" || 
         testpitchesZone7$pitch_type[pitch] == "SI" || testpitchesZone7$pitch_type[pitch] == "FC")
      {
        testpitchesZone7_fastball[x,] <- testpitchesZone7[pitch,]
        x = x+1
      }}}
  view(testpitchesZone7_fastball)
  #Zone 7 AND Fastball AND R Batter
  testpitchesZone7_fastball_R <- data.frame(testpitchesZone7_fastball)
  testpitchesZone7_fastball_R <- testpitchesZone7_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball$stand)){
    if(testpitchesZone7_fastball$stand[pitch] == "R" && is.na(testpitchesZone7_fastball$stand[pitch]) == FALSE){
      testpitchesZone7_fastball_R[x,] <- testpitchesZone7_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_R)
  #Zone 7 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone7_fastball_R_R <- data.frame(testpitchesZone7_fastball_R)
  testpitchesZone7_fastball_R_R <- testpitchesZone7_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball_R$p_throws)){
    if(testpitchesZone7_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone7_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_fastball_R_R[x,] <- testpitchesZone7_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_R_R)
  #Zone 7 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone7_fastball_R_L <- data.frame(testpitchesZone7_fastball_R)
  testpitchesZone7_fastball_R_L <- testpitchesZone7_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball_R$p_throws)){
    if(testpitchesZone7_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone7_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_fastball_R_L[x,] <- testpitchesZone7_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_R_L)
  #Zone 7 AND Fastball AND L Batter
  testpitchesZone7_fastball_L <- data.frame(testpitchesZone7_fastball)
  testpitchesZone7_fastball_L <- testpitchesZone7_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball$stand)){
    if(testpitchesZone7_fastball$stand[pitch] == "L" && is.na(testpitchesZone7_fastball$stand[pitch]) == FALSE){
      testpitchesZone7_fastball_L[x,] <- testpitchesZone7_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_L)
  #Zone 7 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone7_fastball_L_R <- data.frame(testpitchesZone7_fastball_L)
  testpitchesZone7_fastball_L_R <- testpitchesZone7_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball_L$p_throws)){
    if(testpitchesZone7_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone7_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_fastball_L_R[x,] <- testpitchesZone7_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_L_R)
  #Zone 7 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone7_fastball_L_L <- data.frame(testpitchesZone7_fastball_L)
  testpitchesZone7_fastball_L_L <- testpitchesZone7_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_fastball_L$p_throws)){
    if(testpitchesZone7_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone7_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_fastball_L_L[x,] <- testpitchesZone7_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_fastball_L_L)
  #Zone 7 AND Off-speed
  testpitchesZone7_offspeed <- data.frame(testpitchesZone7)
  testpitchesZone7_offspeed <- testpitchesZone7_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7$pitch_type)){
    if(is.na(testpitchesZone7$pitch_type[pitch]) == FALSE){
      if(testpitchesZone7$pitch_type[pitch] == "CH" || testpitchesZone7$pitch_type[pitch] == "FS" || 
         testpitchesZone7$pitch_type[pitch] == "FO" || testpitchesZone7$pitch_type[pitch] == "SC")
      {
        testpitchesZone7_offspeed[x,] <- testpitchesZone7[pitch,]
        x = x+1
      }}}
  view(testpitchesZone7_offspeed)
  
  #Zone 7 AND Offspeed AND R Batter
  testpitchesZone7_offspeed_R <- data.frame(testpitchesZone7_offspeed)
  testpitchesZone7_offspeed_R <- testpitchesZone7_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed$stand)){
    if(testpitchesZone7_offspeed$stand[pitch] == "R" && is.na(testpitchesZone7_offspeed$stand[pitch]) == FALSE){
      testpitchesZone7_offspeed_R[x,] <- testpitchesZone7_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_R)
  #Zone 7 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone7_offspeed_R_R <- data.frame(testpitchesZone7_offspeed_R)
  testpitchesZone7_offspeed_R_R <- testpitchesZone7_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed_R$p_throws)){
    if(testpitchesZone7_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone7_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_offspeed_R_R[x,] <- testpitchesZone7_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_R_R)
  #Zone 7 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone7_offspeed_R_L <- data.frame(testpitchesZone7_offspeed_R)
  testpitchesZone7_offspeed_R_L <- testpitchesZone7_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed_R$p_throws)){
    if(testpitchesZone7_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone7_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_offspeed_R_L[x,] <- testpitchesZone7_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_R_L)
  #Zone 7 AND Offspeed AND L Batter
  testpitchesZone7_offspeed_L <- data.frame(testpitchesZone7_offspeed)
  testpitchesZone7_offspeed_L <- testpitchesZone7_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed$stand)){
    if(testpitchesZone7_offspeed$stand[pitch] == "L" && is.na(testpitchesZone7_offspeed$stand[pitch]) == FALSE){
      testpitchesZone7_offspeed_L[x,] <- testpitchesZone7_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_L)
  #Zone 7 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone7_offspeed_L_R <- data.frame(testpitchesZone7_offspeed_L)
  testpitchesZone7_offspeed_L_R <- testpitchesZone7_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed_L$p_throws)){
    if(testpitchesZone7_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone7_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_offspeed_L_R[x,] <- testpitchesZone7_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_L_R)
  #Zone 7 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone7_offspeed_L_L <- data.frame(testpitchesZone7_offspeed_L)
  testpitchesZone7_offspeed_L_L <- testpitchesZone7_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_offspeed_L$p_throws)){
    if(testpitchesZone7_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone7_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_offspeed_L_L[x,] <- testpitchesZone7_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_offspeed_L_L)
  
  #Zone 7 AND breaking
  testpitchesZone7_breaking <- data.frame(testpitchesZone7)
  testpitchesZone7_breaking <- testpitchesZone7_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7$pitch_type)){
    if(is.na(testpitchesZone7$pitch_type[pitch]) == FALSE){
      if(testpitchesZone7$pitch_type[pitch] == "KN" || testpitchesZone7$pitch_type[pitch] == "SL" || 
         testpitchesZone7$pitch_type[pitch] == "KC" || testpitchesZone7$pitch_type[pitch] == "CU"
         || testpitchesZone7$pitch_type[pitch] == "EP"|| testpitchesZone7$pitch_type[pitch] == "CS")
      {
        testpitchesZone7_breaking[x,] <- testpitchesZone7[pitch,]
        x = x+1
      }}}
  view(testpitchesZone7_breaking)
  
  #Zone 7 AND Breaking AND R Batter
  testpitchesZone7_breaking_R <- data.frame(testpitchesZone7_breaking)
  testpitchesZone7_breaking_R <- testpitchesZone7_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking$stand)){
    if(testpitchesZone7_breaking$stand[pitch] == "R" && is.na(testpitchesZone7_breaking$stand[pitch]) == FALSE){
      testpitchesZone7_breaking_R[x,] <- testpitchesZone7_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_R)
  #Zone 7 AND breaking AND L Batter AND R Pitcher
  testpitchesZone7_breaking_R_R <- data.frame(testpitchesZone7_breaking_R)
  testpitchesZone7_breaking_R_R <- testpitchesZone7_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking_R$p_throws)){
    if(testpitchesZone7_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone7_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_breaking_R_R[x,] <- testpitchesZone7_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_R_R)
  #Zone 7 AND breaking AND R Batter AND L Pitcher
  testpitchesZone7_breaking_R_L <- data.frame(testpitchesZone7_breaking_R)
  testpitchesZone7_breaking_R_L <- testpitchesZone7_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking_R$p_throws)){
    if(testpitchesZone7_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone7_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone7_breaking_R_L[x,] <- testpitchesZone7_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_R_L)
  #Zone 7 AND breaking AND L Batter
  testpitchesZone7_breaking_L <- data.frame(testpitchesZone7_breaking)
  testpitchesZone7_breaking_L <- testpitchesZone7_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking$stand)){
    if(testpitchesZone7_breaking$stand[pitch] == "L" && is.na(testpitchesZone7_breaking$stand[pitch]) == FALSE){
      testpitchesZone7_breaking_L[x,] <- testpitchesZone7_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_L)
  #Zone 7 AND breaking AND L Batter AND R Pitcher
  testpitchesZone7_breaking_L_R <- data.frame(testpitchesZone7_breaking_L)
  testpitchesZone7_breaking_L_R <- testpitchesZone7_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking_L$p_throws)){
    if(testpitchesZone7_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone7_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_breaking_L_R[x,] <- testpitchesZone7_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_L_R)
  #Zone 7 AND breaking AND L Batter AND L Pitcher
  testpitchesZone7_breaking_L_L <- data.frame(testpitchesZone7_breaking_L)
  testpitchesZone7_breaking_L_L <- testpitchesZone7_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone7_breaking_L$p_throws)){
    if(testpitchesZone7_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone7_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone7_breaking_L_L[x,] <- testpitchesZone7_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone7_breaking_L_L)
  
  #Zone 8 #####
  testpitchesZone8 <- data.frame(testpitches)
  testpitchesZone8 <- testpitchesZone8[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 8 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone8[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone8)
  #Zone 8 AND Fastball
  testpitchesZone8_fastball <- data.frame(testpitchesZone8)
  testpitchesZone8_fastball <- testpitchesZone8_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8$pitch_type)){
    if(is.na(testpitchesZone8$pitch_type[pitch]) == FALSE){
      if(testpitchesZone8$pitch_type[pitch] == "FF" || testpitchesZone8$pitch_type[pitch] == "FT" || 
         testpitchesZone8$pitch_type[pitch] == "SI" || testpitchesZone8$pitch_type[pitch] == "FC")
      {
        testpitchesZone8_fastball[x,] <- testpitchesZone8[pitch,]
        x = x+1
      }}}
  view(testpitchesZone8_fastball)
  #Zone 8 AND Fastball AND R Batter
  testpitchesZone8_fastball_R <- data.frame(testpitchesZone8_fastball)
  testpitchesZone8_fastball_R <- testpitchesZone8_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball$stand)){
    if(testpitchesZone8_fastball$stand[pitch] == "R" && is.na(testpitchesZone8_fastball$stand[pitch]) == FALSE){
      testpitchesZone8_fastball_R[x,] <- testpitchesZone8_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_R)
  #Zone 8 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone8_fastball_R_R <- data.frame(testpitchesZone8_fastball_R)
  testpitchesZone8_fastball_R_R <- testpitchesZone8_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball_R$p_throws)){
    if(testpitchesZone8_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone8_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_fastball_R_R[x,] <- testpitchesZone8_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_R_R)
  #Zone 8 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone8_fastball_R_L <- data.frame(testpitchesZone8_fastball_R)
  testpitchesZone8_fastball_R_L <- testpitchesZone8_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball_R$p_throws)){
    if(testpitchesZone8_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone8_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_fastball_R_L[x,] <- testpitchesZone8_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_R_L)
  #Zone 8 AND Fastball AND L Batter
  testpitchesZone8_fastball_L <- data.frame(testpitchesZone8_fastball)
  testpitchesZone8_fastball_L <- testpitchesZone8_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball$stand)){
    if(testpitchesZone8_fastball$stand[pitch] == "L" && is.na(testpitchesZone8_fastball$stand[pitch]) == FALSE){
      testpitchesZone8_fastball_L[x,] <- testpitchesZone8_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_L)
  #Zone 8 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone8_fastball_L_R <- data.frame(testpitchesZone8_fastball_L)
  testpitchesZone8_fastball_L_R <- testpitchesZone8_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball_L$p_throws)){
    if(testpitchesZone8_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone8_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_fastball_L_R[x,] <- testpitchesZone8_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_L_R)
  #Zone 8 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone8_fastball_L_L <- data.frame(testpitchesZone8_fastball_L)
  testpitchesZone8_fastball_L_L <- testpitchesZone8_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_fastball_L$p_throws)){
    if(testpitchesZone8_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone8_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_fastball_L_L[x,] <- testpitchesZone8_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_fastball_L_L)
  #Zone 8 AND Off-speed
  testpitchesZone8_offspeed <- data.frame(testpitchesZone8)
  testpitchesZone8_offspeed <- testpitchesZone8_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8$pitch_type)){
    if(is.na(testpitchesZone8$pitch_type[pitch]) == FALSE){
      if(testpitchesZone8$pitch_type[pitch] == "CH" || testpitchesZone8$pitch_type[pitch] == "FS" || 
         testpitchesZone8$pitch_type[pitch] == "FO" || testpitchesZone8$pitch_type[pitch] == "SC")
      {
        testpitchesZone8_offspeed[x,] <- testpitchesZone8[pitch,]
        x = x+1
      }}}
  view(testpitchesZone8_offspeed)
  
  #Zone 8 AND Offspeed AND R Batter
  testpitchesZone8_offspeed_R <- data.frame(testpitchesZone8_offspeed)
  testpitchesZone8_offspeed_R <- testpitchesZone8_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed$stand)){
    if(testpitchesZone8_offspeed$stand[pitch] == "R" && is.na(testpitchesZone8_offspeed$stand[pitch]) == FALSE){
      testpitchesZone8_offspeed_R[x,] <- testpitchesZone8_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_R)
  #Zone 8 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone8_offspeed_R_R <- data.frame(testpitchesZone8_offspeed_R)
  testpitchesZone8_offspeed_R_R <- testpitchesZone8_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed_R$p_throws)){
    if(testpitchesZone8_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone8_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_offspeed_R_R[x,] <- testpitchesZone8_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_R_R)
  #Zone 8 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone8_offspeed_R_L <- data.frame(testpitchesZone8_offspeed_R)
  testpitchesZone8_offspeed_R_L <- testpitchesZone8_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed_R$p_throws)){
    if(testpitchesZone8_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone8_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_offspeed_R_L[x,] <- testpitchesZone8_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_R_L)
  #Zone 8 AND Offspeed AND L Batter
  testpitchesZone8_offspeed_L <- data.frame(testpitchesZone8_offspeed)
  testpitchesZone8_offspeed_L <- testpitchesZone8_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed$stand)){
    if(testpitchesZone8_offspeed$stand[pitch] == "L" && is.na(testpitchesZone8_offspeed$stand[pitch]) == FALSE){
      testpitchesZone8_offspeed_L[x,] <- testpitchesZone8_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_L)
  #Zone 8 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone8_offspeed_L_R <- data.frame(testpitchesZone8_offspeed_L)
  testpitchesZone8_offspeed_L_R <- testpitchesZone8_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed_L$p_throws)){
    if(testpitchesZone8_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone8_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_offspeed_L_R[x,] <- testpitchesZone8_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_L_R)
  #Zone 8 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone8_offspeed_L_L <- data.frame(testpitchesZone8_offspeed_L)
  testpitchesZone8_offspeed_L_L <- testpitchesZone8_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_offspeed_L$p_throws)){
    if(testpitchesZone8_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone8_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_offspeed_L_L[x,] <- testpitchesZone8_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_offspeed_L_L)
  
  #Zone 8 AND breaking
  testpitchesZone8_breaking <- data.frame(testpitchesZone8)
  testpitchesZone8_breaking <- testpitchesZone8_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8$pitch_type)){
    if(is.na(testpitchesZone8$pitch_type[pitch]) == FALSE){
      if(testpitchesZone8$pitch_type[pitch] == "KN" || testpitchesZone8$pitch_type[pitch] == "SL" || 
         testpitchesZone8$pitch_type[pitch] == "KC" || testpitchesZone8$pitch_type[pitch] == "CU"
         || testpitchesZone8$pitch_type[pitch] == "EP"|| testpitchesZone8$pitch_type[pitch] == "CS")
      {
        testpitchesZone8_breaking[x,] <- testpitchesZone8[pitch,]
        x = x+1
      }}}
  view(testpitchesZone8_breaking)
  
  #Zone 8 AND Breaking AND R Batter
  testpitchesZone8_breaking_R <- data.frame(testpitchesZone8_breaking)
  testpitchesZone8_breaking_R <- testpitchesZone8_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking$stand)){
    if(testpitchesZone8_breaking$stand[pitch] == "R" && is.na(testpitchesZone8_breaking$stand[pitch]) == FALSE){
      testpitchesZone8_breaking_R[x,] <- testpitchesZone8_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_R)
  #Zone 8 AND breaking AND L Batter AND R Pitcher
  testpitchesZone8_breaking_R_R <- data.frame(testpitchesZone8_breaking_R)
  testpitchesZone8_breaking_R_R <- testpitchesZone8_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking_R$p_throws)){
    if(testpitchesZone8_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone8_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_breaking_R_R[x,] <- testpitchesZone8_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_R_R)
  #Zone 8 AND breaking AND R Batter AND L Pitcher
  testpitchesZone8_breaking_R_L <- data.frame(testpitchesZone8_breaking_R)
  testpitchesZone8_breaking_R_L <- testpitchesZone8_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking_R$p_throws)){
    if(testpitchesZone8_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone8_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone8_breaking_R_L[x,] <- testpitchesZone8_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_R_L)
  #Zone 8 AND breaking AND L Batter
  testpitchesZone8_breaking_L <- data.frame(testpitchesZone8_breaking)
  testpitchesZone8_breaking_L <- testpitchesZone8_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking$stand)){
    if(testpitchesZone8_breaking$stand[pitch] == "L" && is.na(testpitchesZone8_breaking$stand[pitch]) == FALSE){
      testpitchesZone8_breaking_L[x,] <- testpitchesZone8_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_L)
  #Zone 8 AND breaking AND L Batter AND R Pitcher
  testpitchesZone8_breaking_L_R <- data.frame(testpitchesZone8_breaking_L)
  testpitchesZone8_breaking_L_R <- testpitchesZone8_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking_L$p_throws)){
    if(testpitchesZone8_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone8_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_breaking_L_R[x,] <- testpitchesZone8_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_L_R)
  #Zone 8 AND breaking AND L Batter AND L Pitcher
  testpitchesZone8_breaking_L_L <- data.frame(testpitchesZone8_breaking_L)
  testpitchesZone8_breaking_L_L <- testpitchesZone8_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone8_breaking_L$p_throws)){
    if(testpitchesZone8_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone8_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone8_breaking_L_L[x,] <- testpitchesZone8_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone8_breaking_L_L)
  
  #Zone 9 #####
  testpitchesZone9 <- data.frame(testpitches)
  testpitchesZone9 <- testpitchesZone9[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 9 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone9[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone9)
  #Zone 9 AND Fastball
  testpitchesZone9_fastball <- data.frame(testpitchesZone9)
  testpitchesZone9_fastball <- testpitchesZone9_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9$pitch_type)){
    if(is.na(testpitchesZone9$pitch_type[pitch]) == FALSE){
      if(testpitchesZone9$pitch_type[pitch] == "FF" || testpitchesZone9$pitch_type[pitch] == "FT" || 
         testpitchesZone9$pitch_type[pitch] == "SI" || testpitchesZone9$pitch_type[pitch] == "FC")
      {
        testpitchesZone9_fastball[x,] <- testpitchesZone9[pitch,]
        x = x+1
      }}}
  view(testpitchesZone9_fastball)
  #Zone 9 AND Fastball AND R Batter
  testpitchesZone9_fastball_R <- data.frame(testpitchesZone9_fastball)
  testpitchesZone9_fastball_R <- testpitchesZone9_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball$stand)){
    if(testpitchesZone9_fastball$stand[pitch] == "R" && is.na(testpitchesZone9_fastball$stand[pitch]) == FALSE){
      testpitchesZone9_fastball_R[x,] <- testpitchesZone9_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_R)
  #Zone 9 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone9_fastball_R_R <- data.frame(testpitchesZone9_fastball_R)
  testpitchesZone9_fastball_R_R <- testpitchesZone9_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball_R$p_throws)){
    if(testpitchesZone9_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone9_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_fastball_R_R[x,] <- testpitchesZone9_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_R_R)
  #Zone 9 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone9_fastball_R_L <- data.frame(testpitchesZone9_fastball_R)
  testpitchesZone9_fastball_R_L <- testpitchesZone9_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball_R$p_throws)){
    if(testpitchesZone9_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone9_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_fastball_R_L[x,] <- testpitchesZone9_fastball_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_R_L)
  #Zone 9 AND Fastball AND L Batter
  testpitchesZone9_fastball_L <- data.frame(testpitchesZone9_fastball)
  testpitchesZone9_fastball_L <- testpitchesZone9_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball$stand)){
    if(testpitchesZone9_fastball$stand[pitch] == "L" && is.na(testpitchesZone9_fastball$stand[pitch]) == FALSE){
      testpitchesZone9_fastball_L[x,] <- testpitchesZone9_fastball[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_L)
  #Zone 9 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone9_fastball_L_R <- data.frame(testpitchesZone9_fastball_L)
  testpitchesZone9_fastball_L_R <- testpitchesZone9_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball_L$p_throws)){
    if(testpitchesZone9_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone9_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_fastball_L_R[x,] <- testpitchesZone9_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_L_R)
  #Zone 9 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone9_fastball_L_L <- data.frame(testpitchesZone9_fastball_L)
  testpitchesZone9_fastball_L_L <- testpitchesZone9_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_fastball_L$p_throws)){
    if(testpitchesZone9_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone9_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_fastball_L_L[x,] <- testpitchesZone9_fastball_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_fastball_L_L)
  #Zone 9 AND Off-speed
  testpitchesZone9_offspeed <- data.frame(testpitchesZone9)
  testpitchesZone9_offspeed <- testpitchesZone9_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9$pitch_type)){
    if(is.na(testpitchesZone9$pitch_type[pitch]) == FALSE){
      if(testpitchesZone9$pitch_type[pitch] == "CH" || testpitchesZone9$pitch_type[pitch] == "FS" || 
         testpitchesZone9$pitch_type[pitch] == "FO" || testpitchesZone9$pitch_type[pitch] == "SC")
      {
        testpitchesZone9_offspeed[x,] <- testpitchesZone9[pitch,]
        x = x+1
      }}}
  view(testpitchesZone9_offspeed)
  
  #Zone 9 AND Offspeed AND R Batter
  testpitchesZone9_offspeed_R <- data.frame(testpitchesZone9_offspeed)
  testpitchesZone9_offspeed_R <- testpitchesZone9_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed$stand)){
    if(testpitchesZone9_offspeed$stand[pitch] == "R" && is.na(testpitchesZone9_offspeed$stand[pitch]) == FALSE){
      testpitchesZone9_offspeed_R[x,] <- testpitchesZone9_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_R)
  #Zone 9 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone9_offspeed_R_R <- data.frame(testpitchesZone9_offspeed_R)
  testpitchesZone9_offspeed_R_R <- testpitchesZone9_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed_R$p_throws)){
    if(testpitchesZone9_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone9_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_offspeed_R_R[x,] <- testpitchesZone9_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_R_R)
  #Zone 9 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone9_offspeed_R_L <- data.frame(testpitchesZone9_offspeed_R)
  testpitchesZone9_offspeed_R_L <- testpitchesZone9_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed_R$p_throws)){
    if(testpitchesZone9_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone9_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_offspeed_R_L[x,] <- testpitchesZone9_offspeed_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_R_L)
  #Zone 9 AND Offspeed AND L Batter
  testpitchesZone9_offspeed_L <- data.frame(testpitchesZone9_offspeed)
  testpitchesZone9_offspeed_L <- testpitchesZone9_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed$stand)){
    if(testpitchesZone9_offspeed$stand[pitch] == "L" && is.na(testpitchesZone9_offspeed$stand[pitch]) == FALSE){
      testpitchesZone9_offspeed_L[x,] <- testpitchesZone9_offspeed[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_L)
  #Zone 9 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone9_offspeed_L_R <- data.frame(testpitchesZone9_offspeed_L)
  testpitchesZone9_offspeed_L_R <- testpitchesZone9_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed_L$p_throws)){
    if(testpitchesZone9_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone9_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_offspeed_L_R[x,] <- testpitchesZone9_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_L_R)
  #Zone 9 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone9_offspeed_L_L <- data.frame(testpitchesZone9_offspeed_L)
  testpitchesZone9_offspeed_L_L <- testpitchesZone9_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_offspeed_L$p_throws)){
    if(testpitchesZone9_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone9_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_offspeed_L_L[x,] <- testpitchesZone9_offspeed_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_offspeed_L_L)
  
  #Zone 9 AND breaking
  testpitchesZone9_breaking <- data.frame(testpitchesZone9)
  testpitchesZone9_breaking <- testpitchesZone9_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9$pitch_type)){
    if(is.na(testpitchesZone9$pitch_type[pitch]) == FALSE){
      if(testpitchesZone9$pitch_type[pitch] == "KN" || testpitchesZone9$pitch_type[pitch] == "SL" || 
         testpitchesZone9$pitch_type[pitch] == "KC" || testpitchesZone9$pitch_type[pitch] == "CU"
         || testpitchesZone9$pitch_type[pitch] == "EP"|| testpitchesZone9$pitch_type[pitch] == "CS")
      {
        testpitchesZone9_breaking[x,] <- testpitchesZone9[pitch,]
        x = x+1
      }}}
  view(testpitchesZone9_breaking)
  
  #Zone 9 AND Breaking AND R Batter
  testpitchesZone9_breaking_R <- data.frame(testpitchesZone9_breaking)
  testpitchesZone9_breaking_R <- testpitchesZone9_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking$stand)){
    if(testpitchesZone9_breaking$stand[pitch] == "R" && is.na(testpitchesZone9_breaking$stand[pitch]) == FALSE){
      testpitchesZone9_breaking_R[x,] <- testpitchesZone9_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_R)
  #Zone 9 AND breaking AND L Batter AND R Pitcher
  testpitchesZone9_breaking_R_R <- data.frame(testpitchesZone9_breaking_R)
  testpitchesZone9_breaking_R_R <- testpitchesZone9_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking_R$p_throws)){
    if(testpitchesZone9_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone9_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_breaking_R_R[x,] <- testpitchesZone9_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_R_R)
  #Zone 9 AND breaking AND R Batter AND L Pitcher
  testpitchesZone9_breaking_R_L <- data.frame(testpitchesZone9_breaking_R)
  testpitchesZone9_breaking_R_L <- testpitchesZone9_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking_R$p_throws)){
    if(testpitchesZone9_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone9_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone9_breaking_R_L[x,] <- testpitchesZone9_breaking_R[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_R_L)
  #Zone 9 AND breaking AND L Batter
  testpitchesZone9_breaking_L <- data.frame(testpitchesZone9_breaking)
  testpitchesZone9_breaking_L <- testpitchesZone9_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking$stand)){
    if(testpitchesZone9_breaking$stand[pitch] == "L" && is.na(testpitchesZone9_breaking$stand[pitch]) == FALSE){
      testpitchesZone9_breaking_L[x,] <- testpitchesZone9_breaking[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_L)
  #Zone 9 AND breaking AND L Batter AND R Pitcher
  testpitchesZone9_breaking_L_R <- data.frame(testpitchesZone9_breaking_L)
  testpitchesZone9_breaking_L_R <- testpitchesZone9_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking_L$p_throws)){
    if(testpitchesZone9_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone9_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_breaking_L_R[x,] <- testpitchesZone9_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_L_R)
  #Zone 9 AND breaking AND L Batter AND L Pitcher
  testpitchesZone9_breaking_L_L <- data.frame(testpitchesZone9_breaking_L)
  testpitchesZone9_breaking_L_L <- testpitchesZone9_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone9_breaking_L$p_throws)){
    if(testpitchesZone9_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone9_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone9_breaking_L_L[x,] <- testpitchesZone9_breaking_L[pitch,]
      x = x+1
    }}
  view(testpitchesZone9_breaking_L_L)

  #Zone 11 ######
  testpitchesZone11 <- data.frame(testpitches)
  testpitchesZone11 <- testpitchesZone11[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 11 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone11[x,] <- testpitches[pitch,]
      x = x+1
    }}
  view(testpitchesZone11)
  #Zone 11 AND Fastball
  testpitchesZone11_fastball <- data.frame(testpitchesZone11)
  testpitchesZone11_fastball <- testpitchesZone11_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11$pitch_type)){
    if(is.na(testpitchesZone11$pitch_type[pitch]) == FALSE){
      if(testpitchesZone11$pitch_type[pitch] == "FF" || testpitchesZone11$pitch_type[pitch] == "FT" || 
         testpitchesZone11$pitch_type[pitch] == "SI" || testpitchesZone11$pitch_type[pitch] == "FC")
      {
        testpitchesZone11_fastball[x,] <- testpitchesZone11[pitch,]
        x = x+1
      }}}
  #Zone 11 AND Fastball AND R Batter
  testpitchesZone11_fastball_R <- data.frame(testpitchesZone11_fastball)
  testpitchesZone11_fastball_R <- testpitchesZone11_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball$stand)){
    if(testpitchesZone11_fastball$stand[pitch] == "R" && is.na(testpitchesZone11_fastball$stand[pitch]) == FALSE){
      testpitchesZone11_fastball_R[x,] <- testpitchesZone11_fastball[pitch,]
      x = x+1
    }}
  #Zone 11 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone11_fastball_R_R <- data.frame(testpitchesZone11_fastball_R)
  testpitchesZone11_fastball_R_R <- testpitchesZone11_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball_R$p_throws)){
    if(testpitchesZone11_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone11_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_fastball_R_R[x,] <- testpitchesZone11_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone11_fastball_R_L <- data.frame(testpitchesZone11_fastball_R)
  testpitchesZone11_fastball_R_L <- testpitchesZone11_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball_R$p_throws)){
    if(testpitchesZone11_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone11_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_fastball_R_L[x,] <- testpitchesZone11_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND Fastball AND L Batter
  testpitchesZone11_fastball_L <- data.frame(testpitchesZone11_fastball)
  testpitchesZone11_fastball_L <- testpitchesZone11_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball$stand)){
    if(testpitchesZone11_fastball$stand[pitch] == "L" && is.na(testpitchesZone11_fastball$stand[pitch]) == FALSE){
      testpitchesZone11_fastball_L[x,] <- testpitchesZone11_fastball[pitch,]
      x = x+1
    }}
  #Zone 11 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone11_fastball_L_R <- data.frame(testpitchesZone11_fastball_L)
  testpitchesZone11_fastball_L_R <- testpitchesZone11_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball_L$p_throws)){
    if(testpitchesZone11_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone11_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_fastball_L_R[x,] <- testpitchesZone11_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 11 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone11_fastball_L_L <- data.frame(testpitchesZone11_fastball_L)
  testpitchesZone11_fastball_L_L <- testpitchesZone11_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_fastball_L$p_throws)){
    if(testpitchesZone11_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone11_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_fastball_L_L[x,] <- testpitchesZone11_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 11 AND Off-speed
  testpitchesZone11_offspeed <- data.frame(testpitchesZone11)
  testpitchesZone11_offspeed <- testpitchesZone11_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11$pitch_type)){
    if(is.na(testpitchesZone11$pitch_type[pitch]) == FALSE){
      if(testpitchesZone11$pitch_type[pitch] == "CH" || testpitchesZone11$pitch_type[pitch] == "FS" || 
         testpitchesZone11$pitch_type[pitch] == "FO" || testpitchesZone11$pitch_type[pitch] == "SC")
      {
        testpitchesZone11_offspeed[x,] <- testpitchesZone11[pitch,]
        x = x+1
      }}}

  #Zone 11 AND Offspeed AND R Batter
  testpitchesZone11_offspeed_R <- data.frame(testpitchesZone11_offspeed)
  testpitchesZone11_offspeed_R <- testpitchesZone11_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed$stand)){
    if(testpitchesZone11_offspeed$stand[pitch] == "R" && is.na(testpitchesZone11_offspeed$stand[pitch]) == FALSE){
      testpitchesZone11_offspeed_R[x,] <- testpitchesZone11_offspeed[pitch,]
      x = x+1
    }}
  #Zone 11 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone11_offspeed_R_R <- data.frame(testpitchesZone11_offspeed_R)
  testpitchesZone11_offspeed_R_R <- testpitchesZone11_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed_R$p_throws)){
    if(testpitchesZone11_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone11_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_offspeed_R_R[x,] <- testpitchesZone11_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone11_offspeed_R_L <- data.frame(testpitchesZone11_offspeed_R)
  testpitchesZone11_offspeed_R_L <- testpitchesZone11_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed_R$p_throws)){
    if(testpitchesZone11_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone11_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_offspeed_R_L[x,] <- testpitchesZone11_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND Offspeed AND L Batter
  testpitchesZone11_offspeed_L <- data.frame(testpitchesZone11_offspeed)
  testpitchesZone11_offspeed_L <- testpitchesZone11_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed$stand)){
    if(testpitchesZone11_offspeed$stand[pitch] == "L" && is.na(testpitchesZone11_offspeed$stand[pitch]) == FALSE){
      testpitchesZone11_offspeed_L[x,] <- testpitchesZone11_offspeed[pitch,]
      x = x+1
    }}
  #Zone 11 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone11_offspeed_L_R <- data.frame(testpitchesZone11_offspeed_L)
  testpitchesZone11_offspeed_L_R <- testpitchesZone11_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed_L$p_throws)){
    if(testpitchesZone11_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone11_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_offspeed_L_R[x,] <- testpitchesZone11_offspeed_L[pitch,]
      x = x+1
    }}
  #Zone 11 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone11_offspeed_L_L <- data.frame(testpitchesZone11_offspeed_L)
  testpitchesZone11_offspeed_L_L <- testpitchesZone11_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_offspeed_L$p_throws)){
    if(testpitchesZone11_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone11_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_offspeed_L_L[x,] <- testpitchesZone11_offspeed_L[pitch,]
      x = x+1
    }}

  #Zone 11 AND breaking
  testpitchesZone11_breaking <- data.frame(testpitchesZone11)
  testpitchesZone11_breaking <- testpitchesZone11_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11$pitch_type)){
    if(is.na(testpitchesZone11$pitch_type[pitch]) == FALSE){
      if(testpitchesZone11$pitch_type[pitch] == "KN" || testpitchesZone11$pitch_type[pitch] == "SL" || 
         testpitchesZone11$pitch_type[pitch] == "KC" || testpitchesZone11$pitch_type[pitch] == "CU"
         || testpitchesZone11$pitch_type[pitch] == "EP"|| testpitchesZone11$pitch_type[pitch] == "CS")
      {
        testpitchesZone11_breaking[x,] <- testpitchesZone11[pitch,]
        x = x+1
      }}}

  #Zone 11 AND Breaking AND R Batter
  testpitchesZone11_breaking_R <- data.frame(testpitchesZone11_breaking)
  testpitchesZone11_breaking_R <- testpitchesZone11_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking$stand)){
    if(testpitchesZone11_breaking$stand[pitch] == "R" && is.na(testpitchesZone11_breaking$stand[pitch]) == FALSE){
      testpitchesZone11_breaking_R[x,] <- testpitchesZone11_breaking[pitch,]
      x = x+1
    }}
  #Zone 11 AND breaking AND L Batter AND R Pitcher
  testpitchesZone11_breaking_R_R <- data.frame(testpitchesZone11_breaking_R)
  testpitchesZone11_breaking_R_R <- testpitchesZone11_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking_R$p_throws)){
    if(testpitchesZone11_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone11_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_breaking_R_R[x,] <- testpitchesZone11_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND breaking AND R Batter AND L Pitcher
  testpitchesZone11_breaking_R_L <- data.frame(testpitchesZone11_breaking_R)
  testpitchesZone11_breaking_R_L <- testpitchesZone11_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking_R$p_throws)){
    if(testpitchesZone11_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone11_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone11_breaking_R_L[x,] <- testpitchesZone11_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 11 AND breaking AND L Batter
  testpitchesZone11_breaking_L <- data.frame(testpitchesZone11_breaking)
  testpitchesZone11_breaking_L <- testpitchesZone11_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking$stand)){
    if(testpitchesZone11_breaking$stand[pitch] == "L" && is.na(testpitchesZone11_breaking$stand[pitch]) == FALSE){
      testpitchesZone11_breaking_L[x,] <- testpitchesZone11_breaking[pitch,]
      x = x+1
    }}
  #Zone 11 AND breaking AND L Batter AND R Pitcher
  testpitchesZone11_breaking_L_R <- data.frame(testpitchesZone11_breaking_L)
  testpitchesZone11_breaking_L_R <- testpitchesZone11_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking_L$p_throws)){
    if(testpitchesZone11_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone11_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_breaking_L_R[x,] <- testpitchesZone11_breaking_L[pitch,]
      x = x+1
    }}
  #Zone 11 AND breaking AND L Batter AND L Pitcher
  testpitchesZone11_breaking_L_L <- data.frame(testpitchesZone11_breaking_L)
  testpitchesZone11_breaking_L_L <- testpitchesZone11_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone11_breaking_L$p_throws)){
    if(testpitchesZone11_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone11_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone11_breaking_L_L[x,] <- testpitchesZone11_breaking_L[pitch,]
      x = x+1
    }}

  #Zone 12 #####
  testpitchesZone12 <- data.frame(testpitches)
  testpitchesZone12 <- testpitchesZone12[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 12 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone12[x,] <- testpitches[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball
  testpitchesZone12_fastball <- data.frame(testpitchesZone12)
  testpitchesZone12_fastball <- testpitchesZone12_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12$pitch_type)){
    if(is.na(testpitchesZone12$pitch_type[pitch]) == FALSE){
      if(testpitchesZone12$pitch_type[pitch] == "FF" || testpitchesZone12$pitch_type[pitch] == "FT" || 
         testpitchesZone12$pitch_type[pitch] == "SI" || testpitchesZone12$pitch_type[pitch] == "FC")
      {
        testpitchesZone12_fastball[x,] <- testpitchesZone12[pitch,]
        x = x+1
      }}}
  #Zone 12 AND Fastball AND R Batter
  testpitchesZone12_fastball_R <- data.frame(testpitchesZone12_fastball)
  testpitchesZone12_fastball_R <- testpitchesZone12_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball$stand)){
    if(testpitchesZone12_fastball$stand[pitch] == "R" && is.na(testpitchesZone12_fastball$stand[pitch]) == FALSE){
      testpitchesZone12_fastball_R[x,] <- testpitchesZone12_fastball[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone12_fastball_R_R <- data.frame(testpitchesZone12_fastball_R)
  testpitchesZone12_fastball_R_R <- testpitchesZone12_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball_R$p_throws)){
    if(testpitchesZone12_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone12_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_fastball_R_R[x,] <- testpitchesZone12_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone12_fastball_R_L <- data.frame(testpitchesZone12_fastball_R)
  testpitchesZone12_fastball_R_L <- testpitchesZone12_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball_R$p_throws)){
    if(testpitchesZone12_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone12_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_fastball_R_L[x,] <- testpitchesZone12_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball AND L Batter
  testpitchesZone12_fastball_L <- data.frame(testpitchesZone12_fastball)
  testpitchesZone12_fastball_L <- testpitchesZone12_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball$stand)){
    if(testpitchesZone12_fastball$stand[pitch] == "L" && is.na(testpitchesZone12_fastball$stand[pitch]) == FALSE){
      testpitchesZone12_fastball_L[x,] <- testpitchesZone12_fastball[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone12_fastball_L_R <- data.frame(testpitchesZone12_fastball_L)
  testpitchesZone12_fastball_L_R <- testpitchesZone12_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball_L$p_throws)){
    if(testpitchesZone12_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone12_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_fastball_L_R[x,] <- testpitchesZone12_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 12 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone12_fastball_L_L <- data.frame(testpitchesZone12_fastball_L)
  testpitchesZone12_fastball_L_L <- testpitchesZone12_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_fastball_L$p_throws)){
    if(testpitchesZone12_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone12_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_fastball_L_L[x,] <- testpitchesZone12_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 12 AND Off-speed
  testpitchesZone12_offspeed <- data.frame(testpitchesZone12)
  testpitchesZone12_offspeed <- testpitchesZone12_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12$pitch_type)){
    if(is.na(testpitchesZone12$pitch_type[pitch]) == FALSE){
      if(testpitchesZone12$pitch_type[pitch] == "CH" || testpitchesZone12$pitch_type[pitch] == "FS" || 
         testpitchesZone12$pitch_type[pitch] == "FO" || testpitchesZone12$pitch_type[pitch] == "SC")
      {
        testpitchesZone12_offspeed[x,] <- testpitchesZone12[pitch,]
        x = x+1
      }}}

  #Zone 12 AND Offspeed AND R Batter
  testpitchesZone12_offspeed_R <- data.frame(testpitchesZone12_offspeed)
  testpitchesZone12_offspeed_R <- testpitchesZone12_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed$stand)){
    if(testpitchesZone12_offspeed$stand[pitch] == "R" && is.na(testpitchesZone12_offspeed$stand[pitch]) == FALSE){
      testpitchesZone12_offspeed_R[x,] <- testpitchesZone12_offspeed[pitch,]
      x = x+1
    }}
  #Zone 12 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone12_offspeed_R_R <- data.frame(testpitchesZone12_offspeed_R)
  testpitchesZone12_offspeed_R_R <- testpitchesZone12_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed_R$p_throws)){
    if(testpitchesZone12_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone12_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_offspeed_R_R[x,] <- testpitchesZone12_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone12_offspeed_R_L <- data.frame(testpitchesZone12_offspeed_R)
  testpitchesZone12_offspeed_R_L <- testpitchesZone12_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed_R$p_throws)){
    if(testpitchesZone12_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone12_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_offspeed_R_L[x,] <- testpitchesZone12_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND Offspeed AND L Batter
  testpitchesZone12_offspeed_L <- data.frame(testpitchesZone12_offspeed)
  testpitchesZone12_offspeed_L <- testpitchesZone12_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed$stand)){
    if(testpitchesZone12_offspeed$stand[pitch] == "L" && is.na(testpitchesZone12_offspeed$stand[pitch]) == FALSE){
      testpitchesZone12_offspeed_L[x,] <- testpitchesZone12_offspeed[pitch,]
      x = x+1
    }}
  #Zone 12 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone12_offspeed_L_R <- data.frame(testpitchesZone12_offspeed_L)
  testpitchesZone12_offspeed_L_R <- testpitchesZone12_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed_L$p_throws)){
    if(testpitchesZone12_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone12_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_offspeed_L_R[x,] <- testpitchesZone12_offspeed_L[pitch,]
      x = x+1
    }}
  #Zone 12 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone12_offspeed_L_L <- data.frame(testpitchesZone12_offspeed_L)
  testpitchesZone12_offspeed_L_L <- testpitchesZone12_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_offspeed_L$p_throws)){
    if(testpitchesZone12_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone12_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_offspeed_L_L[x,] <- testpitchesZone12_offspeed_L[pitch,]
      x = x+1
    }}

  #Zone 12 AND breaking
  testpitchesZone12_breaking <- data.frame(testpitchesZone12)
  testpitchesZone12_breaking <- testpitchesZone12_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12$pitch_type)){
    if(is.na(testpitchesZone12$pitch_type[pitch]) == FALSE){
      if(testpitchesZone12$pitch_type[pitch] == "KN" || testpitchesZone12$pitch_type[pitch] == "SL" || 
         testpitchesZone12$pitch_type[pitch] == "KC" || testpitchesZone12$pitch_type[pitch] == "CU"
         || testpitchesZone12$pitch_type[pitch] == "EP"|| testpitchesZone12$pitch_type[pitch] == "CS")
      {
        testpitchesZone12_breaking[x,] <- testpitchesZone12[pitch,]
        x = x+1
      }}}

  #Zone 12 AND Breaking AND R Batter
  testpitchesZone12_breaking_R <- data.frame(testpitchesZone12_breaking)
  testpitchesZone12_breaking_R <- testpitchesZone12_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking$stand)){
    if(testpitchesZone12_breaking$stand[pitch] == "R" && is.na(testpitchesZone12_breaking$stand[pitch]) == FALSE){
      testpitchesZone12_breaking_R[x,] <- testpitchesZone12_breaking[pitch,]
      x = x+1
    }}
  #Zone 12 AND breaking AND L Batter AND R Pitcher
  testpitchesZone12_breaking_R_R <- data.frame(testpitchesZone12_breaking_R)
  testpitchesZone12_breaking_R_R <- testpitchesZone12_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking_R$p_throws)){
    if(testpitchesZone12_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone12_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_breaking_R_R[x,] <- testpitchesZone12_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND breaking AND R Batter AND L Pitcher
  testpitchesZone12_breaking_R_L <- data.frame(testpitchesZone12_breaking_R)
  testpitchesZone12_breaking_R_L <- testpitchesZone12_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking_R$p_throws)){
    if(testpitchesZone12_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone12_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone12_breaking_R_L[x,] <- testpitchesZone12_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 12 AND breaking AND L Batter
  testpitchesZone12_breaking_L <- data.frame(testpitchesZone12_breaking)
  testpitchesZone12_breaking_L <- testpitchesZone12_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking$stand)){
    if(testpitchesZone12_breaking$stand[pitch] == "L" && is.na(testpitchesZone12_breaking$stand[pitch]) == FALSE){
      testpitchesZone12_breaking_L[x,] <- testpitchesZone12_breaking[pitch,]
      x = x+1
    }}
  #Zone 12 AND breaking AND L Batter AND R Pitcher
  testpitchesZone12_breaking_L_R <- data.frame(testpitchesZone12_breaking_L)
  testpitchesZone12_breaking_L_R <- testpitchesZone12_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking_L$p_throws)){
    if(testpitchesZone12_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone12_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_breaking_L_R[x,] <- testpitchesZone12_breaking_L[pitch,]
      x = x+1
    }}
  #Zone 12 AND breaking AND L Batter AND L Pitcher
  testpitchesZone12_breaking_L_L <- data.frame(testpitchesZone12_breaking_L)
  testpitchesZone12_breaking_L_L <- testpitchesZone12_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone12_breaking_L$p_throws)){
    if(testpitchesZone12_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone12_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone12_breaking_L_L[x,] <- testpitchesZone12_breaking_L[pitch,]
      x = x+1
    }}

  #Zone 13 ######
  testpitchesZone13 <- data.frame(testpitches)
  testpitchesZone13 <- testpitchesZone13[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 13 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone13[x,] <- testpitches[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball
  testpitchesZone13_fastball <- data.frame(testpitchesZone13)
  testpitchesZone13_fastball <- testpitchesZone13_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13$pitch_type)){
    if(is.na(testpitchesZone13$pitch_type[pitch]) == FALSE){
      if(testpitchesZone13$pitch_type[pitch] == "FF" || testpitchesZone13$pitch_type[pitch] == "FT" || 
         testpitchesZone13$pitch_type[pitch] == "SI" || testpitchesZone13$pitch_type[pitch] == "FC")
      {
        testpitchesZone13_fastball[x,] <- testpitchesZone13[pitch,]
        x = x+1
      }}}
  #Zone 13 AND Fastball AND R Batter
  testpitchesZone13_fastball_R <- data.frame(testpitchesZone13_fastball)
  testpitchesZone13_fastball_R <- testpitchesZone13_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball$stand)){
    if(testpitchesZone13_fastball$stand[pitch] == "R" && is.na(testpitchesZone13_fastball$stand[pitch]) == FALSE){
      testpitchesZone13_fastball_R[x,] <- testpitchesZone13_fastball[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone13_fastball_R_R <- data.frame(testpitchesZone13_fastball_R)
  testpitchesZone13_fastball_R_R <- testpitchesZone13_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball_R$p_throws)){
    if(testpitchesZone13_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone13_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_fastball_R_R[x,] <- testpitchesZone13_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone13_fastball_R_L <- data.frame(testpitchesZone13_fastball_R)
  testpitchesZone13_fastball_R_L <- testpitchesZone13_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball_R$p_throws)){
    if(testpitchesZone13_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone13_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_fastball_R_L[x,] <- testpitchesZone13_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball AND L Batter
  testpitchesZone13_fastball_L <- data.frame(testpitchesZone13_fastball)
  testpitchesZone13_fastball_L <- testpitchesZone13_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball$stand)){
    if(testpitchesZone13_fastball$stand[pitch] == "L" && is.na(testpitchesZone13_fastball$stand[pitch]) == FALSE){
      testpitchesZone13_fastball_L[x,] <- testpitchesZone13_fastball[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone13_fastball_L_R <- data.frame(testpitchesZone13_fastball_L)
  testpitchesZone13_fastball_L_R <- testpitchesZone13_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball_L$p_throws)){
    if(testpitchesZone13_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone13_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_fastball_L_R[x,] <- testpitchesZone13_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 13 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone13_fastball_L_L <- data.frame(testpitchesZone13_fastball_L)
  testpitchesZone13_fastball_L_L <- testpitchesZone13_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_fastball_L$p_throws)){
    if(testpitchesZone13_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone13_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_fastball_L_L[x,] <- testpitchesZone13_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 13 AND Off-speed
  testpitchesZone13_offspeed <- data.frame(testpitchesZone13)
  testpitchesZone13_offspeed <- testpitchesZone13_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13$pitch_type)){
    if(is.na(testpitchesZone13$pitch_type[pitch]) == FALSE){
      if(testpitchesZone13$pitch_type[pitch] == "CH" || testpitchesZone13$pitch_type[pitch] == "FS" || 
         testpitchesZone13$pitch_type[pitch] == "FO" || testpitchesZone13$pitch_type[pitch] == "SC")
      {
        testpitchesZone13_offspeed[x,] <- testpitchesZone13[pitch,]
        x = x+1
      }}}

  #Zone 13 AND Offspeed AND R Batter
  testpitchesZone13_offspeed_R <- data.frame(testpitchesZone13_offspeed)
  testpitchesZone13_offspeed_R <- testpitchesZone13_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed$stand)){
    if(testpitchesZone13_offspeed$stand[pitch] == "R" && is.na(testpitchesZone13_offspeed$stand[pitch]) == FALSE){
      testpitchesZone13_offspeed_R[x,] <- testpitchesZone13_offspeed[pitch,]
      x = x+1
    }}
  #Zone 13 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone13_offspeed_R_R <- data.frame(testpitchesZone13_offspeed_R)
  testpitchesZone13_offspeed_R_R <- testpitchesZone13_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed_R$p_throws)){
    if(testpitchesZone13_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone13_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_offspeed_R_R[x,] <- testpitchesZone13_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone13_offspeed_R_L <- data.frame(testpitchesZone13_offspeed_R)
  testpitchesZone13_offspeed_R_L <- testpitchesZone13_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed_R$p_throws)){
    if(testpitchesZone13_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone13_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_offspeed_R_L[x,] <- testpitchesZone13_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND Offspeed AND L Batter
  testpitchesZone13_offspeed_L <- data.frame(testpitchesZone13_offspeed)
  testpitchesZone13_offspeed_L <- testpitchesZone13_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed$stand)){
    if(testpitchesZone13_offspeed$stand[pitch] == "L" && is.na(testpitchesZone13_offspeed$stand[pitch]) == FALSE){
      testpitchesZone13_offspeed_L[x,] <- testpitchesZone13_offspeed[pitch,]
      x = x+1
    }}
  #Zone 13 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone13_offspeed_L_R <- data.frame(testpitchesZone13_offspeed_L)
  testpitchesZone13_offspeed_L_R <- testpitchesZone13_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed_L$p_throws)){
    if(testpitchesZone13_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone13_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_offspeed_L_R[x,] <- testpitchesZone13_offspeed_L[pitch,]
      x = x+1
    }}
  #Zone 13 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone13_offspeed_L_L <- data.frame(testpitchesZone13_offspeed_L)
  testpitchesZone13_offspeed_L_L <- testpitchesZone13_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_offspeed_L$p_throws)){
    if(testpitchesZone13_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone13_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_offspeed_L_L[x,] <- testpitchesZone13_offspeed_L[pitch,]
      x = x+1
    }}

  #Zone 13 AND breaking
  testpitchesZone13_breaking <- data.frame(testpitchesZone13)
  testpitchesZone13_breaking <- testpitchesZone13_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13$pitch_type)){
    if(is.na(testpitchesZone13$pitch_type[pitch]) == FALSE){
      if(testpitchesZone13$pitch_type[pitch] == "KN" || testpitchesZone13$pitch_type[pitch] == "SL" || 
         testpitchesZone13$pitch_type[pitch] == "KC" || testpitchesZone13$pitch_type[pitch] == "CU"
         || testpitchesZone13$pitch_type[pitch] == "EP"|| testpitchesZone13$pitch_type[pitch] == "CS")
      {
        testpitchesZone13_breaking[x,] <- testpitchesZone13[pitch,]
        x = x+1
      }}}

  #Zone 13 AND Breaking AND R Batter
  testpitchesZone13_breaking_R <- data.frame(testpitchesZone13_breaking)
  testpitchesZone13_breaking_R <- testpitchesZone13_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking$stand)){
    if(testpitchesZone13_breaking$stand[pitch] == "R" && is.na(testpitchesZone13_breaking$stand[pitch]) == FALSE){
      testpitchesZone13_breaking_R[x,] <- testpitchesZone13_breaking[pitch,]
      x = x+1
    }}
  #Zone 13 AND breaking AND L Batter AND R Pitcher
  testpitchesZone13_breaking_R_R <- data.frame(testpitchesZone13_breaking_R)
  testpitchesZone13_breaking_R_R <- testpitchesZone13_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking_R$p_throws)){
    if(testpitchesZone13_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone13_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_breaking_R_R[x,] <- testpitchesZone13_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND breaking AND R Batter AND L Pitcher
  testpitchesZone13_breaking_R_L <- data.frame(testpitchesZone13_breaking_R)
  testpitchesZone13_breaking_R_L <- testpitchesZone13_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking_R$p_throws)){
    if(testpitchesZone13_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone13_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone13_breaking_R_L[x,] <- testpitchesZone13_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 13 AND breaking AND L Batter
  testpitchesZone13_breaking_L <- data.frame(testpitchesZone13_breaking)
  testpitchesZone13_breaking_L <- testpitchesZone13_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking$stand)){
    if(testpitchesZone13_breaking$stand[pitch] == "L" && is.na(testpitchesZone13_breaking$stand[pitch]) == FALSE){
      testpitchesZone13_breaking_L[x,] <- testpitchesZone13_breaking[pitch,]
      x = x+1
    }}
  #Zone 13 AND breaking AND L Batter AND R Pitcher
  testpitchesZone13_breaking_L_R <- data.frame(testpitchesZone13_breaking_L)
  testpitchesZone13_breaking_L_R <- testpitchesZone13_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking_L$p_throws)){
    if(testpitchesZone13_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone13_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_breaking_L_R[x,] <- testpitchesZone13_breaking_L[pitch,]
      x = x+1
    }}
  #Zone 13 AND breaking AND L Batter AND L Pitcher
  testpitchesZone13_breaking_L_L <- data.frame(testpitchesZone13_breaking_L)
  testpitchesZone13_breaking_L_L <- testpitchesZone13_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone13_breaking_L$p_throws)){
    if(testpitchesZone13_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone13_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone13_breaking_L_L[x,] <- testpitchesZone13_breaking_L[pitch,]
      x = x+1
    }}

  #Zone 14 #######
  testpitchesZone14 <- data.frame(testpitches)
  testpitchesZone14 <- testpitchesZone14[0,]
  x = 1
  for(pitch in 1:length(testpitches$zone)){
    if(testpitches$zone[pitch] == 14 && is.na(testpitches$zone[pitch]) == FALSE){
      testpitchesZone14[x,] <- testpitches[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball
  testpitchesZone14_fastball <- data.frame(testpitchesZone14)
  testpitchesZone14_fastball <- testpitchesZone14_fastball[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14$pitch_type)){
    if(is.na(testpitchesZone14$pitch_type[pitch]) == FALSE){
      if(testpitchesZone14$pitch_type[pitch] == "FF" || testpitchesZone14$pitch_type[pitch] == "FT" || 
         testpitchesZone14$pitch_type[pitch] == "SI" || testpitchesZone14$pitch_type[pitch] == "FC")
      {
        testpitchesZone14_fastball[x,] <- testpitchesZone14[pitch,]
        x = x+1
      }}}
  #Zone 14 AND Fastball AND R Batter
  testpitchesZone14_fastball_R <- data.frame(testpitchesZone14_fastball)
  testpitchesZone14_fastball_R <- testpitchesZone14_fastball_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball$stand)){
    if(testpitchesZone14_fastball$stand[pitch] == "R" && is.na(testpitchesZone14_fastball$stand[pitch]) == FALSE){
      testpitchesZone14_fastball_R[x,] <- testpitchesZone14_fastball[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone14_fastball_R_R <- data.frame(testpitchesZone14_fastball_R)
  testpitchesZone14_fastball_R_R <- testpitchesZone14_fastball_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball_R$p_throws)){
    if(testpitchesZone14_fastball_R$p_throws[pitch] == "R" && is.na(testpitchesZone14_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_fastball_R_R[x,] <- testpitchesZone14_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball AND R Batter AND L Pitcher
  testpitchesZone14_fastball_R_L <- data.frame(testpitchesZone14_fastball_R)
  testpitchesZone14_fastball_R_L <- testpitchesZone14_fastball_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball_R$p_throws)){
    if(testpitchesZone14_fastball_R$p_throws[pitch] == "L" && is.na(testpitchesZone14_fastball_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_fastball_R_L[x,] <- testpitchesZone14_fastball_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball AND L Batter
  testpitchesZone14_fastball_L <- data.frame(testpitchesZone14_fastball)
  testpitchesZone14_fastball_L <- testpitchesZone14_fastball_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball$stand)){
    if(testpitchesZone14_fastball$stand[pitch] == "L" && is.na(testpitchesZone14_fastball$stand[pitch]) == FALSE){
      testpitchesZone14_fastball_L[x,] <- testpitchesZone14_fastball[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball AND L Batter AND R Pitcher
  testpitchesZone14_fastball_L_R <- data.frame(testpitchesZone14_fastball_L)
  testpitchesZone14_fastball_L_R <- testpitchesZone14_fastball_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball_L$p_throws)){
    if(testpitchesZone14_fastball_L$p_throws[pitch] == "R" && is.na(testpitchesZone14_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_fastball_L_R[x,] <- testpitchesZone14_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 14 AND Fastball AND L Batter AND L Pitcher
  testpitchesZone14_fastball_L_L <- data.frame(testpitchesZone14_fastball_L)
  testpitchesZone14_fastball_L_L <- testpitchesZone14_fastball_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_fastball_L$p_throws)){
    if(testpitchesZone14_fastball_L$p_throws[pitch] == "L" && is.na(testpitchesZone14_fastball_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_fastball_L_L[x,] <- testpitchesZone14_fastball_L[pitch,]
      x = x+1
    }}
  #Zone 14 AND Off-speed
  testpitchesZone14_offspeed <- data.frame(testpitchesZone14)
  testpitchesZone14_offspeed <- testpitchesZone14_offspeed[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14$pitch_type)){
    if(is.na(testpitchesZone14$pitch_type[pitch]) == FALSE){
      if(testpitchesZone14$pitch_type[pitch] == "CH" || testpitchesZone14$pitch_type[pitch] == "FS" || 
         testpitchesZone14$pitch_type[pitch] == "FO" || testpitchesZone14$pitch_type[pitch] == "SC")
      {
        testpitchesZone14_offspeed[x,] <- testpitchesZone14[pitch,]
        x = x+1
      }}}

  #Zone 14 AND Offspeed AND R Batter
  testpitchesZone14_offspeed_R <- data.frame(testpitchesZone14_offspeed)
  testpitchesZone14_offspeed_R <- testpitchesZone14_offspeed_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed$stand)){
    if(testpitchesZone14_offspeed$stand[pitch] == "R" && is.na(testpitchesZone14_offspeed$stand[pitch]) == FALSE){
      testpitchesZone14_offspeed_R[x,] <- testpitchesZone14_offspeed[pitch,]
      x = x+1
    }}
  #Zone 14 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone14_offspeed_R_R <- data.frame(testpitchesZone14_offspeed_R)
  testpitchesZone14_offspeed_R_R <- testpitchesZone14_offspeed_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed_R$p_throws)){
    if(testpitchesZone14_offspeed_R$p_throws[pitch] == "R" && is.na(testpitchesZone14_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_offspeed_R_R[x,] <- testpitchesZone14_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND Offspeed AND R Batter AND L Pitcher
  testpitchesZone14_offspeed_R_L <- data.frame(testpitchesZone14_offspeed_R)
  testpitchesZone14_offspeed_R_L <- testpitchesZone14_offspeed_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed_R$p_throws)){
    if(testpitchesZone14_offspeed_R$p_throws[pitch] == "L" && is.na(testpitchesZone14_offspeed_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_offspeed_R_L[x,] <- testpitchesZone14_offspeed_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND Offspeed AND L Batter
  testpitchesZone14_offspeed_L <- data.frame(testpitchesZone14_offspeed)
  testpitchesZone14_offspeed_L <- testpitchesZone14_offspeed_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed$stand)){
    if(testpitchesZone14_offspeed$stand[pitch] == "L" && is.na(testpitchesZone14_offspeed$stand[pitch]) == FALSE){
      testpitchesZone14_offspeed_L[x,] <- testpitchesZone14_offspeed[pitch,]
      x = x+1
    }}
  #Zone 14 AND Offspeed AND L Batter AND R Pitcher
  testpitchesZone14_offspeed_L_R <- data.frame(testpitchesZone14_offspeed_L)
  testpitchesZone14_offspeed_L_R <- testpitchesZone14_offspeed_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed_L$p_throws)){
    if(testpitchesZone14_offspeed_L$p_throws[pitch] == "R" && is.na(testpitchesZone14_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_offspeed_L_R[x,] <- testpitchesZone14_offspeed_L[pitch,]
      x = x+1
    }}
  #Zone 14 AND Offspeed AND L Batter AND L Pitcher
  testpitchesZone14_offspeed_L_L <- data.frame(testpitchesZone14_offspeed_L)
  testpitchesZone14_offspeed_L_L <- testpitchesZone14_offspeed_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_offspeed_L$p_throws)){
    if(testpitchesZone14_offspeed_L$p_throws[pitch] == "L" && is.na(testpitchesZone14_offspeed_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_offspeed_L_L[x,] <- testpitchesZone14_offspeed_L[pitch,]
      x = x+1
    }}

  #Zone 14 AND breaking
  testpitchesZone14_breaking <- data.frame(testpitchesZone14)
  testpitchesZone14_breaking <- testpitchesZone14_breaking[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14$pitch_type)){
    if(is.na(testpitchesZone14$pitch_type[pitch]) == FALSE){
      if(testpitchesZone14$pitch_type[pitch] == "KN" || testpitchesZone14$pitch_type[pitch] == "SL" || 
         testpitchesZone14$pitch_type[pitch] == "KC" || testpitchesZone14$pitch_type[pitch] == "CU"
         || testpitchesZone14$pitch_type[pitch] == "EP"|| testpitchesZone14$pitch_type[pitch] == "CS")
      {
        testpitchesZone14_breaking[x,] <- testpitchesZone14[pitch,]
        x = x+1
      }}}

  #Zone 14 AND Breaking AND R Batter
  testpitchesZone14_breaking_R <- data.frame(testpitchesZone14_breaking)
  testpitchesZone14_breaking_R <- testpitchesZone14_breaking_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking$stand)){
    if(testpitchesZone14_breaking$stand[pitch] == "R" && is.na(testpitchesZone14_breaking$stand[pitch]) == FALSE){
      testpitchesZone14_breaking_R[x,] <- testpitchesZone14_breaking[pitch,]
      x = x+1
    }}
  #Zone 14 AND breaking AND L Batter AND R Pitcher
  testpitchesZone14_breaking_R_R <- data.frame(testpitchesZone14_breaking_R)
  testpitchesZone14_breaking_R_R <- testpitchesZone14_breaking_R_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking_R$p_throws)){
    if(testpitchesZone14_breaking_R$p_throws[pitch] == "R" && is.na(testpitchesZone14_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_breaking_R_R[x,] <- testpitchesZone14_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND breaking AND R Batter AND L Pitcher
  testpitchesZone14_breaking_R_L <- data.frame(testpitchesZone14_breaking_R)
  testpitchesZone14_breaking_R_L <- testpitchesZone14_breaking_R_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking_R$p_throws)){
    if(testpitchesZone14_breaking_R$p_throws[pitch] == "L" && is.na(testpitchesZone14_breaking_R$p_throws[pitch]) == FALSE){
      testpitchesZone14_breaking_R_L[x,] <- testpitchesZone14_breaking_R[pitch,]
      x = x+1
    }}
  #Zone 14 AND breaking AND L Batter
  testpitchesZone14_breaking_L <- data.frame(testpitchesZone14_breaking)
  testpitchesZone14_breaking_L <- testpitchesZone14_breaking_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking$stand)){
    if(testpitchesZone14_breaking$stand[pitch] == "L" && is.na(testpitchesZone14_breaking$stand[pitch]) == FALSE){
      testpitchesZone14_breaking_L[x,] <- testpitchesZone14_breaking[pitch,]
      x = x+1
    }}
  #Zone 14 AND breaking AND L Batter AND R Pitcher
  testpitchesZone14_breaking_L_R <- data.frame(testpitchesZone14_breaking_L)
  testpitchesZone14_breaking_L_R <- testpitchesZone14_breaking_L_R[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking_L$p_throws)){
    if(testpitchesZone14_breaking_L$p_throws[pitch] == "R" && is.na(testpitchesZone14_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_breaking_L_R[x,] <- testpitchesZone14_breaking_L[pitch,]
      x = x+1
    }}
  #Zone 14 AND breaking AND L Batter AND L Pitcher
  testpitchesZone14_breaking_L_L <- data.frame(testpitchesZone14_breaking_L)
  testpitchesZone14_breaking_L_L <- testpitchesZone14_breaking_L_L[0,]
  x = 1
  for(pitch in 1:length(testpitchesZone14_breaking_L$p_throws)){
    if(testpitchesZone14_breaking_L$p_throws[pitch] == "L" && is.na(testpitchesZone14_breaking_L$p_throws[pitch]) == FALSE){
      testpitchesZone14_breaking_L_L[x,] <- testpitchesZone14_breaking_L[pitch,]
      x = x+1
    }}


#**calculate OUR value for each pitch based on the success of pitches like it**

compare_pitches <- data.frame(testpitches)    #will hold all pitches that we want to compare the outer for loop pitch to
compare_pitches <- compare_pitches[0,]        #this is just initializing the datafram to look like testpitches but empty

calculate_stat <- function (pitches_){
  for(pitch1 in 1:length(pitches_$pitch_type)){      #comparing every pitch to every other pitch
    compare_pitches <- compare_pitches[0,]              #resets compare_pitches to empty for every new pitch
    
    for(pitch2 in 1:length(testpitches$pitch_type)){
      h=0
      
      if(pitches_$pitch_type[pitch1] == "FF" || pitches_$pitch_type[pitch1] == "FT" || 
         pitches_$pitch_type[pitch1] == "SI" || pitches_$pitch_type[pitch1] == "FC")
      {
        if(!is.na(pitches_$release_speed[pitch1]) & !is.na(testpitches$release_speed[pitch2]) & !is.na(testpitches$release_spin_rate[pitch2]) &
           !is.na(pitches_$release_spin_rate[pitch1]) & !is.na(testpitches$zone[pitch2]) & !is.na(pitches_$zone[pitch1]) &
           !is.na(testpitches$p_throws[pitch2]) & !is.na(pitches_$p_throws[pitch1]) & !is.na(testpitches$stand[pitch2]) &
           !is.na(pitches_$stand[pitch1]) &
           (testpitches$release_speed[pitch2] >= (pitches_$release_speed[pitch1] - 1.5)) & 
           (testpitches$release_speed[pitch2] <= pitches_$release_speed[pitch1] + 1.5) & 
           (testpitches$release_spin_rate[pitch2] >= pitches_$release_spin_rate[pitch1] - 97) &
           (testpitches$release_spin_rate[pitch2] <= pitches_$release_spin_rate[pitch1] + 97) &
           testpitches$zone[pitch2] == pitches_$zone[pitch1] & testpitches$p_throws[pitch2] == pitches_$p_throws[pitch1]
           & testpitches$stand[pitch2] == pitches_$stand[pitch1])
        {
          compare_pitches<-rbind(compare_pitches, testpitches[pitch2,])
        }
      }
      
      else if(pitches_$pitch_type[pitch1] == "SL" || pitches_$pitch_type[pitch1] == "CU" || 
              pitches_$pitch_type[pitch1] == "KC")  #no slow curves
      {
        if(!is.na(pitches_$release_speed[pitch1]) & !is.na(testpitches$release_speed[pitch2]) & !is.na(testpitches$release_spin_rate[pitch2]) &
           !is.na(pitches_$release_spin_rate[pitch1]) & !is.na(testpitches$zone[pitch2]) & !is.na(pitches_$zone[pitch1]) &
           !is.na(testpitches$p_throws[pitch2]) & !is.na(pitches_$p_throws[pitch1]) & !is.na(testpitches$stand[pitch2]) &
           !is.na(pitches_$stand[pitch1]) &
           (testpitches$release_speed[pitch2] >= (pitches_$release_speed[pitch1] - 2.2)) & 
           (testpitches$release_speed[pitch2] <= pitches_$release_speed[pitch1] + 2.2) & 
           (testpitches$release_spin_rate[pitch2] >= pitches_$release_spin_rate[pitch1] - 139) &
           (testpitches$release_spin_rate[pitch2] <= pitches_$release_spin_rate[pitch1] + 139) &
           testpitches$zone[pitch2] == pitches_$zone[pitch1] & testpitches$p_throws[pitch2] == pitches_$p_throws[pitch1]
           & testpitches$stand[pitch2] == pitches_$stand[pitch1])
        {
          compare_pitches<-rbind(compare_pitches, testpitches[pitch2,])
        }
      }
      
      else if(pitches_$pitch_type[pitch1] == "KN")    #they were weird so on their own --> none in testpitches
      {
        # if(!is.na(pitches_$release_speed[pitch1]) & !is.na(testpitches$release_speed[pitch2]) & !is.na(testpitches$release_spin_rate[pitch2]) &
        #    !is.na(pitches_$release_spin_rate[pitch1]) & !is.na(testpitches$zone[pitch2]) & !is.na(pitches_$zone[pitch1]) &
        #    !is.na(testpitches$p_throws[pitch2]) & !is.na(pitches_$p_throws[pitch1]) & !is.na(testpitches$stand[pitch2]) &
        #    !is.na(pitches_$stand[pitch1]) &
        #    (testpitches$release_speed[pitch2] >= (pitches_$release_speed[pitch1] - 1.5)) & 
        #    (testpitches$release_speed[pitch2] <= pitches_$release_speed[pitch1] + 1.5) & 
        #    (testpitches$release_spin_rate[pitch2] >= pitches_$release_spin_rate[pitch1] - 97) &
        #    (testpitches$release_spin_rate[pitch2] <= pitches_$release_spin_rate[pitch1] + 97) &
        #    testpitches$zone[pitch2] == pitches_$zone[pitch1] & testpitches$p_throws[pitch2] == pitches_$p_throws[pitch1]
        #    & testpitches$stand[pitch2] == pitches_$stand[pitch1] & testpitches$pitch_type[pitch2] == pitches_$pitch_type[pitch1])
        #                                                                   #only compare KN to other KN
        # {
        #   compare_pitches<-rbind(compare_pitches, testpitches[pitch2,])
        # }
      }
      
      else if(pitches_$pitch_type[pitch1] == "EP")    #they were weird so on their own 
      {
        if(!is.na(pitches_$release_speed[pitch1]) & !is.na(testpitches$release_speed[pitch2]) & !is.na(testpitches$release_spin_rate[pitch2]) &
           !is.na(pitches_$release_spin_rate[pitch1]) & !is.na(testpitches$zone[pitch2]) & !is.na(pitches_$zone[pitch1]) &
           !is.na(testpitches$p_throws[pitch2]) & !is.na(pitches_$p_throws[pitch1]) & !is.na(testpitches$stand[pitch2]) &
           !is.na(pitches_$stand[pitch1]) & testpitches$zone[pitch2] == pitches_$zone[pitch1] & 
           testpitches$p_throws[pitch2] == pitches_$p_throws[pitch1] & 
           testpitches$stand[pitch2] == pitches_$stand[pitch1] & testpitches$pitch_type[pitch2] == pitches_$pitch_type[pitch1])
          #only compare EP to other EP
        {
          compare_pitches<-rbind(compare_pitches, testpitches[pitch2,])
        }
      }
      
      else if(pitches_$pitch_type[pitch1] == "CH" || pitches_$pitch_type[pitch1] == "FS" || 
              pitches_$pitch_type[pitch1] == "FO")  #no screwballs
      {
        if(!is.na(pitches_$release_speed[pitch1]) & !is.na(testpitches$release_speed[pitch2]) & !is.na(testpitches$release_spin_rate[pitch2]) &
           !is.na(pitches_$release_spin_rate[pitch1]) & !is.na(testpitches$zone[pitch2]) & !is.na(pitches_$zone[pitch1]) &
           !is.na(testpitches$p_throws[pitch2]) & !is.na(pitches_$p_throws[pitch1]) & !is.na(testpitches$stand[pitch2]) &
           !is.na(pitches_$stand[pitch1]) &
           (testpitches$release_speed[pitch2] >= (pitches_$release_speed[pitch1] - 1.7)) & 
           (testpitches$release_speed[pitch2] <= pitches_$release_speed[pitch1] + 1.7) & 
           (testpitches$release_spin_rate[pitch2] >= pitches_$release_spin_rate[pitch1] - 160.9) &
           (testpitches$release_spin_rate[pitch2] <= pitches_$release_spin_rate[pitch1] + 160.9) &
           testpitches$zone[pitch2] == pitches_$zone[pitch1] & testpitches$p_throws[pitch2] == pitches_$p_throws[pitch1]
           & testpitches$stand[pitch2] == pitches_$stand[pitch1])
        {
          compare_pitches<-rbind(compare_pitches, testpitches[pitch2,])
        }
      }
    }
    expected_value <- mean(compare_pitches$value)
    pitches_$our_value[pitch1] <- expected_value
    print(expected_value)
  }
  #this is how you would normally sum 
  total <- sum(pitches_$our_value, na.rm = TRUE)
  return(total)
}


verlander_nohitter <- subset(testpitches, testpitches$pitcher==434378)
verlander_nohitter <- subset(verlander_nohitter, verlander_nohitter$game_date == "2019-09-01")
verlander_nohitter$our_value <- NA
verlander_total <- calculate_stat(verlander_nohitter)

unique_pitchers <- unique(testpitches$pitcher)
unique_dates <- unique(testpitches$game_date)
for(pitcher in 1:length(unique_pitchers)){
  for(date in 1:length(unique_dates)){
    pitcher_date <- paste(unique_pitchers[pitcher], unique_dates[date], sep = "_")
    pitcher_date_data <- subset(testpitches, testpitches$pitcher == unique_pitchers[pitcher])
    pitcher_date_data <- subset(pitcher_date_data, pitcher_date_data$game_date == unique_dates[date])
    
    assign(pitcher_date, pitcher_date_data)
  }
}







