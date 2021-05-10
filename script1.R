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
    testpitches$events[pitch] = "ball"
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
    testpitches$value[pitch] <- -0.089376147
  }
  else if(toString(testpitches$events[pitch]) == "ball"){
    testpitches$value[pitch] <- 0.059864181
  }
  else if(toString(testpitches$events[pitch]) == "walk"){
    testpitches$value[pitch] <- 0.059864181
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
    testpitches$value[pitch] <- -0.037604309
  }
  else if(is.na(testpitches$events[pitch])){          #for those weird cases we are ignoring
    testpitches$value[pitch] <- NA
  }
}

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
  expected_value <- mean(compare_pitches$value, na.rm = TRUE)
  pitches_$our_value[pitch1] <- expected_value
  print(expected_value)
  }
  #this is how you would normally sum 
  total <- sum(pitches_$our_value, na.rm = TRUE)
  return(total)
}

#Verlander 9/1/2019 100 
verlander_nohitter <- subset(testpitches, testpitches$pitcher==434378)
verlander_nohitter <- subset(verlander_nohitter, verlander_nohitter$game_date == "2019-09-01")
verlander_nohitter$our_value <- NA
verlander_total <- calculate_stat(verlander_nohitter)

#Bieber 5/19/2019 92
bieber_0519 <- subset(testpitches, testpitches$pitcher == 	669456)
bieber_0519 <- subset(bieber_0519, bieber_0519$game_date == "2019-05-19")
bieber_0519$our_value <- NA
bieber_0519_total <- calculate_stat(bieber_0519)

#Flaherty's loss that is top 50 GsC - Flaherty 9/24/2019 84
flaherty_0924 <- subset(testpitches, testpitches$pitcher == 656427)
flaherty_0924 <- subset(flaherty_0924, flaherty_0924$game_date == "2019-09-24")
flaherty_0924$our_value <- NA
flaherty_0924_total <- calculate_stat(flaherty_0924)

#Alcantra 5/19/2019 90
alcantra_0519 <- subset(testpitches, testpitches$pitcher == 645261)
alcantra_0519 <- subset(alcantra_0519, alcantra_0519$game_date == "2019-05-19")
alcantra_0519$our_value <- NA
alcantra_0519_total <- calculate_stat(alcantra_0519)

#Fiers 9/09/2019 0
fiers_0909 <- subset(testpitches, testpitches$pitcher == 	571666)
fiers_0909 <- subset(fiers_0909, fiers_0909$game_date == "2019-09-09")
fiers_0909$our_value <- NA
fiers_0909_total <- calculate_stat(fiers_0909)

#Gausman 5/29/2109 4
gausman_0529 <- subset(testpitches, testpitches$pitcher == 	592332)
gausman_0529 <- subset(gausman_0529, gausman_0529$game_date == "2019-05-29")
gausman_0529$our_value <- NA
gausman_0529_total <- calculate_stat(gausman_0529)

#Syndergaard 8/28/2019 7
syndergaard_0828 <- subset(testpitches, testpitches$pitcher == 	592789)
syndergaard_0828 <- subset(syndergaard_0828, syndergaard_0828$game_date == "2019-08-28")
syndergaard_0828$our_value <- NA
syndergaard_0828_total <- calculate_stat(syndergaard_0828)

#Carpenter 6/9/2019 9
carpenter_0609 <- subset(testpitches, testpitches$pitcher == 	543001)
carpenter_0609 <- subset(carpenter_0609, carpenter_0609$game_date == "2019-06-09")
carpenter_0609$our_value <- NA
carpenter_0609_total <- calculate_stat(carpenter_0609)

#Ross 9/24/2019 50
ross_0924 <- subset(testpitches, testpitches$pitcher == 605452)
ross_0924 <- subset(ross_0924, ross_0924$game_date == "2019-09-24")
ross_0924$our_value <- NA
ross_0924_total <- calculate_stat(ross_0924)

#Bundy 7/12/2019 10
bundy_0712 <- subset(testpitches, testpitches$pitcher == 605164)
bundy_0712 <- subset(bundy_0712, bundy_0712$game_date == "2019-07-12")
bundy_0712$our_value <- NA
bundy_0712_total <- calculate_stat(bundy_0712)

#Pomeranz 5/6/2019 10
pomeranz_0506 <- subset(testpitches, testpitches$pitcher == 519141)
pomeranz_0506 <- subset(pomeranz_0506, pomeranz_0506$game_date == "2019-05-06")
pomeranz_0506$our_value <- NA
pomeranz_0506_total <- calculate_stat(pomeranz_0506)

#Derek Holland 9/29/2019 14
holland_0929 <- subset(testpitches, testpitches$pitcher == 543037)
holland_0929 <- subset(holland_0929, holland_0929$game_date == "2019-09-29")
holland_0929$our_value <- NA
holland_0929_total <- calculate_stat(holland_0929)

#Cal Quantrill 9/09/2019 14
quantrill_0909 <- subset(testpitches, testpitches$pitcher == 615698)
quantrill_0909 <- subset(quantrill_0909, quantrill_0909$game_date == "2019-09-09")
quantrill_0909$our_value <- NA
quantrill_0909_total <- calculate_stat(quantrill_0909)

#Joe Palumbo 6/19/2019 14
palumbo_0619 <- subset(testpitches, testpitches$pitcher == 643478)
palumbo_0619 <- subset(palumbo_0619, palumbo_0619$game_date == "2019-06-19")
palumbo_0619$our_value <- NA
palumbo_0619_total <- calculate_stat(palumbo_0619)

#Fiers 9/14/2019 37
fiers_0914 <- subset(testpitches, testpitches$pitcher == 	571666)
fiers_0914 <- subset(fiers_0914, fiers_0914$game_date == "2019-09-14")
fiers_0914$our_value <- NA
fiers_0914_total <- calculate_stat(fiers_0914)

#Fiers 6/28/2019 61
fiers_0628 <- subset(testpitches, testpitches$pitcher == 	571666)
fiers_0628 <- subset(fiers_0628, fiers_0628$game_date == "2019-06-28")
fiers_0628$our_value <- NA
fiers_0628_total <- calculate_stat(fiers_0628)








    
