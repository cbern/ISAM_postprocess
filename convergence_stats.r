# modified June 2017 for outer loop
library(dplyr)
library(readr)

message(Sys.time())

#######################
# INPUTS & PARAMETERS
#######################

# Define list of selected Persons for Analysis (person is identified by hhid pid combination)
hlist = c(1359, 990) #c(4563, 764, 307, 845, 111, 29)
plist = c(2,      2) #c(3,    2,   2,      1,  3,  2)

# Set number of outer-loop iterations (starting at 0)
numOuterIters = 2

# Set number of inner-loop iterations (starting at 0)
numIters = 3


# Location of iSAM output files (output will be written here also)
basedir <-"D:/OH_ABM_DTA/7_outerIntegration/170602"


# Set Stability Threshold:
#   Max Abs( adjustedDepTime[n] - adjustedDepTime[n-1] ) > stabilityThreshold (minutes)
stabilityThreshold = 10

# Open Stress Thresholds Lookup
stressThreshold <- read_csv("d:/OH_ABM_DTA/6_Integration/stressThresholds.csv")

#######################
# PROCESSING
#######################

# Each outer loop
#-------------------------
outIters <- c(0:numOuterIters)

for (outerItNum in outIters) {

  dirname <- paste0(basedir, "/outer", outerItNum)  
  setwd(dirname)
  
  # Open ABM Person File first
  per <- read_csv("abmData/PersonList.csv")
  
  trips <- read_csv("abmData/TripList.csv")
  tripVar <- trips[,c("hhid","pnum","uniqueid", "destPurp")]
  
  
  # Each set of inner loops
  #-------------------------
  inIters <- c(1:numIters)
  
  # Read 0th iteration files
  adj0 <- read_csv(paste("inner0/adjusted_schedules_",0,".csv",sep=""))
  colnames(adj0) <- paste(colnames(adj0),"0", sep="")
  
  # "-1" adjusted schedule
  adjNeg1 <- read_csv("inner0/adjusted_schedules_abm.csv")
  colnames(adjNeg1) <- paste(colnames(adjNeg1),"n1", sep="")
  
  #selpersneg1 <- adjNeg1 %>%
  #  subset(hhid == hlist[1] & pnum == plist[1]) %>%
  #  subset(select = c("hhid", "pnum", "tripid", "adjustedDepartMinute", "adjustedArriveMinute", "adjustedDuration")) %>%
  #  mutate(iter = -1)
  
  # Join Person file, Adjusted file, Simualted file, trip file (for "iter -1")
  adj0_per <- left_join(adj0, per, by = c("hhid0" = "hhid", "pnum0" = "pnum"))
  adj0_per_adjNeg1 <- left_join(adj0_per, adjNeg1, by = c("hhid0" = "hhidn1", "pnum0" = "pnumn1", "tripid0" = "tripidn1"))
  
  #selpers0 <- adj0_per %>%
  #  subset(hhid == hlist[1] & pnum == plist[1]) %>%
  #  subset(select = c("hhid", "pnum", "tripid", "persType", "adjustedDepartMinute", "adjustedArriveMinute", "adjustedDuration")) %>%
  #  mutate(iter = 0)
  
  ####### calc statistics and output for iter0 vs iter -1 (ABM original plan)
  
  # Calculate Simulated Duration and Simulated Time *new
  # For -1 iteration (abm) use original ABM schedule AND travel times 
  # for measures of consistency and stress
  adj0_per_adjNeg1 <- adj0_per_adjNeg1 %>% 
    mutate(simulatedTime = adjustedArriveMinuten1 - adjustedDepartMinuten1)
  
  nexttrip <- subset(adj0_per_adjNeg1, select = c(hhid0, pnum0, tripid0, adjustedDepartMinuten1, adjustedDepartMinute0))
  nexttrip <- nexttrip %>%
    mutate(tripidminusone = tripid0 - 1)
  nexttrip <- rename(nexttrip, adjustedDepartMinuteNextTripN1 = adjustedDepartMinuten1 )
  nexttrip <- rename(nexttrip, adjustedDepartMinuteNextTrip0 = adjustedDepartMinute0 )
  
  adj0_per_adjNeg1_p1 <- left_join(adj0_per_adjNeg1, nexttrip, by=c("hhid0" = "hhid0", "pnum0" = "pnum0", "tripid0" = "tripidminusone"))
  
  adj0_per_adjNeg1_p1 <- adj0_per_adjNeg1_p1 %>%
    mutate(simulatedDurN1 = ifelse(is.na(tripid0.y),
                                 0, #30*60 - adjustedDepartMinute.x - simulatedTime, 
                                 adjustedDepartMinuteNextTripN1 - adjustedDepartMinuten1 - simulatedTime),
           simulatedDur0  = ifelse(is.na(tripid0.y),
                                   0, #30*60 - adjustedDepartMinute.x - simulatedTime, 
                                   adjustedDepartMinuteNextTrip0 - adjustedDepartMinute0 - simulatedTime))
  
  # Add Destination Purpose from Trip File
  adj0_per_adjNeg1_p <- left_join(adj0_per_adjNeg1_p1, tripVar, by = c("hhid0" = "hhid", "pnum0" = "pnum", "tripid0" = "uniqueid"))
  
  # Calculate intermediate variables
  adj0_per_adjNeg1_p <- adj0_per_adjNeg1_p %>% 
    mutate(adjustment = adjustedDepartMinute0 - adjustedDepartMinuten1, 
           simDur0 = ifelse(destPurp>0,simulatedDur0,0), 
           simDurNonNeg0 = pmax(0,simDur0),
           negDurN1 = ifelse(simulatedDurN1 < 0, 1, 0))
  
  #selpers0a <- left_join(selpers0, 
  #                       subset(adj0_per_adjNeg1_p, 
  #                              hhid == hlist[1] & pnum == plist[1], 
  #                              select=c("hhid", "pnum", "tripid", "simDur", "simDurNonNeg", "simulatedTime", "adjustment", "destPurp")),
  #                       by = c("hhid", "pnum", "tripid"))
  #
  #selpers <- bind_rows(selpers0a, selpersneg1)
  
  # Aggregate by Person
  perLevel0 <- adj0_per_adjNeg1_p %>%
    group_by(hhid0, pnum0) %>%
    summarise(dailyActivityPattern = max(dailyActivityPattern),
              persType = max(persType),
              numTrips = n(),
              travTime = sum(simulatedTime), 
              actTime0  = sum(simDur0), 
              actTimeNonNeg0 = sum(simDurNonNeg0), 
              maxAdj = max(abs(adjustment)),
              negDurActivitiesN1 = sum(negDurN1)) %>%
    ungroup
  
  # Lookup Stress Threshold based on persType  
  perLevel0 <- left_join(perLevel0, stressThreshold, by = c("persType" = "PersType"))
  
  # Calculate convergence measure inputs
  perLevel0 <- perLevel0 %>%
    mutate(overhead0 = travTime / actTime0,
           overheadNonNeg0 = travTime / actTimeNonNeg0,
           stress1 = ifelse(travTime > MaxTotTravTime, 1, 0),
           stress2 = ifelse(overhead0 > MaxOverhead, 1, 0),
           stress3 = ifelse(actTime0 > MinActTime, 1, 0)
    )
  
  # Calculate convergence measure indicators
  perLevel0 <- perLevel0 %>%
    mutate(isStressed = ifelse(stress1 == 1 | (stress2 == 1 & stress3 == 1),1,0),
           isUnstable = ifelse(maxAdj > stabilityThreshold, 1, 0),
           isInconsistent = ifelse(negDurActivitiesN1 > 0, 1, 0)
    )
  ####
  # Final Person Type / Daily Pattern Type Summary
  perType_dyPattern_summ <- perLevel0 %>%
    group_by(persType, dailyActivityPattern) %>%
    summarise(persons = n(),
              trips = sum(numTrips),
              stressed = sum(isStressed),
              unstable = sum(isUnstable),
              inconsistent = sum(isInconsistent),
              numNegDurActivities = sum(negDurActivitiesN1),
              avgTravelTime = mean(travTime),
              avgActTime = mean(actTime0)
    )%>%
    ungroup
  
  stable_stress <- perType_dyPattern_summ %>%
    select(-c(inconsistent, numNegDurActivities))
  
  inc <- perType_dyPattern_summ %>%
    select(-c(unstable, stressed))
  
  write_csv(stable_stress, paste("inner", 0,"_stable_stressed_summ.csv",sep=""))
  write_csv(inc, paste("inner", "Neg1","_inconsistent_summ.csv",sep=""))
  
  # Final Activity Purpose Summary
  actPurp_summ <- adj0_per_adjNeg1_p %>%
    group_by(destPurp, negDurN1) %>%
    summarise(tripsCount = n())%>%
    ungroup
  
  write_csv(actPurp_summ, paste("inner", 0,"_actPurp_summ.csv",sep=""))
  
  
  
  ####### end iter 0 processing
  
  for (itNum in inIters) {
   itNum
    # Read previous iteration files
    adj0 <- read_csv(paste("inner",itNum-1,"/adjusted_schedules_",itNum-1,".csv",sep=""))
    colnames(adj0) <- paste(colnames(adj0),"0", sep="")
    
    # Open iSAM output files for current iteration
    adj1 <- read_csv(paste("inner",itNum,"/adjusted_schedules_",itNum,".csv",sep=""))
    colnames(adj1) <- paste(colnames(adj1),"1", sep="")
    
    # Join Person file, Adjusted file, Simualted file
    adj1_per <- left_join(adj1, per, by = c("hhid1" = "hhid", "pnum1" = "pnum"))
    
    # Join prev iteration Adjusted file, for calculating stability
    adj1_per_adj0 <- left_join(adj1_per, adj0, by = c("hhid1" = "hhid0", "pnum1" = "pnum0", "tripid1" = "tripid0"))
    
    # Calculate Simulated Duration and Simulated Time *new
    adj1_per_adj0 <- adj1_per_adj0 %>% 
      mutate(simulatedTime = adjustedArriveMinute1 - adjustedDepartMinute1)
    
    nexttrip <- subset(adj1_per_adj0, select = c(hhid1, pnum1, tripid1, adjustedDepartMinute0, adjustedDepartMinute1))
    nexttrip <- nexttrip %>%
      mutate(tripidminusone = tripid1 - 1)
    nexttrip <- rename(nexttrip, adjustedDepartMinuteNextTrip0 = adjustedDepartMinute0 )
    nexttrip <- rename(nexttrip, adjustedDepartMinuteNextTrip1 = adjustedDepartMinute1 )
    
    adj0_per_adj0_p1 <- left_join(adj1_per_adj0, nexttrip, by=c("hhid1" = "hhid1", "pnum1" = "pnum1", "tripid1" = "tripidminusone"))
    
    adj0_per_adj0_p1 <- adj0_per_adj0_p1 %>%
      mutate(simulatedDur0 = ifelse(is.na(tripid1.y),
                                   0, 
                                   adjustedDepartMinuteNextTrip0 - adjustedDepartMinute0 - simulatedTime),
             simulatedDur1 = ifelse(is.na(tripid1.y),
                                    0, 
                                    adjustedDepartMinuteNextTrip1 - adjustedDepartMinute1 - simulatedTime))
   
    
    # Add Destination Purpose from Trip File
    adj1_per_adj0_p <- left_join(adj0_per_adj0_p1, tripVar, by = c("hhid1" = "hhid", "pnum1" = "pnum", "tripid1" = "uniqueid"))
    
    
    # Calculate intermediate variables
    adj1_per_adj0_p <- adj1_per_adj0_p %>% 
      mutate(adjustment = adjustedDepartMinute1 - adjustedDepartMinute0, 
             simDur1 = ifelse(destPurp>0,simulatedDur1,0), 
             simDurNonNeg1 = pmax(0,simDur1),
             negDur0 = ifelse(simulatedDur0 < 0, 1, 0))
    
    #selpersx <- adj1_per_adj0_p %>%
    #  subset(hhid == hlist[1] & pnum == plist[1]) %>%
    #  subset(select = c("hhid", "pnum", "tripid", "persType", "adjustedDepartMinute.x", "adjustedArriveMinute.x", "adjustedDuration.x", "simulatedTime", "simDur", "simDurNonNeg", "adjustment")) %>%
    #  mutate(iter = itNum)
    #
    #selpersx <- rename(selpersx, adjustedDepartMinute = adjustedDepartMinute.x, 
    #                   adjustedArriveMinute = adjustedArriveMinute.x, 
    #                   adjustedDuration = adjustedDuration.x)
    #
    #selpers <- bind_rows(selpers, selpersx)
    
    
    # Aggregate by Person
    perLevel0 <- adj1_per_adj0_p %>%
      group_by(hhid1, pnum1) %>%
      summarise(dailyActivityPattern = max(dailyActivityPattern),
                persType = max(persType),
                numTrips = n(),
                travTime = sum(simulatedTime), 
                actTime1  = sum(simDur1), 
                actTimeNonNeg1 = sum(simDurNonNeg1), 
                maxAdj = max(abs(adjustment)),
                negDurActivities0 = sum(negDur0)) %>%
      ungroup
    
    # Lookup Stress Threshold based on persType  
    perLevel0 <- left_join(perLevel0, stressThreshold, by = c("persType" = "PersType"))
    
    # Calculate convergence measure inputs
    perLevel0 <- perLevel0 %>%
      mutate(overhead1 = travTime / actTime1,
             overheadNonNeg1 = travTime / actTimeNonNeg1,
             stress1 = ifelse(travTime > MaxTotTravTime, 1, 0),
             stress2 = ifelse(overheadNonNeg1 > MaxOverhead, 1, 0),
             stress3 = ifelse(actTime1 > MinActTime, 1, 0)
             )
    
    # Calculate convergence measure indicators
    perLevel0 <- perLevel0 %>%
      mutate(isStressed = ifelse(stress1 == 1 | (stress2 == 1 & stress3 == 1),1,0),
             isUnstable = ifelse(maxAdj > stabilityThreshold, 1, 0),
             isInconsistent = ifelse(negDurActivities0 > 0, 1, 0)
             )
    
    # Final Person Type / Daily Pattern Type Summary
    perType_dyPattern_summ <- perLevel0 %>%
      group_by(persType, dailyActivityPattern) %>%
      summarise(persons = n(),
                trips = sum(numTrips),
                stressed = sum(isStressed),
                unstable = sum(isUnstable),
                inconsistent = sum(isInconsistent),
                numNegDurActivities = sum(negDurActivities0),
                avgTravelTime = mean(travTime),
                avgActTime = mean(actTime1)
                )%>%
      ungroup
    
    stable_stress <- perType_dyPattern_summ %>%
      select(-c(inconsistent, numNegDurActivities))
    
    inc <- perType_dyPattern_summ %>%
      select(-c(unstable, stressed))
    
    write_csv(stable_stress, paste("inner", itNum,"_stable_stressed_summ.csv",sep=""))
    write_csv(inc, paste("inner", itNum-1,"_inconsistent_summ.csv",sep=""))
    
    
    #write_csv(perType_dyPattern_summ, paste("iter", itNum,"_perTypeDyPattern_summ.csv",sep=""))
    
    # Final Activity Purpose Summary
    actPurp_summ <- adj1_per_adj0_p %>%
      group_by(destPurp, negDur0) %>%
      summarise(tripsCount = n())%>%
      ungroup
    
    write_csv(actPurp_summ, paste("inner", itNum,"_actPurp_summ.csv",sep=""))
    
    # Set adj1 and sim1 to adj0 and sim0 for next iter
    #adj0 <- adj1
    #sim0 <- sim1
    
    gc()
    
  } # for / over inner iterations  
  
  message(Sys.time())
} # for / over outer iterations
#write_csv(selpers, "selectedPersonsTripDetails.csv")

