
library(dplyr)
library(readr)

message(Sys.time())

#######################
# INPUTS & PARAMETERS
#######################

# Set number of iterations (starting at 0)
numIters = 7

# Open ABM Person File first
setwd("d:/OH_ABM_DTA/6_Integration/Runs/ABM_160830")
per <- read_csv("PersonList.csv")

trips <- read_csv("TripList.csv")
tripVar <- trips[,c("hhid","pnum","uniqueid", "destPurp")]


# Location of iSAM output files (output will be written here also)
setwd("d:/OH_ABM_DTA/6_Integration/Runs/convergence_12sep2016")


# Set Stability Threshold:
#   Max Abs( adjustedDepTime[n] - adjustedDepTime[n-1] ) > stabilityThreshold (minutes)
stabilityThreshold = 5

# Open Stress Thresholds Lookup
stressThreshold <- read_csv("d:/OH_ABM_DTA/6_Integration/stressThresholds.csv")

#######################
# PROCESSING
#######################
iters <- c(1:numIters)


# Read 0th iteration files
adj0 <- read_csv(paste("iter0/adjusted_schedules_",0,".csv",sep=""))

# "-1" adjusted schedule
adjNeg1 <- read_csv("iter0/adjusted_schedules_abm.csv")

# Join Person file, Adjusted file, Simualted file, trip file (for "iter -1")
adj0_per <- left_join(adj0, per, by = c("hhid" = "hhid", "pnum" = "pnum"))
adj0_per_adjNeg1 <- left_join(adj0_per, adjNeg1, by = c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "tripid"))


####### calc statistics and output for iter0 vs iter -1 (ABM original plan)

# Calculated Simulated Duration and Simulated Time *new
adj0_per_adjNeg1 <- adj0_per_adjNeg1 %>% 
  mutate(simulatedTime = adjustedArriveMinute.y - adjustedDepartMinute.y)

nexttrip <- subset(adj0_per_adjNeg1, select = c(hhid, pnum, tripid, adjustedDepartMinute.x))
nexttrip <- nexttrip %>%
  mutate(tripidminusone = tripid - 1)
nexttrip <- rename(nexttrip, adjustedDepartMinutePrevIsamNextTrip = adjustedDepartMinute.x )

adj0_per_adjNeg1_p1 <- left_join(adj0_per_adjNeg1, nexttrip, by=c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "tripidminusone"))

adj0_per_adjNeg1_p1 <- adj0_per_adjNeg1_p1 %>%
  mutate(simulatedDur = ifelse(is.na(tripid.y),
                               27*60 - adjustedDepartMinute.x - simulatedTime, 
                               adjustedDepartMinutePrevIsamNextTrip - adjustedDepartMinute.x - simulatedTime))

# Add Destination Purpose from Trip File
adj0_per_adjNeg1_p <- left_join(adj0_per_adjNeg1_p1, tripVar, by = c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "uniqueid"))


# Calculate intermediate variables
adj0_per_adjNeg1_p <- adj0_per_adjNeg1_p %>% 
  mutate(adjustment = adjustedDepartMinute.x - adjustedDepartMinute.y, 
         simDur = ifelse(destPurp>0,simulatedDur,0), 
         simDurNonNeg = pmax(0,simDur),
         negDur = ifelse(simulatedDur < 0, 1, 0))

# Aggregate by Person
perLevel0 <- adj0_per_adjNeg1_p %>%
  group_by(hhid, pnum) %>%
  summarise(dailyActivityPattern = max(dailyActivityPattern),
            persType = max(persType),
            numTrips = n(),
            travTime = sum(simulatedTime), 
            actTime  = sum(simDur), 
            actTimeNonNeg = sum(simDurNonNeg), 
            maxAdj = max(abs(adjustment)),
            negDurActivities = sum(negDur)) %>%
  ungroup

# Lookup Stress Threshold based on persType  
perLevel0 <- left_join(perLevel0, stressThreshold, by = c("persType" = "PersType"))

# Calculate convergence measure inputs
perLevel0 <- perLevel0 %>%
  mutate(overhead = travTime / actTime,
         overheadNonNeg = travTime / actTimeNonNeg,
         stress1 = ifelse(travTime > MaxTotTravTime, 1, 0),
         stress2 = ifelse(overhead > MaxOverhead, 1, 0),
         stress3 = ifelse(actTime > MinActTime, 1, 0)
  )

# Calculate convergence measure indicators
perLevel0 <- perLevel0 %>%
  mutate(isStressed = ifelse(stress1 == 1 | (stress2 == 1 & stress3 == 1),1,0),
         isUnstable = ifelse(maxAdj > stabilityThreshold, 1, 0),
         isInconsistent = ifelse(negDurActivities > 0, 1, 0)
  )

# Final Person Type / Daily Pattern Type Summary
perType_dyPattern_summ <- perLevel0 %>%
  group_by(persType, dailyActivityPattern) %>%
  summarise(persons = n(),
            trips = sum(numTrips),
            stressed = sum(isStressed),
            unstable = sum(isUnstable),
            inconsistent = sum(isInconsistent),
            numNegDurActivities = sum(negDurActivities),
            avgTravelTime = mean(travTime),
            avgActTime = mean(actTime)
  )%>%
  ungroup

write_csv(perType_dyPattern_summ, paste("iter", 0,"_perTypeDyPattern_summ.csv",sep=""))

# Final Activity Purpose Summary
actPurp_summ <- adj0_per_adjNeg1_p %>%
  group_by(destPurp, negDur) %>%
  summarise(tripsCount = n())%>%
  ungroup

write_csv(actPurp_summ, paste("iter", 0,"_actPurp_summ.csv",sep=""))

####### end iter 0 processing

for (itNum in iters) {
 itNum
  # Read previous iteration files
  adj0 <- read_csv(paste("iter",itNum-1,"/adjusted_schedules_",itNum-1,".csv",sep=""))
  #sim0 <- read_csv(paste("simulated_schedules_",itNum-1,".csv",sep=""))
  
  
  # Open iSAM output files for current iteration
  adj1 <- read_csv(paste("iter",itNum,"/adjusted_schedules_",itNum,".csv",sep=""))
  
  # Join Person file, Adjusted file, Simualted file
  adj1_per <- left_join(adj1, per, by = c("hhid" = "hhid", "pnum" = "pnum"))
  
  # Join prev iteration Adjusted file, for calculating stability
  adj1_per_adj0 <- left_join(adj1_per, adj0, by = c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "tripid"))
  
  # Calculated Simulated Duration and Simulated Time *new
  adj1_per_adj0 <- adj1_per_adj0 %>% 
    mutate(simulatedTime = adjustedArriveMinute.y - adjustedDepartMinute.y)
  
  nexttrip <- subset(adj1_per_adj0, select = c(hhid, pnum, tripid, adjustedDepartMinute.x))
  nexttrip <- nexttrip %>%
    mutate(tripidminusone = tripid - 1)
  nexttrip <- rename(nexttrip, adjustedDepartMinutePrevIsamNextTrip = adjustedDepartMinute.x )
  
  adj0_per_adj0_p1 <- left_join(adj1_per_adj0, nexttrip, by=c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "tripidminusone"))
  
  adj0_per_adj0_p1 <- adj0_per_adj0_p1 %>%
    mutate(simulatedDur = ifelse(is.na(tripid.y),
                                 27*60 - adjustedDepartMinute.x - simulatedTime, 
                                 adjustedDepartMinutePrevIsamNextTrip - adjustedDepartMinute.x - simulatedTime))
 
  
  # Add Destination Purpose from Trip File
  adj1_per_adj0_p <- left_join(adj0_per_adj0_p1, tripVar, by = c("hhid" = "hhid", "pnum" = "pnum", "tripid" = "uniqueid"))
  
  
  # Calculate intermediate variables
  adj1_per_adj0_p <- adj1_per_adj0_p %>% 
    mutate(adjustment = adjustedDepartMinute.x - adjustedDepartMinute.y, 
           simDur = ifelse(destPurp>0,simulatedDur,0), 
           simDurNonNeg = pmax(0,simDur),
           negDur = ifelse(simulatedDur < 0, 1, 0))
  
  # Aggregate by Person
  perLevel0 <- adj1_per_adj0_p %>%
    group_by(hhid, pnum) %>%
    summarise(dailyActivityPattern = max(dailyActivityPattern),
              persType = max(persType),
              numTrips = n(),
              travTime = sum(simulatedTime), 
              actTime  = sum(simDur), 
              actTimeNonNeg = sum(simDurNonNeg), 
              maxAdj = max(abs(adjustment)),
              negDurActivities = sum(negDur)) %>%
    ungroup
  
  # Lookup Stress Threshold based on persType  
  perLevel0 <- left_join(perLevel0, stressThreshold, by = c("persType" = "PersType"))
  
  # Calculate convergence measure inputs
  perLevel0 <- perLevel0 %>%
    mutate(overhead = travTime / actTime,
           overheadNonNeg = travTime / actTimeNonNeg,
           stress1 = ifelse(travTime > MaxTotTravTime, 1, 0),
           stress2 = ifelse(overhead > MaxOverhead, 1, 0),
           stress3 = ifelse(actTime > MinActTime, 1, 0)
           )
  
  # Calculate convergence measure indicators
  perLevel0 <- perLevel0 %>%
    mutate(isStressed = ifelse(stress1 == 1 | (stress2 == 1 & stress3 == 1),1,0),
           isUnstable = ifelse(maxAdj > stabilityThreshold, 1, 0),
           isInconsistent = ifelse(negDurActivities > 0, 1, 0)
           )
  
  # Final Person Type / Daily Pattern Type Summary
  perType_dyPattern_summ <- perLevel0 %>%
    group_by(persType, dailyActivityPattern) %>%
    summarise(persons = n(),
              trips = sum(numTrips),
              stressed = sum(isStressed),
              unstable = sum(isUnstable),
              inconsistent = sum(isInconsistent),
              numNegDurActivities = sum(negDurActivities),
              avgTravelTime = mean(travTime),
              avgActTime = mean(actTime)
              )%>%
    ungroup
  
  write_csv(perType_dyPattern_summ, paste("iter", itNum,"_perTypeDyPattern_summ.csv",sep=""))
  
  # Final Activity Purpose Summary
  actPurp_summ <- adj1_per_adj0_p %>%
    group_by(destPurp, negDur) %>%
    summarise(tripsCount = n())%>%
    ungroup
  
  write_csv(actPurp_summ, paste("iter", itNum,"_actPurp_summ.csv",sep=""))
  
  # Set adj1 and sim1 to adj0 and sim0 for next iter
  #adj0 <- adj1
  #sim0 <- sim1
  
  gc()
  
} # for / over iterations  

message(Sys.time())


