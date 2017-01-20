####################################################################
# ARC_ABM_to_iSAM.R
# -----------------------
# ARC ABM Output Processing
#   for iSAM input
#
# Chrissy Bernardo (bernardoca@pbworld.com), August 3, 2016 
####################################################################

# Load Libraries
library(readr)
library(dplyr)
library(stats)
library(data.table)
library(reshape2)
library(ggplot2)
library(sqldf)

setwd("D:/C10_ARC/7_R")

#-----------------------
# PARAMETERS
#-----------------------

# Calibrated factor to account for children not being allowed to drive 
# (so probability of being a driver is not exactly 1/occupancy):
#      'driver' if:  randn < driverfactor * 1 / vehocc
driverfactor2 = 1.5 
driverfactor3 = 1.6

nMovingAvg = 100 # Number of moving average iterations for minute time bins

speed = 30	# Assumed speed,in mph, for estimating stop departure times -- 30(equivalent of 2 miles per minute)
HOV3occ = 3.4 # Assumed occupancy for HOV3 trips
stopdur = 10  # starting stop duration assumption (minutes)

# VOT Parameters:
incomescale = 0.8	# Income scaling parameter {e in: U=b×Time+c×(Cost / (Inc^e×Occ^f))} - copied from MORPC
occscalew	= 0.7	# Work Occupancy scaling parameter {f in: U=b×Time+c×(Cost / (Inc^e×Occ^f))} - copied from MORPC
occscalen	= 0.6	# Non-Work Occupancy scaling parameter {f in: U=b×Time+c×(Cost / (Inc^e×Occ^f))} - copied from MORPC
refSOVVOT	= 25	# Reference SOV VOT for reference income group ($/hr) - copied from "HwyAssignAM.rpt"
refIncome	= 75000	# Reference Income for SOV VOT (average HH income)
sigmawork	= 0.50	# Sigma parameter for lognormal VOT distribution for work - copied from MORPC
sigmanwork	= 0.80	#Sigma parameter for lognormal VOT distribution for nonwork - copied from MORPC
seed		= 12345
MinimumIncomeForVOT = 1000

#-----------------------
# INPUTS
#-----------------------

trips_orig <- read_csv("Inputs/personData (2)/tripData.csv")

trips <- subset(trips_orig, select=-c(orig_walk_segment, dest_walk_segment, hh_autos, 
                                      hh_size, hh_wkrs, hh_auto_suff, gender))
#trips1 <- trips[1:10000,]
#trips <- trips[1:10000,]
rm(trips_orig)

#-------------------------------------------------------------
# 1) Create Required Fields 
#-------------------------------------------------------------

# "party" field in CT-RAMP2 format
  trips1 <- trips %>%
      mutate(tour_party = paste("[",gsub(" ",", ",tour_participants),"]",sep=""))

# Joint trip ID
  trips1 <- trips1 %>%
    mutate(joint_indicator = ifelse(num_participants>1, 1, 0))
  
  # Isolate Joint trips1
  j <- trips1[trips1$joint_indicator==1,]
  
  # Create unique list by Household Joint trips1
  hh_j <- j[!duplicated(j[,c("hh_id", "tour_id_uniq", "inbound", "stop_id", "tour_party", "tour_start_period")]),]
  
  # Calculate unique joint trip ID
  hh_j <- hh_j %>% 
    group_by(hh_id) %>%
    mutate(joint_trip_id = ifelse(num_participants>1, cumsum(joint_indicator), 0)) %>%
    ungroup
  
  # Calculate unique joint trip ID
  # hh_j <- hh_j %>% 
  #   group_by(hh_id, tour_id_uniq, tour_party, tour_start_period) %>%
  #   mutate(joint_trip_id = ifelse(num_participants>1, cumsum(joint_indicator), 0)) %>%
  #   ungroup

  # Join back to trip file
  hh_j_1 <- subset(hh_j, select=c(hh_id, tour_id_uniq, inbound, stop_id, tour_party, tour_start_period, joint_trip_id))
  trips1 <- left_join(trips1, hh_j_1, by=c("hh_id", "tour_id_uniq", "inbound", "stop_id", "tour_party", "tour_start_period"))
  trips1$joint_trip_id[is.na(trips1$joint_trip_id)] <- 0

  # Create unique list by Household Joint Tours
  j <- trips1[trips1$joint_indicator==1,]  
  hh_jtour <- j[!duplicated(j[,c("hh_id", "tour_id_uniq", "tour_party", "tour_start_period")]),]
  
  # Calculate joint tour ID
  hh_jtour <- hh_jtour %>% 
    filter(joint_trip_id == 1) %>%
    group_by(hh_id) %>%
    mutate(joint_tour_id = cumsum(joint_indicator))
  
  # Join back to trip file
  hh_jtour <- subset(hh_jtour, select=c(hh_id, tour_id_uniq, tour_party, tour_start_period, joint_tour_id))
  trips1 <- left_join(trips1, hh_jtour, by=c("hh_id", "tour_id_uniq", "tour_party", "tour_start_period"))
  trips1$joint_tour_id[is.na(trips1$joint_tour_id)] <- 0
  
  
# View(hh_j[ , c("hh_id", "person_num", "tour_id", "inbound", "stop_id", "tour_id_uniq", "trip_id", "dest_taz", "tour_party", "tour_start_period","joint_trip_id")])
#   
# View(hh_jtour[ , c("hh_id", "person_num", "tour_id", "inbound", "stop_id", "tour_id_uniq", "trip_id", "dest_taz", "tour_party", "tour_start_period", "joint_tour_id")])
#   
# View(trips1[ , c("hh_id", "person_num", "tour_id", "inbound", "stop_id", "tour_id_uniq", "trip_id", "dest_taz", "tour_party", "tour_start_period","joint_trip_id", "joint_tour_id")])

rm(trips)
rm(j)
rm(hh_jtour)
rm(hh_j_1)
rm(hh_j)
gc()
  
#-------------------------------------------------------------
# 2) Calculate Average and Random VOT 
#-------------------------------------------------------------

set.seed(11111) 

# Generate 1 normal random number for each person
trips2 <- trips1 %>%
  group_by(person_id) %>%
  mutate(norm   = rnorm(1, mean = 0, sd = 1)) %>%
  ungroup


trips2 <- trips2 %>%
  mutate(hh_income = ifelse(hh_income>=0, hh_income, refIncome), # If income is negative, assign reference income value
         vehocc = ifelse(trip_mode == 1 | trip_mode == 2, 1,
                  ifelse(trip_mode == 3 | trip_mode == 4, 2,
                  ifelse((trip_mode == 5 | trip_mode == 6) & joint_indicator == 1, num_participants,
                  ifelse(trip_mode == 5 | trip_mode == 6, HOV3occ, 1)))),
         avgVOT = ifelse(tour_purpose == 'work', refSOVVOT * vehocc ^ occscalew / refIncome ^ incomescale * hh_income ^ incomescale,
                                                 refSOVVOT * vehocc ^ occscalen / refIncome ^ incomescale * hh_income ^ incomescale),
         randVOT = ifelse(tour_purpose == 'work', exp(norm * sigmawork + log(avgVOT) - sigmawork ^ 2 / 2),
                                                  exp(norm * sigmanwork + log(avgVOT) - sigmanwork ^ 2 / 2))
        )

rm(trips1)
gc()




#-------------------------------------------------------------
# 3) Determine Drivers & Passengers 
#-------------------------------------------------------------

set.seed(22222)

# first for joint tours: select oldest person as driver
  j1 <- trips2[trips2$joint_indicator==1 & trips2$trip_mode<=6,]
  
  j1oldest <- j1 %>%
    group_by(hh_id, joint_tour_id) %>%
    filter(age == max(age)) %>%
    summarise(person_num = min(person_num)) %>%
    mutate(occupant = 'driver')

  # !! NOTE: Several joint tours by driving mode
  #          have the oldest person as a non driving age
  #          student (e.g. 6 years old). This is a 
  #          problem with the ABM, for now the oldest 
  #          person on the join tour will be the driver,
  #          regardless of their absolute age.
  

  trips3 <- left_join(trips2, subset(j1oldest, select=c("hh_id","joint_tour_id", "person_num","occupant")), by=c("hh_id", "person_num", "joint_tour_id")) 
  
  trips3 <- trips3 %>%
    mutate(occupant = ifelse(joint_indicator==1 & is.na(occupant) & trip_mode<=6, 'passenger', occupant))

# for individual tours: assign driver/passenger randomly (considering adults only)  
  trips3.1 <- trips3 %>%
    mutate(randn    = runif(nrow(trips3), min = 0, max = 1),
           occupant = ifelse(trip_mode > 6, 'noncar',
                      ifelse(joint_indicator == 1, occupant,
                      ifelse(trip_mode == 1 | trip_mode == 2, 'driver',
                      ifelse(person_type != 'Child too young for school' & person_type != 'Student of non-driving age' & vehocc == 2 &
                               randn < driverfactor2 * 1 / vehocc, 'driver',
                      ifelse(person_type != 'Child too young for school' & person_type != 'Student of non-driving age' & vehocc == 3.4 &
                               randn < driverfactor3 * 1 / vehocc, 'driver', 'passenger')))))
           )

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #  NEED TO CALIBRATE drivefactor
  #  parameter to make sure overall 
  #  occupancy rates are correct
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  #  xtabs(data=trips3.1[trips3.1$joint_indicator ==0,], ~occupant + vehocc)
  
  rm(trips2)
  rm(j1)
  rm(j1oldest)
  gc()


#-------------------------------------------------------------
# 4) Assign Trip Departure Minute
#-------------------------------------------------------------

  # Calculate Moving Average Minute Bins
  #......................................
    bins <- trips3.1 %>%
      group_by(depart_period) %>%
      summarise(n=n())
    
    bins$p <- bins$n / sum(bins$n)
    
    minutebins <- bins[rep(1:nrow(bins), each = 30),]
    minutebins$min <- as.numeric(rownames(minutebins))
    minutebins <- minutebins %>%
      mutate(min30 = min - ((depart_period - 1) * 30))
    
    # Moving Average 1:
    minbin <- as.data.table(minutebins)
    minbin[ , ma := (p + shift(p, 1, type="lag") + shift(p, 1, type="lead"))/3]
    minbin[1, ma := (minbin[2, p] + minbin[1, p]) / 2]
    minbin[nrow(minbin), ma := (minbin[nrow(minbin), p] + minbin[nrow(minbin)-1, p]) / 2]
   
    # Moving Average 2 to nMovingAvg:
    for (N in 2:nMovingAvg) {
      minbin[ , ma := (ma + shift(ma, 1, type="lag") + shift(ma, 1, type="lead"))/3]
      minbin[1, ma := (minbin[2, ma] + minbin[1, p]) / 2]
      minbin[nrow(minbin), ma := (minbin[nrow(minbin), p] + minbin[nrow(minbin)-1, ma]) / 2]
    }
    
    plot(minbin$p)
    lines(minbin$ma, col=rainbow(3))
    
    # Calculate cumulative proportion within each bin
    minbindf <- as.data.frame(minbin)
    
    minbindf <- minbindf %>%
      group_by(depart_period) %>% 
      mutate(binprop = ma / sum(ma),
             cumbinprop = cumsum(binprop),
             binname = paste('cumprob', min30, sep='_'))
    
    # Cast data to create lookup table of 30 minutes by bins
    minlookup <- dcast(minbindf, depart_period~binname, value.var='cumbinprop')

  # Assign Trips to Minutes
  #......................................
    trips4 <- left_join(trips3.1, minlookup, by="depart_period")
    
 rm(trips3.1)  
 rm(trips3)
 gc()
 
    set.seed(33333)
    
    trips4$randn1 <- runif(nrow(trips4), min=0, max=1)
    
    trips4 <- trips4 %>%
      mutate(trip_depart_min = ifelse(                      randn1 <= cumprob_1 , -30 + depart_period * 30 + 0 ,
                               ifelse(randn1 > cumprob_1  & randn1 <= cumprob_2 , -30 + depart_period * 30 + 1 ,
                               ifelse(randn1 > cumprob_2  & randn1 <= cumprob_3 , -30 + depart_period * 30 + 2 ,
                               ifelse(randn1 > cumprob_3  & randn1 <= cumprob_4 , -30 + depart_period * 30 + 3 ,
                               ifelse(randn1 > cumprob_4  & randn1 <= cumprob_5 , -30 + depart_period * 30 + 4 ,
                               ifelse(randn1 > cumprob_5  & randn1 <= cumprob_6 , -30 + depart_period * 30 + 5 ,
                               ifelse(randn1 > cumprob_6  & randn1 <= cumprob_7 , -30 + depart_period * 30 + 6 ,
                               ifelse(randn1 > cumprob_7  & randn1 <= cumprob_8 , -30 + depart_period * 30 + 7 ,
                               ifelse(randn1 > cumprob_8  & randn1 <= cumprob_9 , -30 + depart_period * 30 + 8 ,
                               ifelse(randn1 > cumprob_9  & randn1 <= cumprob_10, -30 + depart_period * 30 + 9 ,
                               ifelse(randn1 > cumprob_10 & randn1 <= cumprob_11, -30 + depart_period * 30 + 10,
                               ifelse(randn1 > cumprob_11 & randn1 <= cumprob_12, -30 + depart_period * 30 + 11,
                               ifelse(randn1 > cumprob_12 & randn1 <= cumprob_13, -30 + depart_period * 30 + 12,
                               ifelse(randn1 > cumprob_13 & randn1 <= cumprob_14, -30 + depart_period * 30 + 13,
                               ifelse(randn1 > cumprob_14 & randn1 <= cumprob_15, -30 + depart_period * 30 + 14,
                               ifelse(randn1 > cumprob_15 & randn1 <= cumprob_16, -30 + depart_period * 30 + 15,
                               ifelse(randn1 > cumprob_16 & randn1 <= cumprob_17, -30 + depart_period * 30 + 16,
                               ifelse(randn1 > cumprob_17 & randn1 <= cumprob_18, -30 + depart_period * 30 + 17,
                               ifelse(randn1 > cumprob_18 & randn1 <= cumprob_19, -30 + depart_period * 30 + 18,
                               ifelse(randn1 > cumprob_19 & randn1 <= cumprob_20, -30 + depart_period * 30 + 19,
                               ifelse(randn1 > cumprob_20 & randn1 <= cumprob_21, -30 + depart_period * 30 + 20,
                               ifelse(randn1 > cumprob_21 & randn1 <= cumprob_22, -30 + depart_period * 30 + 21,
                               ifelse(randn1 > cumprob_22 & randn1 <= cumprob_23, -30 + depart_period * 30 + 22,
                               ifelse(randn1 > cumprob_23 & randn1 <= cumprob_24, -30 + depart_period * 30 + 23,
                               ifelse(randn1 > cumprob_24 & randn1 <= cumprob_25, -30 + depart_period * 30 + 24,
                               ifelse(randn1 > cumprob_25 & randn1 <= cumprob_26, -30 + depart_period * 30 + 25,
                               ifelse(randn1 > cumprob_26 & randn1 <= cumprob_27, -30 + depart_period * 30 + 26,
                               ifelse(randn1 > cumprob_27 & randn1 <= cumprob_28, -30 + depart_period * 30 + 27,
                               ifelse(randn1 > cumprob_28 & randn1 <= cumprob_29, -30 + depart_period * 30 + 28,
                               ifelse(randn1 > cumprob_29 & randn1 <= cumprob_30, -30 + depart_period * 30 + 29,
                                      0)))))))))))))))))))))))))))))))
    
    
    ################
    
    hp <- trips4 %>%
      group_by(person_id, depart_period) %>%
      summarise(tripsinbin = n())
    
    tr4 <- left_join(trips4, hp, by=c("person_id", "depart_period"))
    
    bunched <- filter(tr4, tripsinbin>1)
    
    ggplot(data=bunched) + geom_freqpoly(aes(trip_depart_min), binwidth=1)
    
    adj0 <- read_csv("isam/adjusted_schedules_0.csv")
    adj0$hhid_p1 <- adj0$hhid +1
    
    t4_a0 <- left_join(trips4, adj0, by= c("hh_id" = "hhid_p1", "person_num" = "pnum"))
    
    ################
    
    # add random second to departure minute
    set.seed(44444)
    trips4$randn2 <- runif(nrow(trips4), min=0, max=59.9999)
    
    trips4 <- trips4 %>%
      mutate(trip_depart_min = trip_depart_min + randn2/60)
    
    # add arrival time field cause Java code expecting it; MAS, 20160809
    trips4$trip_arrive_min <- trips4$trip_depart_min + trips4$travel_time

    ggplot(data=trips4) + geom_freqpoly(aes(depart_period), binwidth=1)
    ggplot(data=trips4) + geom_freqpoly(aes(trip_depart_min), binwidth=1)
    ggplot(data=trips4) + geom_histogram(aes(trip_depart_min), binwidth=5)
    ggplot(data=trips4[trips4$randVOT<200,]) + geom_histogram(aes(randVOT), binwidth=2)
    
## Uncomment to print hourly frequency tables, for comparison with traffic counts and travel survey
#     hr <- ggplot(data=trips4) + geom_histogram(aes(trip_depart_min), breaks=seq(180,1620,by=60))
#     hrdata <- ggplot_build(hr)
#     hrdata$data[[1]]
#     
#     
#     hrauto <- ggplot(data=trips4[trips4$trip_mode<=6,]) + geom_histogram(aes(trip_depart_min), breaks=seq(180,1620,by=60))
#     hrautodata <- ggplot_build(hrauto)
#     hrautodata$data[[1]]

  gc()
    
    
#-------------------------------------------------------------
# Sort Trips in Tour Order, Add More Fields for ISAM, Write csv out
#-------------------------------------------------------------
# commented out to maintain chron order, MAS 20160826
#trips4 <- trips4[order(trips4$hh_id, trips4$person_num, trips4$tour_start_period, trips4$inbound, trips4$stop_id), ]
trips4 <- trips4[order(trips4$hh_id, trips4$person_num, trips4$trip_id), ]

# check for correct chronolical order of trips (by hh_id, person_num, trip_id)
# get lagging depart_period for each trip for each person
t4 <- 
  trips4 %>%
  group_by(person_id) %>%
  mutate(lag.depart_period = lag(depart_period, 1),
         period_diff = depart_period - lag.depart_period)
# t5 should be empty
t5 <- 
  t4 %>% 
  filter(depart_period<lag.depart_period)

# create uniqueid; unique within each hh
trips4 <-
  trips4 %>%
  group_by(hh_id) %>%
  mutate(uniqueid = row_number())

# assign integer values for orig_purpose, dest_purpose, and trip_mode
# orig purposes
trips4 <- 
  trips4 %>%
  mutate(origPurp = ifelse(orig_purpose =='Home', 0,
                     ifelse(orig_purpose =='work'|orig_purpose =='Work'|orig_purpose =='atwork', 1,
                     ifelse(orig_purpose =='university', 2,
                     ifelse(orig_purpose =='school', 3,
                     ifelse(orig_purpose =='escort', 42,
                     ifelse(orig_purpose =='shopping', 5,
                     ifelse(orig_purpose =='othmaint', 6,
                     ifelse(orig_purpose =='eatout', 7,
                     ifelse(orig_purpose =='social', 8,
                     ifelse(orig_purpose =='othdiscr', 9,
                            -1)))))))))))

# dest purposes
trips4 <- 
  trips4 %>%
  mutate(destPurp = ifelse(dest_purpose =='Home', 0,
                     ifelse(dest_purpose =='work'|dest_purpose =='Work'|dest_purpose =='atwork', 1,
                     ifelse(dest_purpose =='university', 2,
                     ifelse(dest_purpose =='school', 3,
                     ifelse(dest_purpose =='escort', 42,
                     ifelse(dest_purpose =='shopping', 5,
                     ifelse(dest_purpose =='othmaint', 6,
                     ifelse(dest_purpose =='eatout', 7,
                     ifelse(dest_purpose =='social', 8,
                     ifelse(dest_purpose =='othdiscr', 9,
                            -1)))))))))))

# trip modes, consistent with CTRAMP-2
# trips4 <- 
#   trips4 %>%
#   mutate(mode = ifelse(trip_mode ==1|trip_mode ==2, 1,
#                ifelse((trip_mode ==3|trip_mode ==4) & occupant=='driver', 2,
#                ifelse((trip_mode ==5|trip_mode ==6) & occupant=='driver', 3,
#                ifelse((trip_mode > 2trip_mode < 7) & occupant=='passenger',4,
#                ifelse(trip_mode ==9,5,
#                ifelse(trip_mode ==13,6,
#                ifelse(trip_mode ==11,7,
#                ifelse(trip_mode ==10,8,
#                ifelse(trip_mode ==14,9,
#                ifelse(trip_mode ==12,10,
#                ifelse(trip_mode ==7,11,
#                ifelse(trip_mode ==8,12,
#                ifelse(trip_mode ==15,14,
#                       -1))))))))))))))

# CTRAMP-2 mode codes for reference
# public static final int SOV_MODE_INDEX = 1;
# public static final int HOV2_DRV_MODE_INDEX = 2;
# public static final int HOV3_DRV_MODE_INDEX = 3;       
# public static final int HOV_PASS_MODE_INDEX = 4;
# public static final int WLK_CONV_MODE_INDEX = 5;
# public static final int KNR_CONV_MODE_INDEX = 6;
# public static final int PNR_CONV_MODE_INDEX = 7;
# public static final int WLK_PREM_MODE_INDEX = 8;
# public static final int KNR_PREM_MODE_INDEX = 9;
# public static final int PNR_PREM_MODE_INDEX = 10;
# public static final int WALK_MODE_INDEX = 11;
# public static final int BIKE_MODE_INDEX = 12;
# public static final int TAXI_MODE_INDEX = 13;
# public static final int SCHL_BUS_MODE_INDEX = 14;

trips4$trip_mode[trips4$occupant=='passenger'] <- 16

# remove unneeded fields    
tripsout <- subset(trips4, select=-c(norm, randn, randn1, randn2, hh_inc_bin,
                                     cumprob_1 ,               
                                     cumprob_10,                cumprob_11,                cumprob_12,                cumprob_13,              
                                     cumprob_14,                cumprob_15,                cumprob_16,                cumprob_17,              
                                     cumprob_18,                cumprob_19,                cumprob_2 ,                cumprob_20,              
                                     cumprob_21,                cumprob_22,                cumprob_23,                cumprob_24,              
                                     cumprob_25,                cumprob_26,                cumprob_27,                cumprob_28,              
                                     cumprob_29,                cumprob_3 ,                cumprob_30,                cumprob_4 ,              
                                     cumprob_5 ,                cumprob_6 ,                cumprob_7 ,                cumprob_8 ,              
                                     cumprob_9 )) 

# integerize fields for ISAM, mas 20160907
tripsout <- 
  tripsout %>% 
  mutate(origPurp = as.integer(origPurp),
         destPurp = as.integer(destPurp),
         trip_mode = as.integer(trip_mode),
         joint_indicator = as.integer(joint_indicator),
         joint_trip_id = as.integer(joint_trip_id),
         joint_tour_id = as.integer(joint_tour_id),
         vehocc = as.integer(vehocc))

# index household ids starting at 0
colnames(tripsout)[names(tripsout) == "hh_id"] <- "hhid"
tripsout$hhid <- as.integer(tripsout$hhid - 1)

# rename person_num, depart minute, arrive minute, and person trip id
colnames(tripsout)[names(tripsout) == "person_num"] <- "pnum"
colnames(tripsout)[names(tripsout) == "trip_depart_min"] <- "finalDepartMinute"
colnames(tripsout)[names(tripsout) == "trip_arrive_min"] <- "finalArriveMinute"
colnames(tripsout)[names(tripsout) == "trip_id"] <- "personTripNum"

# remove all unneeded fields; trying to debug why entire file cannot be processed by ISAM
tripsout <- subset(tripsout, select=-c(person_id, tour_id, stop_id, inbound,
                           tour_category, tour_purpose, orig_purpose, dest_purpose, 
                           parking_taz, depart_period, tour_mode,
                           num_participants, tour_participants, tour_start_period,
                           tour_id_uniq, 
                           orig_purpose_start_period, home_taz, 
                           hh_income, hh_fp_choice, 
                           age, person_type, activity_pattern, tour_mode_name, trip_mode_name,
                           travel_time, joint_indicator, joint_tour_id, vehocc, avgVOT, 
                           occupant))

write_csv(tripsout, "output/TripList.csv")  


#-------------------------------------------------------------
# Read, modify, and write hhData and personData
#-------------------------------------------------------------
# read hh and person data files
hhData <- read_csv("Inputs/hhData.csv")
personData <- read_csv("Inputs/personData.csv")

# index household ids starting at 0
colnames(hhData)[names(hhData) == "hh_id"] <- "hhid"
colnames(personData)[names(personData) == "hh_id"] <- "hhid"
hhData$hhid <- as.integer(hhData$hhid - 1)
personData$hhid <- as.integer(personData$hhid - 1)

# integerize pers_type field
personData <- 
  personData %>%
  mutate(persType = ifelse(type =='Full-time worker', 1,
                     ifelse(type =='Part-time worker', 2,
                     ifelse(type =='University student', 3,
                     ifelse(type =='Non-worker', 4,
                     ifelse(type =='Retired', 5,
                     ifelse(type =='Student of driving age', 6,
                     ifelse(type =='Student of non-driving age', 7,
                     ifelse(type =='Child too young for school', 8,
                     -1)))))))))

# integerize daily activity pattern
personData <-
  personData %>%
  mutate(dailyActivityPattern = ifelse(activity_pattern == "M", 1,
                                ifelse(activity_pattern == "N", 2,
                                3)))

# integerize fields for ISAM
personData <- 
  personData %>% 
  mutate(persType = as.integer(persType),
         dailyActivityPattern = as.integer(dailyActivityPattern))

# rename person_num
colnames(personData)[names(personData) == "person_num"] <- "pnum"

# sort hh and person files by hh_id
hhData <- hhData[order(hhData$hhid), ]
personData <- personData[order(personData$hhid), ]

# write out modified person data file
write_csv(hhData, "output/HouseholdList.csv")  
write_csv(personData, "output/PersonList.csv")  


#-------------------------------------------------------------
# create sample trip, hh, and person data for ISAM testing
#-------------------------------------------------------------
# pick first 100 households appearing in tripsout
hh_list <- sqldf("select distinct(hhid) as hhid from hhData where hhid=137")

# select hh records person records, and trips for those X households
hhData_sample <- left_join(hh_list, hhData)
personData_sample <- left_join(hh_list, personData)
tripsout_sample <- left_join(hh_list, tripsout)
tripsout_sample <- tripsout_sample %>% filter(!(is.na(tripsout_sample$tour_id)))

# write out sample files
write_csv(hhData_sample, "output/sample_HouseholdList.csv")  
write_csv(personData_sample, "output/sample_PersonList.csv")  
write_csv(tripsout_sample, "output/sample_TripList.csv")  

gc()

#table(floor(tripsout$finalDepartMinute/60)+3, tripsout$trip_mode)

#nointras <- tripsout %>% filter(orig_taz!=dest_taz)

#table(floor(nointras$finalDepartMinute/60)+3, nointras$trip_mode)


#----------------------------------------------------------------------------------------
# Process ext-truck file to smooth departure time curve and add random second to dep time
#----------------------------------------------------------------------------------------

trk_ext <- read_csv("Inputs/ARC_TRK-EXT.csv")

ggplot(data=trk_ext) + geom_histogram(aes(dephr), binwidth=1)
ggplot(data=trk_ext) + geom_freqpoly(aes(depmin), binwidth=1)

nMovingAvg = 700 # Number of moving average iterations for minute time bins
#-------------------------------------------------------------
# T1. Assign Truck/External Trip Departure Minute
#-------------------------------------------------------------

# Calculate Moving Average Minute Bins
#......................................
bins <- trk_ext %>%
  group_by(dephr) %>%
  summarise(n=n())

bins$p <- bins$n / sum(bins$n)

minutebins <- bins[rep(1:nrow(bins), each = 60),]
minutebins$min <- as.numeric(rownames(minutebins))
minutebins <- minutebins %>%
  mutate(min60 = min - ((dephr - 3) * 60))

# Moving Average 1:
minbin <- as.data.table(minutebins)
minbin[ , ma := (p + shift(p, 1, type="lag") + shift(p, 1, type="lead"))/3]
minbin[1, ma := (minbin[2, p] + minbin[1, p]) / 2]
minbin[nrow(minbin), ma := (minbin[nrow(minbin), p] + minbin[nrow(minbin)-1, p]) / 2]

# Moving Average 2 to nMovingAvg:
for (N in 2:nMovingAvg) {
  minbin[ , ma := (ma + shift(ma, 1, type="lag") + shift(ma, 1, type="lead"))/3]
  minbin[1, ma := (minbin[2, ma] + minbin[1, p]) / 2]
  minbin[nrow(minbin), ma := (minbin[nrow(minbin), p] + minbin[nrow(minbin)-1, ma]) / 2]
}

plot(minbin$p)
lines(minbin$ma, col=rainbow(3))

# Calculate cumulative proportion within each bin
minbindf <- as.data.frame(minbin)

minbindf <- minbindf %>%
  group_by(dephr) %>% 
  mutate(binprop = ma / sum(ma),
         cumbinprop = cumsum(binprop),
         binname = paste('cumprob', min60, sep='_'))

# Cast data to create lookup table of 30 minutes by bins
minlookup <- dcast(minbindf, dephr~binname, value.var='cumbinprop')

# Assign Trips to Minutes
#......................................
trk_ext1 <- left_join(trk_ext, minlookup, by="dephr")

set.seed(55555)

trk_ext1$randn1 <- runif(nrow(trk_ext1), min=0, max=1)

trk_ext1 <- trk_ext1 %>%
           mutate(depmin =  ifelse(randn1 <= cumprob_1 , dephr * 60 + 0 ,
                            ifelse(randn1 > cumprob_1  & randn1 <= cumprob_2 , dephr * 60 + 1 ,
                            ifelse(randn1 > cumprob_2  & randn1 <= cumprob_3 , dephr * 60 + 2 ,
                            ifelse(randn1 > cumprob_3  & randn1 <= cumprob_4 , dephr * 60 + 3 ,
                            ifelse(randn1 > cumprob_4  & randn1 <= cumprob_5 , dephr * 60 + 4 ,
                            ifelse(randn1 > cumprob_5  & randn1 <= cumprob_6 , dephr * 60 + 5 ,
                            ifelse(randn1 > cumprob_6  & randn1 <= cumprob_7 , dephr * 60 + 6 ,
                            ifelse(randn1 > cumprob_7  & randn1 <= cumprob_8 , dephr * 60 + 7 ,
                            ifelse(randn1 > cumprob_8  & randn1 <= cumprob_9 , dephr * 60 + 8 ,
                            ifelse(randn1 > cumprob_9  & randn1 <= cumprob_10, dephr * 60 + 9 ,
                            ifelse(randn1 > cumprob_10 & randn1 <= cumprob_11, dephr * 60 + 10,
                            ifelse(randn1 > cumprob_11 & randn1 <= cumprob_12, dephr * 60 + 11,
                            ifelse(randn1 > cumprob_12 & randn1 <= cumprob_13, dephr * 60 + 12,
                            ifelse(randn1 > cumprob_13 & randn1 <= cumprob_14, dephr * 60 + 13,
                            ifelse(randn1 > cumprob_14 & randn1 <= cumprob_15, dephr * 60 + 14,
                            ifelse(randn1 > cumprob_15 & randn1 <= cumprob_16, dephr * 60 + 15,
                            ifelse(randn1 > cumprob_16 & randn1 <= cumprob_17, dephr * 60 + 16,
                            ifelse(randn1 > cumprob_17 & randn1 <= cumprob_18, dephr * 60 + 17,
                            ifelse(randn1 > cumprob_18 & randn1 <= cumprob_19, dephr * 60 + 18,
                            ifelse(randn1 > cumprob_19 & randn1 <= cumprob_20, dephr * 60 + 19,
                            ifelse(randn1 > cumprob_20 & randn1 <= cumprob_21, dephr * 60 + 20,
                            ifelse(randn1 > cumprob_21 & randn1 <= cumprob_22, dephr * 60 + 21,
                            ifelse(randn1 > cumprob_22 & randn1 <= cumprob_23, dephr * 60 + 22,
                            ifelse(randn1 > cumprob_23 & randn1 <= cumprob_24, dephr * 60 + 23,
                            ifelse(randn1 > cumprob_24 & randn1 <= cumprob_25, dephr * 60 + 24,
                            ifelse(randn1 > cumprob_25 & randn1 <= cumprob_26, dephr * 60 + 25,
                            ifelse(randn1 > cumprob_26 & randn1 <= cumprob_27, dephr * 60 + 26,
                            ifelse(randn1 > cumprob_27 & randn1 <= cumprob_28, dephr * 60 + 27,
                            ifelse(randn1 > cumprob_28 & randn1 <= cumprob_29, dephr * 60 + 28,
                            ifelse(randn1 > cumprob_29 & randn1 <= cumprob_30, dephr * 60 + 29,
                            ifelse(randn1 > cumprob_30 & randn1 <= cumprob_31, dephr * 60 + 30,
                            ifelse(randn1 > cumprob_31 & randn1 <= cumprob_32, dephr * 60 + 31,
                            ifelse(randn1 > cumprob_32 & randn1 <= cumprob_33, dephr * 60 + 32,
                            ifelse(randn1 > cumprob_33 & randn1 <= cumprob_34, dephr * 60 + 33,
                            ifelse(randn1 > cumprob_34 & randn1 <= cumprob_35, dephr * 60 + 34,
                            ifelse(randn1 > cumprob_35 & randn1 <= cumprob_36, dephr * 60 + 35,
                            ifelse(randn1 > cumprob_36 & randn1 <= cumprob_37, dephr * 60 + 36,
                            ifelse(randn1 > cumprob_37 & randn1 <= cumprob_38, dephr * 60 + 37,
                            ifelse(randn1 > cumprob_38 & randn1 <= cumprob_39, dephr * 60 + 38,
                            ifelse(randn1 > cumprob_39 & randn1 <= cumprob_40, dephr * 60 + 39,
                            ifelse(randn1 > cumprob_40 & randn1 <= cumprob_41, dephr * 60 + 40,
                            ifelse(randn1 > cumprob_41 & randn1 <= cumprob_42, dephr * 60 + 41,
                            ifelse(randn1 > cumprob_42 & randn1 <= cumprob_43, dephr * 60 + 42,
                            ifelse(randn1 > cumprob_43 & randn1 <= cumprob_44, dephr * 60 + 43,
                            ifelse(randn1 > cumprob_44 & randn1 <= cumprob_45, dephr * 60 + 44,
                            ifelse(randn1 > cumprob_45 & randn1 <= cumprob_46, dephr * 60 + 45,
                            ifelse(randn1 > cumprob_46 & randn1 <= cumprob_47, dephr * 60 + 46,
                            ifelse(randn1 > cumprob_47 & randn1 <= cumprob_48, dephr * 60 + 47,
                            ifelse(randn1 > cumprob_48 & randn1 <= cumprob_49, dephr * 60 + 48,
                                   0))))))))))))))))))))))))))))))))))))))))))))))))))

trk_ext1 <- trk_ext1 %>%
  mutate(depmin = ifelse(randn1 > cumprob_49 & randn1 <= cumprob_50, dephr * 60 + 49,
                  ifelse(randn1 > cumprob_50 & randn1 <= cumprob_51, dephr * 60 + 50,
                  ifelse(randn1 > cumprob_51 & randn1 <= cumprob_52, dephr * 60 + 51,
                  ifelse(randn1 > cumprob_52 & randn1 <= cumprob_53, dephr * 60 + 52,
                  ifelse(randn1 > cumprob_53 & randn1 <= cumprob_54, dephr * 60 + 53,
                  ifelse(randn1 > cumprob_54 & randn1 <= cumprob_55, dephr * 60 + 54,
                  ifelse(randn1 > cumprob_55 & randn1 <= cumprob_56, dephr * 60 + 55,
                  ifelse(randn1 > cumprob_56 & randn1 <= cumprob_57, dephr * 60 + 56,
                  ifelse(randn1 > cumprob_57 & randn1 <= cumprob_58, dephr * 60 + 57,
                  ifelse(randn1 > cumprob_58 & randn1 <= cumprob_59, dephr * 60 + 58,
                  ifelse(randn1 > cumprob_59 & randn1 <= cumprob_60, dephr * 60 + 59, depmin))))))))))))

# add random second to departure minute
set.seed(66666)
trk_ext1$randn <- runif(nrow(trk_ext1), min=0, max=59.9999)

trk_ext1 <- trk_ext1 %>%
  mutate(depmin = depmin + randn/60)

ggplot(data=trk_ext1) + geom_freqpoly(aes(depmin), binwidth=1)

trk_ext1 <- subset(trk_ext1, select=-c(randn, randn1, 
                                       cumprob_1, cumprob_2, cumprob_3, cumprob_4, cumprob_5, cumprob_6, cumprob_7, cumprob_8, cumprob_9, cumprob_10,
                                       cumprob_11, cumprob_12, cumprob_13, cumprob_14, cumprob_15, cumprob_16, cumprob_17, cumprob_18, cumprob_19, cumprob_20,
                                       cumprob_21, cumprob_22, cumprob_23, cumprob_24, cumprob_25, cumprob_26, cumprob_27, cumprob_28, cumprob_29, cumprob_30,
                                       cumprob_31, cumprob_32, cumprob_33, cumprob_34, cumprob_35, cumprob_36, cumprob_37, cumprob_38, cumprob_39, cumprob_40,
                                       cumprob_41, cumprob_42, cumprob_43, cumprob_44, cumprob_45, cumprob_46, cumprob_47, cumprob_48, cumprob_49, cumprob_50,
                                       cumprob_51, cumprob_52, cumprob_53, cumprob_54, cumprob_55, cumprob_56, cumprob_57, cumprob_58, cumprob_59, cumprob_60))

write_csv(trk_ext1, "output/ARC_TRK-EXT_dep_second.csv")
