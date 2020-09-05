library(dplyr)
library(tidyr)
library(data.table)
library(corrplot)
library(reshape)
library(plyr)
library(reshape2)
library(readr)
library(tictoc)
toc()
setwd("C:/Users/X1/Documents")
# read csv file
data_v <- read.csv("C:/Users/X1/Dropbox/X2/2018/2018.02.02- RNN and clustering/reconstracted-I-80.csv") 
# sort file based on frame number, location, and vehicle ID
data_v <- data_v[order(data_v$Frame, data_v$Local_Y, data_v$vehicle_ID, data_v$Follower_ID, data_v$Leader_ID),] 

# filter vehicle to passnger cars which travel in lanes 2 to 5
select_v <- subset(data_v[,c("Frame","Local_Y","vehicle_ID","Lane_ID", "v_Class", "Follower_ID", "Leader_ID"),], v_Class == 2) #only LDV
select_v <- subset(select_v[,c("Frame","Local_Y","vehicle_ID","Lane_ID","Follower_ID", "Leader_ID"),], Lane_ID %in% 2:5) #for other lanes will be adjusted
select_v <- subset(select_v[,c("Frame","Local_Y","vehicle_ID","Lane_ID","Follower_ID", "Leader_ID"),], Follower_ID > 0 & Leader_ID > 0) #remove data if follower or leader data is not available

# find vehicles with no lane changeing (selected vehciles)
ln1 <- ddply(select_v, c('vehicle_ID'), summarize,lane1=min(Lane_ID)) 
ln2 <- ddply(select_v, c('vehicle_ID'), summarize,lane1=max(Lane_ID)) 
select_vehcile <- as.data.frame(rep(0,nrow(ln2)))
names(select_vehcile) <- "vehicle_ID"
for(i in 1:nrow(ln2)){
  if(ln1$lane1[i] == ln2$lane1[i]){
    select_vehcile$vehicle_ID [i] <- ln1$vehicle_ID[i]}
  else{
    select_vehcile$vehicle_ID [i] <- NA
  }
}

select_vehcile <- select_vehcile[complete.cases(select_vehcile), ] 
select_v1 <- subset(select_v[,c("Frame","Local_Y","vehicle_ID","Lane_ID"),], select_v$vehicle_ID %in% select_vehcile)

# filter vehicles to vehicles in lane 1 to 5 (potential surrounding vehicles)
data_v_lane <- subset(data_v[,c("Frame","Local_Y","vehicle_ID","Lane_ID", "v_Class"),], Lane_ID %in% 1:6)

# Find HDVs and mark them
data_v_lane <- mutate(data_v_lane, vehicle_ID = ifelse(v_Class == 2, vehicle_ID, -2))
data_v_lanes <- subset(data_v_lane[,c("Frame","Local_Y","vehicle_ID","Lane_ID"),])


# create a data frame to hold main information of vehicles
data_vehicles <- data.frame(vehicle_ID = as.numeric(),
                            Frame = as.numeric(),
                            Lane_ID = as.numeric(),
                            Local_Y_ID = as.numeric(),
                            
                            vehicle_ID_right = as.numeric(),
                            Local_Y_right = as.numeric(),
                            
                            vehicle_ID_left = as.numeric(),
                            Local_Y_left = as.numeric()
                            )
  
for(i in 1:nrow(select_v1)){
  vehicle <- select_v1$vehicle_ID[i]
  time <- select_v1$Frame[i]
  lane <- select_v1$Lane_ID[i]
  y <- select_v1$Local_Y[i]
  data_subset <- subset(data_v_lanes, ((Frame == time) & (Lane_ID == (lane+1))))
  data_subset2 <- subset(data_v_lanes, ((Frame == time) & (Lane_ID == (lane-1))))
  
  tt= 0  
  if(nrow(data_subset)>=1){ # shows if there is any matching vehicle on the right lane
    for(j in 1:nrow(data_subset)){   #searching for the vehicle on the right lane
      if(data_subset$Local_Y[j] > y){
        vehicle_right <- data_subset$vehicle_ID[j]
        vehicle_right_y <- data_subset$Local_Y[j]
        tt= 1
        break
      }
    }
  }
  #If there is no vehicle on the right lane forget this line of data
  if (tt == 0 ){  
    next
  }
  #if the vheilce on the right lane is HDV, remove the line
  if (vehicle_right == -2){ next}
  
  tt= 0  
  #searching for any matching vehicle on the left lane
  if(nrow(data_subset2)>=1){    
    for(k in 1:nrow(data_subset2)){
      if(data_subset2$Local_Y[k] > y){
        vehicle_left <- data_subset2$vehicle_ID[k]
        vehicle_left_y <- data_subset2$Local_Y[k]
        tt= 1
        break
      }
    }
  }
  if (tt == 0){   #If there is any vehicle on the left lane forget this line of data
    next
  }
  #if the vheilce on the left lane is HDV, remove the line
  if (vehicle_left == -2){ next}
  
  # Add and marge surrounding vehicles attributes  
  data1 <- as.data.frame(cbind(vehicle, time, lane, y, vehicle_right, vehicle_right_y))
  data2 <- as.data.frame(cbind(vehicle_left,vehicle_left_y))
  
  names(data1) <- c("vehicle_ID","Frame","Lane_ID","Local_Y_ID","vehicle_ID_right","Local_Y_right")
  names(data2) <- c("vehicle_ID_left","Local_Y_left")
  
  data1 <- as.data.frame(transform(data1, 
                     vehicle_ID = as.numeric(as.character(vehicle_ID)), 
                     Frame <- as.numeric(as.character(Frame)),
                     Lane_ID = as.numeric(as.character(Lane_ID)), 
                     Local_Y_ID = as.numeric(as.character(Local_Y_ID)), 
                     
                     vehicle_ID_right = as.numeric(as.character(vehicle_ID_right)),
                     Local_Y_right = as.numeric(as.character(Local_Y_right)))[,1:6],stringsAsFactors = FALSE) 
  
  
  data2 <- as.data.frame(transform(data2,                      
                                   vehicle_ID_left = as.numeric(as.character(vehicle_ID_left)),
                                   Local_Y_left = as.numeric(as.character(Local_Y_left)))[,1:2],stringsAsFactors = FALSE)
  
 data_vehicles <- as.data.frame(rbind(data_vehicles,cbind(data1[1,],data2[1,])))
 if((i %% 100) == 1){ #print codes progress every 100 cycles
   print(i)}
}
toc()

# Add extra attributes to the restructured dataset

# subject vehicle data
vehicles_around_vars <- left_join(data_vehicles, data_v[,c("Frame","vehicle_ID","length", "Speed", "Acceleration", "Leader_ID", "Follower_ID")]
                                                        , by=c("Frame"="Frame","vehicle_ID"="vehicle_ID"))
# Left vehicle data
vehicles_around_vars <- left_join(vehicles_around_vars,data_v[,c("Frame","vehicle_ID","length", "Speed", "Acceleration")], 
                                  by=c("Frame"="Frame","vehicle_ID_left"="vehicle_ID"))
# Right vehicle data
vehicles_around_vars <- left_join(vehicles_around_vars,data_v[,c("Frame","vehicle_ID","length", "Speed","Acceleration")],
                                  by=c("Frame"="Frame","vehicle_ID_right"="vehicle_ID"))
# leading vehicle data
vehicles_around_vars <- left_join(vehicles_around_vars,data_v[,c("Frame","vehicle_ID","Local_Y","length", "v_Class", "Speed","Acceleration", "Leader_ID")],
                                  by=c("Frame"="Frame","Leader_ID"="vehicle_ID"))
# 2nd ahead vehicle data
vehicles_around_vars <- left_join(vehicles_around_vars,data_v[,c("Frame","vehicle_ID","Local_Y","length", "v_Class", "Speed","Acceleration")],
                                  by=c("Frame"="Frame","Leader_ID.y"="vehicle_ID"))
# Following vehicle (back vehicle)
vehicles_around_vars <- left_join(vehicles_around_vars,data_v[,c("Frame","vehicle_ID","Local_Y","length", "v_Class", "Speed","Acceleration")],
                                  by=c("Frame"="Frame","Follower_ID"="vehicle_ID"))


names(vehicles_around_vars) <- c("subject_ID","Frame","subject_Lane","subject_Local_Y",
                                 "right_ID","right_Local_Y",
                                 "left_ID","left_Local_Y",
                                 "subject_length","subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                 "left_length",  "left_speed",   "left_Acc",  
                                 "right_length", "right_speed",  "right_Acc",
                                 "leading_Y", "leading_length", "leading_Class","leading_speed", "leading_Acc", 
                                 "head2_ID", "head2_Y","head2_length", "head2_Class","head2_speed", "head2_Acc",
                                 "back_Y","back_length", "back_Class","back_speed", "back_Acc"
                                   )

# Remove rows with class 3 surrounding vehicles or no surrounding vehicle
all_vehicles_around <- subset(vehicles_around_vars[,c("subject_ID","Frame","subject_Lane","subject_Local_Y",
                                                       "right_ID","right_Local_Y",
                                                       "left_ID","left_Local_Y",
                                                       "subject_length","subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                                       "left_length",  "left_speed",   "left_Acc",  
                                                       "right_length", "right_speed",  "right_Acc",
                                                       "leading_Y", "leading_length", "leading_Class","leading_speed", "leading_Acc", 
                                                       "head2_ID", "head2_Y","head2_length", "head2_Class","head2_speed", "head2_Acc",
                                                       "back_Y","back_length", "back_Class","back_speed", "back_Acc"),], 
                               leading_Class==2 & head2_Class==2 & back_Class==2  & head2_ID > 0 )

# Generate new attributes
all_vehicles_around <- all_vehicles_around[order(all_vehicles_around$subject_ID, all_vehicles_around$Frame),] 
all_vehicles_around <- mutate(all_vehicles_around, left_speed_diff = subject_speed - left_speed, right_speed_diff = subject_speed - right_speed, 
                              leading_speed_diff = subject_speed - leading_speed, head2_speed_diff = subject_speed - head2_speed, 
                              back_speed_diff = subject_speed - back_speed, 
                              left_headway = left_Local_Y - subject_Local_Y - (subject_length + left_length)/2, right_headway = right_Local_Y - subject_Local_Y - (subject_length + right_length)/2,
                              leading_headway = leading_Y - subject_Local_Y - (subject_length + leading_length)/2, head2_headway = head2_Y - subject_Local_Y - (subject_length + head2_length)/2,
                              back_headway = subject_Local_Y - back_Y  - (subject_length + back_length)/2)
all_vehicles_around <- transform(all_vehicles_around, subject_Acc_1 = lag(subject_Acc)) 
all_vehicles_around <- transform(all_vehicles_around, subject_Acc_2 = lag(subject_Acc_1)) 
all_vehicles_around <- transform(all_vehicles_around, subject_Acc_3 = lag(subject_Acc_2))
all_vehicles_around <- transform(all_vehicles_around, subject_Acc_4 = lag(subject_Acc_3)) 
all_vehicles_around <- transform(all_vehicles_around, traget_value = lead(subject_Acc)) 
all_vehicles_around <- subset(all_vehicles_around[,c("subject_ID","Frame","subject_Lane","subject_Local_Y",
                                                     "right_ID","right_Local_Y",
                                                     "left_ID","left_Local_Y",
                                                     "subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                                     "left_speed",   "left_Acc",  
                                                     "right_speed",  "right_Acc",
                                                     "leading_Y", "leading_speed", "leading_Acc", 
                                                     "head2_ID", "head2_Y", "head2_speed", "head2_Acc",
                                                     "back_Y","back_speed", "back_Acc", 
                                                     "left_speed_diff", "right_speed_diff", "leading_speed_diff", "head2_speed_diff","back_speed_diff", 
                                                     "left_headway", "right_headway", "leading_headway", "head2_headway", "back_headway",
                                                     "subject_Acc_1", "subject_Acc_2", "subject_Acc_3", "subject_Acc_4", "traget_value"),])
 

#************************* clean up between chanaging vehicle or disrupted periods 
tic()
all_vehicles_batch <- data.frame(
                             subject_ID =as.numeric() ,Frame=as.numeric(),subject_Lane=as.numeric(),subject_Local_Y=as.numeric(),
                             right_ID =as.numeric() ,right_Local_Y =as.numeric(),
                             left_ID =as.numeric(), left_Local_Y=as.numeric(),
                             subject_speed =as.numeric(), subject_Acc=as.numeric(), leading_ID=as.numeric(), follower_ID=as.numeric(),
                             left_speed=as.numeric(),left_Acc= as.numeric(),  
                             right_speed=as.numeric(),right_Acc=as.numeric(),
                             leading_Y=as.numeric(), leading_speed=as.numeric(), leading_Acc=as.numeric(), 
                             head2_ID=as.numeric(), head2_Y=as.numeric(), head2_speed=as.numeric(), head2_Acc=as.numeric(),
                             back_Y=as.numeric(),back_speed=as.numeric(), back_Acc=as.numeric(), 
                             left_speed_diff=as.numeric(), right_speed_diff=as.numeric(), leading_speed_diff=as.numeric(), 
                             head2_speed_diff=as.numeric(),back_speed_diff=as.numeric(), 
                             left_headway=as.numeric(), right_headway=as.numeric(), leading_headway=as.numeric(), 
                             head2_headway=as.numeric(), back_headway=as.numeric(),
                             subject_Acc_1=as.numeric(), subject_Acc_2=as.numeric(), subject_Acc_3=as.numeric(), 
                             subject_Acc_4=as.numeric(), traget_value=as.numeric())

batch_size <- 120
i <- 5
while (i < nrow(all_vehicles_around) - batch_size){
  if (all_vehicles_around$subject_ID[i] == all_vehicles_around$subject_ID[(i+ batch_size -1)] & #vehicle has not changed within a batch
      all_vehicles_around$Frame[i]== all_vehicles_around$Frame[i-4] + 4 & #remove previous 4 acceration movement between batches
      all_vehicles_around$Frame[i+ batch_size-1] == all_vehicles_around$Frame[i+ batch_size] -1 & # taget value adjustment
      all_vehicles_around$Frame[i]== (all_vehicles_around$Frame[i+batch_size-1] - batch_size + 1) # no interruption inside a batch
     ){
    for (j in 0:(batch_size-1)){
      all_vehicles_batch <- as.data.frame(rbind(all_vehicles_batch, all_vehicles_around[i+j,]))
    }
    i <- i+ batch_size-1
  }

  i <- i +1 
}
toc()

# Vehicles in lane #2 (left most lane)
vehicle_lane_2 <- subset(all_vehicles_batch[,c("subject_ID","Frame","subject_Local_Y",
                                                 "right_ID","right_Local_Y",
                                                 "left_ID","left_Local_Y",
                                                 "subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                                 "left_speed",   "left_Acc",  
                                                 "right_speed",  "right_Acc",
                                                 "leading_Y", "leading_speed", "leading_Acc", 
                                                 "head2_ID", "head2_Y", "head2_speed", "head2_Acc",
                                                 "back_Y","back_speed", "back_Acc", 
                                                 "left_speed_diff", "right_speed_diff", "leading_speed_diff", "head2_speed_diff","back_speed_diff", 
                                                 "left_headway", "right_headway", "leading_headway", "head2_headway", "back_headway",
                                                 "subject_Acc_1", "subject_Acc_2", "subject_Acc_3", "subject_Acc_4", "traget_value"),], 
                                                 all_vehicles_batch$subject_Lane == 2)

# Vehicles in lane #3 & 4 (middle lanes)
vehicle_lane_34 <- subset(all_vehicles_batch[,c("subject_ID","Frame","subject_Local_Y",
                                               "right_ID","right_Local_Y",
                                               "left_ID","left_Local_Y",
                                               "subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                               "left_speed",   "left_Acc",  
                                               "right_speed",  "right_Acc",
                                               "leading_Y", "leading_speed", "leading_Acc", 
                                               "head2_ID", "head2_Y", "head2_speed", "head2_Acc",
                                               "back_Y","back_speed", "back_Acc", 
                                               "left_speed_diff", "right_speed_diff", "leading_speed_diff", "head2_speed_diff","back_speed_diff", 
                                               "left_headway", "right_headway", "leading_headway", "head2_headway", "back_headway",
                                               "subject_Acc_1", "subject_Acc_2", "subject_Acc_3", "subject_Acc_4", "traget_value"),], 
                                              all_vehicles_batch$subject_Lane %in% 3:4)

# Vehicles in lane #5 (right most lane)
vehicle_lane_5 <- subset(all_vehicles_batch[,c("subject_ID","Frame","subject_Local_Y",
                                               "right_ID","right_Local_Y",
                                               "left_ID","left_Local_Y",
                                               "subject_speed", "subject_Acc", "leading_ID", "follower_ID",
                                               "left_speed",   "left_Acc",  
                                               "right_speed",  "right_Acc",
                                               "leading_Y", "leading_speed", "leading_Acc", 
                                               "head2_ID", "head2_Y", "head2_speed", "head2_Acc",
                                               "back_Y","back_speed", "back_Acc", 
                                               "left_speed_diff", "right_speed_diff", "leading_speed_diff", "head2_speed_diff","back_speed_diff", 
                                               "left_headway", "right_headway", "leading_headway", "head2_headway", "back_headway",
                                               "subject_Acc_1", "subject_Acc_2", "subject_Acc_3", "subject_Acc_4", "traget_value"),], 
                                               all_vehicles_batch$subject_Lane == 5)


# save the file
write.table(vehicle_lane_2,"vehicle_lane_2_left_most_lane.csv", sep=";")
write.table(vehicle_lane_34,"vehicle_lane_34_middle_lanes.csv", sep=";")
write.table(vehicle_lane_5,"vehicle_lane_5_right_most_lane.csv", sep=";")


# Initial Correlations and plotting 
correlation_data <- all_vehicles_batch[,c(
  "subject_Local_Y",
  "right_Local_Y",
  "left_Local_Y",
  "subject_speed", "subject_Acc",
  "left_speed",   "left_Acc",  
  "right_speed",  "right_Acc",
  "leading_Y", "leading_speed", "leading_Acc", 
  "head2_Y", "head2_speed", "head2_Acc",
  "back_Y","back_speed", "back_Acc", 
  "left_speed_diff", "right_speed_diff", "leading_speed_diff", "head2_speed_diff","back_speed_diff", 
  "left_headway", "right_headway", "leading_headway", "head2_headway", "back_headway",
  "subject_Acc_1", "subject_Acc_2", "subject_Acc_3", "subject_Acc_4", "traget_value")]

correlation_results <- cor(as.matrix(correlation_data),use = "complete.obs")
write.table(correlation_results,"correlation_results_ALL.csv")
col<- colorRampPalette(c("#f8766d", "white", "#00bfc4"))(20)
corrplot(correlation_results, type="upper", col=col, tl.col="black",addCoef.col = "#737373", number.font=1, number.digits=1)


