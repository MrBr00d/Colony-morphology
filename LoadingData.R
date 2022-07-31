#Library load
library(tidyverse)
library(readr)
library(janitor)
library(grid)
library(gridExtra)
library(zoo)
library(RcppRoll)
begin <- Sys.time()
#RawData
#FM145
#Load in data
FM145ColonyData <- read_csv("SFM/FM145/FM145ColonyData.csv")
FM145HistDataRed <- read_csv("SFM/FM145/FM145HistDataRed.csv", 
                             col_names = FALSE)
FM145HistDataGreen <- read_csv("SFM/FM145/FM145HistDataGreen.csv", 
                               col_names = FALSE)
FM145HistDataBlue <- read_csv("SFM/FM145/FM145HistDataBlue.csv", 
                               col_names = FALSE)
FM145RawData <- read_csv("SFM/FM145/FM145RawData.csv")

#Fix Raw Data


FM145sep_dfs <- which(FM145RawData$`1` == "val1") #Get row numbers where the R/G/B profiles start to seperate them into their own df
FM145RedProfiles <- FM145RawData[1:(FM145sep_dfs[1]-1),]
FM145GreenProfiles <- FM145RawData[FM145sep_dfs[1]:(FM145sep_dfs[2]-1),]
FM145GreenProfiles <- FM145GreenProfiles %>% row_to_names(row_number = 1)
FM145BlueProfiles <- FM145RawData[FM145sep_dfs[2]:(FM145sep_dfs[3]-1),]
FM145BlueProfiles <- FM145BlueProfiles %>% row_to_names(row_number = 1)
FM145MaxDiffProfiles <- FM145RawData[FM145sep_dfs[3]:nrow(FM145RawData),]
FM145MaxDiffProfiles <- FM145MaxDiffProfiles %>% row_to_names(row_number = 1)
FM145MaxDiffProfiles <- FM145MaxDiffProfiles %>% select(1:val360)

#combining hist & profile datasets
FM145BlueProfiles$Profile <- "Blue"
FM145BlueProfiles <- FM145BlueProfiles %>% relocate(Profile,.before = val1)
names(FM145BlueProfiles) <- c("Colony", "Profile", 1:(ncol(FM145BlueProfiles)-2))
FM145GreenProfiles$Profile <- "Green"
FM145GreenProfiles <- FM145GreenProfiles %>% relocate(Profile,.before = val1)
names(FM145GreenProfiles) <- c("Colony", "Profile", 1:(ncol(FM145GreenProfiles)-2))
FM145RedProfiles$Profile <- "Red"
FM145RedProfiles <- FM145RedProfiles %>% relocate(Profile,.after = `Red profiles`)
names(FM145RedProfiles) <- c("Colony", "Profile", 1:(ncol(FM145RedProfiles)-2))


FM145Profiles <- rbind(FM145RedProfiles, FM145GreenProfiles, FM145BlueProfiles)
FM145Profiles <-  FM145Profiles %>% select_if(~!all(is.na(.))) # remove columns that are all na
FM145Profiles <- pivot_longer(FM145Profiles, cols = 3:974, names_to = "PixelRow", values_to = "Intensity")
rm(FM145RedProfiles, FM145GreenProfiles, FM145BlueProfiles, FM145RawData, FM145sep_dfs)

#Modify  columns to be the correct data type
FM145Profiles$Profile <- as.factor(FM145Profiles$Profile)
FM145Profiles$PixelRow <- as.numeric(FM145Profiles$PixelRow)
FM145Profiles$Intensity <- as.numeric(FM145Profiles$Intensity)
FM145Profiles$Colony <- as.factor(FM145Profiles$Colony)

#Add 10-moving average + 50 lag subtraction
FM145Profiles <- FM145Profiles %>% group_by(Colony, Profile) %>% mutate(rolling_avg = roll_mean(Intensity,n=10, fill = NA))
FM145Profiles <- FM145Profiles %>% group_by(Colony, Profile) %>% mutate(difference = c(rep(NA, times = 50), diff(rolling_avg, lag = 50)))
min_FM145Profiles <- FM145Profiles %>% filter(difference == min(difference, na.rm = TRUE))

FM145_differences <- c()
for (i in 1:nrow(min_FM145Profiles)) {
 FM145_differences[i] <- which(FM145Profiles == min_FM145Profiles$difference[i], arr.ind = TRUE)[1]
}
FM145_differences <- FM145Profiles$PixelRow[FM145_differences]
FM145_differences <- FM145_differences - 50


#Lividans
#Load in data
LivColonyData <- read_csv("SFM/Liv/LivColonyData.csv")
LivHistDataRed <- read_csv("SFM/Liv/LivHistDataRed.csv", 
                             col_names = FALSE)
LivHistDataGreen <- read_csv("SFM/Liv/LivHistDataGreen.csv", 
                               col_names = FALSE)
LivHistDataBlue <- read_csv("SFM/Liv/LivHistDataBlue.csv", 
                              col_names = FALSE)
LivRawData <- read_csv("SFM/Liv/LivRawData.csv")

#Fix Raw Data


Livsep_dfs <- which(LivRawData$`1` == "val1") #Get row numbers where the R/G/B profiles start to seperate them into their own df
LivRedProfiles <- LivRawData[1:(Livsep_dfs[1]-1),]
LivGreenProfiles <- LivRawData[Livsep_dfs[1]:(Livsep_dfs[2]-1),]
LivGreenProfiles <- LivGreenProfiles %>% row_to_names(row_number = 1)
LivBlueProfiles <- LivRawData[Livsep_dfs[2]:(Livsep_dfs[3]-1),]
LivBlueProfiles <- LivBlueProfiles %>% row_to_names(row_number = 1)
LivMaxDiffProfiles <- LivRawData[Livsep_dfs[3]:nrow(LivRawData),]
LivMaxDiffProfiles <- LivMaxDiffProfiles %>% row_to_names(row_number = 1)
LivMaxDiffProfiles <- LivMaxDiffProfiles %>% select(1:val360)

#combining hist & profile datasets
LivBlueProfiles$Profile <- "Blue"
LivBlueProfiles <- LivBlueProfiles %>% relocate(Profile,.before = val1)
names(LivBlueProfiles) <- c("Colony", "Profile", 1:(ncol(LivBlueProfiles)-2))
LivGreenProfiles$Profile <- "Green"
LivGreenProfiles <- LivGreenProfiles %>% relocate(Profile,.before = val1)
names(LivGreenProfiles) <- c("Colony", "Profile", 1:(ncol(LivGreenProfiles)-2))
LivRedProfiles$Profile <- "Red"
LivRedProfiles <- LivRedProfiles %>% relocate(Profile,.after = `Red profiles`)
names(LivRedProfiles) <- c("Colony", "Profile", 1:(ncol(LivRedProfiles)-2))

LivProfiles <- rbind(LivRedProfiles, LivGreenProfiles, LivBlueProfiles)
LivProfiles <-  LivProfiles %>% select_if(~!all(is.na(.))) # remove columns that are all na
LivProfiles <- pivot_longer(LivProfiles, cols = 3:974, names_to = "PixelRow", values_to = "Intensity")
rm(LivRedProfiles, LivGreenProfiles, LivBlueProfiles, LivRawData, Livsep_dfs)

#Modify  columns to be the correct data type
LivProfiles$Profile <- as.factor(LivProfiles$Profile)
LivProfiles$PixelRow <- as.numeric(LivProfiles$PixelRow)
LivProfiles$Intensity <- as.numeric(LivProfiles$Intensity)
LivProfiles$Colony <- as.factor(LivProfiles$Colony)

#Add 10-moving average + 50 lag subtraction
LivProfiles <- LivProfiles %>% group_by(Colony, Profile) %>% mutate(rolling_avg = roll_mean(Intensity,n=10, fill = NA))
LivProfiles <- LivProfiles %>% group_by(Colony, Profile) %>% mutate(difference = c(rep(NA, times = 50), diff(rolling_avg, lag = 50)))
min_LivProfiles <- LivProfiles %>% filter(difference == min(difference, na.rm = TRUE))

Liv_differences <- c()
for (i in 1:nrow(min_LivProfiles)) {
  Liv_differences[i] <- which(LivProfiles == min_LivProfiles$difference[i], arr.ind = TRUE)[1]
}
Liv_differences <- LivProfiles$PixelRow[Liv_differences]
Liv_differences <- Liv_differences - 50

#M145A
#Load in data
M145AColonyData <- read_csv("SFM/M145A/M145AColonyData.csv")
M145AHistDataRed <- read_csv("SFM/M145A/M145AHistDataRed.csv", 
                           col_names = FALSE)
M145AHistDataGreen <- read_csv("SFM/M145A/M145AHistDataGreen.csv", 
                             col_names = FALSE)
M145AHistDataBlue <- read_csv("SFM/M145A/M145AHistDataBlue.csv", 
                            col_names = FALSE)
M145ARawData <- read_csv("SFM/M145A/M145ARawData.csv")

#Fix Raw Data


M145Asep_dfs <- which(M145ARawData$`1` == "val1") #Get row numbers where the R/G/B profiles start to seperate them into their own df
M145ARedProfiles <- M145ARawData[1:(M145Asep_dfs[1]-1),]
M145AGreenProfiles <- M145ARawData[M145Asep_dfs[1]:(M145Asep_dfs[2]-1),]
M145AGreenProfiles <- M145AGreenProfiles %>% row_to_names(row_number = 1)
M145ABlueProfiles <- M145ARawData[M145Asep_dfs[2]:(M145Asep_dfs[3]-1),]
M145ABlueProfiles <- M145ABlueProfiles %>% row_to_names(row_number = 1)
M145AMaxDiffProfiles <- M145ARawData[M145Asep_dfs[3]:nrow(M145ARawData),]
M145AMaxDiffProfiles <- M145AMaxDiffProfiles %>% row_to_names(row_number = 1)
M145AMaxDiffProfiles <- M145AMaxDiffProfiles %>% select(1:val360)

#combining hist & profile datasets
M145ABlueProfiles$Profile <- "Blue"
M145ABlueProfiles <- M145ABlueProfiles %>% relocate(Profile,.before = val1)
names(M145ABlueProfiles) <- c("Colony", "Profile", 1:(ncol(M145ABlueProfiles)-2))
M145AGreenProfiles$Profile <- "Green"
M145AGreenProfiles <- M145AGreenProfiles %>% relocate(Profile,.before = val1)
names(M145AGreenProfiles) <- c("Colony", "Profile", 1:(ncol(M145AGreenProfiles)-2))
M145ARedProfiles$Profile <- "Red"
M145ARedProfiles <- M145ARedProfiles %>% relocate(Profile,.after = `Red profiles`)
names(M145ARedProfiles) <- c("Colony", "Profile", 1:(ncol(M145ARedProfiles)-2))

M145AProfiles <- rbind(M145ARedProfiles, M145AGreenProfiles, M145ABlueProfiles)
M145AProfiles <-  M145AProfiles %>% select_if(~!all(is.na(.))) # remove columns that are all na
M145AProfiles <- pivot_longer(M145AProfiles, cols = 3:974, names_to = "PixelRow", values_to = "Intensity")
rm(M145ARedProfiles, M145AGreenProfiles, M145ABlueProfiles, M145ARawData, M145Asep_dfs)

#Modify  columns to be the correct data type
M145AProfiles$Profile <- as.factor(M145AProfiles$Profile)
M145AProfiles$PixelRow <- as.numeric(M145AProfiles$PixelRow)
M145AProfiles$Intensity <- as.numeric(M145AProfiles$Intensity)
M145AProfiles$Colony <- as.factor(M145AProfiles$Colony)

#Add 10-moving average + 50 lag subtraction
M145AProfiles <- M145AProfiles %>% group_by(Colony, Profile) %>% mutate(rolling_avg = roll_mean(Intensity,n=10, fill = NA))
M145AProfiles <- M145AProfiles %>% group_by(Colony, Profile) %>% mutate(difference = c(rep(NA, times = 50), diff(rolling_avg, lag = 50)))
min_M145AProfiles <- M145AProfiles %>% filter(difference == min(difference, na.rm = TRUE))

M145A_differences <- c()
for (i in 1:nrow(min_M145AProfiles)) {
  M145A_differences[i] <- which(M145AProfiles == min_M145AProfiles$difference[i], arr.ind = TRUE)[1]
}
M145A_differences <- M145AProfiles$PixelRow[M145A_differences]
M145A_differences <- M145A_differences - 50

#HistData
#Red
#Remove false colonies
False_FM145 <- which(FM145ColonyData$Area > 2000)
False_Liv <- which(LivColonyData$Area > 2000)
False_M145A <- which(M145AColonyData$Area > 2000)

FM145HistDataRed <- FM145HistDataRed[False_FM145,]
LivHistDataRed <- LivHistDataRed[False_Liv,]
M145AHistDataRed <- M145AHistDataRed[False_M145A,]

#setdiff(FM145HistDataGreen, FM145HistDataRed) #Test if the 2 datasets are the same

#Fix dataframe
names(FM145HistDataRed) <- c("Colony", 1:256)
names(LivHistDataRed) <- c("Colony", 1:256)
names(M145AHistDataRed) <- c("Colony", 1:256)
FM145HistDataRed <- pivot_longer(FM145HistDataRed, cols = 2:257, names_to = "RGB", values_to = "Intensity")
LivHistDataRed <- pivot_longer(LivHistDataRed, cols = 2:257, names_to = "RGB", values_to = "Intensity")
M145AHistDataRed <- pivot_longer(M145AHistDataRed, cols = 2:257, names_to = "RGB", values_to = "Intensity")

FM145HistDataRed$Spec <- "FM145"
LivHistDataRed$Spec <- "Liv"
M145AHistDataRed$Spec <- "M145A"

HistDataRed <- rbind(FM145HistDataRed, LivHistDataRed, M145AHistDataRed)
HistDataRed$Colony <- as.factor(HistDataRed$Colony)
HistDataRed$RGB <- as.numeric(HistDataRed$RGB)
HistDataRed$Spec <- as.factor(HistDataRed$Spec)

#Green
FM145HistDataGreen <- FM145HistDataGreen[False_FM145,]
LivHistDataGreen <- LivHistDataGreen[False_Liv,]
M145AHistDataGreen <- M145AHistDataGreen[False_M145A,]

#Fix dataframe
names(FM145HistDataGreen) <- c("Colony", 1:256)
names(LivHistDataGreen) <- c("Colony", 1:256)
names(M145AHistDataGreen) <- c("Colony", 1:256)
FM145HistDataGreen <- pivot_longer(FM145HistDataGreen, cols = 2:257, names_to = "RGB", values_to = "Intensity")
LivHistDataGreen <- pivot_longer(LivHistDataGreen, cols = 2:257, names_to = "RGB", values_to = "Intensity")
M145AHistDataGreen <- pivot_longer(M145AHistDataGreen, cols = 2:257, names_to = "RGB", values_to = "Intensity")

FM145HistDataGreen$Spec <- "FM145"
LivHistDataGreen$Spec <- "Liv"
M145AHistDataGreen$Spec <- "M145A"

HistDataGreen <- rbind(FM145HistDataGreen, LivHistDataGreen, M145AHistDataGreen)
HistDataGreen$Colony <- as.factor(HistDataGreen$Colony)
HistDataGreen$RGB <- as.numeric(HistDataGreen$RGB)
HistDataGreen$Spec <- as.factor(HistDataGreen$Spec)

#Blue
FM145HistDataBlue <- FM145HistDataBlue[False_FM145,]
LivHistDataBlue <- LivHistDataBlue[False_Liv,]
M145AHistDataBlue <- M145AHistDataBlue[False_M145A,]

#Fix dataframe
names(FM145HistDataBlue) <- c("Colony", 1:256)
names(LivHistDataBlue) <- c("Colony", 1:256)
names(M145AHistDataBlue) <- c("Colony", 1:256)
FM145HistDataBlue <- pivot_longer(FM145HistDataBlue, cols = 2:257, names_to = "RGB", values_to = "Intensity")
LivHistDataBlue <- pivot_longer(LivHistDataBlue, cols = 2:257, names_to = "RGB", values_to = "Intensity")
M145AHistDataBlue <- pivot_longer(M145AHistDataBlue, cols = 2:257, names_to = "RGB", values_to = "Intensity")

FM145HistDataBlue$Spec <- "FM145"
LivHistDataBlue$Spec <- "Liv"
M145AHistDataBlue$Spec <- "M145A"

HistDataBlue <- rbind(FM145HistDataBlue, LivHistDataBlue, M145AHistDataBlue)
HistDataBlue$Colony <- as.factor(HistDataBlue$Colony)
HistDataBlue$RGB <- as.numeric(HistDataBlue$RGB)
HistDataBlue$Spec <- as.factor(HistDataBlue$Spec)