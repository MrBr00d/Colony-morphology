#FM145
#Red
FM145_Colonies <- as.vector(unique(FM145Profiles$Colony))
model_list <- list()
b <- 1
Red_diff <- c(length(FM145_Colonies))

# make lm for all colonies
for (i in FM145_Colonies){
  new <- lm(data = FM145Profiles[FM145Profiles$Colony == i & 
                                   FM145Profiles$Profile == "Red" & 
                                   FM145Profiles$PixelRow < FM145_differences[b],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
}

SS_Red <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Red[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
FM145Red_Profile_SS <- as.data.frame(SS_Red)
FM145Red_Profile_SS$Colony <- FM145_Colonies
FM145Red_Profile_SS <- arrange(FM145Red_Profile_SS, SS_Red)
print(head(FM145Red_Profile_SS))
print(tail(FM145Red_Profile_SS))

#Green
FM145_Colonies <- as.vector(unique(FM145Profiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff)+1

# make lm for all colonies
for (i in FM145_Colonies){
  new <- lm(data = FM145Profiles[FM145Profiles$Colony == i & 
                                   FM145Profiles$Profile == "Green" & 
                                   FM145Profiles$PixelRow < FM145_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Green <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Green[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
FM145Green_Profile_SS <- as.data.frame(SS_Green)
FM145Green_Profile_SS$Colony <- FM145_Colonies
FM145Green_Profile_SS <- arrange(FM145Green_Profile_SS, SS_Green)
print(head(FM145Green_Profile_SS))
print(tail(FM145Green_Profile_SS))

#Blue
FM145_Colonies <- as.vector(unique(FM145Profiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff*2)+1

# make lm for all colonies
for (i in FM145_Colonies){
  new <- lm(data = FM145Profiles[FM145Profiles$Colony == i & 
                                   FM145Profiles$Profile == "Blue" & 
                                   FM145Profiles$PixelRow < FM145_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Blue <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Blue[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
FM145Blue_Profile_SS <- as.data.frame(SS_Blue)
FM145Blue_Profile_SS$Colony <- FM145_Colonies
FM145Blue_Profile_SS <- arrange(FM145Blue_Profile_SS, SS_Blue)
print(head(FM145Blue_Profile_SS))
print(tail(FM145Blue_Profile_SS))

#Liv
#Red
Liv_Colonies <- as.vector(unique(LivProfiles$Colony))
model_list <- list()
b <- 1
Red_diff <- c(length(Liv_Colonies))

# make lm for all colonies
for (i in Liv_Colonies){
  new <- lm(data = LivProfiles[LivProfiles$Colony == i & 
                                 LivProfiles$Profile == "Red" & 
                                 LivProfiles$PixelRow < Liv_differences[b],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
}

SS_Red <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Red[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
LivRed_Profile_SS <- as.data.frame(SS_Red)
LivRed_Profile_SS$Colony <- Liv_Colonies
LivRed_Profile_SS <- arrange(LivRed_Profile_SS, SS_Red)
print(head(LivRed_Profile_SS))
print(tail(LivRed_Profile_SS))

#Green
Liv_Colonies <- as.vector(unique(LivProfiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff)+1

# make lm for all colonies
for (i in Liv_Colonies){
  new <- lm(data = LivProfiles[LivProfiles$Colony == i & 
                                 LivProfiles$Profile == "Green" & 
                                 LivProfiles$PixelRow < Liv_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Green <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Green[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
LivGreen_Profile_SS <- as.data.frame(SS_Green)
LivGreen_Profile_SS$Colony <- Liv_Colonies
LivGreen_Profile_SS <- arrange(LivGreen_Profile_SS, SS_Green)
print(head(LivGreen_Profile_SS))
print(tail(LivGreen_Profile_SS))

#Blue
Liv_Colonies <- as.vector(unique(LivProfiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff*2)+1

# make lm for all colonies
for (i in Liv_Colonies){
  new <- lm(data = LivProfiles[LivProfiles$Colony == i & 
                                 LivProfiles$Profile == "Blue" & 
                                 LivProfiles$PixelRow < Liv_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Blue <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Blue[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
LivBlue_Profile_SS <- as.data.frame(SS_Blue)
LivBlue_Profile_SS$Colony <- Liv_Colonies
LivBlue_Profile_SS <- arrange(LivBlue_Profile_SS, SS_Blue)
print(head(LivBlue_Profile_SS))
print(tail(LivBlue_Profile_SS))

#M145A
#Red
M145A_Colonies <- as.vector(unique(M145AProfiles$Colony))
model_list <- list()
b <- 1
Red_diff <- c(length(M145A_Colonies))

# make lm for all colonies
for (i in M145A_Colonies){
  new <- lm(data = M145AProfiles[M145AProfiles$Colony == i & 
                                 M145AProfiles$Profile == "Red" & 
                                 M145AProfiles$PixelRow < M145A_differences[b],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
}

SS_Red <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Red[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
M145ARed_Profile_SS <- as.data.frame(SS_Red)
M145ARed_Profile_SS$Colony <- M145A_Colonies
M145ARed_Profile_SS <- arrange(M145ARed_Profile_SS, SS_Red)
print(head(M145ARed_Profile_SS))
print(tail(M145ARed_Profile_SS))

#Green
M145A_Colonies <- as.vector(unique(M145AProfiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff)+1

# make lm for all colonies
for (i in M145A_Colonies){
  new <- lm(data = M145AProfiles[M145AProfiles$Colony == i & 
                                 M145AProfiles$Profile == "Green" & 
                                 M145AProfiles$PixelRow < M145A_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Green <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Green[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
M145AGreen_Profile_SS <- as.data.frame(SS_Green)
M145AGreen_Profile_SS$Colony <- M145A_Colonies
M145AGreen_Profile_SS <- arrange(M145AGreen_Profile_SS, SS_Green)
print(head(M145AGreen_Profile_SS))
print(tail(M145AGreen_Profile_SS))

#Blue
M145A_Colonies <- as.vector(unique(M145AProfiles$Colony))
model_list <- list()
b <- 1
c <- (Red_diff*2)+1

# make lm for all colonies
for (i in M145A_Colonies){
  new <- lm(data = M145AProfiles[M145AProfiles$Colony == i & 
                                 M145AProfiles$Profile == "Blue" & 
                                 M145AProfiles$PixelRow < M145A_differences[c],], Intensity ~ PixelRow)
  model_list[[b]] <- new
  b <- b+1
  c <- c+1
}

SS_Blue <- vector()
#extract sum of squares
for (i in 1:length(model_list)){
  SS_Blue[[i]] <- deviance(model_list[[i]])
}

#Store + order SS dataframe
M145ABlue_Profile_SS <- as.data.frame(SS_Blue)
M145ABlue_Profile_SS$Colony <- M145A_Colonies
M145ABlue_Profile_SS <- arrange(M145ABlue_Profile_SS, SS_Blue)
print(head(M145ABlue_Profile_SS))
print(tail(M145ABlue_Profile_SS))

einde <- Sys.time()
#Print out data to pdf
g1 <- tableGrob(head(FM145Red_Profile_SS))
g2 <- tableGrob(tail(FM145Red_Profile_SS))
t1 <- grid.text("Red profile")
g3 <- tableGrob(head(FM145Green_Profile_SS))
g4 <- tableGrob(tail(FM145Green_Profile_SS))
t2 <- grid.text("Green profile")
g5 <- tableGrob(head(FM145Blue_Profile_SS))
g6 <- tableGrob(tail(FM145Blue_Profile_SS))
t3 <- grid.text("Blue profile")

g7 <- tableGrob(head(LivRed_Profile_SS))
g8 <- tableGrob(tail(LivRed_Profile_SS))
t4 <- grid.text("Red profile")
g9 <- tableGrob(head(LivGreen_Profile_SS))
g10 <- tableGrob(tail(LivGreen_Profile_SS))
t5 <- grid.text("Green profile")
g11 <- tableGrob(head(LivBlue_Profile_SS))
g12 <- tableGrob(tail(LivBlue_Profile_SS))
t6 <- grid.text("Blue profile")

g13 <- tableGrob(head(M145ARed_Profile_SS))
g14 <- tableGrob(tail(M145ARed_Profile_SS))
t7 <- grid.text("Red profile")
g15 <- tableGrob(head(M145AGreen_Profile_SS))
g16 <- tableGrob(tail(M145AGreen_Profile_SS))
t8 <- grid.text("Green profile")
g17 <- tableGrob(head(M145ABlue_Profile_SS))
g18 <- tableGrob(tail(M145ABlue_Profile_SS))
t9 <- grid.text("Blue profile")
pdf("Results.pdf")
grid.arrange(t1,g1,g2,t2,g3,g4,t3,g5,g6, top = "FM145")
grid.arrange(t4,g7,g8,t5,g9,g10,t6,g11,g12, top = "Liv")
grid.arrange(t7,g13,g14,t8,g15,g16,t9,g17,g18, top = "M145A")
dev.off()
