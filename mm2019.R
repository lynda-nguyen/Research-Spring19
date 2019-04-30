# libraries
library(stringr)
library(tidyr)
library(splitstackshape)
library(ggplot2)
library(tidyverse)
library(plyr)
library(gridExtra)
library(kableExtra)
library(rowr)

# load in datasets
vt_data <- read.csv("~/Downloads/Virginia_Tech_Stats - Sheet1.csv", header = FALSE)
vt_rows <- nrow(vt_data)
uva.bb <- read.csv("~/Downloads/UVA_Stats - Sheet1.csv", header = FALSE)
uva_rows <- nrow(uva.bb)
gonzaga.bb <- read.csv("~/Downloads/Gonzaga_Stat - Sheet1.csv", header = FALSE)
gonzaga_rows <- nrow(gonzaga.bb)
michiganSt.bb <- read.csv("~/Downloads/MichiganSt_Stat - Sheet1.csv", header = FALSE)
michiganSt_rows <- nrow(michiganSt.bb)
northCarolina.bb <- read.csv("~/Downloads/North_Carolina_Stat - Sheet1.csv", header = FALSE)
northCarolina_rows <- nrow(northCarolina.bb)
duke.bb <- read.csv("~/Downloads/Duke_Stat - Sheet1.csv", header = FALSE)
duke_rows <- nrow(duke.bb)
michigan.bb <- read.csv("~/Downloads/Michigan_Stats - Sheet1.csv", header = FALSE)
michigan_rows <- nrow(michigan.bb)
kentucky.bb <- read.csv("~/Downloads/Kentucky_Stats - Sheet1.csv", header = FALSE)
kentucky_rows <- nrow(kentucky.bb)
texasTech.bb <- read.csv("~/Downloads/Texas_Tech_Stats - Sheet1.csv", header = FALSE)
texasTech_rows <- nrow(texasTech.bb)
purdue.bb <- read.csv("~/Downloads/Purdue_Stats - Sheet1.csv", header = FALSE)
purdue_rows <- nrow(purdue.bb)
tennessee.bb <- read.csv("~/Downloads/Tennessee_Stats - Sheet1.csv", header = FALSE)
tennessee_rows <- nrow(tennessee.bb)
houston.bb <- read.csv("~/Downloads/Houston_Stats - Sheet1.csv", header = FALSE)
houston_rows <- nrow(houston.bb)
auburn.bb <- read.csv("~/Downloads/Auburn_Stats - Sheet1.csv", header = FALSE)
auburn_rows <- nrow(auburn.bb)
floridaSt.bb <- read.csv("~/Downloads/Florida_St_Stats - Sheet1.csv", header = FALSE)
floridaSt_rows <- nrow(floridaSt.bb)
lsu.bb <- read.csv("~/Downloads/LSU - Sheet1.csv", header = FALSE)
lsu_rows <- nrow(lsu.bb)
floridaSt.bb <- read.csv("~/Downloads/Florida_St_Stats - Sheet1.csv", header = FALSE)
floridaSt_rows <- nrow(floridaSt.bb)
lsu.bb <- read.csv("~/Downloads/LSU - Sheet1.csv", header = FALSE)
lsu_rows <- nrow(lsu.bb)
oregon.bb <- read.csv("~/Downloads/Oregon_Stats - Sheet1.csv", header = FALSE)
oregon_rows <- nrow(oregon.bb)

#combine all datasets into one (going down into rows)
sweet16 <- rbind.fill(vt_data, uva.bb,
                      gonzaga.bb,michiganSt.bb,duke.bb,michigan.bb,
                      northCarolina.bb,kentucky.bb,texasTech.bb,purdue.bb,tennessee.bb,
                      houston.bb, auburn.bb, floridaSt.bb, lsu.bb, oregon.bb)

#combining by columns
schools_list <- cbind.fill(vt_data, uva.bb, gonzaga.bb,michiganSt.bb,duke.bb,michigan.bb, northCarolina.bb,kentucky.bb,texasTech.bb,purdue.bb,tennessee.bb, houston.bb, auburn.bb, floridaSt.bb, lsu.bb, oregon.bb)
numOfSchools <- (length(schools_list))/10
#include headers: Date	Rk	Opponent	Result	 	 	Location	Record	Conf	
colnames(sweet16) <- c("Date","Rank","Opponent Rank","Opponent","Result","N/A","OT","Location","Record","Conf")

#create a column that indicates which team by going through # rows
numRows <- c(vt_rows,uva_rows,gonzaga_rows,michiganSt_rows,duke_rows,michigan_rows,northCarolina_rows,kentucky_rows, texasTech_rows, purdue_rows,tennessee_rows,houston_rows,auburn_rows,floridaSt_rows,lsu_rows,oregon_rows)
names <- c("vt.bb", "uva.bb", "gonzaga.bb","michiganSt.bb","duke.bb","michigan.bb", "northCarolina.bb","kentucky.bb","texasTech.bb","purdue.bb","tennessee.bb", "houston.bb", "auburn.bb", "floridaSt.bb", "lsu.bb", "oregon.bb")
school_change <- 1
counter <- 1
School <- ""
for(i in numRows){
  for (j in 1:(i)){
    School[counter] <- names[school_change]
    counter <- counter + 1
  }
  school_change <- school_change + 1
}
sweet16 <- cbind(sweet16, School)

## DATA CLEANING
post_rows <- which(is.na(sweet16$Rank),arr.ind=TRUE)
post_rows <- post_rows[-3] # vt data is before the rest
sweet16.data <- sweet16[-37,] #^  

# replace the second column for the cumsum of the initial second column
numRowsCum <- cumsum(numRows)
post_rows <- post_rows[c(TRUE, FALSE)]

#test by removing VT post season
sweet16.data <- sweet16.data[-c(post_rows[1]: (numRowsCum[1]-1)),]
deletedVT <- length(post_rows[1]: (numRowsCum[1]-1)) + 1

for(i in 2:numOfSchools){
  for(j in post_rows[i]:(numRowsCum[i])){
    sweet16.data[j - deletedVT,] <- NA
  }
}

sweet16.df <- sweet16.data[!is.na(sweet16.data$Rank), ] # result
sweet16.df <- data.frame(sweet16.df)

Win_Lost <- str_split_fixed(sweet16.df$Result, ",", 2)[,1]
scores <- str_split_fixed(sweet16.df$Result, ",", 2)[,2]
Home_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,1])
Opp_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,2])

s16_data <- cbind(sweet16.df , Win_Lost,Home_Points,Opp_Points)
s16_data <- s16_data[,c(1,2,3,4,5,6,11,12,13,14,7,8,9,10)]
s16_data <- s16_data[, -5] #deletes original results and n/a columns
s16_data  <- s16_data[, -5] #deletes original results and n/a columns

# we need to swap points were Home loses, because while cleaning we got all the higher points
Home_points2 <- Home_Points
s16.df <- data.frame(cbind(s16_data, Home_points2))

for (i in (which(s16.df$Win_Lost == "L"))){
  s16.df$Home_Points[i] = s16.df$Opp_Points[i]
  s16.df$Opp_Points[i] = s16.df$Home_points2[i]
}
s16.df <- s16.df[, -ncol(s16.df)]

home_loc <- which(s16.df$Location == "Home")

# x(g) = margin by which the home team beats the road team
x.home <- 0
for (g in home_loc){
  x.home[g] <- s16.df$Home_Points[g] - s16.df$Opp_Points[g]
}

#place the home_loc according to row
s16.df <- cbind.fill(s16.df, x.home)

# check for errors (especially at the end)
err <- which(!is.na(s16.df$object) & s16.df$Location == 'Away')
s16.df$object[err] <- NA
colnames(s16.df)[colnames(s16.df)=="object"] <- "home.point.diff" 

#which(s16.df$OT == "OT" & s16.df$Location == "Home") ##considering overtime
home.game.count <- length(x.home) - length(which(is.na(x.home)))


#####################################################################################
#2019
abileneChr.bb <- read.csv("~/Downloads/AbileneChristian19_Stats - Sheet1.csv", header = FALSE)
abileneChr_rows <- nrow(abileneChr.bb)
belmont.bb <- read.csv("~/Downloads/Belmont19_Stats - Sheet1.csv", header = FALSE)
belmont_rows <- nrow(belmont.bb)
baylor.bb <- read.csv("~/Downloads/Baylor19_Stats - Sheet1.csv", header = FALSE)
baylor_rows <- nrow(baylor.bb)
arizonaSt.bb <- read.csv("~/Downloads/ArizonaSt19_Stats - Sheet1.csv", header = FALSE)
arizonaSt_rows <- nrow(arizonaSt.bb)
bradley.bb <- read.csv("~/Downloads/Bradley19_Stats - Sheet1.csv", header = FALSE)
bradley_rows <- nrow(bradley.bb)
buffalo.bb <- read.csv("~/Downloads/Buffalo19_Stats - Sheet1.csv", header = FALSE)
buffalo_rows <- nrow(buffalo.bb)
cincinnati.bb <- read.csv("~/Downloads/Cincinati19_Stats - Sheet1.csv", header = FALSE)
cincinnati_rows <- nrow(cincinnati.bb)
colgate.bb <- read.csv("~/Downloads/Colgate19_Stats - Sheet1.csv", header = FALSE)
colgate_rows <- nrow(colgate.bb)
fairDick.bb <- read.csv("~/Downloads/FairleighDickinson19_Stats - Sheet1.csv", header = FALSE)
fairDick_rows <- nrow(fairDick.bb)
florida.bb <- read.csv("~/Downloads/Florida19_Stats - Sheet1.csv", header = FALSE)
florida_rows <- nrow(florida.bb)
gardWebb.bb <- read.csv("~/Downloads/GardnerWebb19_Stats - Sheet1.csv", header = FALSE)
gardWebb_rows <- nrow(gardWebb.bb )
georgiaSt.bb <- read.csv("~/Downloads/GeorgiaSt19_Stats - Sheet1.csv", header = FALSE)
georgiaSt_rows <- nrow(georgiaSt.bb)
iona.bb <- read.csv("~/Downloads/Iona19_Stats - Sheet1.csv", header = FALSE)
iona_rows <- nrow(iona.bb)
iowa.bb <- read.csv("~/Downloads/Iowa19_Stats - Sheet1.csv", header = FALSE)
iowa_rows <- nrow(iowa.bb)
iowaSt.bb <- read.csv("~/Downloads/IowaSt19_Stats - Sheet1.csv", header = FALSE)
iowaSt_rows <- nrow(iowaSt.bb)
kansas.bb <- read.csv("~/Downloads/Kansas19_Stats - Sheet1.csv", header = FALSE)
kansas_rows <- nrow(kansas.bb)
kansasSt.bb <- read.csv("~/Downloads/KansasSt19_Stats - Sheet1.csv", header = FALSE)
kansasST_rows <- nrow(kansasSt.bb)
liberty.bb <- read.csv("~/Downloads/Liberty19_Stats - Sheet1.csv", header = FALSE)
liberty_rows <- nrow(liberty.bb)
louisville.bb <- read.csv("~/Downloads/Lousville19_Stats - Sheet1.csv", header = FALSE)
louisville_rows <- nrow(louisville.bb)
marquette.bb <- read.csv("~/Downloads/Marquette19_Stats - Sheet1.csv", header = FALSE)
marquette_rows <- nrow(marquette.bb)
maryland.bb <- read.csv("~/Downloads/Maryland19_Stats - Sheet1.csv", header = FALSE)
maryland_rows <- nrow(maryland.bb)
minnesota.bb <- read.csv("~/Downloads/Minnesota19_Stats - Sheet1.csv", header = FALSE)
minnesota_rows <- nrow(minnesota.bb)
mississippiSt.bb <- read.csv("~/Downloads/MississippiSt19_Stats - Sheet1.csv", header = FALSE)
mississippiSt_rows <- nrow(mississippiSt.bb)
montana.bb <- read.csv("~/Downloads/Montana19_Stats - Sheet1.csv", header = FALSE)
montana_rows <- nrow(montana.bb)
murraySt.bb <- read.csv("~/Downloads/MurraySt19_Stats - Sheet1.csv", header = FALSE)
murraySt_rows <- nrow(murraySt.bb)
nevada.bb <- read.csv("~/Downloads/Nevada19_Stats - Sheet1.csv", header = FALSE)
nevada_rows <- nrow(nevada.bb)
newmexicoSt.bb <- read.csv("~/Downloads/NewMexicoSt19_Stats - Sheet1.csv", header = FALSE)
newmexicoSt_rows <- nrow(newmexicoSt.bb)
ndSt.bb <- read.csv("~/Downloads/NorthDakotaSt19_Stats - Sheet1.csv", header = FALSE)
ndSt_rows <- nrow(ndSt.bb)
northEastern.bb <- read.csv("~/Downloads/Northeastern19_Stats - Sheet1.csv", header = FALSE)
northEastern_rows <- nrow(northEastern.bb)
northernKentucky.bb <- read.csv("~/Downloads/NorthernKentucky19_Stats - Sheet1.csv", header = FALSE)
northernKentucky_rows <- nrow(northernKentucky.bb)
ohioSt.bb <- read.csv("~/Downloads/OhioSt19_Stats - Sheet1.csv", header = FALSE)
ohioSt_rows <- nrow(ohioSt.bb)
oklahoma.bb <- read.csv("~/Downloads/Oklahoma19_Stats - Sheet1.csv", header = FALSE)
oklahoma_rows <- nrow(oklahoma.bb)
odu.bb <- read.csv("~/Downloads/OldDominion19_Stats - Sheet1.csv", header = FALSE)
odu_rows <- nrow(odu.bb)
oleM.bb <- read.csv("~/Downloads/OleMiss19_Stats - Sheet1.csv", header = FALSE)
oleM_rows <- nrow(oleM.bb)
stLouis.bb <- read.csv("~/Downloads/SaintLouis19_Stats - Sheet1.csv", header = FALSE)
stLouis_rows <- nrow(stLouis.bb)
stMarys.bb <- read.csv("~/Downloads/SaintMarys19_Stats - Sheet1.csv", header = FALSE)
stMarys_rows <- nrow(stMarys.bb)
seton.bb <- read.csv("~/Downloads/SetonHall19_Stats - Sheet1.csv", header = FALSE)
seton_rows <- nrow(seton.bb)
syracuse.bb <- read.csv("~/Downloads/Syracuse19_Stats - Sheet1.csv", header = FALSE)
syracuse_rows <- nrow(syracuse.bb)
ucf.bb <- read.csv("~/Downloads/UCF19_Stats - Sheet1.csv", header = FALSE)
ucf_rows <- nrow(ucf.bb)
ucIrvine.bb <- read.csv("~/Downloads/UCIrvine19_Stats - Sheet1.csv", header = FALSE)
ucIrvine_rows <- nrow(ucIrvine.bb)
utahSt.bb <- read.csv("~/Downloads/UtahSt19_Stats - Sheet1.csv", header = FALSE)
utahSt_rows <- nrow(utahSt.bb)
vcu.bb <- read.csv("~/Downloads/VCU19_Stats - Sheet1.csv", header = FALSE)
vcu_rows <- nrow(vcu.bb)
vermont.bb <- read.csv("~/Downloads/Vermont19_Stats - Sheet1.csv", header = FALSE)
vermont_rows <- nrow(vermont.bb)
villanova.bb <- read.csv("~/Downloads/Villanova19_Stats - Sheet1.csv", header = FALSE)
villanova_rows <- nrow(villanova.bb)
washington.bb <- read.csv("~/Downloads/Washington19_Stats - Sheet1.csv", header = FALSE)
washington_rows <- nrow(washington.bb)
wisconsin.bb <- read.csv("~/Downloads/Wisconsin19_Stats - Sheet1.csv", header = FALSE)
wisconsin_rows <- nrow(wisconsin.bb)
wofford.bb <- read.csv("~/Downloads/Wofford19_Stats - Sheet1.csv", header = FALSE)
wofford_rows <- nrow(wofford.bb)
yale.bb <- read.csv("~/Downloads/Yale19_Stats - Sheet1.csv", header = FALSE)
yale_rows <- nrow(yale.bb)

mm2019 <- rbind.fill(abileneChr.bb, arizonaSt.bb, baylor.bb, belmont.bb, bradley.bb,buffalo.bb,cincinnati.bb, colgate.bb, fairDick.bb, florida.bb, gardWebb.bb, georgiaSt.bb,iona.bb, iowa.bb, iowaSt.bb, kansas.bb, kansasSt.bb, liberty.bb, louisville.bb,marquette.bb, maryland.bb, minnesota.bb, mississippiSt.bb, montana.bb, murraySt.bb,nevada.bb, newmexicoSt.bb, ndSt.bb, northEastern.bb, northernKentucky.bb, ohioSt.bb,oklahoma.bb, odu.bb, oleM.bb, stLouis.bb, stMarys.bb, seton.bb, syracuse.bb, ucf.bb,ucIrvine.bb, utahSt.bb, vcu.bb, vermont.bb, villanova.bb, washington.bb, wisconsin.bb,
wofford.bb, yale.bb)

colnames(mm2019) <- c("Date","Rank","Opponent Rank","Opponent","Result","N/A","OT","Location","Record","Conf")

#combining by columns
schools_list2 <- cbind.fill(abileneChr.bb, arizonaSt.bb, baylor.bb, belmont.bb, bradley.bb,buffalo.bb,cincinnati.bb, colgate.bb, fairDick.bb, florida.bb, gardWebb.bb, georgiaSt.bb,iona.bb, iowa.bb, iowaSt.bb, kansas.bb, kansasSt.bb, liberty.bb, louisville.bb,marquette.bb, maryland.bb, minnesota.bb, mississippiSt.bb, montana.bb, murraySt.bb,nevada.bb, newmexicoSt.bb, ndSt.bb, northEastern.bb, northernKentucky.bb, ohioSt.bb,oklahoma.bb, odu.bb, oleM.bb, stLouis.bb, stMarys.bb, seton.bb, syracuse.bb, ucf.bb,ucIrvine.bb, utahSt.bb, vcu.bb, vermont.bb, villanova.bb, washington.bb, wisconsin.bb,
                           wofford.bb, yale.bb)
numOfSchools2 <- (length(schools_list2))/10

#create a column that indicates which team by going through # rows
numRows2 <- c(abileneChr_rows,arizonaSt_rows,baylor_rows,belmont_rows,bradley_rows,buffalo_rows,cincinnati_rows,colgate_rows, fairDick_rows, florida_rows,gardWebb_rows,georgiaSt_rows,iona_rows,iowa_rows,iowaSt_rows,kansas_rows, kansasST_rows,liberty_rows,louisville_rows,marquette_rows, maryland_rows, minnesota_rows, mississippiSt_rows, montana_rows, murraySt_rows, nevada_rows, newmexicoSt_rows, ndSt_rows, northEastern_rows, northernKentucky_rows, ohioSt_rows, oklahoma_rows, odu_rows, oleM_rows, stLouis_rows, stMarys_rows, seton_rows,syracuse_rows, ucf_rows, ucIrvine_rows, utahSt_rows, vcu_rows, vermont_rows, villanova_rows, washington_rows, wisconsin_rows, wofford_rows, yale_rows)
names2 <- c("abileneChr.bb", "arizonaSt.bb", "baylor.bb", "belmont.bb", "bradley.bb","buffalo.bb","cincinnati.bb", "colgate.bb", "fairDick.bb", "florida.bb", "gardWebb.bb", "georgiaSt.bb","iona.bb", "iowa.bb", "iowaSt.bb", "kansas.bb", "kansasSt.bb", "liberty.bb", "louisville.bb","marquette.bb", "maryland.bb", "minnesota.bb", "mississippiSt.bb", "montana.bb", "murraySt.bb","nevada.bb", "newmexicoSt.bb", "ndSt.bb", "northEastern.bb", "northernKentucky.bb", "ohioSt.bb","oklahoma.bb", "odu.bb", "oleM.bb", "stLouis.bb", "stMarys.bb", "seton.bb", "syracuse.bb", "ucf.bb","ucIrvine.bb", "utahSt.bb", "vcu.bb", "vermont.bb", "villanova.bb", "washington.bb", "wisconsin.bb",
           "wofford.bb", "yale.bb")
school_change2 <- 1
counter2 <- 1
School2 <- ""
for(i in numRows2){
  for (j in 1:(i)){
    School2[counter2] <- names2[school_change2]
    counter2 <- counter2 + 1
  }
  school_change2 <- school_change2 + 1
}
mm2019 <- cbind(mm2019, School2)

## DATA CLEANING
post_rows2 <- which(is.na(mm2019$Rank),arr.ind=TRUE)

# replace the second column for the cumsum of the initial second column
numRowsCum2 <- cumsum(numRows2)
post_rows2 <- post_rows2[c(TRUE, FALSE)]

for(i in 1:numOfSchools2){
  for(j in post_rows2[i]:(numRowsCum2[i])){
    mm2019[j ,] <- NA
  }
}

mm2019.df <- mm2019[!is.na(mm2019$Rank), ] # result
mm2019.df <- data.frame(mm2019.df)


Win_Lost <- str_split_fixed(mm2019.df$Result, ",", 2)[,1]
scores <- str_split_fixed(mm2019.df$Result, ",", 2)[,2]
Home_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,1])
Opp_Points <- as.numeric(str_split_fixed(scores, "-", 2)[,2])

mm2019_data <- cbind(mm2019.df , Win_Lost,Home_Points,Opp_Points)
mm2019_data <- mm2019_data[,c(1,2,3,4,5,6,11,12,13,14,7,8,9,10)]
mm2019_data <- mm2019_data[, -5] #deletes original results and n/a columns
mm2019_data  <- mm2019_data[, -5] #deletes original results and n/a columns

# we need to swap points were Home loses, because while cleaning we got all the higher points
Home_points2 <- Home_Points
mm2019_df <- data.frame(cbind(mm2019_data, Home_points2))

for (i in (which(mm2019_df$Win_Lost == "L"))){
  mm2019_df$Home_Points[i] = mm2019_df$Opp_Points[i]
  mm2019_df$Opp_Points[i] = mm2019_df$Home_points2[i]
}
mm2019_df <- mm2019_df[, -ncol(mm2019_df)]

home_loc2 <- which(mm2019_df$Location == "Home")

# x(g) = margin by which the home team beats the road team
x.home2 <- 0
for (g in home_loc2){
  x.home2[g] <- mm2019_df$Home_Points[g] - mm2019_df$Opp_Points[g]
}

#place the home_loc according to row
mm2019_df <- cbind.fill(mm2019_df, x.home2)

# check for errors (especially at the end)
err <- which(!is.na(mm2019_df$object) & mm2019_df$Location == 'Away')
mm2019_df$object[err[1]] <- NA
mm2019_df$object[err[2]] <- NA
colnames(mm2019_df)[colnames(mm2019_df)=="object"] <- "home.point.diff" 

#which(s16.df$OT == "OT" & s16.df$Location == "Home") ##considering overtime
home.game.count2 <- length(x.home2) - length(which(is.na(x.home2)))


#combine top sweet 16 with the rest of 48 teams for 2019
colnames(mm2019_df)[colnames(mm2019_df)=="School2"] <- "School" 
tourn2019 <- rbind(s16.df, mm2019_df)
