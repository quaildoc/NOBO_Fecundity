#Formatting Nest Survival

#need these packages
library(tidyverse)
library(readxl)
library(data.table)
library(reshape2)


df <- read.csv("ANF_MasterTelemetryMerged_2019&2020_FINAL.csv")

#This function takes the left side characters of a string
str_left <- function(string, n) {
  substr(string, 1, n)
}

df$Date <- str_left(df$Date_Created,10)
df$Date<- as.Date(df$Date, format="%Y-%m-%d")

df$Year <- year(df$Date)
test <- df %>% 
  group_by(Bird_ID,Year) %>% 
  summarise(count = n_distinct(Bird_ID))


df2019 <- subset(df, df$Date >= "2019-04-01" & df$Date < "2019-10-01")
df2020 <- subset(df, df$Date >= "2020-04-01" & df$Date < "2020-10-01")


##############################################
#
#  Formatting data for daily survival rate so that time varying covariate is most accurate
#################################################

#### 2019
df2019$day <- as.Date(cut(df2019$Date, 
                          breaks="day",
                          start.on.monday=FALSE))

unique(df2019$Fate)  #notice the typos

#
# ###the next few lines are recoding the Fates so they are either ALive, Mortaility, or Censor
df2019$Fate.Type <- ifelse(df2019$Fate.Type == "", "Alive",df2019$Fate.Type)
df2019$Fate.Type <- ifelse(df2019$Fate.Type == " ", "Alive",df2019$Fate.Type)
df2019$Fate.Type[agrep("Mort", df2019$Fate.Type)] <- "Mortality"
df2019$Fate.Type[agrep("Nest", df2019$Fate.Type)] <- "Alive"
table(df2019$Fate.Type) # 41 morts

df2019$cjs <-ifelse(df2019$Fate.Type =='Alive', 1,                       #alive and recaptured
                    ifelse(df2019$Fate.Type=='Mortality', 2,              #dead and recovered
                           ifelse(df2019$Fate.Type =='Censor', 0,        #not seen or recovered
                                  2)))

ech2019day <- suppressWarnings(dcast(df2019, Bird_ID ~ day, value.var="cjs", fun.aggregate=max))

rownames(ech2019day) <- ech2019day[,1]
rowName2019 <- rownames(ech2019day) 
ech2019day <- ech2019day[,-1]
mat2019full <- matrix(NA,length(unique(df2019$Bird_ID)),183)
rownames(mat2019full) = rowName2019
en2019 <- as.Date("2019-04-01")
st2019 <- as.Date("2019-09-30")
ll2019 <- seq(st2019, en2019, by = "-1 day")
ll2019 <- sort(ll2019)

colnames(mat2019full) <- as.character(ll2019) 

cols2019 <- colnames(as.matrix(mat2019full))[colnames(as.matrix(mat2019full)) %in% colnames(as.matrix(ech2019day))]

mat2019full[, cols2019] <- as.matrix(ech2019day[, cols2019])

mat2019full[mat2019full == '-Inf'] <- NA


#treatment covariate matrix for 2019


df$day <- as.Date(cut(df$Date, 
                      breaks="day",
                      start.on.monday=FALSE))
df$Treat <- ifelse(df$Treatment=='Treatment',1,0)

treat_mat <- suppressWarnings(dcast(df, Bird_ID ~ day, value.var="Treat", fun.aggregate=max))
treat_mat[treat_mat=="-Inf"]=NA
rownames(treat_mat) <- treat_mat[,1]
treat_mat <-treat_mat[,2:ncol(treat_mat)]

cov2019full <- matrix(NA, nrow = dim(mat2019full)[1],dim(mat2019full)[2])
colnames(cov2019full) <- as.character(ll2019) 
rownames(cov2019full) <- rownames(mat2019full) 

cols2 <- colnames(as.matrix(cov2019full))[colnames(as.matrix(cov2019full)) %in% colnames(as.matrix(treat_mat))]
rows2 <- rownames(as.matrix(cov2019full))[rownames(as.matrix(cov2019full)) %in% rownames(as.matrix(treat_mat))]


cov2019full[rows2, cols2] <- as.matrix(treat_mat[rows2, cols2])
cov2019full  #2019 treatment cov matrix




####2020
df2020$day <- as.Date(cut(df2020$Date, 
                          breaks="day",
                          start.on.monday=FALSE))


df2020$Fate.Type <- ifelse(df2020$Fate.Type == "", "Alive",df2020$Fate.Type)
df2020$Fate.Type <- ifelse(df2020$Fate.Type == " ", "Alive",df2020$Fate.Type)
df2020$Fate.Type[agrep("Mort", df2020$Fate.Type)] <- "Mortality"
df2020$Fate.Type[agrep("Nest", df2020$Fate.Type)] <- "Alive"
table(df2020$Fate.Type) #64 morts


df2020$cjs <-ifelse(df2020$Fate.Type =='Alive', 1,                       #alive and recaptured
                    ifelse(df2020$Fate.Type=='Mortality', 2,              #deadand recovered  
                           ifelse(df2020$Fate.Type =='Censor', 0,        #not seen or recovered
                                  2)))    

ech2020day <- suppressWarnings(dcast(df2020, Bird_ID ~ day, value.var="cjs", fun.aggregate=max))



mat2020full <- matrix(NA,length(unique(df2020$Bird_ID)),183)

en2020 <- as.Date("2020-04-01")
st2020 <- as.Date("2020-09-30")
ll2020 <- seq(st2020, en2020, by = "-1 day")
ll2020 <- sort(ll2020)
colnames(mat2020full) <- as.character(ll2020) 
rownames(mat2020full) <- ech2020day[,1]
#rowName2020 <- rownames(ech2020day) 
#ech2020day <- ech2020day[,-1]
cols <- colnames(as.matrix(mat2020full))[colnames(as.matrix(mat2020full)) %in% colnames(as.matrix(ech2020day))]
mat2020full[, cols] <- as.matrix(ech2020day[, cols])
mat2020full[mat2020full == '-Inf'] <- NA
rownames(mat2020full) <- c("196012_2", "196045_2","196081_2",rownames(mat2020full)[-1:-3])

#rownames(mat2020full) <- rowName2020

cov2020full <- matrix(NA, nrow = dim(mat2020full)[1],dim(mat2020full)[2])
colnames(cov2020full) <- as.character(ll2020) 
rownames(cov2020full) <- rownames(mat2020full) 

cols2020 <- colnames(as.matrix(cov2020full))[colnames(as.matrix(cov2020full)) %in% colnames(as.matrix(treat_mat))]
rows2020 <- rownames(as.matrix(cov2020full))[rownames(as.matrix(cov2020full)) %in% rownames(as.matrix(treat_mat))]


cov2020full[rows2020, cols2020] <- as.matrix(treat_mat[rows2020, cols2020])
cov2020full  #2020 treatment cov matrix
rownames(cov2020full) <- c("196012_2", "196045_2","196081_2",rownames(cov2020full)[-1:-3])


#####Stacking the two years

ll <- seq(st2019, en2019, by = "-1 day")
ll <- sort(ll)
dm <- format(ll, format = "%m-%d")


colnames(mat2020full) <- as.character(dm) 
colnames(mat2019full) <- as.character(dm) 
mat2020full <- as.data.frame(mat2020full)
yrdummy2020 <- rep("2020", dim(mat2020full)[1])
mat2020full < cbind(mat2020full,(yrdummy2020))
bothyears_survmat <- rbind(mat2020full,mat2019full)

colnames(cov2020full) <- as.character(dm) 
colnames(cov2019full) <- as.character(dm) 
bothyears_treat_mat <- rbind(cov2020full,cov2019full)




###########Resident vs Translocated covariate

status2019 <- data.frame(Bird_ID = df2019$Bird_ID, status = df2019$Res_Tran)

status2019 <- status2019 %>% distinct()

status2020 <- data.frame(Bird_ID = df2020$Bird_ID, status = df2020$Res_Tran)

status2020 <- status2020 %>% distinct()
status2020 <- status2020[order(status2020$Bird_ID),]
status2020$Bird_ID <- c("196012_2", "196045_2","196081_2",status2020$Bird_ID[-1:-3])

status_full <- rbind(status2019,status2020)
levels(status_full$Bird_ID) <- c("1","0")

status_full$status <- recode_factor(status_full$status, Translocated = "0", 
                                    Resident = "1")

status_full <- status_full[ order(status_full$Bird_ID), ]
rownames(status_full) <- status_full$Bird_ID

bothyears_survmat <- bothyears_survmat[ order(row.names(bothyears_survmat)), ]
bothyears_treat_mat <- bothyears_treat_mat[ order(row.names(bothyears_treat_mat)), ]

identical(rownames(bothyears_survmat), rownames(bothyears_treat_mat))
identical(rownames(bothyears_survmat), rownames(status_full))
identical(rownames(bothyears_treat_mat), rownames(status_full))


#####

first <- last <- last.alive <- numeric()
#
#first <- apply(ech3, 1, function(x) min(which(x>0)))
#
for (i in 1:length(bothyears_survmat[,1])){
  first[i] <- min(which(!is.na(bothyears_survmat[i,])))
  last[i] <- max(which(!is.na(bothyears_survmat[i,])))
}



for(i in 1:nrow(bothyears_survmat)){
  if(first[i]==last[i]) # birds where last and first interval are equal
    next()
  if(last[i]-first[i]==1) # birds where last and first interval are adjacent
    next()
  bothyears_survmat[i,(first[i]+1):(last[i])-1] <- 1
}


bothyears_survmat[bothyears_survmat == 2] <- 0

nind <- nrow(bothyears_survmat)



dayvec <- seq(1,183,1)


alive_breed <- bothyears_survmat %>%
                filter(`04-15` == 1 )

alive_breed_vec <- rownames(alive_breed)

treat_locs <- as.data.frame(table(df$Bird_ID,df$Treatment))

alive_treatment <- treat_locs %>% 
              rename(Band_ID = Var1) %>% 
              pivot_wider(,names_from = Var2, values_from = Freq) %>% 
              mutate(sum_locs = Control+Treatment + `Off site`,
                     treat_percent = Treatment/sum_locs,
                     control_percent = (Control+`Off site`)/sum_locs,
                     dominate_treat = case_when(treat_percent > 0.50 ~ "Fed", TRUE~"Unfed"))
saveRDS(alive_treatment,"alive_treatment.rds")

