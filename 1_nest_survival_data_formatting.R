
#Formatting Nest Survival
library(tidyverse)
library(lubridate)
library(purrr)
library(tibble)
library(dplyr)

df <- read.csv("ANF_NestingData_2019&2020.csv", header = T)


df$Found_Date <- as.Date(df$Found_Date, format="%m/%d/%y")
df$Fate_Date <- as.Date(df$Fate_Date, format="%m/%d/%y")
df$year <- year(df$Found_Date)
first_nest_day <- min(df$Found_Date)
last_nest_day <- max(df$Found_Date)

df <- df %>% 
  mutate(Success_Unsuccessful = case_when(Success_Unsuccessful == "Success" ~ "Success",
                                          TRUE ~ "Unsuccessful"))

#dftmp <- df %>% 
 # dplyr::group_by(year)  %>% 
  #dplyr::mutate(first_nest_day = min(Found_Date),
   #                last_nest_day = max(Fate_Date)) 


dftmp2 <- df %>% 
          dplyr::group_by(year,Band_ID) %>% 
          slice(rep(1:n(), 2))

dftmp3 <- dftmp2 %>%
          dplyr::group_by(year, Band_ID,Attempt) %>% 
          mutate(nest_check = row_number(Attempt))

dftmp4 <- dftmp3 %>%
          dplyr::group_by(year,Band_ID,Attempt) %>% 
          mutate(new_found_date = case_when(Found_Date == Fate_Date ~ Found_Date-1,
                                            TRUE ~Found_Date ))


dftmp5 <- dftmp4 %>%
  dplyr::group_by(year,Band_ID,Attempt) %>% 
  mutate(check_date = case_when(nest_check == 1 ~ new_found_date,
                                nest_check == 2 ~ Fate_Date))

capt_hist <- dftmp5 %>%
  dplyr::group_by(year,Band_ID,Attempt) %>%
  # remove duplicates, which may occur when individuals are caught multiple times in an event
  # For example, your event may be a year and an individual may be caught multiple times in a year.
  distinct() %>%
  # spread out data. The fill = 0 adds rows for combinations of id and event where individuals were not observerd
  pivot_wider(names_from = check_date, values_from = nest_check,
              values_fill = NA)


#capt_hist <- capt_hist %>% select(order(colnames(capt_hist)))


#capt_hist_mat <- rowid_to_column(capt_hist)
capt_hist_mat <- as.matrix(capt_hist)

rownames(capt_hist_mat) <- paste(capt_hist$Band_ID,
                                capt_hist$Attempt,capt_hist$year,
                                sep = "_")

neh <- matrix(data = NA, nrow = nrow(capt_hist_mat),
                    ncol = length(seq(first_nest_day,last_nest_day,1)))

rownames(neh) <- paste(capt_hist$Band_ID,
                                capt_hist$Attempt,capt_hist$year,
                                sep = "_")

colnames(neh) <- as.character(seq(first_nest_day,last_nest_day,1))


#neh <- rowid_to_column(as_tibble(neh)) 


cols <- colnames(neh)[colnames(neh) %in% colnames(capt_hist_mat)]
rows <- rownames(neh)[rownames(neh) %in% rownames(capt_hist_mat)]

neh[rows, cols] <- capt_hist_mat[rows, cols]

rownames(df) <- paste(df$Band_ID,
                          df$Attempt, df$year,
                          sep = "_")

df <- df[order(row.names(df)), ]
neh <- neh[order(row.names(neh)), ]
#check
rownames(neh) == rownames(df)



first <- last <- last.alive <- numeric()
#
#first <- apply(ech3, 1, function(x) min(which(x>0)))
#
for (i in 1:length(neh[,1])){
  first[i] <- min(which(!is.na(neh[i,])))
  last[i] <- max(which(!is.na(neh[i,])))
}


for(i in 1:nrow(neh)){
  if(first[i]==last[i]) # birds where last and first interval are equal
    next()
  if(last[i]-first[i]==1) # birds where last and first interval are adjacent
    next()
  neh[i,(first[i]+1):(last[i])-1] <- 1
}

for (i in 1:nrow(neh)){
  for(t in 1:(first[i]):(last[i])){
  neh[i,last[i]] <- ifelse(df$Success_Unsuccessful[i] == "Success",1,0)
  }
}


#checks
neh[25,]
df[25,]

neh[65,]
df[65,]

neh[17,]
df[17,]


save.image("nest_enounter_history_format.Rdata")








