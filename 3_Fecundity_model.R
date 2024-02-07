load("nest_enounter_history_format.Rdata")
df
treatment <- as.numeric(factor(df$Treatment))
year <- as.numeric(factor(df$year))-1

eggs <- as.numeric(ifelse(df$Clutch_Size == "N/A",NA, df$Clutch_Size ))
hatched_eggs <- df$Hatched_Eggs
success <- ifelse(df$Success_Unsuccessful=="Success",1,0)


eggs[23] <- eggs[23] +1
inits <- function() {list(eggs = ifelse(is.na(eggs), 20, NA))}

neh <- matrix(
  as.numeric(neh), ncol = ncol(neh)) 

jags.data <- list(neh = neh, treatment = treatment, year = year,
                  first = first, eggs = eggs, hatched_eggs = hatched_eggs,
                  last = last, success = success,treatment_pred = c(1,2,3),
                  nind = nrow(neh))


# Parameters monitored
parameters <- c("mean.surv","beta.treatment","beta.year","clutch_treatment",
                "clutch0", "hatch_treatment", "hatch0", "avg_eggs",
                "avg_hatch","eggs")

# MCMC settings
ni <- 5000
nt <- 3
nb <- 5
nc <- 3

# Call JAGS from R  
library(jagsUI)

m1 <- jagsUI(data = jags.data, parameters.to.save = parameters, 
             model.file = "Nest_survival.txt", inits = inits,
             n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb,n.adapt = 1000)
m1
