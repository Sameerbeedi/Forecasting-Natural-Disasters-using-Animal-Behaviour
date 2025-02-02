######################
# load packages 
######################

require(lmerTest)
require(lmeresampler)


############################################################################
# bring the the data and format correctly 
############################################################################

irma.df <- read.csv("./irma_mvmt_rates_LMER.csv")

irma.df$DT <- as.POSIXct(irma.df$DT, tz='EST')

# Create column with log-transformed velocity
irma.df$logvel<-log(irma.df$vel)

# Drop infinite values occuring during tranformation
irma.df<-irma.df[-which(is.infinite(irma.df$logvel)==T),]

# Create a column where landfall is 24 hours at reported time of landfall in Marco Island
irma.df$irma1day<-ifelse(irma.df$DT > '2017-09-10 15:00:00' & irma.df$DT < '2017-09-11 15:00:00', 1,0)

##############################################
#LMER modelling 
##############################################

## Model for effect of single day of storm on velocity with random effect of individual - 
fm <- lmer(logvel ~ as.factor(irma1day)*factor(sex) + (1|animal.id), data=irma.df)

summary(fm)

##############################################
#Bootstraapping to get CI and P-values 
##############################################

mySumm <- function(.) {
  s <- getME(., "sigma")
  c(beta = getME(., "beta"), sigma = s, sig01 = unname(s * getME(., "theta")))
}

# now we need to bootstrap to get a mean for each day - 
bsf<-bootstrap(fm, fn=mySumm, type='parametric', B=200)

head(bsf$t)
exp(mean(bsf$t[,1]))

bsm<-bootstrap(fm, fn=mySumm, type='parametric', B=200)

head(bsm$t)
exp(mean(bsm$t[,1]))



