# load packages load packages
require(MuMIn)
require(plotrix)
require(ResourceSelection)

#############################
# read in data - 

lud <- read.csv("./deer_homerange_chra.csv")

# calculate standard error 
se<-std.error(lud$max_dist)

#scale -
lud$ud_size<-scale(lud$ud_size)
lud$elev_mean<-scale(lud$elev_mean)
lud$elev_min<-scale(lud$elev_min)
lud$elev_max<-scale(lud$elev_max)
lud$pflat_area_prop<-scale(lud$pflat_area_prop)

#set factors -
lud$sex<-as.factor(lud$sex)
lud$uniq_id<-as.factor(lud$uniq_id)
lud$leftUD<-as.factor(lud$leftUD)

g <- glm(leftUD ~ sex + ud_size + elev_max + pine_prop,  data=lud, family='binomial', na.action = "na.fail")
d <- dredge(g, rank="AIC")

top <- glm(leftUD ~ ud_size + pflat_area_prop+elev_max,  data=lud, family='binomial')

##################################
# test data fit 
###################################

h <- hoslem.test(top$y, top$fitted, g=10)
