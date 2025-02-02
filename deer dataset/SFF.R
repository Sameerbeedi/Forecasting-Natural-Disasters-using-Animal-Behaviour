######################
# load packages 
######################

require(survival)
require(AICcmodavg)

#############################
# read in data - 
#############################

fin <- read.csv("./SFF_data.csv")
 
#############################
# Model generation - 
#############################

# create a blank list 
mod.sel <- list()

# Global 
mod.sel[[1]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane + sex +
                                   hurricane*(shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + sex) +
                                   sex*(shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane) +
                                   hurricane*sex*(shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2) +
                                   cluster(id) + strata(pair), method = "approximate", data = fin) 

# model 1
mod.sel[[2]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane + sex +
                                   cluster(id) + strata(pair), method = "approximate", data = fin) 
# model 2 
mod.sel[[3]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane +
                                   cluster(id) + strata(pair), method = "approximate", data = fin)

# model 3
mod.sel[[4]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane + sex +
                                   hurricane*(shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + sex) + 
                                   cluster(id) + strata(pair), method = "approximate", data = fin) 
# model 4 
mod.sel[[5]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane + sex +
                                   sex*(shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2) + 
                                   cluster(id) + strata(pair), method = "approximate", data = fin) 
# model 5 
mod.sel[[6]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane + sex +
                                   sex*hurricane + cluster(id) + strata(pair), method = "approximate", data = fin) 
# model 6
mod.sel[[7]] <- survival::clogit(use ~ shrub2 + elev2 + ham2 + mar2 + cyp2 + pra2 + pwood2 + hurricane
                                 + cluster(id) + strata(pair), method = "approximate", data = fin) 

# null 
mod.sel[[8]] <- survival::clogit(use ~ 1 + cluster(id) + strata(pair), method = "approximate", data = fin) 


# Name models
Model.names <- c("global", "hab_p_hur_p_sex", "hab_p_hur", "hur_hab","hab_p_hur_i_hab_hur_i_sex",
                 "hab_p_hur_p_hur_i_sex", "hab_p_hur_p_sex_p_hur_i_sex", "null")

Model.names <- c("global", "model 1", "model 2", "model3", "model 4", "model 5", "model 6", "null")

################################
# Model selection based on AIC
################################

((out <- aictab(cand.set = mod.sel, modnames = Model.names, second.ord=F)))



