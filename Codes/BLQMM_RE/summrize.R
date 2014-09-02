#------------ code to summarize simulation results -------------#

############################################################################
############################## simulated data ##############################
############################################################################
rm(list=ls())
setwd('/Users/askming/Dropbox/RA works/Self/Proposal writing/dataout')
dir()
load(dir()[5])

# ---------- plot density for rando effects -------------------- #
re1 = unlist(lapply(mn_t3_2re_10[[1]], `[[`, 're_int'))
re2 = unlist(lapply(mn_t3_2re_10[[2]], `[[`, 're_int'))
re3 = unlist(lapply(mn_t3_2re_10[[3]], `[[`, 're_int'))

plot(density(re3))
# lines(seq(-4,6, 0.01),dt(seq(-4,6, 0.01), 3))
# lines(density(re2))

# ---------- calculate bias for fixed effects -------------------- #
bias = function(data, index, tau){

	beta = lapply(data[[index]], `[[`, 'beta')
	beta0= unlist(lapply(beta, `[`, 1:3000, 1)) 
	beta1= unlist(lapply(beta, `[`, 1:3000, 2))

	bias0 = (mean(beta0)-(5+qt(tau[index], 3)))/5
	bias1 = mean(beta1)-(-1+1/11*qt(tau[index], 3))
	c(bias0, bias1)
}

bias(mn_t3_2re_10, 1, c(0.25, 0.5, 0.75))


############################################################################
############################## simulated data ##############################
############################################################################
setwd('/Users/askming/Dropbox/RA works/Self/Proposal writing/simulationdata')
load(dir()[1])
# ls()
ui = lapply(mn_t3_2re_30_new, `[[`, 'ui1')
# par(mfrow=c(2,2))
plot(density(ui[[1]]), ylim=c(0, 1), main='random intercept densities', sub="data=mn_t3_2re_30")
for (i in 2: 10){
	lines(density(ui[[i]]), lty=i, col=i)
}
