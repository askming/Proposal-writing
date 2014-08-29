#------------ code to summarize simulation results -------------#

############################################################################
############################## simulated data ##############################
############################################################################
setwd('/Users/askming/Dropbox/RA works/Self/Proposal writing/dataout')
dir()
load(dir()[1])

# ---------- plot density for rando effects --------------------#
re1 = unlist(lapply(mn_t3_10_DPoutput[[1]], `[[`, 'RE'))
re2 = unlist(lapply(mn_t3_10_DPoutput[[2]], `[[`, 'RE'))
re3 = unlist(lapply(mn_t3_10_DPoutput[[3]], `[[`, 'RE'))

plot(density(re1))

# ---------- calculate bias for fixed effects --------------------#
beta = lapply(mn_t3_10_DPoutput[[1]], `[[`, 'beta')
beta0= unlist(lapply(beta, `[`, 1:3000, 1)) 
beta1= unlist(lapply(beta, `[`, 1:3000, 2))

bias0 = mean(beta0)-(1+qt(0.25, 3))
bias1 = mean(beta1)-(-1+1/11*qt(0.25, 3))
c(bias0, bias1)


############################################################################
############################## simulated data ##############################
############################################################################
setwd('/Users/askming/Dropbox/RA works/Self/Proposal writing/simulationdata')
load(dir()[1])
# ls()
ui = lapply(mn_t3_100, `[[`, 'ui')
# par(mfrow=c(2,2))
plot(density(ui[[1]]), ylim=c(0, 1.3), main='random effects densities')
for (i in 2: 10){
	lines(density(ui[[i]]), lty=i, col=i)
}