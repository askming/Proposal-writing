#------------ code to summarize simulation results -------------#

############################################################################
############################## sampled data ################################
############################################################################
rm(list=ls())
setwd('/Users/askming/Dropbox/RA works/Self/Proposal writing/dataout')
dir()
load(dir()[5])

# ---------- function to get posterior samples for rando effects -------------------- #
get_res = function(post_data, re, qt){
 	# post_data is the posterior data
 	# re is the indicator of re_int or re_slope
 	# qt is the index of quantile, 1,2,3,...
 	r = unlist(lapply(post_data[[qt]], `[[`, re))
 	# r2 = unlist(lapply(post_data[[2]], `[[`, 're'))
 	# r3 = unlist(lapply(post_data[[3]], `[[`, 're'))
 	return(r)
 	
}

re_int1 = get_res(mn_t3_2re_10, 're_int', 1)
re_int2 = get_res(mn_t3_2re_10, 're_int', 2)
re_int3 = get_res(mn_t3_2re_10, 're_int', 3)

re_slope1 = get_res(mn_t3_2re_10, 're_slope', 1)
re_slope2 = get_res(mn_t3_2re_10, 're_slope', 2)
re_slope3 = get_res(mn_t3_2re_10, 're_slope', 3)




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
# ---------- function to plot the density of rando effects -------------------- #
plot_data = function(data, re, ...){
 	# data is the original simulated data
 	# re is the indicator of re_int or re_slope
 	if (re == 're_int') var = 'ui1'
 	if (re == 're_slope') var = 'ui2'	
 	ui = lapply(data, `[[`, var)
 	plot(density(unlist(ui)), ...)	
}

#### try ####

plot_data(mn_t3_2re_30, 're_int', ylim=c(0, 0.3), main='random intercept densities', sub="data=mn_t3_2re_30")
lines(density(re_int1+1), lty=2, col=2)
lines(density(re_int2+1), lty=3, col=3)
lines(density(re_int3+1), lty=4, col=4)

# lines(density(re_slope1), lty=2, col=2)
# lines(density(re_slope2), lty=3, col=3)
# lines(density(re_slope3), lty=4, col=4)

legend('topright', legend=c("empirical data", "tau=0.25", "tau=0.5", "tau=0.75"), col=c(1, 2, 3, 4), lty=c(1,2,3, 4), cex=0.75)
