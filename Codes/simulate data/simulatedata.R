### simulation study ###
### data generation file ####
rm(list=ls())
### y_ij = beta_0 + beta_1*x_1ij + alpha_1i + alpha_2i*x_1ij + e_ij

# function to generate mixture of normal distribution
gen_mn = function(I, p, m, s){
	# n is the sample size
	# p is the proportion of two mixture
	# m and s are the mean and std of normal distributions
		i = runif(I) < p
		rnorm(I, mean = ifelse(i,m[1],m[2]), sd=ifelse(i,s[1],s[2]))	
}

# generate mixture normal random effects
mn = gen_mn(100, 0.3, c(-0.7,0.3), c(0.3,0.3))

# function to generate single data set
gen_data = function(beta, I, t, re, error){
	# I is the number of subjects
	# t is the number of repeated measures
	# re is the random effects
	# beta si the vector of regression coefficients
	# ui1 is the random intercep
	# ui2 is the random slope
	if (length(error)!=I*t) stop("check the length of error term!")
	ncol = length(beta)
	
	x = matrix(rep(1, I*t*ncol), ncol=ncol)
	x[,2] = runif(I*t,0,10)
	# re1 = rnorm(I, 0, 9) # random intercept
	
	y = numeric(I*t)
	ind <- seq(1,(I+1)*t, by=t) # indicatior of observation
	for (i in 1:I){ 
		for (j in ind[i]:ind[i+1]-1){
    		y[j] <- beta[1] + beta[2]*x[j,2] + re[i] + (1+x[j,2]/11)*error[j]
    	}
 	}
	outdata = list(y=y, x=x, t=ind, ui=re, e=error)	
}

# test = gen_data(c(1, -1), 100, 5, re=re, error=rt(500, 3))

mn_t3_300 = lapply(rep(100,500), gen_data, beta=c(1,-1), t = 5, re=mn, error=rt(500, 3))


## savedata
setwd("/Users/askming/Dropbox/RA works/Self/Proposal writing/simulationdata")
# save(mn_t3_300 ,file="mn_t3_300.Rdata")





