### simulation study ###
### data generation file ####
### y_ij = beta_0 + beta_1*x_1ij + alpha_1i + alpha_2i*x_1ij + e_ij

rm(list=ls())

# function to generate mixture of normal distribution
gen_mn = function(I, p, m, s){
	# n is the sample size
	# p is the proportion of two mixture
	# m and s are the mean and std of normal distributions
		i = runif(I) < p
		rnorm(I, mean = ifelse(i,m[1],m[2]), sd=ifelse(i,s[1],s[2]))	
}


# function to generate single data set
gen_data = function(beta, I, t){
	# I is the number of subjects
	# t is the number of repeated measures
	# re is the random effects
	# beta is the vector of regression coefficients
	# ui1 is the random intercep
	# ui2 is the random slope
	ncol = length(beta)	
	x = array(rep(1, I*t*ncol), dim=c(I, t, ncol=ncol))
	x[,,2] = rnorm(I*t, 0, 4)
	re1 = rnorm(100, 0, 2)
	# re2 = rbeta(100, 2, 5)
	re2 = rnorm(100, 0, 2)
	# re2 = gen_mn(100, 0.3, c(-0.7,0.3), c(0.3,0.3))
	# error = matrix(rt(500, 3), nrow=100)
	error = matrix(rchisq(500, 3), nrow=100)
	
	y = matrix(NA, nrow=I, ncol=t)
	# ind <- seq(1,(I+1)*t, by=t) # indicatior of observation
	for (i in 1:I){ 
		for (j in 1: t){
    		y[i,j] <- beta%*%x[i,j,] + re1[i] + x[i,j,2]*re2[i]+ error[i,j]
    	}
 	}
	outdata = list(y=y, x=x, ui1=re1, ui2=re2, e=error)	
}

# test = gen_data(c(1, -1), 100, 5, re=re, error=rt(500, 3))
### error term is t3 distributed
mn_t3_2re_30_new = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5, error=matrix(rt(500, 3), nrow=100))
norm_t3_2re_30_new = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5)
beta_t3_2re_30_new = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5)

### error term is chisq3 distributed #####
mn_chisq3_2re_30 = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5)
beta_chisq3_2re_30 = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5)
norm_chisq3_2re_30 = lapply(rep(100,30), gen_data, beta=c(5,-1), t = 5)

## savedata
setwd("/Users/askming/Dropbox/RA works/Self/Proposal writing/simulationdata")
save(mn_t3_2re_30_new ,file="mn_t3_2re_30_new.Rdata")
save(norm_t3_2re_30_new ,file="norm_t3_2re_30_new.Rdata")
save(beta_t3_2re_30_new ,file="beta_t3_2re_30_new.Rdata")

save(mn_chisq3_2re_30 ,file="mn_chisq3_2re_30_new.Rdata")
save(beta_chisq3_2re_30 ,file="beta_chisq3_2re_30_new.Rdata")
save(norm_chisq3_2re_30 ,file="norm_chisq3_2re_30_new.Rdata")


