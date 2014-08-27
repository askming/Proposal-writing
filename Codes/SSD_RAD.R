###############################################################################
### code to calculate the statistics to evaluate the predicted random effects #
###############################################################################

rm(list=ls())
setwd('/Users/askming/Dropbox/RA works/Self/5. Dirichlet Process prior/Paper4/out_data/0821_rerun DPs')
data = dir()
load(data[1])
load(data[2])
load(data[3])
load(data[4])
load(data[5])
load(data[6])
load(data[7])
load(data[8])
load(data[9])
load(data[10])
load(data[11])
load(data[12])
load(data[13])
load(data[14])
load(data[15])

setwd('/Users/askming/Dropbox/RA works/Self/5. Dirichlet Process prior/Paper4/in_data')
load('cdp_data.Rdata')

###############################################################
# define SSD calculation function                     #########
###############################################################
SSD = function(true_re, jagsoutput){
	# true_re, the simulated r.e.
	# jagsoutput is the output from MCMC
	# prior is the chr specification of prior model used
	sampled_re = jagsoutput$RE
	sampled_re_mean = apply(sampled_re, 2, mean)
	ssd = sum((true_re-sampled_re_mean)^2)
	ssd
}

########## calculate SSD ############
SSD(sim_data3$ui11, jags_fit_y11_cdp)
SSD(sim_data3$ui11, jags_fit_y11_dp)
SSD(sim_data3$ui11, jags_fit_y11_normal)

SSD(sim_data3$ui33, jags_fit_y33_cdp)
SSD(sim_data3$ui33, jags_fit_y33_dp)
SSD(sim_data3$ui33, jags_fit_y33_normal)

SSD(sim_data3$ui44, jags_fit_y44_cdp)
SSD(sim_data3$ui44, jags_fit_y44_dp)
SSD(sim_data3$ui44, jags_fit_y44_normal)

# ssd_normal = SSD(sim_data2$ui44, fit_normalprior, 'normal')
# ssd_normal
# [1] 13.5269

# relative magnitude w.r.t normal prior
# c(ssd_normal/ssd_normal, ssd_dp/ssd_normal, ssd_cdp/ssd_normal)



###############################################################
### function to calculate the relative absolute deviation (RAD)
###############################################################
RAD = function(data, y, jagsoutput){
	# data is the simulated data
	# y is the variable y used as outcome
	# jagsoutput is the output from MCMC 
	# prior: str, indicating prior model used, normal or not
	
	sampled_re = jagsoutput$RE
	sigma = jagsoutput$sigma
	beta = jagsoutput$beta

	I = dim(data$x)[1]
	J = dim(data$x)[2]
	L = dim(sampled_re)[1]
	x = data$x
	y_true = data[[y]]
	y_pred = array(NA, dim=c(I, J, L)) # initiate prediction of y
	# set.seed(123)
	for (k in 1:L){
		for (i in 1:I){
			for (j in 1: J){
				xbeta = as.numeric(x[i,j,]%*%beta[k,])
				error = rnorm(1, 0, sigma[k])
				y_pred[i, j, k] = sampled_re[k,i] + xbeta + error
			}
		}
	}	
	y_pred_mean = apply(y_pred, c(1,2), mean)
	RAD = sum((y_pred_mean-y_true)/y_true)
	list(RAD=RAD, y_pred=y_pred_mean, y_true=y_true)
}
####### try RAD #########
normal1 = RAD(sim_data3, y='y11', jags_fit_y11_normal) # beta
normal2 = RAD(sim_data2, y='y33', jags_fit_y33_normal) # normal
normal3 = RAD(sim_data3, y='y44', jags_fit_y44_normal) # mixture-normal

dp1 = RAD(sim_data3, y='y11', jags_fit_y11_dp)
dp2 = RAD(sim_data2, y='y33', jags_fit_y33_dp)
dp3 = RAD(sim_data3, y='y44', jags_fit_y44_dp)

cdp1 = RAD(sim_data3, y='y11', jags_fit_y11_cdp)
cdp2 = RAD(sim_data2, y='y33', jags_fit_y33_cdp)
cdp3 = RAD(sim_data3, y='y44', jags_fit_y44_cdp)

normal_multi = lapply(X=rep('y44', 30), FUN=RAD, data=sim_data3, jagsoutput=jags_fit_y44_normal)
dp3_multi = lapply(X=rep('y44', 30), FUN=RAD, data=sim_data3, jagsoutput=jags_fit_y44_dp)
cdp3_multi = lapply(X=rep('y44', 30), FUN=RAD, data=sim_data3, jagsoutput=jags_fit_y44_cdp)

# test = RAD(sim_data2, y='y44', fit_normalprior, 'normal')
# > test
# [1] 243.0066



###############################
####### draw q-q plot #########
###############################
qq_plot = function(radout){
	qqnorm(radout[[2]], col='red', pch='.')
	par(new=T)
	qqplot(radout[[2]], radout[[3]], pch='.', xlab=NULL)
}
####### try #######
qq_plot(cdp1)



############################
####### density plot #######
############################
den_plot = function(data, re, cdpoutput, dpoutput, normaloutput,...){
	# re is the random effect specification
	plot(density(data[[re]]),lwd=2,lty=1,col="gray", ...)
	lines(density(cdpoutput$RE),col="red",lwd=2,lty=2)
	lines(density(dpoutput$RE),col="green",lwd=2,lty=4)
	lines(density(normaloutput$RE),col="blue",lwd=2,lty=3)
	legend("topright",c("emprical data","CDPM (N=200)","DPM (N=200)", 'normal'),col=c("gray","red","green","blue"),lty=c(1,2,4,3),lwd=c(2,2,2,2,2), cex=0.7)
}
####### try #######
den_plot(sim_data3, 'ui11', jags_fit_y11_cdp, jags_fit_y11_dp, jags_fit_y11_normal, ylim=c(0, 4.5), main='r.e. ~ beta(2, 5)') #beta
den_plot(sim_data3, 'ui33', jags_fit_y33_cdp, jags_fit_y33_dp, jags_fit_y33_normal, ylim=c(0, 1)) #normal
den_plot(sim_data3, 'ui44', jags_fit_y44_cdp, jags_fit_y44_dp, jags_fit_y44_normal, ylim=c(0, 1), main='r.e.~ mixture normal') #mixture normal
