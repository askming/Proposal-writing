rm(list=ls())
require(R2jags)
# load simulation data
setwd("/work/02784/myang3/proposal_sim_revision/data_in")
load("cdp_data.Rdata")

# load model files
setwd("/work/02784/myang3/proposal_sim_revision/model")
### 1. LMM using DPM prior ##########
model.file<-"/work/02784/myang3/proposal_sim_revision/model/model_dp_int.txt"
### 2. LMM using CDPPM, model file ##########
model.file2<-"/work/02784/myang3/proposal_sim_revision/model/model_cdp.txt"


# define function to run gibbs sampler using differet prior specifications
runjags = function(data=sim_data2, y='y11', x='x', N=200, model){
	#### parameters of interest #####
	# gamma[] # beta[] 
	# change the model parameter to use diff priors for r.e.
	I<-150 # number of subjects
	J<-10 # number of repeated measures
	N<-N # always people use 52 ## increasing N makes big difference

	jags.data <- list(y=matrix(unlist(data[[y]]),nrow=I),x=array(unlist(data[[x]]),dim=c(I,J,3)),I=I,J=J,N=N)
	jags.params <- c("beta","gamma","alpha","p", "sigma")
	jags.inits <- function(){list(beta=c(0.1,0.1,0.1), sigma=0.1, b=0.1, sigmaF0=0.1, mu0=0, alpha=3)}
	jags.fit <- jags(data=jags.data,inits=jags.inits,jags.params, n.iter=2000,model.file=model)
	beta.est<-jags.fit$BUGSoutput$sims.list$beta
	sigma.est<-jags.fit$BUGSoutput$sims.list$sigma
	re.est<-jags.fit$BUGSoutput$sims.list$gamma
	out = list(beta=beta.est, RE=re.est, sigma=sigma.est)
	out	
}

jags_fit_y11_dp = runjags(data=sim_data2, y='y11', x='x', N=200, model=model.file)
jags_fit_y11_cdp = runjags(data=sim_data2, y='y11', x='x', N=200, model=model.file2)
jags_fit_y33_dp = runjags(data=sim_data2, y='y33', x='x', N=200, model=model.file)
jags_fit_y33_cdp = runjags(data=sim_data2, y='y33', x='x', N=200, model=model.file2)
jags_fit_y44_dp = runjags(data=sim_data2, y='y44', x='x', N=200, model=model.file)
jags_fit_y44_cdp = runjags(data=sim_data2, y='y44', x='x', N=200, model=model.file2)

# save output 
# setwd("/Users/askming/Dropbox/RA Works/Self/5. Dirichlet Process prior/Paper4/out_data")
setwd("/work/02784/myang3/proposal_sim_revision/data_out")
save(jags_fit_y11_dp,file="y11_dp.Rdata")
save(jags_fit_y11_cdp,file="y11_cdp.Rdata")
save(jags_fit_y33_dp,file="y33_dp.Rdata")
save(jags_fit_y33_cdp,file="y33_cdp.Rdata")
save(jags_fit_y44_dp,file="y44_dp.Rdata")
save(jags_fit_y44_cdp,file="y44_cdp.Rdata")


###### collect & plot the results ######
beta.est<-jags.fit_dp_int$BUGSoutput$sims.list$beta
# alpha.est<-jags.fit_cdp$BUGSoutput$sims.list$alpha
# p.est<-jags.fit_cdp$BUGSoutput$sims.list$p
# apply(beta.est,2,mean)
# apply(beta.est,2,quantile,c(0.025,0.975))
# [1]  1.034173 -1.008076

### the random effect from DPPM ####
# ui.est<-jags.fit_dp_int$BUGSoutput$sims.list$gamma





