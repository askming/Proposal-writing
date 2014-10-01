rm(list=ls())
require(R2jags)
# load simulation data
setwd("/work/02784/myang3/proposal_sim_revision/data_in/0930")

load(dir()[6])

# load model files

### BLQMM re_int only, using DPM, model file ##########
setwd("/work/02784/myang3/proposal_sim_revision/model/quantile_regression")
model.file <- '/work/02784/myang3/proposal_sim_revision/model/quantile_regression/BQLMM_DP_v5.txt'



# define function to run gibbs sampler using differet prior specifications
# for blqmm using dpm, re_int and re_slope
runjags = function(data, y='y', x='x', tau, model, I, K){
	#### parameters of interest #####
	# gamma[] # beta[] 
	# change the model parameter to use diff priors for r.e.
	I<-100 # number of subjects
	J<-5 # number of repeated measures
	N<-K # always people use 52 ## increasing N makes big difference

	jags.data <- list(y=matrix(unlist(data[[y]]),nrow=I),x=array(unlist(data[[x]]),dim=c(I,J,2)), I=I, J=J, N=N, qt=tau)
	jags.params <- c("beta","gamma1","gamma2","alpha","alpha2", "p", "sigma")
	jags.inits <- function(){list(beta=c(0.1,0.1), sigma=0.1, b=0.1, sigmaF0=0.1, mu0=0, alpha=1)}
	jags.fit <- jags(data=jags.data, inits=jags.inits, jags.params, n.iter=2000, model.file=model)
	beta.est<-jags.fit$BUGSoutput$sims.list$beta
	sigma.est<-jags.fit$BUGSoutput$sims.list$sigma
	re.int<-jags.fit$BUGSoutput$sims.list$gamma1
	re.slope<-jags.fit$BUGSoutput$sims.list$gamma2
	alpha1<-jags.fit$BUGSoutput$sims.list$alpha
	alpha2<-jags.fit$BUGSoutput$sims.list$alpha2
	out = list(beta=beta.est, re_int=re.int, re_slope=re.slope, sigma=sigma.est, alpha1=alpha1, alpha2=alpha2)
	return(out)	
}

# for blqmm using dpm, re_int only
runjags2 = function(data, y='y', x='x', tau, model, I, K){
	#### parameters of interest #####
	# gamma[] # beta[] 
	# change the model parameter to use diff priors for r.e.
	I<-100 # number of subjects
	J<-5 # number of repeated measures
	N<-K # always people use 52 ## increasing N makes big difference

	jags.data <- list(y=matrix(unlist(data[[y]]),nrow=I),x=array(unlist(data[[x]]),dim=c(I,J,2)), I=I, J=J, N=N, qt=tau)
	jags.params <- c("beta","gamma1","alpha", "p", "sigma")
	jags.inits <- function(){list(beta=c(0.1,0.1), sigma=0.1, b=0.1, sigmaF0=0.1, mu0=0, alpha=1)}
	jags.fit <- jags(data=jags.data, inits=jags.inits, jags.params, n.iter=2000, model.file=model)
	beta.est<-jags.fit$BUGSoutput$sims.list$beta
	sigma.est<-jags.fit$BUGSoutput$sims.list$sigma
	re.int<-jags.fit$BUGSoutput$sims.list$gamma1
	alpha1<-jags.fit$BUGSoutput$sims.list$alpha
	out = list(beta=beta.est, re_int=re.int, sigma=sigma.est, alpha1=alpha1)
	return(out)	
}


#### parallel
library(foreach)
#library(doRNG)
library(doParallel)
# library(doSNOW)
#--------------#
# setting up multinodes multicore computing env.
# c339-215.ls4.tacc.utexas.edu
# raw_PE <- switch(jEOM+1, F = system("echo $HOSTNAME",intern=T), T = system("cat $PE_HOSTFILE",intern=T))      # wrong! $HOSTNAME in launcher env will be always the same - the master.
jEOM = F
# setting up multinodes multicore computing env.
# c339-215.ls4.tacc.utexas.edu
# raw_PE <- switch(jEOM+1, F = system("echo $HOSTNAME",intern=T), T = system("cat $PE_HOSTFILE",intern=T))      # wrong! $HOSTNAME in launcher env will be always the same - the master.
raw_PE <- switch(jEOM+1, F = "localhost.ls4.tacc.utexas.edu", T = system("cat $PE_HOSTFILE",intern=T))  # or use $HOSTNAME ,`hostname -s`
catt('raw_PE is',raw_PE)
nodes_name <- sub("^([\\w-]+)\\..*$","\\1",raw_PE,perl=T)
nodes_slots <- switch(jEOM+1, F = as.numeric(system("cat /proc/cpuinfo | grep processor|wc -l",intern=T)),T = as.numeric( sub("^([\\w-]+)\\..* (\\d+?) .*$","\\2",raw_PE,perl=T) ) )

#---------------------------------------#
######################################
## code confirmed correct.
socketHosts_name <- rep(nodes_name,times=nodes_slots)
cl <- makePSOCKcluster(socketHosts_name,homogeneous=TRUE)
# cl <- makeNWScluster(socketHosts_name,port=22)
clusterCall(cl, function() Sys.info()[c("nodename","machine")])
# clusterEvalQ(cl,system('export LD_LIBRARY_PATH=/work/02784/myang3/softwares/TACC_JAGS/JAGS-3.4.0/lib:$LD_LIBRARY_PATH'))
# clusterEvalQ(cl, {library(rjags)} )
registerDoParallel(cl)
# registerDoRNG(1234)
clusterExport(cl=cl,ls())


# for blqmm usign dpm
t1 = Sys.time()
mn_t3_1re_30_ncdp <- foreach(qt=c(0.25,0.5,0.75),.packages=c("rjags","R2jags","foreach")) %do% {
  foreach(i=1:30,.packages=c("rjags","R2jags","foreach"),.verbose=T )  %dopar% {
  	set.seed(123)
  temp <- runjags2(mn_t3_1re_30[[i]], y='y', x='x', tau=qt, I=100, K=200, model=model.file)
  temp
  	}
  }  

t2 = Sys.time()
(t2-t1)




###### collect & plot the results ######
beta.est<-jags.fit_dp_int$BUGSoutput$sims.list$beta
# alpha.est<-jags.fit_cdp$BUGSoutput$sims.list$alpha
# p.est<-jags.fit_cdp$BUGSoutput$sims.list$p
# apply(beta.est,2,mean)
# apply(beta.est,2,quantile,c(0.025,0.975))
# [1]  1.034173 -1.008076

### the random effect from DPPM ####
# ui.est<-jags.fit_dp_int$BUGSoutput$sims.list$gamma





