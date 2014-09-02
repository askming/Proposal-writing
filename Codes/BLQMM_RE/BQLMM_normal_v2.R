# Simulation study
# Bayesian linear quantile mixed model using Gaussian prior
# estimation of the fixed effects

rm(list=ls())
setwd("/work/02784/myang3/proposal_sim_revision/data_in")
load(dir()[4])
require(R2jags)

setwd("/work/02784/myang3/proposal_sim_revision/model/quantile_regression")
model_normal<-"/work/02784/myang3/proposal_sim_revision/model/quantile_regression/BQLMM_normal_v2.txt"

run_jags<-function(data, y='y', x='x', tau, model){	
	I = 100
	J = 5 
	jags.data <- list(y=matrix(unlist(data[[y]]),nrow=I),x=array(unlist(data[[x]]),dim=c(I,J,2)), I=I, J=J, qt=tau)
 	jags.params<-c("beta","gamma1","gamma2", "sigma")
 	jags.inits<-function(){		list(beta=c(0.1,0.1), gamma1=rep(0.1,I), gamma2=rep(0.1,I), sigma=0.1, gamma1.phi=0.1, gamma2.phi=0.1)	
  }	
  	jags.fit = jags(data=jags.data, inits=jags.inits, jags.params, n.iter=20000, n.burnin=10000,model.file=model)	
  	beta.est<-jags.fit$BUGSoutput$sims.list$beta
	sigma.est<-jags.fit$BUGSoutput$sims.list$sigma
	re.int<-jags.fit$BUGSoutput$sims.list$gamma1
	re.slope<-jags.fit$BUGSoutput$sims.list$gamma2
	out = list(beta=beta.est, re_int=re.int, re_slope=re.slope, sigma=sigma.est)
	out

}


######## data1
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

Sys.time()
mn_t3_30_new_normaloutput <- foreach(qt=c(0.25,0.5,0.75),.packages=c("rjags","R2jags","foreach")) %do% {
  foreach(i=1:30,.packages=c("rjags","R2jags","foreach"),.verbose=T )  %dopar% {
  	set.seed(123)
# system.time(results <- dforeach(i = 1:100) %dopar% {
  temp <- run_jags(mn_t3_2re_30_new[[i]], y='y', x='x', tau=qt, model=model_normal)
  temp
  	}
  }  

#names(results) <- RdataStrings
setwd("/work/02784/myang3/proposal_sim_revision/data_out/BQLMM")
save( mn_t3_30_new_normaloutput,file="mn_t3_30_new_normaloutput.Rdata")

Sys.time()


