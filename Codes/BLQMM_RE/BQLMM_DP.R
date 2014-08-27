# Simulation study
# Bayesian linear quantile mixed model using DPM prior
# estimation of the fixed effects

rm(list=ls())
setwd("/work/02784/myang3/proposal_sim_revision//data_in")
load(dir()[3])
require(R2jags)		

#### define the custom jags function ########
run_jags<-function(data,tau,I,K){
	# K is number of truncation
	setwd("/work/02784/myang3/blqmm_pd/model")
	model.file<-"/work/02784/myang3/blqmm_pd/model/BQLMM_DP.txt"
	N <- length(data$y)
	jags.data <- list(y=data$y, x=data$x, qt=tau, t=data$t, I=I, K=K)
 	jags.params<-c("beta","gamma","sigma")
 	jags.inits<-function(){	list(beta=c(0.1,0.1), gamma=rep(0.1,I), sigma=0.1, alpha=0.1, er=rep(0.1,N))	
  }	
  jags(data=jags.data, inits=jags.inits, jags.params, n.iter=2000, n.burnin=1000,model.file=model.file)	
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

######## run model on data ########
mn_t3_30_DPoutput <- foreach(qt=c(0.25,0.5,0.75),.packages=c("rjags","R2jags","foreach")) %do% {
  foreach(i=1:300,.packages=c("rjags","R2jags","foreach"),.verbose=T )  %dopar% {
  	set.seed(123)
# system.time(results <- dforeach(i = 1:100) %dopar% {
  temp <- run_jags(mn_t3_300[[i]],tau=qt,I=100,K=200)
  temp$BUGSoutput$sims.list
  	}
}  

######### save data ########
setwd("/work/02784/myang3/proposal_sim_revision/data_out")
save( mn_t3_30_DPoutput,file="mn_t3_30_DPoutput.Rdata")
