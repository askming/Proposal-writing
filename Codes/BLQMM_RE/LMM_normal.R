rm(list=ls())
require(R2jags)
# load simulation data
setwd("/work/02784/myang3/proposal_sim_revision/data_in")
load("cdp_data.Rdata")

setwd("/work/02784/myang3/proposal_sim_revision/model")
### LMM using normal prior, model file ##########
normal_model <-"/work/02784/myang3/proposal_sim_revision/model/model_normal_int.txt"


###### normal prior model #####
runjags_normal = function(data, y, niter){
	normal_model <-"/work/02784/myang3/proposal_sim_revision/model/model_normal_int.txt"
	I<-150
	J<-10
	jags.data <- list(y=matrix(unlist(data[[y]]), nrow=I), x=array(unlist(data[['x']]), dim=c(I,J,3)), I=I, J=J)
	jags.params <- c("beta","gamma","sigma")
	jags.inits  <-function(){list(beta=c(0.1, 0.1, 0.1), sigma=0.1)}
	jags.fit <- jags(data=jags.data, inits=jags.inits, jags.params, n.iter=niter, model.file=normal_model)
	
	beta.est <- jags.fit$BUGSoutput$sims.list$beta
	sigma.est <- jags.fit$BUGSoutput$sims.list$sigma
	re.est_normal <- jags.fit$BUGSoutput$sims.list$gamma
	out = list(beta=beta.est, RE=re.est_normal, sigma=sigma.est)
	out
}

jags_fit_y11_normal = runjags_normal(data=sim_data2, y='y11', niter=20000)
jags_fit_y33_normal = runjags_normal(data=sim_data2, y='y33', niter=20000)
jags_fit_y44_normal = runjags_normal(data=sim_data2, y='y44', niter=20000)

setwd("/work/02784/myang3/proposal_sim_revision/data_out")
save(jags_fit_y11_normal,file="y11_normal.Rdata")
save(jags_fit_y33_normal,file="y33_normal.Rdata")
save(jags_fit_y44_normal,file="y44_normal.Rdata")

# summary statistics for posterior samples
beta = fit_normalprior$beta
p_mean_beta = apply(beta, 2, mean)
ci_beta = apply(beta, 2, quantile, p=c(0.025, 0.975))


