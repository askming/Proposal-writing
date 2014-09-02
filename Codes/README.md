R code files 
================

- **SSD_RAD.R**. Used to calculated sum of squared difference (SSD) of the predicted REs and the imperial ones; and relative absolute deviation (RAD) of b/t the posterior predictive values and the imperial data.

- **BLQMM_RE** . Folder. R files to generate data; for running the MCMC simulations; and to summarize the results	
	- BLQMM_DP.R/ BLQMM_DP_v2.R / BLQMM_DP_v3.R. R code files to run MCMC simulations using CDPM prior for the random effects. v3 is the latest version.
	- BLQMM_normal.R / BLQMM_normal_v2.R. R code files to run MCMC simulations using normal prior for the random effects. v2 is the latest version.
	- simulatedata.R / simulate data_v2.R. R code used to generate datasets. v2 is the latest version.
	- summarize.R. R code used to summarize the simulation results.

- **LMM_RE** . Folder. R files to run MCMC models for the linear mixed models.
	- LMM_DP.R. uses CDPM prior to model the random effects
	- LMM_normal.R. uses normal prior to model the REs.