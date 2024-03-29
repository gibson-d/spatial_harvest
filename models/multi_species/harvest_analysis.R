#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Contents:
# The model code and data necessary to run the multispecies spatially-explicit direct recovery harvest model in the Nimble program language.
# The six-species model, and likely the three-species model, requires access to a more advanced computation cluster due to the physical size of the model space.
# 
# Section A) Data and Library prep
# Section B) Model Code
# Section C) Model Run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########################################################################################################################
# Section A: Load libraries and Data
########################################################################################################################
# Load libraries
library(nimble)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(tidyr)
library(coda)

# Load three or six-species data/model packages
#load('six_species.rdata')
load('three_species.rdata')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Data Contents:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1) harvest_model: Nimble model code for the spatially-explicit harvest model (also presented below if changes need to be made)
# 2) nimble.data: The nimble data file necessary to run the harvest model. Consists of the band-recoveries for the multispecies model and the control-reward band study
# 3) nimble.constants: A list of the structural constants, identifiers, and covariate data necessary to run the harvest model
# 4) initsFunction: The function used to generate initial values. 
# The initsfunction may not work, but inits_1, inits_2, and inits_3 and three random realizations of initial values that can be used to start the Bayesian updating procedure.


########################################################################################################################
# Section B: Model Code
########################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Start Nimble Model Code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
harvest_model <- nimbleCode({
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 1) Model Priors and Constraints
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Fixed Crippling rate (see Riecke et al. 2022)
  cr  <- 0.2 
  
  # Global mean intercept and error     
      global_mu ~ dnorm(-6, sd = 2)
   global_sigma ~ T(dt(0, pow(2.5, -2), 1),0,)
  
  # Residual variation in mean recovery rates associated with release location
  local_sigma  ~ T(dt(0, pow(2.5, -2), 1),0,)
  
  # Residual spatial and temporal variation in recovery rates
  for (i in 1:n.species){  
    sigma_space[i]  ~ T(dt(0, pow(2.5, -2), 1),0,)
        tau_eps[i] <- 1/sigma_space[i]^2
       sig_time[i] ~ T(dt(0, pow(2.5, -2), 1),0,)    
  }
  
  # Era-specific mean and error for reporting rates
  for(k in 1:2){
    mu_rr[k] ~ dnorm(0, sd = 1)
    sd_rr[k] ~ T(dt(0, pow(2.5, -2), 1),0,)
  }
  
  # Annual estimates for reporting rates
  for (t in 1:n.year){
               mu[t] ~  dnorm(mu_rr[era[t]], sd = sd_rr[era[t]])
       logit(rr[t,2]) <-  mu[t]   # Control/Global
    logit(rr_keep[t]) <-  mu[t]
             rr[t,1] <- 1            # Reward
  }
  
  # Regression Coefficients for wetland habitat variables
  for(g in 1:n.species){
    for(k in 1:3){
       mu_beta_wet[g,k]~dnorm(0,1)
      sig_beta_wet[g,k]~ T(dt(0, pow(2.5, -2), 1),0,)
    }
    
    for(j in 1:5){
      beta_wet_delta[g,j] ~ dnorm(mu_beta_wet[g,1], sd = sig_beta_wet[g,1])
          beta_delta[g,j] ~ dnorm(mu_beta_wet[g,2], sd = sig_beta_wet[g,2])
        beta_wetland[g,j] ~ dnorm(mu_beta_wet[g,3], sd = sig_beta_wet[g,3])
    }
    
    # Regression Coefficients for time trend and climate variables
      mu_beta[g] ~ dlogis(0,1)
    beta_temp[g] ~ dlogis(0,1)
      sig_def[g] ~ T(dt(0, pow(2.5, -2), 1),0,)
    
      mu_trend[g] ~ dlogis(0,1)
    temp_trend[g] ~ dlogis(0,1)
    
    for(k in 1:n.regions){
      beta_def[g,k] ~ dnorm(mu_beta[g] + beta_temp[g] * tmax[k], sd = sig_def[g])
    }
  }
  
  # Harvest rate intecept (species by sex by age)
  for(g in 1:n.species){
    for(l in 1:n.sex){
      for(i in 1:n.age){
        mean.psi[g,l,i] ~ dnorm(global_mu, sd = global_sigma)
      }
    }
  }
  
  # Harvest rate intecept (species by sex by age by banding location)
  for(t in 1:n.type){
    for(l in 1:n.releases){
      mu.psi[l,t] ~ dnorm(mean.psi[species[t],sex[t],age[t]], sd = local_sigma)
    }
  }
  
  # Hierachical time trend effect informed by regional patterns in temperature
  for (i in 1:n.species){
    lam_trend[i] ~ dexp(.1)
    for(k in 1:n.regions){
      for(l in 1:n.releases){
        # Fit using Bayesian lasso to reduce overfitting
        beta_trend[i,l,k] ~ ddexp(mu_trend[i] + temp_trend[i] * tmax[k], scale = 1/ pow(lam_trend[i],.5))
      }
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 2) Estimate spatiotemporal variation in the recovery process
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  for(i in 1:n.species){
     Delta[i,i] <- pow(Q[i,i], -0.5)
    Lambda[i,i] <- sig_time[i]
  }
  for (i in 2:n.species){
    for (j in 1:(i-1)){
      Lambda[i,j] <- 0
       Delta[i,j] <- 0
    }
  }
  
  # Variance-Covariance Matrix for temporal variation
  Sigma[1:n.species,1:n.species] <- Lambda[1:n.species,1:n.species] %*% P[1:n.species,1:n.species] %*% Lambda[1:n.species,1:n.species]
      Q[1:n.species,1:n.species] ~ dinvwish(S = I[1:n.species,1:n.species], df = n.species + 1)
      P[1:n.species,1:n.species] <- Delta[1:n.species,1:n.species] %*% Q[1:n.species,1:n.species] %*% Delta[1:n.species,1:n.species]
  
  # Cross-species correlations in temporal variation in harvest rates
  for (k in 1:n.species){
    for(j in 1:n.species){
      rho[j,k] <- Sigma[j,k]/sqrt(Sigma[j, j] * Sigma[k, k])
    }
  }
  
  # CAR model describing spatial variation in the recovery process
  for(l in 1:n.releases){
    for (i in 1:n.species){
      # The intercept is being estimated outside of the CAR model (i.e., zero_mean = 1) to allow for a multi-level modeling of the intercept terms
      epsilon_prime[i,l,1:n.regions] ~ dcar_normal(adj[1:L], weights[1:L], num[1:n.regions], tau_eps[i], zero_mean = 1)
    }
    
    # Temporal variation centered on the estimated spatial variation in the recovery process
    for(j in 1:n.year){
      for(k in 1:n.regions){
        epsilon[1:n.species,l,k,j] ~ dmnorm( epsilon_prime[1:n.species,l,k] , cov = Sigma[1:n.species,1:n.species])
      }
    }
  }
  
  # Specify Log-linear harvest rate model
  for(j in 1:59){
    for(l in 1:n.releases){
      for(t in 1:n.type){
        for(k in 1:n.regions){
          psi.prime[j,k,l,t] <- exp(mu.psi[l,t] + 
                                      beta_trend[species[t],l,k] * time[j] +  beta_def[species[t],k] * wdef[j,k] +
                                      beta_wetland[species[t],rep_groups[k]] * wetland[k,decade[j]] + beta_delta[species[t],rep_groups[k]] * delta_wet[k,decade[j]] +
                                      beta_wet_delta[species[t],rep_groups[k]] * delta_wet[k,decade[j]] * wetland[k,decade[j]] + epsilon[species[t],l,k,j] )
        }
        
        psi.prime[j,n.regions+1,l,t] <- 1 # not immediately harvested
        
        # Spatially-explicit harvest rates for each type of dabbling duck
        for(k in 1:n.regions){
          kappa[j,k,l,t] <-  psi.prime[j,k,l,t]/sum(psi.prime[j,1:(n.regions+1),l,t])
        }
        # Derive an oVerall Harvest Rate
        kap[t,l,j] <- sum( kappa[j,1:n.regions,l,t])
      }
    }
  }
  # Minor adjustment to the log-linear covariate model during the last 2 years.
  for(j in 60:n.year){
    for(l in 1:n.releases){
      for(t in 1:n.type){
        for(k in 1:n.regions){
          psi.prime[j,k,l,t] <- exp(mu.psi[l,t] + beta_trend[species[t],l,k] * time[j] + beta_wetland[species[t],rep_groups[k]] * wetland[k,decade[j]] +
                                      beta_delta[species[t],rep_groups[k]] * delta_wet[k,decade[j]] + beta_wet_delta[species[t],rep_groups[k]] * delta_wet[k,decade[j]] * wetland[k,decade[j]] +
                                      epsilon[species[t],l,k,j]) 
        }
        psi.prime[j,n.regions+1,l,t] <- 1
        # Spatially-explicit harvest rates for each type of dabbling duck
        for(k in 1:n.regions){
          kappa[j,k,l,t] <-  psi.prime[j,k,l,t]/sum(psi.prime[j,1:(n.regions+1),l,t])
        }
        # OVerall Harvest Rate
        kap[t,l,j] <- sum( kappa[j,1:n.regions,l,t])
      }
    }
  }
  
  # Harvest rate adjustment between the global population and the control/reward band study
  rc_adj ~ dunif(.5, 2)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#     
  # 3) Model Likelihood for multispecies spatial-explicit direct recovery model
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  
  # Calculate recovery probability: harvest rate * reporting rate * (1 - crippling rate)
  for(i in 1:N){
    for(k in 1:n.regions){
      f[i,k]  <- kappa[year[i],k,releases[i],type[i]] * (1 - cr) * rr[year[i],2]
    }
           f[i,n.regions+1] <- 1 - sum(f[i,1:n.regions])
    marr[i,1:(n.regions+1)] ~ dmulti(f[i,1:(n.regions+1)],  rel[i])   
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # 4) Reward-Control Band Model Likelihood 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  for(i in 1:N_rc){
    for(k in 1:n.regions){
      f_rc[i,k]  <- kappa[year_rc[i],k,releases_rc[i],type_rc[i]] * rc_adj * (1 - cr) * rr[year_rc[i],re_co[i]]
    }
           f_rc[i,n.regions+1] <- 1 - sum(f_rc[i,1:n.regions])
    marr_rc[i,1:(n.regions+1)] ~ dmulti(f_rc[i,1:(n.regions+1)],  rel_rc[i]) 
  }
  
})
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# End Nimble Model Code
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########################################################################################################################
# Section C: Model Run
########################################################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Nimble parallel process model run
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
inits <- list(inits_1, inits_2, inits_3)

nc <- 3 # number of chains

cl <-makeCluster(nc,timeout=5184000)

clusterExport(cl, c("harvest_model", "inits", "nimble.data", "nimble.constants"))

for (j in seq_along(cl)) {
  
  clusterExport(cl[j], "inits")
}

out <- clusterEvalQ(cl, {
  library(nimble)
  library(coda)
model <- nimbleModel( code = harvest_model, constants = nimble.constants,  dat =  nimble.data, inits = inits[[j]])

model$initializeInfo()

model$calculate()

modelConf  <- configureMCMC(model, useConjugacy = FALSE,thin2 = 5,
                            
                            monitors = c('mean.psi','mu.psi','global_mu','global_sigma', 'rc_adj','rho','beta_trend','beta_delta','beta_wetland','beta_wet_delta',
                                         'local_sigma','sigma_space','sig_time', 'beta_def','mu_beta','sig_def','beta_temp', 'mu_trend','temp_trend',
                                         'rr_keep','mu_rr','sd_rr')) #,
                            
                           # monitors2 = c('kappa','kap')) # monitoring these parameters requires a lot of HD space

modelMCMC  <- buildMCMC( modelConf )
Cmodel     <- compileNimble(model)
CmodelMCMC <- compileNimble(modelMCMC)

CmodelMCMC$run(5000, thin = 2, thin2 = 10, reset = FALSE)

return(list( as.mcmc(as.matrix(CmodelMCMC$mvSamples)),
             as.mcmc(as.matrix(CmodelMCMC$mvSamples2))))

# return(as.mcmc(out1))
})

burn <- 500

samples1 <- list(chain1 =   out[[1]][[1]][-c(1:(burn+1),], 
                 chain2 =   out[[2]][[1]][-c(1:(burn+1),],
                 chain3 =   out[[3]][[1]][-c(1:(burn+1),])


samples2 <- list(chain1 =   out[[1]][[2]][-c(1:(burn/5+1),], 
                 chain2 =   out[[2]][[2]][-c(1:(burn/5+1),],
                 chain3 =   out[[3]][[2]][-c(1:(burn/5+1),])


mcmcList1 <- as.mcmc.list(lapply(samples1, mcmc))
mcmcList2 <- as.mcmc.list(lapply(samples2, mcmc))

# Continue running the model

start_time <- Sys.time()
out2 <- clusterEvalQ(cl, {
  CmodelMCMC$run(5000, reset = FALSE, thin = 2, thin2 = 10)
  return(list( as.mcmc(as.matrix(CmodelMCMC$mvSamples)),
               as.mcmc(as.matrix(CmodelMCMC$mvSamples2))))
  
})

end_time <- Sys.time()
end_time - start_time

burn <- 500

samples1 <- list(chain1 =   out2[[1]][[1]][-c(1:(burn+1),], 
                 chain2 =   out2[[2]][[1]][-c(1:(burn+1),],
                 chain3 =   out2[[3]][[1]][-c(1:(burn+1),])


samples2 <- list(chain1 =   out2[[1]][[2]][-c(1:(burn/5+1),], 
                 chain2 =   out2[[2]][[2]][-c(1:(burn/5+1),],
                 chain3 =   out2[[3]][[2]][-c(1:(burn/5+1),])

mcmcList1 <- as.mcmc.list(lapply(samples1, mcmc))
mcmcList2 <- as.mcmc.list(lapply(samples2, mcmc))
