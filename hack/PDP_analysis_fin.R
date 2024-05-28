
dat <- readRDS("datPD.RDS")

data <- dat |>
  dplyr::group_by(sub,cond) |>
  dplyr::summarize(
    Y = sum(resp),
    N = length(resp)
  ) |> 
  as.data.frame(.)
data


#-------------

#A und R for each person

mod3 <- "
model{
  for (i in 1:N){
    R[i] ~ dunif(a,b)
    A[i] ~ dunif(a,b)
      
    p_inc[i] = R[i]+(1-R[i])*A[i]
    p_exc[i] = (1-R[i])*A[i]
    
    Y[(i*2) -1] ~ dbin(p_inc[i],50)
    Y[(i*2)] ~ dbin(p_exc[i],50)
  }
}"


runMod=function(data,M=1000){
  setup=list(
    "Y" = data$Y,
    "N" = length(unique(data$sub)))
  prior =list("a"=0,
              "b"=1)
  pars = c("R","A")
  out=jags(data=c(setup, prior), 
           parameters=pars, 
           model.file = textConnection(mod3), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}

out=runMod(data)

# print distribution of parameters
R=out$BUGSoutput$sims.list$R
hist(R)
A=out$BUGSoutput$sims.list$A
hist(A)


#-------------

#A und R for each person from normal

mod3 <- "
model{
  for (i in 1:N){
    probit(R[i]) = thetaR[i]
    thetaR[i] ~ dnorm(a,b)
    
    probit(A[i]) = thetaA[i]
    thetaA[i] ~ dnorm(a,b)
      
    p_inc[i] = R[i]+(1-R[i])*A[i]
    p_exc[i] = (1-R[i])*A[i]
    
    Y[(i*2) -1] ~ dbin(p_inc[i],50)
    Y[(i*2)] ~ dbin(p_exc[i],50)
  }
}"

runMod=function(data,M=1000){
  setup=list(
    "Y" = data$Y,
    "N" = length(unique(data$sub)))
  prior =list("a"=0.5,
              "b"=0.5)
  pars = c("R","A","thetaR", "thetaA")
  out=jags(data=c(setup, prior), 
           parameters=pars, 
           model.file = textConnection(mod3), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}

out=runMod(data)

# print distribution of parameters
R=out$BUGSoutput$sims.list$R
hist(R)
A=out$BUGSoutput$sims.list$A
hist(A)

#  Stan model
#
stan_ls <- list(
  N = length(unique(data$sub)), 
  Y = data$Y
)

file <- file.path("./", "pdp.stan")
mdl <- cmdstanr::cmdstan_model(file, pedantic = TRUE)
fit <- mdl$sample(data=stan_ls, parallel_chains = 4)
fit$cmdstan_diagnose()
