
require('R2jags')

m1.mod <-"
model {
  for (i in 1:N){
    Y[i] ~ dnorm(mu, invS2)}
  mu ~ dnorm(a,1/b)
  invS2 ~ dgamma(.5,.5*r)
}
"

runMod1 = function(Y,M=1000){
  setup=list(
    "Y" = Y,
    "N" = length(Y))
  prior =list("a"=100,"b"=5^2,"r"=15^2)
  pars = c("mu","invS2")
  out=jags(data=c(setup,prior), 
           parameters=pars, 
           model.file = textConnection(m1.mod), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}


m2.mod <-"

model {
  eta ~ dnorm(a,1/b)
  invDel ~dgamma(.5,.5*r2)
  invS2 ~ dgamma(.5,.5*r1)
  for (i in 1:I){
    mu[i] ~ dnorm(eta,invDel)}
  for (n in 1:N){
    Y[n] ~ dnorm(mu[sub[n]],invS2)}  
}
"

runM2=function(dat,prior,M=1000){
  setup=list(
    "Y" = dat$y,
    "I" = length(unique(dat$sub)),
    "N" = length(dat$y),
    "sub" = dat$sub)
  pars = c("mu","invS2","eta","invDel")
  out=jags(data=c(setup,prior), 
           parameters=pars, 
           model.file = textConnection(m2.mod), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}



m2a.mod <-"

model {
  mu ~ dnorm(a,1/b)
  invDel ~dgamma(.5,.5*r2)
  invS2 ~ dgamma(.5,.5*r1)
  for (i in 1:I){
    alpha[i] ~ dnorm(0,invDel)}
  for (n in 1:N){
    Y[n] ~ dnorm(mu+alpha[sub[n]],invS2)}  
}
"

runM2a=function(dat,prior,M=1000){
  setup=list(
    "Y" = dat$y,
    "I" = length(unique(dat$sub)),
    "N" = length(dat$y),
    "sub" = dat$sub)
  pars = c("mu","invS2","alpha","invDel")
  out=jags(data=c(setup,prior), 
           parameters=pars, 
           model.file = textConnection(m2a.mod), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}



m2a.mod <-"

model {
  mu ~ dnorm(a,1/b)
  invDel ~dgamma(.5,.5*r2)
  invS2 ~ dgamma(.5,.5*r1)
  for (i in 1:I){
    alpha[i] ~ dnorm(0,invDel)}
  for (n in 1:N){
    Y[n] ~ dnorm(mu+alpha[sub[n]],invS2)}  
}
"

runM2a=function(dat,prior,M=1000){
  setup=list(
    "Y" = dat$y,
    "I" = length(unique(dat$sub)),
    "N" = length(dat$y),
    "sub" = dat$sub)
  pars = c("mu","invS2","alpha","invDel")
  out=jags(data=c(setup,prior), 
           parameters=pars, 
           model.file = textConnection(m2a.mod), 
           n.chains=1,n.iter=M,n.burnin=M/10,n.thin=1)
  return(out)}

