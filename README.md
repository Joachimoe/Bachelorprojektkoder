# Koder i R # OBS: Det tager omtrent 5 minutter at simulere denne, så lad den blot køre
Forventet.n <- seq(from=30, to=2000, by=50)     # Samplestørrelse
Styrke <- rep(NA, length(Forventet.n))          # Matrice til at "store" estimater
alpha <- 0.05                                   # sign. niveau
sims <- 1000                                    # Antal sim for hvert N

#### YDRE LOPP ####
for (j in 1:length(Forventet.n)){
  N <- Forventet.n [j]                           
  
  signifikante.eksperimenter <- rep(NA, sims)     

  
  #### INDRE LOOP ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=60, sd=20)              
    tau <- 5                                       # Effekt af lavstatusgruppe
    Y1 <- Y0 + tau                                 
    Z.sim <- rbinom(n=N, size=1, prob=.5)          
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               
    fit.sim <- lm(Y.sim ~ Z.sim)                   
    p.value <- summary(fit.sim)$coefficients[2,4] 
    signifikante.eksperimenter[i] <- (p.value <= alpha) 
    0.05
  }
  
  Styrke[j] <- mean(signifikante.eksperimenter)      
}
plot(Forventet.n, Styrke, ylim=c(0,1))
abline(h=0.8, col="blue")
abline(h=0.9, col="blue")

