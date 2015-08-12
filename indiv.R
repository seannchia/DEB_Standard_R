indiv<-function(simu){
  #---------------------------------------------------------------
  # Compute model predictions (numerical integration)
  #  
  # simu: 1-structure with individual features (parameters, env...),
  #             see init.m
  #
  # tEVHR : (nt,4) matrix with time and state variables
  #         t, d, time
  #         E , J, reserve energy
  #         V , cm^3, structural volume
  #         E_H , J , cumulated energy inversted into maturity
  #         E_R , J, reproduction buffer
  #         
  # called by : main.m
  # calls : flux.m
  #
  # 2013/03/15 - Laure Pecquerie
  # Must install package "deSolve"
  #--------------------------------------------------------------
  # EVHR = matrix(c(1,2,3,4), nrow=1, ncol=4, byrow=FALSE) # temp for testing script errors before I figure ode out
  # t = matrix(c(1), nrow=1, ncol=1, byrow=FALSE) # temp for testing script errors before I figure ode out
  
  library(deSolve)
  
  par = simu$par
  tc = simu$t0 # set current time to initial time
  EVHR_tc = simu$EVHR_init # set current values of state variables to initial values
  
  cnames = c("t","E","V","H","R")
  tEVHR = matrix(numeric(0), nrow=1, ncol=5, byrow=FALSE, dimnames=list("",cnames)) # initialize outputs
  i = 0 # initialize year index
  while (tc < simu$tm){
    i = i + 1 # spawning every year
    tnext = simu$ts + 365 * i # integrate between two spawning dates (one spawning each year)
      if (tnext > simu$tm){
          tnext = simu$tm
      }
      
    t = seq(from = tc, to = tnext, by = 1)
    EVHR<-rk(EVHR_tc, t,func=flux,parms=simu,method="rk23bs")
    tEVHR = rbind(tEVHR, EVHR) # append to output
    E_Hc = EVHR[nrow(EVHR),4]
    if (E_Hc >= par[14]){ # E_Hp
      EVHR[nrow(EVHR),5] = 0
    }
    EVHR_tc = tail(EVHR, n=1)[2:5]
    tc = tail(t, n=1)
  }
  tEVHR=tEVHR[2:dim(tEVHR)[1],]
  return(tEVHR)
}