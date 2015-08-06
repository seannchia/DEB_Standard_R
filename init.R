init <- function(){

  #---------------------------------------------------------------
  # Define simulation and individual features
  #  
  # simu: structure with individual features (parameters, env...)
  #        (simu(i).par = parameters of individual i)
  #
  # called by : main.m
  # calls : set_par.m
  #
  # 2013/03/15 - Laure Pecquerie
  #--------------------------------------------------------------

  t0 = 1# initial time # January 1st
  tm = 10 * 365# final time  
  ts = t0  # spawning time

  # choose if environment is constant or variable
  envT = 1# 1 = T constant, 2 = T variable
  envX = 1# 1 = constant food, 2 = variable food
  
  # color for plots
  if (envT == 1 && envX == 1){
    col = 'k'
  } else if (envT ==2 && envX == 1){
    col = 'b'
  } else if (envT == 1 && envX == 2){
    col = 'g'
  } else{
    col = 'r'
  }
  
  # initial forcing variable
  T = 19 + 273
  
  f = 0.99
  
  E_0 = 3.6980e5 # J, initial reserve energy
  V_0 = 0.0001 # cm^3, initial structural volume
  E_H0 = 0 # J, initial cum. energy invested into maturation
  E_R0 = 0 # J, reproduction buffer
  
  par = set_par()
  simu = list(par = par,
              t0 = t0,
              tm = tm,
              ts = ts,
              envT = envT,
              envX = envX,
              Tinit = T,
              Xinit = (f* par[22]/(1 - f)),
              EVHR_init = c(E_0, V_0, E_H0, E_R0),
              col = col
              )
  return(simu)
}