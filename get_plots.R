get_plots<-function(simu){

  par = simu$par # parameters
  E_Hp = par[14]
  K = par[22] 
  Em = par[5] / par[6] # J/cm^3  [Em] = pAm / v

  # time , environmental forcing
  t = simu$tEVHR[,1]
  
  if (simu$envX == 1){
        X = rep(simu$Xinit, length(t))
  } else {
      X = food(t)
  }
  
  if (simu$envT == 1){
      T = rep(simu$Tinit, length(t)) - 273 # in DegC
  } else {
      T = temp(t) - 273
  }
  
  f = X/(X + K) #scaled functional response
  
  # state variables
  E = simu$tEVHR[,2]
  V = simu$tEVHR[,3]
  E_H = simu$tEVHR[,4]
  E_R = simu$tEVHR[,5]
  
  sc_res_dens = E/(V*Em) # -, scaled reserve density e
  
  # spawning date indices
  # If [(E_R==0)][i] is found to equal [(E_H>=E_Hp)][j] then the value returned in the i-th position of the return value is j.
  i_sp = which(E_R==0)[which(!is.na(match(which(E_R==0),which(E_H>=E_Hp))))]
  
  i_sp = i_sp - 1  # look at the preceding line with E_R value before spawning
  
   
  # observable quantities
  Lw = simu$obs[,2]
  Ww = simu$obs[,3]
  Ew = simu$obs[,4]
  Fecundity  = simu$obs[,5]

  graphics.off() # Close all previous windows
  
  windows() # Plot window 1
  dev.set(2) # plot window 1 is taken by a NULL window
  # Matrix specifies the order in cwhich the graphs are plotted, "0" means leave the space blank
  layout(matrix(c(1,2,0,3,4,5,6,7,8,9,10,11), nrow=3, ncol=4, byrow = TRUE))
  # layout.show(11)
  plot(t, T,
        main="Temperature", xlab="Time (d)", ylab=expression(paste("T (",degree,"C)")),
        type="l",col=simu$col, lty=1,lwd=2.5,
        xlim=c(0,length(t)), ylim=c(0,25))
  
  plot(t, X,
       main="Food", xlab="Time (d)", ylab="Food density (units up to you)",
       type="l",col=simu$col, lty=1,lwd=2.5)
  
  plot(t, f,
       main="Scaled reserve density", xlab="Time (d)", ylab="e (-)",
       type="l",col=simu$col, lty=1,lwd=2.5,
       xlim=c(0,length(t)), ylim=c(0,1.7))
  points(t, sc_res_dens, pch=".:", cex=2.25) #
  
  # E Reserve energy
  plot(t, E,
       main="Reserve energy", xlab="Time (d)", ylab="E (J)",
       type="l",col=simu$col, lty=1,lwd=2.5)

  # V
  plot(t, V,
       main="Structural volume", xlab="Time (d)", ylab="V (cm^3)",
      type="l",col=simu$col, lty=1,lwd=2.5)

  # E_H
  plot(t, E_H,
       main="Cumulated energy invested into maturation", xlab="Time (d)", ylab="E_H (J)",
      type="l",col=simu$col, lty=1,lwd=2.5)
  # E_R
  plot(t, E_R,
       main="Reproduction buffer (J)", xlab="Time (d)", ylab="E_R (J)",
      type="l",col=simu$col, lty=1,lwd=2.5)
  
  # Lw
  plot(t, Lw,
       main="Physical length (cm)", xlab="Time (d)", ylab="Lw (cm)",
      type="l",col=simu$col, lty=1,lwd=2.5)
 
  # Ww_
  plot(Lw, Ww,
       main="Weight-Length", xlab="Lw (cm)", ylab="Ww (g)",
      type="l",col=simu$col, lty=1,lwd=2.5)
  
   # Ew
  plot(t, Ew,
       main="Weight-specific energy content (J/g)", xlab="Time (d)", ylab=expression(paste("<Ew> (J.g"^"-1)")),
       type="l",col=simu$col, lty=1,lwd=2.5)
  
  # Fecundity
  plot(Lw[i_sp], Fecundity[i_sp], pch="+",
       main="Fecundity-Length", xlab="Lw (cm)", ylab="Fecundity (#)",
       col=simu$col, lty=1,cex=2)
  
  dev.copy(tiff,filename = "fig_DEB_standard.tiff")
  dev.off()

  windows()
  dev.set(3) # T
  v=c(0,length(t)/365, 0,25)
  plot(t/365, T,
       main="Temperature", xlab="Time (years)", ylab=expression(paste("T (",degree,"C)")),
       type="l",col=simu$col, lty=1,lwd=2.5,
       xlim=v[1:2], ylim=v[3:4])
  text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(a)', cex=1.5)
  dev.copy(tiff,filename = "fig_T.tiff")
  dev.off()

  windows()
  dev.set(4) # Food Density
  plot(t/365, X,
       main="Food Density", xlab="Time (years)", ylab="Food density",
       type="l",col=simu$col, lty=1,lwd=2.5)
  v = par()$usr
  text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(b)', cex=1.5)

  
  # Lw
  windows()
  dev.set(5)
  plot(t/365, Lw,
       xlab="Time (years)", ylab="Length (cm)",
       type="l",col=simu$col, lty=1,lwd=2.5)
  v = par()$usr
  text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(c)', cex=1.5)
 
  # Ww_
  windows()
  dev.set(6)
  plot(Lw, Ww,
       xlab="Time (years)", ylab="Wet weight (g)",
       type="l",col=simu$col, lty=1,lwd=2.5)
  v = par()$usr
  text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(d)', cex=1.5)
  
  # Ew
  windows()
  dev.set(7)
  plot(t/365, Ew,
       xlab="Time (years)", ylab=expression(paste("<Ew> (J.g"^"-1) Ww")),
       type="l",col=simu$col, lty=1,lwd=2.5)
    v = par()$usr
    text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(e)', cex=1.5)
    
  #  Fecundity
  windows()
  dev.set(8)
  v = c(0, max(Lw[i_sp]), 0, max(Fecundity[i_sp]))
  plot(Lw[i_sp], Fecundity[i_sp],
       xlab="Length (cm)", ylab="Fecundity (#)",
       pch="+",col=simu$col, lty=1,cex=2,
       xlim=v[1:2],ylim=v[3:4])
  text(x=((v[2]-v[1]) * 0.07 + v[1]), y=((v[4]-v[3]) *0.9 + v[3]), labels='(f)', cex=1.5)
    
    }