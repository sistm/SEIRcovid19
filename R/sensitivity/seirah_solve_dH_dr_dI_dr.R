#Sensitivity equation dx/dtheta solving 
library("deSolve")
library("viridis")
alpha=7
De=5.2
Di=2.3
Dh=30
N=65000000
n=0.0000*N
b=1.75
r=0.41
Dq=9.92
#'
init_X<-c(9999467, 346, 80, 0, 80, 27)
init_der_X_theta = matrix(0,1,42)
init_tot = c(init_X,c(init_der_X_theta ))

time_pred<-seq(0,10,0.1)
par_ini <- c(b, r, alpha, De, Di, Dq, Dh, N, n,30,30,0,3000000)
list_name_param = list("b","r"," alpha","De","Di","Dq","Dh", "N", "n")
list_name_state_variable = list("S","E","I","R","A","H")
#'

#Index of the parameter of interest
ind_par_to_var = 2
ind_state_variable = 3

#Range of tested values & range of color
val_param_trial = seq(0,1,0.1)
code_color = viridis(length(val_param_trial))

par_pert = par_ini 
par_pert[ind_par_to_var] = val_param_trial[1]

sol_X_dX_dtheta_pert =ode(init_tot, time_pred,seirah_sens_param_ode,par_pert)

dX_dtheta =  sol_X_dX_dtheta_pert[,8:49] 

#Estimation of dh/dtheta and di/dtheta
dXi_dtheta = matrix(0,length(time_pred),7)
for (nt in 1:length(time_pred)){
  dXnt =  matrix(dX_dtheta[nt,],6,7)
  dXi_dtheta[nt,] = dXnt[ind_state_variable,]
}

main_title = paste("Parameter ", list_name_param [[ind_par_to_var]]," from ", val_param_trial[1], " to ",val_param_trial[length(val_param_trial)],  sep="")
ylab_cur = paste("d",list_name_state_variable[[ind_state_variable]],"/d", list_name_param [[ind_par_to_var]],sep="")
matplot(time_pred,dXi_dtheta[,ind_par_to_var ],main = main_title,ylab =ylab_cur ,xlab="days",type="l",col=code_color[1])
for (np in 2:length(val_param_trial)){
  par_pert = par_ini 
  par_pert[ind_par_to_var] = val_param_trial[np]
  print(par_pert)
  sol_X_dX_dtheta_pert =ode(init_tot, time_pred,seirah_sens_param_ode,par_pert)
  
  dX_dtheta =  sol_X_dX_dtheta_pert[,8:49] 
  
  #Estimation of dh/dtheta and di/dtheta
  dXi_dtheta = matrix(0,length(time_pred),7)
  for (nt in 1:length(time_pred)){
    dXnt =  matrix(dX_dtheta[nt,],6,7)
    dXi_dtheta[nt,] = dXnt[ind_state_variable,]
  }
  lines(time_pred,dXi_dtheta[,ind_par_to_var ],col=code_color[np])
}

