#Sensitivity equation dx/dtheta solving 
library("deSolve")

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

time_pred<-seq(0,10)
par <- c(b, r, alpha, De, Di, Dq, Dh, N, n,30,30,0,3000000)
list_name_param = list("b","r"," alpha","De","Di","Dq","Dh", "N", "n")
#'
#' # Solve system using lsoda

sol_X_dX_dtheta =ode(init_tot, time_pred,seirah_sens_param_ode,par)

sol_X = sol_X_dX_dtheta[,2:7] 
dX_dtheta = sol_X_dX_dtheta[,8:49] 

#Estimation of dh/dtheta and di/dtheta
dI_dtheta = matrix(0,length(time_pred),7)
dH_dtheta = matrix(0,length(time_pred),7)

#Approximation of the Hessian at assumed parameter value
Mat_tensor = matrix(0,7,7)
mat_Obs = rbind(c(0,0,1,0,0,0),c(0,0,0,0,0,1))
for (nt in 1:length(time_pred)){
 dXnt =  matrix(dX_dtheta[nt,],6,7)
 
 Mat_tensor =  Mat_tensor+t(mat_Obs%*%dXnt)%*%(mat_Obs%*%dXnt)
 dI_dtheta[nt,] = dXnt[3,]
 dH_dtheta[nt,] = dXnt[6,]
}

Val_eigen_decomp = eigen(Mat_tensor)

#Compute eigen decomposition indeed alpha seems to correspond to lowest descend direction
#Eigenvalues
print(Val_eigen_decomp$values)

#Eigenvectors
print(Val_eigen_decomp$vectors)

for (nbp in 1:7){
ylab_nbp = paste("dI/d",list_name_param[[nbp]], sep = "")
matplot(time_pred,dI_dtheta[,nbp],type="l",xlab='Time (days)',ylab = ylab_nbp)
}

for (nbp in 1:7){
  ylab_nbp = paste("dH/d",list_name_param[[nbp]], sep = "")
  matplot(time_pred,dH_dtheta[,nbp],type="l",xlab='Time (days)',ylab = ylab_nbp)
}



