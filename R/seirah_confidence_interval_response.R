#' Monte Carlo estimator of  5% and 95% percent quantiles of model response
#'
#' 
#'
#' @export
seirah_confidence_interval_response <- function(b,bsd,Dq,Dqsd,E0,E0sd,A0,A0sd,beta,betasd,De,Di,Dh,r,alpha,R0,I0,H0,N,n,time_beg,time_end,time_beg_conf,length_confinment,typecov){
 
  Times_integ = seq(time_beg,time_end,1)
  nb_mc = 1000
  
  mat_resp_S = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_E = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_I = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_R = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_A = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_H = matrix(0,nb_mc,length(Times_integ ))
  
  mat_resp_Y1 = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_Y2 = matrix(0,nb_mc,length(Times_integ ))
  
  for (mc_cur  in 1:nb_mc){
   
    b_cur = max(0.00001,b + bsd*rnorm(1,0))
    Dq_cur = max(0.00001,Dq + Dqsd*rnorm(1,0))
    E0_cur = max(E0 + E0sd*rnorm(1,0),1)
    A0_cur = max(A0 + A0sd*rnorm(1,0),1)
    beta_cur = beta + betasd*rnorm(1,0)

    S0=N-E0_cur-I0-R0-A0_cur-H0
    
    init_cur <-c(S0, E0_cur, I0, R0, A0_cur, H0)
    par_cur <- c(b_cur, r, alpha, De, Di, Dq_cur, Dh, N, n,time_beg_conf,length_confinment,0, typecov,beta_cur)

res_ode_cur = ode(init_cur,Times_integ,seirah_ode,par_cur)
    
    mat_resp_S[mc_cur,] = t(res_ode_cur[,2])
    mat_resp_E[mc_cur,] = t(res_ode_cur[,3])
    mat_resp_I[mc_cur,] = t(res_ode_cur[,4])
    mat_resp_R[mc_cur,] = t(res_ode_cur[,5])
    mat_resp_A[mc_cur,] = t(res_ode_cur[,6])
    mat_resp_H[mc_cur,] = t(res_ode_cur[,7])
    mat_resp_Y1[mc_cur,] = t( as.numeric(par_cur[2])*res_ode_cur[,3]/as.numeric(par_cur[4]))
    mat_resp_Y2[mc_cur,] = t(res_ode_cur[,4]/as.numeric(par_cur[6]))
    
  }
  
  mat_mean_q_05_95_S = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_E = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_I = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_R = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_A = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_H = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_Y1 = matrix(0,3,length(Times_integ ))
  mat_mean_q_05_95_Y2 = matrix(0,3,length(Times_integ ))
  
  
  for (nt in 1:length(Times_integ )){
   qu_S_nt =  quantile(mat_resp_S[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_S[1,nt] = qu_S_nt[1]
   mat_mean_q_05_95_S[2,nt] = qu_S_nt[2]
   mat_mean_q_05_95_S[3,nt] = mean(mat_resp_S[,nt],na.rm=TRUE)
   
   qu_E_nt =  quantile(mat_resp_E[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_E[1,nt] = qu_E_nt[1]
   mat_mean_q_05_95_E[2,nt] = qu_E_nt[2]
   mat_mean_q_05_95_E[3,nt] = mean(mat_resp_E[,nt],na.rm=TRUE)
   
   qu_I_nt =  quantile(mat_resp_I[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_I[1,nt] = qu_I_nt[1]
   mat_mean_q_05_95_I[2,nt] = qu_I_nt[2]
   mat_mean_q_05_95_I[3,nt] = mean(mat_resp_I[,nt],na.rm=TRUE)
   
   qu_R_nt =  quantile(mat_resp_R[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_R[1,nt] = qu_R_nt[1]
   mat_mean_q_05_95_R[2,nt] = qu_R_nt[2]
   mat_mean_q_05_95_R[3,nt] = mean(mat_resp_R[,nt],na.rm=TRUE)
   
   qu_A_nt =  quantile(mat_resp_A[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_A[1,nt] = qu_A_nt[1]
   mat_mean_q_05_95_A[2,nt] = qu_A_nt[2]
   mat_mean_q_05_95_A[3,nt] = mean(mat_resp_A[,nt],na.rm=TRUE)
   
   qu_H_nt =  quantile(mat_resp_H[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_H[1,nt] = qu_H_nt[1]
   mat_mean_q_05_95_H[2,nt] = qu_H_nt[2]
   mat_mean_q_05_95_H[3,nt] = mean(mat_resp_H[,nt],na.rm=TRUE)
   
   qu_Y1_nt =  quantile(mat_resp_Y1[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_Y1[1,nt] = qu_Y1_nt[1]
   mat_mean_q_05_95_Y1[2,nt] = qu_Y1_nt[2]
   mat_mean_q_05_95_Y1[3,nt] = mean(mat_resp_Y1[,nt],na.rm=TRUE)
   
   qu_Y2_nt =  quantile(mat_resp_Y2[,nt],c(0.025,0.975),na.rm=TRUE)
   mat_mean_q_05_95_Y2[1,nt] = qu_Y2_nt[1]
   mat_mean_q_05_95_Y2[2,nt] = qu_Y2_nt[2]
   mat_mean_q_05_95_Y2[3,nt] = mean(mat_resp_Y2[,nt],na.rm=TRUE)
  }
  
  res_list = list(Times_integ = Times_integ,mat_mean_q_05_95_S = mat_mean_q_05_95_S, mat_mean_q_05_95_E = mat_mean_q_05_95_E,
                  mat_mean_q_05_95_I = mat_mean_q_05_95_I,mat_mean_q_05_95_R=mat_mean_q_05_95_R,mat_mean_q_05_95_A = mat_mean_q_05_95_A,
                  mat_mean_q_05_95_H=mat_mean_q_05_95_H,mat_mean_q_05_95_Y1=mat_mean_q_05_95_Y1,mat_mean_q_05_95_Y2=mat_mean_q_05_95_Y2)
  
  donnees<-data.frame(time= Times_integ,Smoyen = mat_mean_q_05_95_S[3,],Emoyen = mat_mean_q_05_95_E[3,],Imoyen= mat_mean_q_05_95_I[3,],
                      Rmoyen = mat_mean_q_05_95_R[3,],Amoyen = mat_mean_q_05_95_A[3,],Hmoyen = mat_mean_q_05_95_H[3,],Y1moyen= mat_mean_q_05_95_Y1[3,],Y2moyen= mat_mean_q_05_95_Y2[3,],
                      Smin = mat_mean_q_05_95_S[1,],Emin = mat_mean_q_05_95_E[1,],Imin = mat_mean_q_05_95_I[1,],Rmin = mat_mean_q_05_95_R[1,],
                      Amin = mat_mean_q_05_95_A[1,],Hmin = mat_mean_q_05_95_H[1,],Y1min= mat_mean_q_05_95_Y1[1,],Y2min= mat_mean_q_05_95_Y2[1,],
                      Smax = mat_mean_q_05_95_S[2,],Emax = mat_mean_q_05_95_E[2,],Imax = mat_mean_q_05_95_I[2,],Rmax = mat_mean_q_05_95_R[2,],
                      Amax = mat_mean_q_05_95_A[2,],Hmax = mat_mean_q_05_95_H[2,],Y1max= mat_mean_q_05_95_Y1[2,],Y2max= mat_mean_q_05_95_Y2[2,])
 # colnames(donnees) <-c("time","Smoyen","Emoyen","Imoyen","Rmoyen",
  #                      "Amoyen","Hmoyen","Smin","Emin","Imin","Rmin","Amin","Hmin","Smax","Emax","Imax","Rmax","Amax","Hmax")
  
  return(donnees)
}


