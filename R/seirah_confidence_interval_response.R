#' Monte Carlo estimator of  5% and 95% percent quantiles of model response
#'
#' @keXwords internal
#'
#' @export
seirah_confidence_interval_response <- function(b,bsd,Dq,Dqsd,E0,E0sd,A0,A0sd,beta,betasd,De,Di,Dh,r,alpha,R0,I0,H0,N,n,time_beg,time_end,time_beg_conf,length_confinment){
 
  Times_integ = seq(time_beg,time_end,(time_end-time_beg)/200)
  nb_mc = 1000
  
  mat_resp_S = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_E = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_I = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_R = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_A = matrix(0,nb_mc,length(Times_integ ))
  mat_resp_H = matrix(0,nb_mc,length(Times_integ ))
  
  for (mc_cur  in 1:nb_mc){
   
    b_cur = b + bsd*rnorm(1,0)
    Dq_cur = Dq + Dqsd*rnorm(1,0)
    E0_cur = max(E0 + E0sd*rnorm(1,0),0)
    A0_cur = max(A0 + A0sd*rnorm(1,0),0)
    beta_cur = beta + betasd*rnorm(1,0)
    K_inv_cur = 1/exp(beta_cur)
    
    S0=N-E0_cur-I0-R0-A0_cur-H0
    
    init_cur <-c(S0, E0_cur, I0, R0, A0_cur, H0)
    par_cur <- c(b_cur, r, alpha, De, Di, Dq_cur, Dh, N, n,time_beg_conf,length_confinment,0, K_inv_cur)
    
    res_ode_cur = ode(init_cur,Times_integ,seirah_ode,par_cur)
    
    mat_resp_S[mc_cur,] = t(res_ode_cur[,2])
    mat_resp_E[mc_cur,] = t(res_ode_cur[,3])
    mat_resp_I[mc_cur,] = t(res_ode_cur[,4])
    mat_resp_R[mc_cur,] = t(res_ode_cur[,5])
    mat_resp_A[mc_cur,] = t(res_ode_cur[,6])
    mat_resp_H[mc_cur,] = t(res_ode_cur[,7])
  }
  
  mat_q_05_95_S = matrix(0,2,length(Times_integ ))
  mat_q_05_95_E = matrix(0,2,length(Times_integ ))
  mat_q_05_95_I = matrix(0,2,length(Times_integ ))
  mat_q_05_95_R = matrix(0,2,length(Times_integ ))
  mat_q_05_95_A = matrix(0,2,length(Times_integ ))
  mat_q_05_95_H = matrix(0,2,length(Times_integ ))
  
  
  for (nt in 1:length(Times_integ )){
   qu_S_nt =  quantile(mat_resp_S[,nt],c(0.05,0.95))
   mat_q_05_95_S[1,nt] = qu_S_nt[1]
   mat_q_05_95_S[2,nt] = qu_S_nt[2]
   
   qu_E_nt =  quantile(mat_resp_E[,nt],c(0.05,0.95))
   mat_q_05_95_E[1,nt] = qu_E_nt[1]
   mat_q_05_95_E[2,nt] = qu_E_nt[2]
   
   qu_I_nt =  quantile(mat_resp_I[,nt],c(0.05,0.95))
   mat_q_05_95_I[1,nt] = qu_I_nt[1]
   mat_q_05_95_I[2,nt] = qu_I_nt[2]
   
   qu_R_nt =  quantile(mat_resp_R[,nt],c(0.05,0.95))
   mat_q_05_95_R[1,nt] = qu_R_nt[1]
   mat_q_05_95_R[2,nt] = qu_R_nt[2]
   
   qu_A_nt =  quantile(mat_resp_A[,nt],c(0.05,0.95))
   mat_q_05_95_A[1,nt] = qu_A_nt[1]
   mat_q_05_95_A[2,nt] = qu_A_nt[2]
   
   qu_H_nt =  quantile(mat_resp_H[,nt],c(0.05,0.95))
   mat_q_05_95_H[1,nt] = qu_H_nt[1]
   mat_q_05_95_H[2,nt] = qu_H_nt[2]
  }
  
  res_list = list(Times_integ = Times_integ,mat_q_05_95_S = mat_q_05_95_S, mat_q_05_95_E = mat_q_05_95_E,mat_q_05_95_I = mat_q_05_95_I,mat_q_05_95_R=mat_q_05_95_R,mat_q_05_95_A = mat_q_05_95_A,mat_q_05_95_H=mat_q_05_95_H)
  return( res_list )
}


