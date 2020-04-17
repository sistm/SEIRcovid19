#' Monte Carlo estimator of  5% and 95% percent quantiles of model response
#'
#'@import pbapply
#'
#' @export
seirah_confidence_interval_response <- function(b,bsd,Dq,Dqsd,E0,E0sd,A0,A0sd,beta,betasd,b3,beta2sd,De,Di,Dh,r,alpha,R0,I0,H0,N,n,time_beg,time_end,time_beg_conf,length_confinment,typecov,
                                                ncores=1){

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

  mc_res <- parallel::mclapply(X = 1:nb_mc, mc.cores=ncores, FUN=function(mc_cur){
    b_cur = max(0.00001,b + bsd*rnorm(1,0))
    Dq_cur = max(0.00001,Dq + Dqsd*rnorm(1,0))
    E0_cur = max(E0 + E0sd*rnorm(1,0),1)
    A0_cur = I0*(1-r)/r
    beta_cur = beta + betasd*rnorm(1,0)
    beta2_cur = b3 + beta2sd*rnorm(1,0)
    
    S0=N-E0_cur-I0-R0-A0_cur-H0

    init_cur <-c(S0, E0_cur, I0, R0, A0_cur, H0)
    par_cur <- c(b_cur, r, alpha, De, Di, Dq_cur, Dh, N, n,time_beg_conf,length_confinment,0, typecov,beta_cur,beta2_cur)

    res_ode_cur = ode(init_cur,Times_integ,seirah_ode,par_cur)

    return(cbind.data.frame("S" = res_ode_cur[,2],
          "E" = res_ode_cur[,3],
          "I" = res_ode_cur[,4],
          "R" = res_ode_cur[,5],
          "A" = res_ode_cur[,6],
          "H" = res_ode_cur[,7],
          "Y1" = as.numeric(par_cur[2])*res_ode_cur[,3]/as.numeric(par_cur[4]),
          "Y2" = res_ode_cur[,4]/as.numeric(par_cur[6])
    )
    )
  }
  )


mat_mean_q_05_95_S <- rbind(apply(sapply(mc_res, "[[", "S"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "S"), na.rm=TRUE))
mat_mean_q_05_95_E <- rbind(apply(sapply(mc_res, "[[", "E"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "E"), na.rm=TRUE))
mat_mean_q_05_95_I <- rbind(apply(sapply(mc_res, "[[", "I"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "I"), na.rm=TRUE))
mat_mean_q_05_95_R <- rbind(apply(sapply(mc_res, "[[", "R"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "R"), na.rm=TRUE))
mat_mean_q_05_95_A <- rbind(apply(sapply(mc_res, "[[", "A"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                           rowMeans(sapply(mc_res, "[[", "A"), na.rm=TRUE))
mat_mean_q_05_95_H <- rbind(apply(sapply(mc_res, "[[", "H"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "H"), na.rm=TRUE))
mat_mean_q_05_95_Y1 <- rbind(apply(sapply(mc_res, "[[", "Y1"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                            rowMeans(sapply(mc_res, "[[", "Y1"), na.rm=TRUE))
mat_mean_q_05_95_Y2 <- rbind(apply(sapply(mc_res, "[[", "Y2"), 1, FUN=quantile, probs = c(0.025, 0.975)),
                             rowMeans(sapply(mc_res, "[[", "Y2"), na.rm=TRUE))

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


