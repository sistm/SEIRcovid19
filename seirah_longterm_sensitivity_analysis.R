library("viridis")

t<-seq(0,365*3)

alpha=1.5
De=5.2
Di=2.3
Dh=30
N=65000000
n=0.01*N
b= 0.8
r=0.1
Dq=1.2

time_confinment = 15
length_confinment = 45
new_n = 0.001*N
strength_confinment = 1.3


E0 =  730
I0= 1
R0=0
A0 =  100
H0 = 1
S0=65000000-E0-A0-I0-R0-H0

init<-c(S0, E0, I0, R0, A0, H0)
par_ini <- c(b, r, alpha, De, Di, Dq, Dh, N, n,time_confinment,length_confinment,new_n,strength_confinment)


interval_b= seq(0.4,1.2,0.08)
interval_r= seq(0,1,0.1)
interval_alpha= seq(1,2,0.1)
interval_De = seq(1,15,1.4)
interval_Di = seq(1,15,1.4)
interval_Dq = seq(1,15,1.4)
interval_Dh = seq(1,15,1.4)
interval_N = seq(60000000,70000000,10^6)
interval_n = seq(0,0.1,0.01)*N
interval_beg_cont = seq(10,20,1)
interval_dur_cont =  seq(40,90,5)
interval_new_inbound_outbound =0.1*seq(0,0.1,0.01)*N
interval_magn_cont = seq(1,2,0.1)

interval_S0 = seq(0,1500,150)
interval_E0 = seq(0,1500,150)
interval_I0 = seq(0,1500,150)
interval_R0 = seq(0,1500,150)
interval_A0 = seq(0,1500,150)
interval_H0 = seq(0,1500,150)

list_name_param = list("b","r"," alpha","De","Di","Dq","Dh", "N", "n")
list_name_state_ini = list("S0"," E0","I0","R0","A0","H0")

legend_par_b = "Transmission rate of\nascertained cases (b)"
legend_par_r = "Ascertainement rate (r)"
legend_par_alpha = "Ratio of transmission between A and I (alpha)"
legend_par_De = "Latent period (De)"
legend_par_Di = "Infectious period (Di)"
legend_par_Dq = "Duration from I onset to H (Dq)"
legend_par_Dh = "Hospitalization period (Dh)"
legend_par_N = "Population size (N)"
legend_par_n = "Daily inbound and outbound size (n)"
legend_par_beg_cont = "Beginning of containment"
legend_par_dur_cont = "Duration of containment"
legend_par_inbound_outbound ="Inflow and outflow\nafter containment (n)"
legend_par_magn_cont = "Effect of containment (exp(beta))"

list_legend_par = list(legend_par_b ,legend_par_r,legend_par_alpha,legend_par_De,legend_par_Di,legend_par_Dq,legend_par_Dh
                       ,legend_par_N,legend_par_n,legend_par_beg_cont,legend_par_dur_cont,legend_par_inbound_outbound,legend_par_magn_cont )


legend_IC_S0 = "Initial numbers of susceptible (S0)"
legend_IC_E0 = "Initial numbers of latent (E0)"
legend_IC_I0 = "Initial numbers of reported infection (I0)"
legend_IC_R0 = "Initial numbers of removed/recovered (R0)"
legend_IC_A0 = "Initial numbers of unreported infection (A0)"
legend_IC_H0 = "Initial numbers of hospitalized (H0)"

list_legend_IC = list(legend_IC_S0,legend_IC_E0,legend_IC_I0,legend_IC_R0,legend_IC_A0,legend_IC_H0)


list_interval_param  = list(interval_b,interval_r,interval_alpha,interval_De,interval_Di,interval_Dq,interval_Dh
                            ,interval_N,interval_n,interval_beg_cont,interval_dur_cont,interval_new_inbound_outbound,interval_magn_cont  )
list_interval_state_ini = list(interval_S0,interval_E0,interval_I0,interval_R0 ,interval_A0,interval_H0 )

#Index of the parameter of interest
indice_param_trial = c(1,12)

repartition_graphe = c(1,2)

# GRAPH FOR  LATENT
#' ODE solution of reference
#'
# par(mfrow = repartition_graphe)
# max_val_E_ref =1.5*10^7
# for (ind_par_to_var in indice_param_trial){
#
#   #Range of tested values & range of color
#   val_param_trial =  list_interval_param[[ind_par_to_var]]
#   code_color = viridis(length(val_param_trial))
#
#   par_pert = par_ini
#   par_pert[ind_par_to_var] = val_param_trial[1]
#   sol_pert <- seirah_solve(init, t, par_pert)
#
#   main_title = list_legend_par[[ind_par_to_var]]
#   matplot(t,sol_pert$E,main = main_title,ylab = "E",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_E_ref))
#   for (np in 1:length(val_param_trial)){
#     par_pert = par_ini
#     par_pert[ind_par_to_var] = val_param_trial[np]
#     sol_pert <- seirah_solve(init, t, par_pert)
#     lines(t,sol_pert$E,col=code_color[np])
#   }
# }

sol_pert <- list()
for (v in indice_param_trial){
  val_param_trial =  list_interval_param[[v]]
  temp <- list()
  for (i in val_param_trial){
    par_pert <-  par_ini
    par_pert[v] <-  i
    temp[[as.character(i)]] <- seirah_solve(init, t, par_pert)
    temp[[as.character(i)]]$param <- list_legend_par[[v]]
    temp[[as.character(i)]]$val_param_trial <- i
  }
  sol_pert[[as.character(v)]] <- do.call(rbind.data.frame, temp)
}
sol_pert_all <- do.call(rbind.data.frame, sol_pert)

library(reshape2)
solPert2plot <- melt(sol_pert_all, id.vars=c("time", "param", "val_param_trial"))

library(ggplot2)
library(ggnewscale)
ggplot() +
  geom_line(data = solPert2plot %>% filter(param == "Transmission rate of\nascertained cases (b)"),
            aes(x=time, y=value, color=val_param_trial, group=val_param_trial)) +
  scale_color_viridis_c("b") +
  new_scale("color") +
  geom_line(data = solPert2plot %>% filter(param == "Inflow and outflow\nafter containment (n)"),
            aes(x=time, y=value, color=val_param_trial, group=val_param_trial)) +
  scale_color_viridis_c("n") +
  theme_bw() +
  #facet_wrap(~param + variable, scales="free", ncol=6) +
  facet_grid(variable~param , scales="free") +
  #ylim(0, 1.5*10^7) +
  ylab(NULL) +
  xlab("Day")
ggsave(height=7.5, width=5.5, filename = "longterm_sensitivity.pdf")
ggsave(height=7.5, width=5.5, filename = "longterm_sensitivity.jpeg", dev="jpeg")



# GRAPH FOR REPORTED INFECTED
#' ODE solution of reference
#'
par(mfrow = repartition_graphe)
max_val_I_ref =2*10^5


for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)

  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,sol_pert$I,main = main_title,ylab = "I",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_I_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,sol_pert$I,col=code_color[np])
  }
}



# GRAPH FOR REMOVED
#' ODE solution of reference
#'
par(mfrow = repartition_graphe)
max_val_R_ref =8*10^7


for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)

  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,sol_pert$R,main = main_title,ylab = "R",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_R_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,sol_pert$R,col=code_color[np])
  }
}



# GRAPH FOR REMOVED
#' ODE solution of reference
#'
par(mfrow = repartition_graphe)
max_val_A_ref =5*10^6

for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)

  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,sol_pert$A,main = main_title,ylab = "A",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_A_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,sol_pert$A,col=code_color[np])
  }
}



# GRAPH FOR HOSPITALISED
#' ODE solution of reference
par(mfrow = repartition_graphe)
max_val_H_ref =2.5*10^6

for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)

  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,sol_pert$H,main = main_title,ylab = "H",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_H_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,sol_pert$H,col=code_color[np])
  }
}


# GRAPH FOR Y1
#' ODE solution of reference
par(mfrow = repartition_graphe)
max_val_Y1_ref =3*10^5

for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)

  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,par_pert[2]*sol_pert$E/par_pert[4],main = main_title,ylab = "Ascertained cases (incident numbers)",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_Y1_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,par_pert[2]*sol_pert$E/par_pert[4],col=code_color[np])
  }
}


# GRAPH FOR Y2
#' ODE solution of reference
par(mfrow = repartition_graphe)
max_val_Y2_ref =10^7


for (ind_par_to_var in indice_param_trial){

  #Range of tested values & range of color
  val_param_trial =  list_interval_param[[ind_par_to_var]]
  code_color = viridis(length(val_param_trial))

  par_pert = par_ini
  par_pert[ind_par_to_var] = val_param_trial[1]
  sol_pert <- seirah_solve(init, t, par_pert)


  main_title = list_legend_par[[ind_par_to_var]]
  matplot(t,sol_pert$E/par_pert[6],main = main_title,ylab = "Hospitalized cases (incident numbers)",xlab="days",type="l",col=code_color[1],ylim=c(0,max_val_Y2_ref))
  for (np in 1:length(val_param_trial)){
    par_pert = par_ini
    par_pert[ind_par_to_var] = val_param_trial[np]
    sol_pert <- seirah_solve(init, t, par_pert)
    lines(t,sol_pert$E/par_pert[6],col=code_color[np])
  }
}





seirah_solve <- function(init, t, par){
  solution <- deSolve::ode(init, c(0,t[2]), seirah_ode2, par)
  result<-as.data.frame(solution)
  #print(result)
  for(comp in 1:6){
    if(result[which(result[,"time"]==t[2]),comp]<1)result[which(result[,"time"]==t[2]),comp]=0
  }

  for (i in 3:length(t)){
    #print(i)
    temp<- deSolve::ode(as.numeric(result[which(result[,"time"]==t[i-1]),2:7]), c(t[i-1],t[i]), seirah_ode2, par)
    for(comp in 1:6){
      if(temp[which(temp[,"time"]==t[i]),comp]<1)temp[which(temp[,"time"]==t[i]),comp]=0
    }
    result<-rbind(result,temp[2,])
  }
  solution <- as.data.frame(result)
  #print(str(solution))
  colnames(solution) <- c("time", "S", "E", "I", "R", "A", "H")
  class(solution) <- c("seirah_solve", "deSolve", "data.frame" )
  #print(solution)
  return(solution)
}

