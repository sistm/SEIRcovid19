#' Monte Carlo estimator of 5% and 95% percent quantiles of model response


# Random parameter: mean value and standart deviation
b = 0.78516677950003
bsd = 0.0290832034096524

Dq = 1.661486980081
Dqsd = 0.46

E0 = 976.719496173479
E0sd = 255.706917085646

A0 =  181.543127277625 
A0sd = 177.304875844831

beta =-0.328373805834875
betasd = 0.0141888987560929


# Fixed parameter 
De=5.1
Di=2.3
Dh=30
r= 0.05
alpha=1.5
R0=0
I0=1
H0 =1
S0=E0-I0-R0-A0-H0
N =65000000
n =  0

#Beginning & end of integration time
time_beg = 0
time_end = 30

#Beginning of containment & duration of containment 
time_beg_conf = 15
length_confinment = 45

#Computation of 5% and 95% percent quantiles for each state variable 
data_frame_res = seirah_confidence_interval_response(b,bsd,Dq,Dqsd,E0,E0sd,A0,A0sd,beta,betasd,De,Di,Dh,r,alpha,R0,I0,H0,N,n,
                                    time_beg,time_end,time_beg_conf,length_confinment)



h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("E") + 
  geom_ribbon(aes(ymin = data_frame_res$Emin, ymax = data_frame_res$Emax), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Emoyen))

h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("I") + 
  geom_ribbon(aes(ymin = data_frame_res$Imin, ymax = data_frame_res$Imax), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Imoyen))

h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("R") + 
  geom_ribbon(aes(ymin = data_frame_res$Rmin, ymax = data_frame_res$Rmax), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Rmoyen))

h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("A") + 
  geom_ribbon(aes(ymin = data_frame_res$Amin, ymax = data_frame_res$Amax), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Amoyen))

h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("H") + 
  geom_ribbon(aes(ymin = data_frame_res$Hmin, ymax = data_frame_res$Hmax), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Hmoyen))


h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("Ascertained cases (incident numbers)") + 
  geom_ribbon(aes(ymin = data_frame_res$Y1min, ymax = data_frame_res$Y1max), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Y1moyen))

h <- ggplot(data_frame_res, aes(data_frame_res$time))
h + xlab("Days") + ylab("Hospitalized cases (incident numbers)") + 
  geom_ribbon(aes(ymin = data_frame_res$Y2min, ymax = data_frame_res$Y2max), fill = "grey70") +
  geom_line(aes(y = data_frame_res$Y2moyen))




