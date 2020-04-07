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
list_res = seirah_confidence_interval_response(b,bsd,Dq,Dqsd,E0,E0sd,A0,A0sd,beta,betasd,De,Di,Dh,r,alpha,R0,I0,H0,N,n,
                                    time_beg,time_end,time_beg_conf,length_confinment)



matplot(list_res$Times_integ,t(list_res$mat_q_05_95_E),type="l",ylab = "E",xlab="days")
matplot(list_res$Times_integ,t(list_res$mat_q_05_95_I),type="l",ylab = "I",xlab="days")
matplot(list_res$Times_integ,t(list_res$mat_q_05_95_R),type="l",ylab = "R",xlab="days")
matplot(list_res$Times_integ,t(list_res$mat_q_05_95_A),type="l",ylab = "A",xlab="days")
matplot(list_res$Times_integ,t(list_res$mat_q_05_95_H),type="l",ylab = "H",xlab="days")
