rm(list=ls(all=TRUE))

#load("Sim_Asymmetric.rdata")
#load("Sim_CA_N10.rdata")
#load("Sim_Copula_A.rdata")
#load("Sim_FGM_N10.rdata")
#load("Sim_MixFrank_1.rdata")
#load("Sim_MixFrank_2.rdata")
#load("Sim_qrm.rdata")
#load("Sim_srm.rdata")


#b
#TS
#Rejection
#Start.Time
#Current.Time
#Diff.Time
#cores

B = 1000
temp1 = array(,B); 
temp2 = array(,B); 

for(i in 1:B){
  temp1[i] = Test_b[[i]]$Rejection
  temp2[i] = Test_b[[i]]$Diff.Time
}

mean(temp1)
mean(temp2)
median(temp2)
range(temp2)
