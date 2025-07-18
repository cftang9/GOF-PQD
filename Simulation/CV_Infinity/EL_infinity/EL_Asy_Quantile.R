rm(list=ls(all=TRUE))

Data = read.csv("EL_Inf.csv")
Data$EL_Inf
hist(Data$EL_Inf)
quantile(Data$EL_Inf,c(0.8,0.9,0.95,0.99,0.999))
# n = 10000; B = 10000; 
#      80%       90%       95%       99%     99.9% 
#0.7346073 1.0268457 1.3078578 2.0473108 2.9866454 