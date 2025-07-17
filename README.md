# Goodness-of-fit tests for positive quadrant dependence

Here we provide numerical illustrations with R codes for the paper "Goodness-of-fit Test for Positive Quadrant Dependence." 
Additionally, all the numerical results in the paper can be reproduced using the codes provided here. 

## R Code Demonstration

### A simple example
```R
# Source the main function from PQD_GOF.R online
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
set.seed(10000)
library(copula)
Data = rCopula(50,claytonCopula(iTau(claytonCopula(),-0.6)))
Temp = PQD_GOF(Data,method="All",Figure=T,log_Fig=F)
```
Users can choose "Method" from "EL_A", "EL_F", "DS_04", "S_05", "GS_10", "GS_13", "LW_14", "LG_22" for an individual GOF test. 
When Method = "All", a list of test statistics, critical values, p-value, and time will be provided. 


### For your dataset

## Simulation Results

#### Clayton, Frank, Gumbel, and Gaussian copula

#### FGM, CA, and restricted-t

#### Other copulas

####

## Data Analysis

### NCAA basketball attendance data

### Danish insurance data

### Necrotizing enterocolitis data























