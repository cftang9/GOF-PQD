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
Data = rCopula(50,claytonCopula(iTau(claytonCopula(),-0.2)))
Temp = PQD_GOF(Data,method="ALL",Figure=T,log_Fig=F)
```
Users can choose "Method" from "EL_A", "EL_F", "DS_04", "S_05", "GS_10", "GS_13", "LW_14", "LG_22" for an individual GOF test. 
When Method = "ALL", a list of test statistics, critical values, p-value, and time will be provided. 
```R
      Test statistic Critical value    p-value       Time
EL_A       1.6836183      1.3078578 0.02170000   0.282223
EL_F       1.6836183      1.0745115 0.01110000   5.488664
DS_04     10.2401185      8.4960000 0.02459046   0.431073
S_05       0.8768124      0.6618519 0.00270000  35.102992
GS_10     19.9327802     12.9055105 0.01500000 102.143397
GS_13      1.2212406      0.4959487 0.00300000 377.651592
LW_14     -2.4486330     -2.4863221 0.05345000  18.588535
LG_22      0.9756259      0.8003936 0.05000000  34.879393
```
<img width="2221" height="1204" alt="Illustration_Plots" src="https://github.com/user-attachments/assets/70bbc05e-b406-432f-9c22-a9b22df7651a" />

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























