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
      Test statistic Critical value     p-value       Time
EL_A       1.4826943      1.3078578 0.033300000   0.181318
EL_F       1.4826943      1.0745115 0.019500000   5.487228
DS_04     12.9556311      8.9260000 0.009883926   0.439888
S_05       0.6109403      0.6618519 0.087200000  35.741555
GS_10     22.3303691     12.9055105 0.010100000 103.828279
GS_13      0.6360344      0.5629177 0.037000000 376.480714
LW_14     -1.7766998     -2.4863221 0.204730000  18.682560
LG_22      0.4655116      0.7543264 0.050000000  34.719584
```
<img width="2221" height="1200" alt="Illustrating_Plots" src="https://github.com/user-attachments/assets/a8818d26-c0dd-4101-9cc0-c667f057f3f0" />


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























