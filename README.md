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























