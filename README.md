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
PQD_GOF(Data,method="ALL",Figure=T,log_Fig=F)
```

<img width="2186" height="1232" alt="Illustrating_Plot" src="https://github.com/user-attachments/assets/4735d9b8-e134-453a-b45c-4a7438bb1530" />

Users can choose "Method" from "EL_A", "EL_F", "DS_04", "S_05", "GS_10", "GS_13", "LW_14", "LG_22" for an individual GOF test. 
When Method = "ALL", a list of test statistics, critical values, p-value, and time will be provided. 
```R
      Test statistic Critical value   p-value   Time(sec)
EL_A       1.2168735      1.3078578 0.0629000   0.2981939
EL_F       1.2168735      1.0745115 0.0359000   5.6952441
DS_04      3.2812208      8.2720000 0.3503397   0.4368880
S_05       0.7353911      0.6618519 0.0204000  35.5506978
GS_10     16.9405461     12.9055105 0.0260000 102.5256040
GS_13      0.6377916      0.4912701 0.0240000 376.3544869
LW_14     -2.5812064     -2.4863221 0.0365700  19.0609691
LG_22      0.4881553      0.6012889 0.0820000  34.9922369
```

### For your dataset
```R
# Source the main function from PQD_GOF.R online
source("https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/PQD_GOF.R")
# Save your data into "Data" as an n by 2 matrix, for example, Data = array(runif(2*50),c(50,2)) with n=50. 
PQD_GOF(Data,method="ALL",Figure=T,log_Fig=F)
```

## Simulation Results

#### Clayton, Frank, Gumbel, and Gaussian copula

#### FGM, CA, and restricted-t

#### Other copulas

####

## Data Analysis

### NCAA basketball attendance data 
Here we provide the dataset [NCAA.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/1_NCAA_basketball_attendance_data/NCAA.csv) and the R code [NCAA.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/1_NCAA_basketball_attendance_data/NCAA.R) performing all the GOF tests for PQD. 

### Danish insurance data

This dataset [Danish_2y_all_pos.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/2_Danish_insurance_data/Danish_2y_all_pos.csv) (1989-1990 with all positive losses in Buildings, Contents, and Profits) is collected from the R package fitdistrplus. 
We applied the R code [Insurance_2y12.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/2_Danish_insurance_data/Insurance_2y12.R), to perform and compare the GOF tests for PQD. 

### Necrotizing enterocolitis data

Here we provide the dataset [NEC.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/3_Necrotizing_enterocolities_data/NEC.csv) and the R code [NEC.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/3_Necrotizing_enterocolities_data/NEC.R), performing all the GOF tests for PQD. 





















