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
# Run the main function for all the GOF tests for PQD. When "log_Fig" is TRUE, a log-transformed scatterplot will be provided if "Figure" is TRUE. 
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

Here, we provide all the codes and results in the [Simulation](https://github.com/cftang9/GOF-PQD/tree/main/Simulation) folder. 
To reproduce the simulation results, users must download the entire folder and run the R code corresponding to the simulation settings. 
The asymptotic critical values for the EL test are in the [EL_infinity](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/CV_Infinity/EL_infinity) folder. 
Other critical values generated from independent copulas are provided in [CV_n100](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/CV_n100) and [CV_n200](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/CV_2100) folders. 

#### Clayton, Frank, Gumbel, and Gaussian copulas (Table 1 and Table C1)

We provide the R codes 
[Table_1_Clayton_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_1_CFGG_n100/Table_1_Clayton_n100.R), 
[Table_1_Frank_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_1_CFGG_n100/Table_1_Frank_n100.R), 
[Table_1_Gumbel_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_1_CFGG_n100/Table_1_Gumbel_n100.R), and 
[Table_1_Gaussian_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_1_CFGG_n100/Table_1_Gaussian_n100.R) for the size and power comparisons of the Clayton, Frank, Gumbel, and Gaussian copulas for sample size 100 in Table 1 and results in [Table_1_CFGG_n100](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/Table_1_CFGG_n100) folder. 
For the results from a sample size of 200 in Table C1, the codes and results can be found in the [Table_C1_CFGG_n100](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/Table_C1_CFGG_n100) folder. 


####  Farlie-Gumbel-Morgenstern (FGM), Cuadras-Augé (CA), and restricted-t (Table 2 and Table C2)

In addition, we provide the R codes 
[Table_2_FGM_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_2_FGM_CA_Rt_100/Table_2_FGM_n100.R), 
[Table_2_CA_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_2_FGM_CA_Rt_100/Table_2_CA_n100.R), and 
[Table_2_Rt_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_2_FGM_CA_Rt_100/Table_2_Rt_n100.R) for the size and power comparisons of the FGM, CA, restricted-t copulas for sample size 100 in Table 1 and results in [Table_2_FGM_CA_Rt_100](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/Table_2_FGM_CA_Rt_n100) folder. 
For the results from a sample size of 200 in Table C1, the codes and results can be found in the [Table_2_FGM_CA_Rt_200](Table_2_FGM_CA_Rt_n200) folder. 

#### Copulas 1 - 6 (Table C3)

Last, we provide the R codes for Copulas 1 to 6 (MixFrank I, MixFrank II, Asymmetric copula, Copula A, and copulas from quadratic and sinusoidal regressions)
[6C_n100](https://github.com/cftang9/GOF-PQD/blob/main/Simulation/Table_C3_6copulas/n100/Table_2_6C_n100.R) for the size and power comparisons with sample size 100 in Table C3 in [n100](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/Table_C3_6copulas/n100) folder. 
For the results from a sample size of 200 in Table C3, the codes and results can be found in the [n200](https://github.com/cftang9/GOF-PQD/tree/main/Simulation/Table_C3_6copulas/n100) folder. 


####

## Data sets

### NCAA basketball attendance data 
Here we provide the dataset [NCAA.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/1_NCAA_basketball_attendance_data/NCAA.csv) and the R code [NCAA.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/1_NCAA_basketball_attendance_data/NCAA.R) performing all the GOF tests for PQD. 

### Danish insurance data

This dataset [Danish_2y_all_pos.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/2_Danish_insurance_data/Danish_2y_all_pos.csv) (1989-1990 with all positive losses in Buildings, Contents, and Profits) is collected from the R package fitdistrplus. 
We applied the R code [Insurance_2y12.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/2_Danish_insurance_data/Insurance_2y12.R), to perform and compare the GOF tests for PQD. 

### Necrotizing enterocolitis data

Here we provide the dataset [NEC.csv](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/3_Necrotizing_enterocolities_data/NEC.csv) and the R code [NEC.r](https://raw.githubusercontent.com/cftang9/GOF-PQD/refs/heads/main/Data%20sets/3_Necrotizing_enterocolities_data/NEC.R), performing all the GOF tests for PQD. 





















