## 1.How to analyze wastewater surveillance data including left-censored data
1. Download [Brief Guide](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/Brief%20guide%20for%20using%20the%20state-space%20model.pdf)
2. Download [R code](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/state_space_model_with_logistic_highspeed.R),[Stan code](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/state_space_model_with_logistic_highspeed.stan), and [CSV file](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/template_file.csv)


## 2.How to download files from GitHub
<img width="1131" height="395" alt="image" src="https://github.com/user-attachments/assets/127c26ba-17ad-4bb7-9785-8ddd7467b791" />


## 3.Demonstration
1. [Toy Data](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/XX_Demonstration/Demonstration_toydata.csv), [R code](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/XX_Demonstration/state_space_model_with_logistic_highspeed.R), [stan code](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/XX_Demonstration/state_space_model_with_logistic_highspeed.stan)  
2. [Result_estimated_wastewaterconcentration](https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/XX_Demonstration/Result_Demonstration_wastewaterconcentration.csv)

**Estimated wastwater concentration (log10 copies/L)**
<img width="700" height="500" alt="image" src="https://github.com/Hiroki-Ando1998/202410_WBE_censored_data/blob/main/XX_Demonstration/Result_demonstration_wastewaterconcentration.svg"/>


🟡 Yellow plots represent wastewater concentrations on days with non-detection observations.


🔵 Blue plots represent concentrations on days when all samples were successfully detected.
<br><br><br>
**Estimated parameters**


<img width="513" height="144" alt="image" src="https://github.com/user-attachments/assets/a9573ed1-0943-414d-baa3-7309858eaa29" />


C1 and C2 are parameters of the logistic model

S1 is a parameter for a state formula

S2 is a parameter representing a measurment error

