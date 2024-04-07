rm(list = ls())
library(ggplot2)
library(dplyr)
superlife_data <- read.csv("superlife_data.csv",header = TRUE)

#Checking which policy type is purchased for different age groups
age_vs_ptype <- table(superlife_data$Policy.type,superlife_data$Issue.age)
barplot(age_vs_ptype,beside = T,
        legend = rownames(age_vs_ptype), main = "age_vs_policy-type")

#Policy type vs face amount
FACE_AMT_vs_ptype <- table(superlife_data$Policy.type,superlife_data$Face.amount)
barplot(FACE_AMT_vs_ptype,beside = T,
        legend = rownames(FACE_AMT_vs_ptype), main = "FACE_AMT_vs_policy-type")


#Smoker Status vs face amount
FACE_AMT_vs_ptype <- table(superlife_data$Face.amount,superlife_data$Smoker.Status)

#SEX vs face amount
SEX_vs_ptype <- table(superlife_data$Face.amount,superlife_data$Sex)

#Sex vs underwriting class -> Shows males prefer more risk due to almost double the number of policies for males in the high risk and moderate risk class compared to a relatively less difference in the low risk and very low risk sector
SEX_vs_UW <- table(superlife_data$Sex,superlife_data$Underwriting.Class)


Age_vs_Reg <- table(superlife_data$Issue.age,superlife_data$Region)


UC_vs_Reg <- table(superlife_data$Underwriting.Class,superlife_data$Region)



DC_vs_Reg <- table(superlife_data$Distribution.Channel,superlife_data$Region)



COD_vs_Reg <- table(superlife_data$Cause.of.Death,superlife_data$Region)


#Rural vs urban population by age.
age_vs_ru <- table(superlife_data$Urban.vs.Rural,superlife_data$Issue.age)
barplot(age_vs_ru,beside = T,legend = rownames(age_vs_ru), main = "age_vs_rural")


#Rural vs urban population by sex
sex_vs_age <- table(superlife_data$Sex,superlife_data$Issue.age)
barplot(sex_vs_age,beside = T,legend = rownames(sex_vs_age), main = "sex_vs_age")


sex_vs_COD <- table(superlife_data$Sex,superlife_data$Cause.of.Death)
barplot(sex_vs_COD,beside = T,legend = rownames(sex_vs_COD), main = "sex_vs_COD")



ru_vs_COD <- table(superlife_data$Urban.vs.Rural,superlife_data$Cause.of.Death)
barplot(ru_vs_COD,beside = T,legend = rownames(ru_vs_COD), main = "ru_vs_COD")



ru_vs_YOD <- table(superlife_data$Urban.vs.Rural,superlife_data$Year.of.Death)
barplot(ru_vs_YOD,beside = T,legend = rownames(ru_vs_YOD)
        , main = "rural/urban_vs_YearOfDth")



reg_vs_YOD <- table(superlife_data$Region,superlife_data$Year.of.Death)
barplot(reg_vs_YOD,beside = T,legend = rownames(reg_vs_YOD), main = "reg_vs_YOD")


reg_vs_ru <- table(superlife_data$Region,superlife_data$Urban.vs.Rural)
barplot(reg_vs_ru,beside = T,legend = rownames(reg_vs_ru), main = "reg_vs_ru")


reg_vs_COD <- table(superlife_data$Cause.of.Death,superlife_data$Region)
barplot(reg_vs_COD,beside = T,legend = rownames(reg_vs_COD), main = "reg_vs_COD")





smoker_vs_age <- table(superlife_data$Smoker.Status,superlife_data$Issue.age)
barplot(smoker_vs_age,beside = T,legend = rownames(smoker_vs_age), main = "smoker_vs_age")

COD_prop_tab <- superlife_data %>%
  filter(Cause.of.Death != "")
COD_prop <- prop.table(table(COD_prop_tab$Cause.of.Death))
barplot(COD_prop,las = 2,ylim = c(0,0.4),main = "Proportions for COD"
        , xlab = "", ylab = "proportion", 
        legend.text = c("C00-D48 = Cancer", "I00-I99 = Heart"), 
        col = "lightblue")
mtext(text = "Cause of Death (COD)", side = 1)
#Age distribution for each face amount 
FAMT_vs_AGE <- table(superlife_data$Face.amount,superlife_data$Issue.age)
par(mfrow = c(3, 2))
for(i in 1:6){
  barplot(FAMT_vs_AGE[i,],main = rownames(FAMT_vs_AGE)[i])
}

#Age distribution for each region
REG_vs_AGE <- table(superlife_data$Region,superlife_data$Issue.age)
par(mfrow = c(3, 2))
for(i in 1:6){
  barplot(REG_vs_AGE[i,],main = rownames(REG_vs_AGE)[i])
}


#Age distribution for each COD
COD_vs_AGE <- table(superlife_data$Cause.of.Death,superlife_data$Issue.age)

par(mfrow = c(1, 2))
barplot(COD_vs_AGE[3,],main = rownames(COD_vs_AGE)[3],
        xlab = "Age", ylab = "Policy Count")
barplot(COD_vs_AGE[8,],main = rownames(COD_vs_AGE)[8],
        xlab = "Age", ylab = "Policy Count")

#Face amount vs avg age
super_life_data_avgage <- superlife_data %>%
  group_by(Face.amount) %>%
  summarise(mean(Issue.age),max(Issue.age),min(Issue.age),n())
super_life_data_avgage

#Dth in each region
super_life_data_dth <- superlife_data %>%
  group_by(Region) %>%
  filter(!is.na(Death.indicator)) %>%
  summarise(num_of_dth = sum(Death.indicator))
super_life_data_dth

super_life_data_pop <- superlife_data %>%
  group_by(Region) %>%
  summarise(pop_size = n())
super_life_data_pop

Dth_by_region <- super_life_data_dth$num_of_dth/super_life_data_pop$pop_size
  
Dth_by_region


#Face amount vs avg age
super_life_data_avgage <- superlife_data %>%
  filter(Smoker.Status == "S") %>%
  group_by(Issue.age) %>%
  summarise(avg_FC_amt = mean(Face.amount))

barplot(super_life_data_avgage$avg_FC_amt)


super_life_data_var <- superlife_data %>%
  filter(Smoker.Status == "S") %>%
  group_by(Issue.age) %>%
  summarise(exp_FC_amt = mean(Face.amount),var_FC_amt = var(Face.amount))


super_life_data_avgage_NS <- superlife_data %>%
  filter(Smoker.Status == "NS") %>%
  group_by(Issue.age) %>%
  summarise(avg_FC_amt = mean(Face.amount))

barplot(super_life_data_avgage_NS$avg_FC_amt)


#---------------------------------------------------------------------------
#TABLE FOR AVERAGE FACE VALUE & POLICY COUNT ACROSS EACH AGE
#SMOKER INFO
super_life_smoker_info <- superlife_data %>%
  filter(Smoker.Status == "S") %>%
  group_by(Issue.age) %>%
  summarise(active_policy_count = n(),avg_FC_amt = mean(Face.amount),min_FC_amt = min(Face.amount),max_FC_amt = max(Face.amount),total_FC_amt = sum(Face.amount))

#NON-SMOKER INFO
super_life_non_smoker_info <- superlife_data %>%
  filter(Smoker.Status == "NS") %>%
  group_by(Issue.age) %>%
  summarise(active_policy_count = n(),avg_FC_amt = mean(Face.amount),min_FC_amt = min(Face.amount),max_FC_amt = max(Face.amount),total_FC_amt = sum(Face.amount))

#Number of new policies over time
new_policy_count <- superlife_data %>%
  group_by(Issue.year) %>%
  summarise(new_pol = n())

#Policy End date
table(superlife_data$Policy.type,superlife_data$Lapse.Indicator)

pol_lapse_info <- superlife_data %>%
  filter(Policy.type == "T20") %>%
  group_by(Year.of.Lapse) %>%
  summarise(pol_lapse_info = n())
  
plot(pol_lapse_info)


table(superlife_data$Policy.type,superlife_data$Lapse.Indicator)

#Policy Lapse Rate
lapse_rate_info <- superlife_data %>%
  filter(Policy.type == "T20") %>%
  mutate()

#MORTALITY EXPERIENCE 
Mortality_exp <- superlife_data %>%
  filter(Death.indicator == "1") %>%
  group_by(Issue.age) %>%
  summarise(policy_count = n(),claim_payouts = sum(Face.amount), 
            avg_cost_per_policy = sum(Face.amount)/n())
barplot(Mortality_exp$avg_cost_per_policy, 
        main = "avg_cost_per_policy", names = Mortality_exp$Issue.age)

#Smokers cause of death
Smoker_COD <- superlife_data %>%
  filter(Death.indicator == "1") %>%
  filter(Smoker.Status == "S") %>%
  group_by(Cause.of.Death) %>%
  summarise(policy_count = n())

#Non Smokers COD
Non_Smoker_COD <- superlife_data %>%
  filter(Death.indicator == "1") %>%
  filter(Smoker.Status == "NS") %>%
  group_by(Cause.of.Death) %>%
  summarise(policy_count = n())
t(Non_Smoker_COD)


#Cause of Death dist
Smoker_COD <- superlife_data %>%
  filter(Death.indicator == "1") %>%
  group_by(Cause.of.Death) %>%
  summarise(dist_prop = n())
