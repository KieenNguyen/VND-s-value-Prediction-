install.packages('urca')
install.packages('vars')
install.packages('xlsx')
install.packages('tsDyn')
install.packages('tseries')
library('vars')
library('tsDyn')
library('xlsx')
library('tseries')
library('urca')
library('tsDyn')
library('forcecast')
library('fixest')
library('lmtest')
library('plm')

#Question 1:
output1 = lm(ln_fxrate ~ delta_cpi,data=Q1)
summary(output1) 

output2 = lm(ln_fxrate ~ M2GDP, data = Q1)
summary(output2)

output4 =lm(lnex ~ Inflation, data = CPI)
summary(output4)

#Question 2:
#Johansen
attach(Q2)
model = ca.jo(data.frame(vn_index, IIP, inflation, fx_rate, one_year_bond, interbank_rate), ecdet = "const", type ="trace", K=6)
summary(model)
detach(Q2)

#VECM:
#Choose the optimal lag: 
attach(Q2)
lagselect = VARselect(data.frame(vn_index, IIP, inflation, fx_rate, one_year_bond, interbank_rate),lag.max=7,type ='const')
lagselect
#Run VECM: 
attach(Q2)
vecm = VECM(data.frame(vn_index, IIP, inflation, fx_rate, one_year_bond, interbank_rate),lag =1, r=1, estim = "ML")
summary(vecm)

