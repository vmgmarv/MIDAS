library(midasr)
library(quantmod)
library(lubridate)
library(dplyr)

usqgdp <-  getSymbols("GDP",src="FRED",auto.assign=FALSE)
infl <- getSymbols("CPALTT01USM659N",src="FRED",auto.assign=FALSE)

usqgdp <- usqgdp["/2022-04-01",]
infl <- infl["/2022-06-01"]

gdpg <- diff(log(usqgdp))*100

mr <- midas_r(mlsd(infl,2,gdpg)~mlsd(gdpg, 1:2, gdpg)+mlsd(infl,3+0:8, gdpg), data=list(gdpg=gdpg, infl=infl), start = NULL)

##################################################################################################
library(readxl)
library(tidyverse)
npl = read_excel("D:/My documents/THESIS WRITING/modeling/PHP_NPL.xlsx")
#npl <- npl %>% column_to_rownames(., var = "Dates")
npl <- npl[, c("PHP_NPL/TLP_diff")]
#npl <- diff(npl)
npl_vec <- npl[['PHP_NPL/TLP_diff']]
npl_vec <- npl_vec[!is.na(npl_vec)]

##################################################################################################

gdp = read_excel("D:/My documents/THESIS WRITING/modeling/GDP_growth.xlsx")
#gdp <- gdp %>% column_to_rownames(., var = "Dates")
gdp <- gdp[, c("GDP growth rate_constant")]
#gdp <- diff(gdp)
gdp_vec <- gdp[['GDP growth rate_constant']]
gdp_vec <- gdp_vec[!is.na(gdp_vec)]

##################################################################################################
ex_rate = read_excel("D:/My documents/THESIS WRITING/modeling/exchange_rate.xlsx")
ex_rate$US_dollar <- as.numeric(as.character(ex_rate$US_dollar))
ex_rate <- na.omit (ex_rate, "US_dollar") 

ex_rate$week <- floor_date(ex_rate$Period, "week")

ex_rate <- ex_rate %>%
  group_by(week) %>%
  summarize(mean = mean(US_dollar))


ex_rate_vec <- ex_rate[['mean']]
ex_rate_vec <- ex_rate_vec[!is.na(ex_rate_vec)]

##################################################################################################
pub_debt = read_excel("D:/My documents/THESIS WRITING/modeling/public_debt.xlsx")
pub_debt$External_Debt_to_GDP <- as.numeric(as.character(pub_debt$External_Debt_to_GDP))
pub_debt <- na.omit (pub_debt, "External_Debt_to_GDP") 

pub_debt_vec <- pub_debt[['External_Debt_to_GDP']]
pub_debt_vec <- pub_debt_vec[!is.na(pub_debt_vec)]
##################################################################################################

mr <- midas_r(mlsd(NPL,2,gdpg)~mlsd(gdpg,1:2, gdpg), data=list(gdpg=gdp_vec, NPL=npl_vec), start = NULL)


pr <- predict(mr, newdata = list(gdpg = gdp_vec, NPL = npl_vec))
cbind(pr[length(pr)-(5:1)],mr$fitted.values[length(mr$fitted.values)-(4:0)])

mrh_2 <- update(mr, formula = mlsd(NPL, 1, gdpg)~.)
mrh_3 <- update(mr, formula = mlsd(NPL, 0, gdpg)~.)

pr2 <- predict(mrh_2, newdata = list(gdpg = gdp_vec, NPL = npl_vec))
pr3 <- predict(mrh_3, newdata = list(gdpg = gdp_vec, NPL = npl_vec))
q2f <- c(tail(pr, n = 1), tail(pr2, n = 1 ), tail(pr3, n = 1))

##################################################################################################

mr2 <- midas_r(mlsd(NPL,2,Exchnange_rate)~mlsd(Exchnange_rate,5, NPL), 
               data=list(Exchnange_rate=ex_rate_vec, NPL=npl_vec), start = NULL)

mr2 <- midas_r(mlsd(NPL,2,Exchnange_rate)~mlsd(Exchnange_rate,0:5, NPL), 
               data=list(Exchnange_rate=ex_rate_vec, NPL=npl_vec), start = NULL)


mr3 <- midas_r(mlsd(NPL,2,Public_Debt)~mlsd(Public_Debt,1, Public_Debt), data=list(Public_Debt=pub_debt_vec, NPL=npl_vec), start = NULL)
summary(mr3)

midas_r(mlsd(npl_vec,2,ex_rate_vec) ~mlsd(ex_rate_vec,0:5,npl_vec),start = list(x = c(1, 
                                                                                      -0.5), z = c(2, 0.5, -0.1)))

midas_r(mlsd(gdp_vec,2,npl_vec) ~mlsd(npl_vec,2,npl_vec),start = NULL)


trend = c(1:93)
midas_u(npl_vec ~ trend + mls(ex_rate_vec, 0:2, 280))

eq_u <- lm(gdp_vec ~ trend + mls(gdp_vec, k = 0:2, m = 4) + mls(npl_vec, k = 0:16, m = 12))
eq_u <- lm(gdp_vec ~ trend + mls(npl_vec, k = 0:2, m = 3.010753))
#mlsd(ex_rate_vec,0:5,npl_vec)