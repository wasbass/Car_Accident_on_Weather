#####library#####
{
  library(lattice)
  library(readxl)
  library(generics)
  library(ggplot2)
  library(car)
  library(broom)
  library(corrgram)
  library(GGally)
  library(tidyr)
  library(AER)
  library(MASS)
  library(tseries)
  library(sandwich)
  library(rms)
}
#####資料預處理#####
setwd("C:\\RRR")
accident1921 <- read_excel("accident1921.xlsx")
{
accident1921$all = accident1921$carminor + accident1921$motorminor
accident1921$logall = log(accident1921$all)
accident1921$logmrt = log(accident1921$mrt)
accident1921$logcar = log(accident1921$carminor)
accident1921$logmotor = log(accident1921$motorminor)
accident1921$rainsq = accident1921$raindrop^2
accident1921$diflogmrt = log(accident1921$mrt) - log(accident1921$mrtl1)
accident1921$diflogmrtl1 = log(accident1921$mrtl1) - log(accident1921$mrtl2)
accident1921$diflogmrtl2 = log(accident1921$mrtl2) - log(accident1921$mrtl3)
accident1921$diflogmrtl3 = log(accident1921$mrtl3) - log(accident1921$mrtl4)
accident1921$diflogmrtl4 = log(accident1921$mrtl4) - log(accident1921$mrtl5)
accident1921$diflogmrtl5 = log(accident1921$mrtl5) - log(accident1921$mrtl6)
accident1921$diflogmrtl6 = log(accident1921$mrtl6) - log(accident1921$mrtl7)
accident1921$carfatallast5 = accident1921$carfatall1 + accident1921$carfatall2 + accident1921$carfatall3 + accident1921$carfatall4 + accident1921$carfatall5
accident1921$motorfatallast5 = accident1921$motorfatall1 + accident1921$motorfatall2 + accident1921$motorfatall3 + accident1921$motorfatall4 + accident1921$motorfatall5
}
#####對應變數做單根檢定#####
adf.test(accident1921$logmotor)
adf.test(accident1921$logcar)
adf.test(accident1921$logall)
#這邊可以看到車禍的數量取對數後都呈現I(0)
xyplot(logcar ~ t , data = accident1921)
xyplot(logmotor ~ t , data = accident1921)
#雖然定態但似乎有時間趨勢，並且趨勢存在明顯的斷點
#我們這邊考慮用spline來擷取趨勢

regc_splinetrend_simp <- lm(logcar~ auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regc_splinetrend_simp)
xyplot(regc_splinetrend_simp$residuals ~ accident1921$t)

regm_splinetrend_simp <- lm(logmotor~ auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regm_splinetrend_simp)
xyplot(regm_splinetrend_simp$residuals ~ accident1921$t)
#在用三個spline來擷取趨勢後，車禍數量的殘差接近white noise了
#所以在之後的模型都要放入這些spline trend來做分析

#####對自變數做單根檢定#####
adf.test(accident1921$mrt)
adf.test(accident1921$logmrt)
#捷運人數不管有無取對數都存在單根問題
adf.test(accident1921$diflogmrt)
#取對數後再做一階差分就呈現I(0)，可以放入模型中了
adf.test(lm(logall~ diflogmrt , data = accident1921)$residuals) #殘差為I(0)，沒有問題

xyplot(diflogmrt~t , data = accident1921)

#觀察捷運搭乘人數增減率與車禍數量的關係

xyplot(logcar ~ diflogmrt , data = accident1921)
xyplot(logmotor ~ diflogmrt , data = accident1921)
#似乎沒有明顯的線性關係，或許也可以考慮對應變數(車禍量)取對數再做差分


#接著對天氣變數做單根檢定
adf.test(accident1921$wingspeed)
adf.test(accident1921$rainhour)
adf.test(accident1921$maxwing)
adf.test(accident1921$visb)
#可以觀察到天氣變數都是I(0)，可以直接放入迴歸模型中

#####EDA區#####
#我們觀察捷運人數和車禍人數與T的關係

xyplot(logcar~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 ,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

xyplot(logmotor~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 ,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

xyplot(diflogmrt~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 ,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

#如果再加上天氣的因素

xyplot(logcar~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 + 3 * accident1921$sunny,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

xyplot(logmotor~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 + 3 * accident1921$sunny,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

xyplot(diflogmrt~t , data = accident1921,
       col = ( accident1921$holiday | accident1921$typhoonholiday ) +1 + 3 * accident1921$sunny ,
       pch = ( accident1921$holiday | accident1921$typhoonholiday ) +15)

#觀察車禍數量與雨量的關係
xyplot(carminor ~ raindrop , data = accident1921)
xyplot(motorminor ~ raindrop , data = accident1921)

xyplot(logcar ~ raindrop , data = accident1921)
xyplot(logmotor ~ raindrop , data = accident1921)

#####模型比較區#####
#接著開始比較模型，先從沒放天氣資料也沒放捷運人數的模型
regc_pure <- lm(logcar~ holiday + typhoonholiday +           
                 auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regc_pure)

regm_pure <- lm(logmotor~ holiday + typhoonholiday +           
                  auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regm_pure)

#這邊看到汽車的解釋變異比機車小很多
#*****************************************************************
#接著放入車禍資料的落遲項

regc_lagy <- lm(logcar~ holiday + typhoonholiday +
                  log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                  auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regc_lagy)

regm_lagy <- lm(logmotor~ holiday + typhoonholiday + 
                  log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                  auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regm_lagy)

#這邊由adj R^2挑選放入四項落遲項
#*****************************************************************
#接著放入天氣項目，這邊雨量先放入數值資料並放入平方項
regc_rainnoclass <- lm(logcar~ raindrop + rainsq +
                      temp + wingspeed + rainhour + maxwing + visb + 
                        holiday + typhoonholiday + 
                        log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                        auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regc_rainnoclass)#天氣對汽車變異貢獻了0.015的解釋能力

regm_rainnoclass <- lm(logmotor~ raindrop + rainsq +
                         temp + wingspeed + rainhour + maxwing + visb + 
                         holiday + typhoonholiday + 
                         log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                         auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regm_rainnoclass)#天氣對汽車變異貢獻了0.028的解釋能力

#雖然天氣項目貢獻了將近2%的變異解釋力，但這邊注意到雨量完全不顯著，我們改成分組再放入模型

regc_rainwithclass = lm(logcar~ drizzle + light + medium + heavy + storm +
                          temp + wingspeed + rainhour + maxwing + visb + 
                          holiday + typhoonholiday + 
                          log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                          auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regc_rainwithclass) 

regm_rainwithclass = lm(logmotor~ drizzle + light + medium + heavy + storm + 
                         temp + wingspeed + rainhour + maxwing + visb + 
                         holiday + typhoonholiday + 
                         log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                         auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regm_rainwithclass)
#分組後R^2都上升了，且這邊的雨量分組大致都顯著，即使不顯著也為負相關，代表下雨會讓車禍數量減少
#*****************************************************************
#這邊再加上前一天的雨量

regc_twodayrain = lm(logcar~ drizzle + light + medium + heavy + storm + 
                         lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                         holiday + typhoonholiday +
                         temp + wingspeed + rainhour + maxwing + visb +
                         log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                         auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(regc_twodayrain)

regm_twodayrain = lm(logmotor~ drizzle + light + medium + heavy + storm + 
                       lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                       holiday + typhoonholiday +
                       temp + wingspeed + rainhour + maxwing + visb +
                       log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                       auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(regm_twodayrain)
#R^2有顯著增加，雖然只有前一天為毛毛雨的係數顯著
#*****************************************************************
#接著再放入潛在的交互作用項
regc_interaction = lm(logcar~ drizzle + light + medium + heavy + storm + 
                       lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                       holiday + typhoonholiday +
                       temp + wingspeed + rainhour + maxwing + visb +
                       log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                       drizzle * holiday +  light * holiday + medium * holiday + heavy * holiday + storm * holiday+ heavy * typhoonholiday +
                       drizzle * wingspeed +  light * wingspeed + medium * wingspeed + heavy * wingspeed + storm * wingspeed +
                       auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(regc_interaction)

regm_interaction = lm(logmotor~ drizzle + light + medium + heavy + storm + 
                       lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                       holiday + typhoonholiday +
                       temp + wingspeed + rainhour + maxwing + visb +
                       log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                       drizzle * holiday +  light * holiday + medium * holiday + heavy * holiday + storm * holiday+ heavy * typhoonholiday +
                       drizzle * wingspeed +  light * wingspeed + medium * wingspeed + heavy * wingspeed + storm * wingspeed +
                       auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(regm_interaction)

#R^2有顯著增加，但可以修剪一下

regc_interaction_fix = lm(logcar~ drizzle + light + medium + heavy + storm + 
                            lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                            holiday + typhoonholiday +
                            temp + wingspeed + rainhour + maxwing + visb +
                            log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                            drizzle * wingspeed + 
                            auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(regc_interaction_fix)

regm_interaction_fix = lm(logmotor~ drizzle + light + medium + heavy + storm + 
                            lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                            holiday + typhoonholiday +
                            temp + wingspeed + rainhour + maxwing + visb +
                            log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                            medium * holiday +
                            drizzle * wingspeed +  light * wingspeed + heavy * wingspeed + storm * wingspeed +
                            auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921)  

summary(regm_interaction_fix)
#根據adj R^2做模型選擇，兩者留下的交互作用項不太一樣
#此時car的R^2為0.4656，adj R^2為0.4511
#此時motor的R^2為0.678，adj R^2為0.6676
#*****************************************************************
#最後再放入捷運搭乘人數，並考慮落遲項
regc_auxmrt_lag = lm(logcar~ drizzle + light + medium + heavy + storm + 
                   lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
                   holiday + typhoonholiday +
                   temp + wingspeed + rainhour + maxwing + visb +
                   log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                   drizzle * wingspeed +
                   diflogmrt + diflogmrtl1 + diflogmrtl2+diflogmrtl3+diflogmrtl4+diflogmrtl5+
                   auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921)
summary(regc_auxmrt_lag)

regm_auxmrt_lag = lm(logmotor~ drizzle + light + medium + heavy + storm + 
              lastdrizzle + lastlight + lastmedium + lastheavy + laststorm +
              holiday + typhoonholiday +
              temp + wingspeed + rainhour + maxwing + visb +
              log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
              medium * holiday +
              drizzle * wingspeed +  light * wingspeed + heavy * wingspeed + storm * wingspeed +
              diflogmrt + diflogmrtl1 + diflogmrtl2+diflogmrtl3+diflogmrtl4+diflogmrtl5+ 
              auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921)
summary(regm_auxmrt_lag)

#這邊顯示放到五階落遲項都很顯著，六階後的進步幅度不明顯，我們先考慮只放五階
#car的R^2為0.4982，adj R^2為0.4816，增加0.033
#motor的R^2為0.6934，adj R^2為0.6818，增加0.016
#*****************************************************************
#這邊做個對照，如果只放捷運人數但沒放天氣

regc_mrtnoweather = lm(logcar~  holiday + typhoonholiday +
                        diflogmrt + diflogmrtl1+ diflogmrtl2+diflogmrtl3+diflogmrtl4+diflogmrtl5+
                        log(carl1) + log(carl2) + log(carl3) + log(carl4) +
                        auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)

summary(regc_mrtnoweather)

regm_mrtnoweather = lm(logmotor~  holiday + typhoonholiday +
                         diflogmrt + diflogmrtl1+ diflogmrtl2+diflogmrtl3+diflogmrtl4+diflogmrtl5+
                         log(motorl1) + log(motorl2) + log(motorl3) + log(motorl4) +
                         auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3), data = accident1921)
summary(regm_mrtnoweather)
#car的R^2為0.4716，跟lagy model的0.4396，提升了0.032
#motor的R^2為0.6628，跟lagy model的0.6424提升了0.02
#代表天氣和捷運運量或許存在共變異，也就是兩者會共同解釋到其中一部份
#而注意到天氣是無法人為決定的，但捷運搭乘卻可以被天氣所決定，可能存在因果關係

reg_mrtpure = lm(diflogmrt~ holiday + typhoonholiday +
                  auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921)

reg_mrtonweather = lm(diflogmrt~ drizzle + light + medium + heavy + storm + 
                         holiday + typhoonholiday +
                         temp + wingspeed + rainhour + maxwing + visb +
                         auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3) , data = accident1921) 

summary(reg_mrtpure)
summary(reg_mrtonweather)
#比較這兩個模型可知天氣確實對捷運搭乘人數有解釋力，儘管非常有限
#*****************************************************************
xyplot(regc_auxmrt_lag$residuals ~ accident1921$t )
xyplot(regm_auxmrt_lag$residuals ~ accident1921$t )

#在最後的reg_interaction模型中，殘差大致呈現White noise
#####HAC標準誤做t-test#####

coeftest(regc_auxmrt_lag, vcov = vcovHC( regc_auxmrt_lag, type = "HC3", method = "arellano" ))
coeftest(regm_auxmrt_lag, vcov = vcovHC( regm_auxmrt_lag, type = "HC3", method = "arellano" ))
#注意HC2和HC3會根據1-h_ii(leverage ratio)的倒數做權重，所以每個特徵(包括交互作用項)的資料不能只有一筆，否則該筆資料的h_ii會為1

max(lm.influence(regm_auxmrt_lag)$hat)
which.max(lm.influence(regm_auxmrt_lag)$hat)
regm_auxmrt_lag_hatmatrix <- data.frame(lm.influence(regm_auxmrt_lag)$hat)

#####Logistic model#####
xyplot( carfatal ~ t , data = accident1921)
xyplot( motorfatal ~ t , data = accident1921)

logitc_null <- glm( carfatal ~ 1 , data = accident1921 , family = binomial)
logitm_null <- glm( motorfatal ~ 1 , data = accident1921 , family = binomial)
#*****************************************************************

logitc_lagy <- glm(carfatal ~ carfatallast5,
                   data = accident1921 , family = binomial)
summary(logitc_lagy)
lrtest(logitc_null,logitc_lagy) #前五天的汽車死亡車禍數量對模型有影響

logitc_pure <- glm( carfatal ~  holiday + typhoonholiday +
                      carfatallast5 +
                auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3),
                data = accident1921,family = binomial)
summary(logitc_pure)
lrtest(logitc_lagy,logitc_pure) #car-pure模型不顯著

#*****************************************************************

logitm_lagy <- glm( motorfatal ~ motorfatallast5,
                   data = accident1921 , family = binomial)
summary(logitm_lagy)
lrtest(logitm_null,logitm_lagy) #前五天的機車死亡車禍數量對模型沒有影響

logitm_pure <- glm( motorfatal ~  holiday + typhoonholiday +
                      motorfatallast5 +
                      auxt1 + I(auxt1^2) + auxt2 + I(auxt2^2) + auxt3 + I(auxt3^2) + I(auxt3^3),
                    data = accident1921,family = binomial)
summary(logitm_pure)
lrtest(logitm_null,logitm_pure) #motor-pure模型不顯著

#*****************************************************************

logitc_weather_fullinteraction = glm(carfatal ~ carfatallast5 +
                                   drizzle + light + medium + heavy + storm + 
                                   wingspeed + rainhour + visb +
                                   drizzle * wingspeed + drizzle * wingspeed+ light * wingspeed + medium * wingspeed + heavy * wingspeed + storm * wingspeed 
                                  ,data = accident1921,family = binomial)
summary(logitc_weather_fullinteraction)
lrtest(logitc_lagy,logitc_weather_fullinteraction)
#加入所有的天氣變數，模型仍然不顯著，嘗試修剪變數

logitc_weather_revised = glm(carfatal ~ carfatallast5 +
                                       drizzle + 
                                       wingspeed + 
                                       drizzle * wingspeed 
                                     ,data = accident1921,family = binomial)
summary(logitc_weather_revised)
lrtest(logitc_lagy,logitc_weather_revised)
#p-value幾乎沒有變化，甚至上升了，嘗試再加入捷運人數

logitc_mrt = glm(carfatal ~ carfatallast5 +
                               drizzle + light + medium + heavy + storm +
                               wingspeed + 
                               drizzle * wingspeed +
                               log(mrt)
                             ,data = accident1921,family = binomial)

summary(logitc_mrt)
lrtest(logitc_lagy,logitc_mrt)
#加入捷運人數後，模型還是不顯著，在logit的架構下汽車死亡案件似乎完全為隨機事件，只與前五天的死亡車禍數量有關

logitm_weather_fullinteraction = glm(motorfatal ~ motorfatallast5 +
                                       drizzle + light + medium + heavy + storm + 
                                       wingspeed + rainhour + visb +
                                       drizzle * wingspeed + drizzle * wingspeed+ light * wingspeed + medium * wingspeed + heavy * wingspeed + storm * wingspeed 
                                     ,data = accident1921,family = binomial)
summary(logitm_weather_fullinteraction)
lrtest(logitm_null,logitm_weather_fullinteraction)
#在加入所有天氣變數下，模型完全不顯著，跟Null Model沒兩樣
#嘗試修剪變數

logitm_weather_revised = glm(motorfatal ~ motorfatallast5 +
                               drizzle + light + medium + heavy + storm +
                               wingspeed +
                               drizzle * wingspeed 
                             ,data = accident1921,family = binomial)
summary(logitm_weather_revised)
lrtest(logitm_null,logitm_weather_revised)
#仍然不顯著，再嘗試加入捷運人數
logitm_mrt = glm(motorfatal ~ motorfatallast5 +
                               drizzle + light + medium + heavy + storm +
                               wingspeed +
                               drizzle * wingspeed+
                               log(mrt)
                             ,data = accident1921,family = binomial)
summary(logitm_mrt)
lrtest(logitm_null,logitm_mrt)
#加入捷運人數後，模型還是不顯著，在logit的架構下機車死亡案件完全為隨機事件，跟Null Model沒兩樣