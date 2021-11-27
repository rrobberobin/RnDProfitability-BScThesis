rm(list=ls())
cat("\14")

library(readxl)
text = 6
#url = "Data/withCapexAndLeverage.xls"
url = "Data/TaiwanJapanKoreaWithDepr.xls"
orig <- read_excel(url, sheet = "Screening", skip = 7, na = "-")
#orig <- read_excel(url, sheet = "Screening", skip = 7, col_types=c(rep("text",text), rep("numeric",84)))
#"RD v3 criteria.xls"
#df = na.omit(orig)

# test = orig[!is.na(orig[,133]),]
# table(orig["Country/Region of Incorporation"])
# 
# countryPattern = "Taiwan|Japan|South Korea"
# filtercountry = sapply(orig["Country/Region of Incorporation"], grepl, pattern=countryPattern)
# df = orig[filtercountry,]
df = orig

numberOfFirms = nrow(df)

# countryUsed = c("Taiwan","Japan","South Korea")
# df = orig[orig["Country/Region of Incorporation"]==countryUsed,]
# df = orig[orig["Country/Region of Incorporation"]=="South Korea",]

#Data splitting----
#R&D
filterRD = sapply(names(df), grepl, pattern="R&D Exp")
RD = df[filterRD]
#RDChange = mapply(function(x,y) x-y, RDChange[,-1],RDChange[,-ncol(RDChange)])
RDChange = RD[,-1] - RD[,-ncol(RD)]

#Net income
filterNI = sapply(names(df), grepl, pattern="Net Income")
netIncome = df[filterNI]
NIChange = netIncome[,-1] - netIncome[,-ncol(netIncome)]

#Revenue
filterRev = sapply(names(df), grepl, pattern="Total Revenue")
revenue = df[filterRev]
revenue[revenue < 0] = NA

#Employees
#filterEmploy = sapply(names(df), grepl, pattern="Total Employees")
#employees = as.matrix(df[filterEmploy])

#Capex
filterCapex = sapply(names(df), grepl, pattern="Capital Expenditure")
capex = df[filterCapex]
capexChange = capex[,-1] - capex[,-ncol(capex)]

#Depreciation
filterDepr = sapply(names(df), grepl, pattern="Depreciation")
Depreciation = df[filterDepr]
DeprChange = Depreciation[,-1] - Depreciation[,-ncol(Depreciation)]

#Liabilties
filterLiab = sapply(names(df), grepl, pattern="Total Liabilities ")
liabilities = df[filterLiab]

#Equity
filterEquity = sapply(names(df), grepl, pattern="Market Cap")
equity = df[filterEquity]
equity = equity[,1:21]

#Leverage
leverage = liabilities/(liabilities+equity)
leverage[mapply(is.infinite, leverage)] = NA
#leverage = as.matrix(leverage)
levrChange = leverage[,-1] - leverage[,-ncol(leverage)]

#Net margin
netMargin = netIncome/revenue
netMargin[mapply(is.infinite, netMargin)] = NA
#netMargin = as.matrix(netMargin)
netMargChange = netMargin[,-1] - netMargin[,-ncol(netMargin)]

#RD Intensity 
RDIntensity = RD/revenue
RDIntensity[mapply(is.infinite, RDIntensity)] = NA
RDIntensity[RDIntensity<=0] = NA
#RDIntensity = as.matrix(RDIntensity)
RDIntensityChange = RDIntensity[,-1] - RDIntensity[,-ncol(RDIntensity)]

#Capex Intensity 
CapexIntensity = capex/revenue
CapexIntensity[mapply(is.infinite, CapexIntensity)] = NA
CapexIntensity[CapexIntensity<=0] = NA
#CapexIntensity = as.matrix(CapexIntensity)
CapexIntensityChange = CapexIntensity[,-1] - CapexIntensity[,-ncol(CapexIntensity)]

#Depr Intensity 
DeprIntensity = Depreciation/revenue
DeprIntensity[mapply(is.infinite, DeprIntensity)] = NA
DeprIntensity[DeprIntensity<=0] = NA
#DeprIntensity = as.matrix(DeprIntensity)
DeprIntensityChange = DeprIntensity[,-1] - DeprIntensity[,-ncol(DeprIntensity)]

#firm, sector and region fixed effects for panel data
#"factor" creates dummy variables. Automatically omits one dummy to avoid collinearity problem
sectorDummies = factor(df[["Primary Sector"]]) #12 sectors
geoDummies = factor(df[["Geographic Region"]]) #5 regions
firmDummies = factor(df[["Company Name"]])

#Panel format----
startYear = 6
NettoVinst = unlist(NIChange[,startYear:20])
NettoVinstLag = unlist(NIChange[,(startYear-1):19])
NettoVinstLag2 = unlist(NIChange[,(startYear-2):18])
NettoVinstNivå = unlist(netIncome[,(startYear-1):19])
NettoVinstMedel = rowMeans(netIncome[,(startYear-1):19])
NettoVinstFrånMedelvärdet = NettoVinstNivå - NettoVinstMedel

VinstMarginal = unlist(netMargChange[,startYear:20])
VinstMarginalLag = unlist(netMargChange[,(startYear-1):19])
VinstMarginalLag2 = unlist(netMargChange[,(startYear-2):18])
VinstMarginalNivå = unlist(netMargin[,(startYear-1):19])
VinstMarginalNivåMedel = rowMeans(netMargin[,(startYear-1):19])
VinstMarginalFrånMedelvärdet = VinstMarginalNivå - VinstMarginalNivåMedel

FoU = unlist(RDChange[,startYear:20])
FoULag1 = unlist(RDChange[,(startYear-1):19])
FoULag2 = unlist(RDChange[,(startYear-2):18])
FoULag3 = unlist(RDChange[,(startYear-3):17])
FoULag4 = unlist(RDChange[,(startYear-4):16])
FoULag5 = unlist(RDChange[,(startYear-5):15])

FoUInt = unlist(RDIntensityChange[,startYear:20])
FoUIntLag1 = unlist(RDIntensityChange[,(startYear-1):19])
FoUIntLag2 = unlist(RDIntensityChange[,(startYear-2):18])
FoUIntLag3 = unlist(RDIntensityChange[,(startYear-3):17])
FoUIntLag4 = unlist(RDIntensityChange[,(startYear-4):16])
FoUIntLag5 = unlist(RDIntensityChange[,(startYear-5):15])

Avskriv = unlist(DeprChange[,startYear:20])
CapexLag1 = unlist(capexChange[,(startYear-1):19])
CapexLag2 = unlist(capexChange[,(startYear-2):18])
CapexLag3 = unlist(capexChange[,(startYear-3):17])
CapexLag4 = unlist(capexChange[,(startYear-4):16])
CapexLag5 = unlist(capexChange[,(startYear-5):15])

AvskrivInt = unlist(DeprIntensityChange[,startYear:20])
CapexIntLag1 = unlist(CapexIntensityChange[,(startYear-1):19])
CapexIntLag2 = unlist(CapexIntensityChange[,(startYear-2):18])
CapexIntLag3 = unlist(CapexIntensityChange[,(startYear-3):17])
CapexIntLag4 = unlist(CapexIntensityChange[,(startYear-4):16])
CapexIntLag5 = unlist(CapexIntensityChange[,(startYear-5):15])
CapexInt = unlist(CapexIntensityChange)

Skuldsättning = unlist(levrChange[,startYear:20])
Företag = rep(firmDummies, 21-startYear)
Period = rev(rep(startYear:20, each=nrow(RDIntensityChange)))
# Sektor = rep(sectorDummies,startYear)
# GeografisktOmråde = rep(geoDummies,startYear)
# `Vinstmarginal 2010-2020` = c(VinstMarginal[,(21-startYear):21])
levels(Sektor) = c("-", "Telekom", "Utökad konsumtion", "Dagvarukonsumtion","Energi","Finans","Hälsa","Industri","IT","Råvaror","Fastigheter","Tjänster")
#https://sp500.se/sp500-och-de-olika-sektorerna/
# Försäljning20 = c(revenue[,startYear:21])
# Försäljning19 = c(revenue[,(startYear-1):20])
# Försäljning18 = c(revenue[,(startYear-2):19])
# Försäljning17 = c(revenue[,(startYear-3):18])
# Försäljning16 = c(revenue[,(startYear-4):17])



allNoDupes = data.frame(Företag,Period,
                        NettoVinst,NettoVinstLag,NettoVinstLag2,NettoVinstFrånMedelvärdet,
                        VinstMarginal,VinstMarginalLag,VinstMarginalLag2,VinstMarginalFrånMedelvärdet,
                        FoU,FoULag1,FoULag2,FoULag3,FoULag4,FoULag5,
                        FoUInt,FoUIntLag1,FoUIntLag2,FoUIntLag3,FoUIntLag4,FoUIntLag5,
                        Avskriv,CapexLag1,CapexLag2,CapexLag3,CapexLag4,CapexLag5,
                        AvskrivInt,CapexIntLag1,CapexIntLag2,CapexIntLag3,CapexIntLag4,CapexIntLag5,
                        Skuldsättning)

levels = data.frame(NettoVinst,NettoVinstLag,
                    NettoVinstLag2,NettoVinstFrånMedelvärdet,
                    FoU,FoULag1,FoULag2,FoULag3,FoULag4,FoULag5,
                    Avskriv,CapexLag1,CapexLag2,CapexLag3,CapexLag4,CapexLag5,
                    Skuldsättning)
marg = data.frame(VinstMarginal,VinstMarginalLag,
                  VinstMarginalLag2,VinstMarginalFrånMedelvärdet,
                  FoUInt,FoUIntLag1,FoUIntLag2,FoUIntLag3,FoUIntLag4,FoUIntLag5,
                  AvskrivInt,CapexIntLag1,CapexIntLag2,CapexIntLag3,CapexIntLag4,CapexIntLag5,
                  Skuldsättning)




#Tests on data----

#Descriptives
summary(NettoVinst)
summary(VinstMarginal)
summary(FoU)
summary(Capex)
summary(Avskriv)
summary(Skuldsättning)
a = summary(allNoDupes)


library(moments)
skew = c(skewness(allNoDupes,na.rm=T))
kurt = c(kurtosis(allNoDupes,na.rm=T))
kS = round(rbind(skew,kurt),2)
kurtSkew = data.frame(kS, row.names = c("Skewness","Kurtosis"))

library(stargazer)
stargazer(allNoDupes,
          type = "text",
          out = "desc.html",
          title = "Deskriptiv statistik",
          flip = T,
          digits = 2,
          digits.extra = 0,
          median=T,
          align	=T,
          add.lines = list(skew,kurt)
)


Korrelationer = round(cor(all,use = "complete.obs"),3)
write.csv(Korrelationer, "korrelationer.csv")



#Autokorr (modify for panel data)
acf()
pacf()
Box.test(netIncome,lag=1,type="Ljung-Box")



#Unit root
library(fUnitRoots)
adfTest(Nettovinst, lags = 252, type = c("nc"), title = NULL, description = NULL)

#Test for cointegration?
library(urca)
marginRoots <- ca.jo(levels[,1:10], type = "trace", ecdet = "const", K = 2)
summary(marginRoots)
profitRoots <- ca.jo(marg, type = "trace", ecdet = "const", K = 3)
summary(profitRoots)

#Unit root for panel data
purtest(marg)





#Plotting----
#these wont work because they have sales in common?
# plot(netIncome, RD, log="xy")
# plot(netMargin, RDIntensity, log="xy")

#sales is not in common. Good!
plot(netIncome[[21]], RDIntensity[,20], log="xy")
plot(netMargin[,21], RD[[20]], log="xy")
plot(netMargin[,21], RD[,20], log="xy", col=factor(as.matrix(df[,3])))
plot(netMargin[,21], RD[,20], log="xy", col=factor(as.matrix(df[,6])))
plot(c(RDIntensity[,9:19]),c(netIncome[,11:21]), log="xy")
plot(c(RD[,9:19]),c(netMargin[,11:21]), log="xy")

#Does it look normal?----
hist(FoU)
hist(NettoVinst)
plot(FoU)
plot(NettoVinst)
plot(VinstMarginal)
quantile(round(NettoVinst,3),na.rm=T,prob = seq(0, 1, length = 101))
qqnorm(FoU)
qqnorm(NettoVinst)

#RD logarithm
hist(FoU)
test = as.vector(na.omit(FoU))
jarque.test(test)
logTest = log(FoU)
logTest = as.vector(na.omit(logTest))
summary(logTest)
hist(logTest,breaks=50)
jarque.test(logTest)


#Look at the data without the extreme values----
# library(DescTools)
# winsTranform = Winsorize(NettoVinst,probs = c(0.02, 0.85))
# summary(NettoVinst)
# summary(winsTranform)
# hist(winsTranform,breaks=100)
# quantile(NettoVinst,0.95)
outlierRemove = NettoVinst[NettoVinst < quantile(NettoVinst,0.70) & NettoVinst > quantile(NettoVinst,0.05)]
outlierRemove = NettoVinst[abs(NettoVinst)<1800]
hist(outlierRemove, breaks=500)
summary(outlierRemove)

outlierChangeRemove = netIncomeChange[netIncomeChange < quantile(netIncomeChange,0.70) & netIncomeChange > quantile(netIncomeChange,0.25)]
hist(outlierChangeRemove, breaks=100)

#Transformations----
#Transformation?: 1.Divide by biggest value, 2. all+1 , 3. logarithm.... Or neglog
#Transformation: Nettovinst + minimum
#logTransformation
mini = min(NettoVinst)
NettoVinstTransform = log(NettoVinst - mini * 2)
hist(NettoVinstTransform,breaks=500)
qqnorm(NettoVinstTransform)
summary(NettoVinstTransform)

#RootTransformation
rootTransform = mapply(function(x) sign(x)*abs(x)^(1/5), NettoVinst)
rootTransform = pracma:::nthroot(NettoVinst, 5)
hist(rootTransform,breaks=500)
qqnorm(rootTransform)
summary(rootTransform)

#Robin root transformation
robinTransform = mapply(function(x) sign(x)*((abs(x)+1)^(1/36)-1), NettoVinst)
hist(robinTransform, breaks=500)
hist(outlierRemove, breaks=500)
qqnorm(robinTransform)

#NegLog
negLogTransform = mapply(function(x) sign(x)*log(abs(x)+1), NettoVinst)
hist(negLogTransform, breaks=500)

#BoxCox
library(MASS)
#boxCoxTransformation= boxcox(NettoVinst)

#LambertW
if(!require(LambertW)) {install.packages("LambertW")
  library(LambertW)}
test_norm(NettoVinst)
lambertTransform = MLE_LambertW(NettoVinst, distname = "normal", type = "hh")
summary(lambertTransform)
xx <- get_input(lambertTransform)
summary(xx)
test_norm(xx)
hist(xx, breaks=500)
lambertTransform2 = MLE_LambertW(NettoVinst, distname = "normal", type = "h")
xx2 <- get_input(lambertTransform2)
test_norm(xx2)
hist(xx2, breaks=500)

gaussianTransform = Gaussianize(NettoVinst, type="hh")[,1]
summary(gaussianTransform)
test_norm(gaussianTransform)
hist(gaussianTransform, breaks=500)
gaussianTransform2 = Gaussianize(NettoVinst, type="h")[,1]
test_norm(gaussianTransform2)
hist(gaussianTransform2, breaks=500)



#Normal or not----
if(!require(moments)) {install.packages("moments")
  library(moments)}
jarque.test(NettoVinst)
jarque.test(netIncomeChange)
jarque.test(NettoVinstTransform)
jarque.test(rootTransform)
jarque.test(negLogTransform)
jarque.test(xx)
jarque.test(xx2)
jarque.test(gaussianTransform)
jarque.test(gaussianTransform2)
jarque.test(outlierRemove)
jarque.test(outlierChangeRemove)
jarque.test(robinTransform)

#Transformation residuals
hist(standardModel$residuals,breaks=50)
jarque.test(standardModel$residuals)
hist(logModel$residuals,breaks=50)
jarque.test(logModel$residuals)
hist(rootModel$residuals,breaks=50)
jarque.test(rootModel$residuals)
hist(negLogModel$residuals,breaks=50)
jarque.test(negLogModel$residuals)
robinResiduals = residuals(robinRootModel)
hist(robinResiduals,breaks=50)
jarque.test(robinResiduals)

#FoU logged residuals
hist(standardModelFoUlog$residuals,breaks=50)
jarque.test(standardModelFoUlog$residuals)
hist(negLogModelFoUlog$residuals,breaks=50)
jarque.test(negLogModelFoUlog$residuals)
hist(robinRootModelFoUlog$residuals,breaks=50)
jarque.test(robinRootModelFoUlog$residuals)


#Regression----
reg1 = lm(netMargin[,21] ~ log(RD[,19]) + log(RDIntensity[,19]) + log(Sales[,19:21]), na.action=na.omit)
reg2 = lm(netMargin[,16] ~ rowMeans(RD[,10:15]) + rowMeans(RDIntensity[,10:15]) + Sales[,15:16] + employees[,15:16], na.action=na.omit)
#we can take the logarithm if necessary


#Transformed models----
standardModel = lm(NettoVinst ~ FoU + FoULag1 + FoULag2 + Sektor + GeografisktOmråde, na.action=na.omit)
logModel = lm(NettoVinstTransform ~ FoU + FoULag1 + FoULag2 + Sektor + GeografisktOmråde, na.action=na.omit)
rootModel = lm(rootTransform ~ FoU + FoULag1 + FoULag2 + Sektor + GeografisktOmråde, na.action=na.omit)
negLogModel = lm(negLogTransform ~ FoU + FoULag1 + FoULag2 + Sektor + GeografisktOmråde, na.action=na.omit)
robinRootModel = lm(robinTransform ~ FoU + FoULag1 + FoULag2 + Sektor + GeografisktOmråde, na.action=na.omit)

#FoU logged
standardModelFoUlog = lm(NettoVinst ~ log(FoU) + log(FoULag1) + log(FoULag2) + Sektor + GeografisktOmråde, na.action=na.omit)
negLogModelFoUlog = lm(negLogTransform ~ log(FoU) + log(FoULag1) + log(FoULag2) + Sektor + GeografisktOmråde, na.action=na.omit)
robinRootModelFoUlog = lm(robinTransform ~ log(FoU) + log(FoULag1) + log(FoULag2) + Sektor + GeografisktOmråde, na.action=na.omit)


#Transform back
logCoef =logModel$coefficients
logBack = exp(logCoef) + mini*2
rootCoef = (rootModel$coefficients)^3
rootBack = (rootModel$coefficients)^3
negLogBack = mapply(function(x) sign(x)*exp(abs(x))-1, negLogModel$coefficients)
negLogBack = exp(negLogModel$coefficients)-1
robinRootBack = ((robinRootModel$coefficients+1)^36-1)
standard = standardModel$coefficients

allModels = list(standardModel,logModel,rootModel,negLogModel,robinRootModel)
allCoefs = data.frame(mapply(function(x) x["coefficients"], allModels))
formatC(as.matrix(allCoefs)[1:4,], format = "e", digits = 2)

allBack = cbind(standard,logBack,rootBack,negLogBack,robinRootBack)[1:4,]
formatC(allBack, format = "e", digits = 2)


#Report----
library(stargazer)
stargazer(standardModel,logModel,rootModel,negLogModel,robinRootModel,
  type = "text",
  title = "Modeller",
  style = "default",
  omit=c("Företag","Sektor","GeografisktOmråde"),
  no.space = T,
  omit.stat=c("f", "ser"),
  digits = 2
  #digits.extra = 0,
  #flip = T
)

stargazer(standardModel,standardModelFoUlog,negLogModel,negLogModelFoUlog,robinRootModel,robinRootModelFoUlog,
          type = "text",
          title = "Modeller",
          style = "default",
          omit=c("Företag","Sektor","GeografisktOmråde"),
          no.space = T,
          omit.stat=c("f", "ser"),
          digits = 2
          #digits.extra = 0,
          #flip = T
)


#Better models----

# formula = VinstMarginal ~ VinstMarginalFrånMedelvärdet + VinstMarginalLag + 
#   FoUInt + FoUIntLag1 + FoUIntLag2 + FoUIntLag3 + FoUIntLag4 + FoUIntLag5 +
#   AvskrivInt + CapexIntLag1 + CapexIntLag2 + CapexIntLag3 + CapexIntLag4 + CapexIntLag5 +
#   Skuldsättning #+ Företag


library(plm)
pdata = pdata.frame(allNoDupes, index = c("Företag","Period"))
firmRandom = plm(formula, effect="individual", model= "random", data=pdata)
firmFixed = plm(formula, effect="individual", model= "within", data=pdata)

#Slumpmässig eller fasteffekt
phtest(firmFixed,firmRandom)
summary(firmFixed) #bättre


#dynamic model
lagFormulaProfit = NettoVinst ~ NettoVinstFrånMedelvärdet + NettoVinstLag + 
  FoU + FoULag1 + FoULag2 + FoULag3 + FoULag4 + FoULag5 +
  Avskriv + CapexLag1 + CapexLag2 + CapexLag3 + CapexLag4 + CapexLag5 +
  Skuldsättning|.-NettoVinstLag+NettoVinstLag2
lagFormulaProfitNoIV = NettoVinst ~ NettoVinstFrånMedelvärdet + NettoVinstLag + 
  FoU + FoULag1 + FoULag2 + FoULag3 + FoULag4 + FoULag5 +
  Avskriv + CapexLag1 + CapexLag2 + CapexLag3 + CapexLag4 + CapexLag5 +
  Skuldsättning##|.-NettoVinstLag+NettoVinstLag2

lagFormulaMargin = VinstMarginal ~ VinstMarginalFrånMedelvärdet + VinstMarginalLag + 
  FoUInt + FoUIntLag1 + FoUIntLag2 + FoUIntLag3 + FoUIntLag4 + FoUIntLag5 +
  AvskrivInt + CapexIntLag1 + CapexIntLag2 + CapexIntLag3 + CapexIntLag4 + CapexIntLag5 +
  Skuldsättning| . -VinstMarginalLag+VinstMarginalLag2
lagFormulaMarginNoIV = VinstMarginal ~ VinstMarginalFrånMedelvärdet + VinstMarginalLag + 
  FoUInt + FoUIntLag1 + FoUIntLag2 + FoUIntLag3 + FoUIntLag4 + FoUIntLag5 +
  AvskrivInt + CapexIntLag1 + CapexIntLag2 + CapexIntLag3 + CapexIntLag4 + CapexIntLag5 +
  Skuldsättning##| . -VinstMarginalLag+VinstMarginalLag2

dynamicProfit = plm(lagFormulaProfit, effect="twoways", model= "within", data=pdata)
summary(dynamicProfit)
dynamicMargin = plm(lagFormulaMargin, effect="twoways", model= "within", data=pdata)
summary(dynamicMargin)

dynamicProfitPooled = plm(lagFormulaProfitNoIV, model= "pooling", data=pdata)
dynamicMarginPooled = plm(lagFormulaMarginNoIV, model= "pooling", data=pdata)
summary(dynamicMarginPooled)
dynamicMarginNoIV = plm(lagFormulaMarginNoIV, effect="twoways", model= "within", data=pdata)
dynamicProfitNoIV = plm(lagFormulaProfitNoIV, effect="twoways", model= "within", data=pdata)



#Joint model----

allFoUInt = FoUInt + FoUIntLag1 + FoUIntLag2 + FoUIntLag3 + FoUIntLag4 + FoUIntLag5
allFoU = FoU + FoULag1 + FoULag2 + FoULag3 + FoULag4 + FoULag5
  

jointFormulaProfit = NettoVinst ~ NettoVinstFrånMedelvärdet + NettoVinstLag + 
  allFoU +
  Avskriv + CapexLag1 + CapexLag2 + CapexLag3 + CapexLag4 + CapexLag5 +
  Skuldsättning|.-NettoVinstLag+NettoVinstLag2

jointFormulaMargin = VinstMarginal ~ VinstMarginalFrånMedelvärdet + VinstMarginalLag + 
  allFoUInt +
  AvskrivInt + CapexIntLag1 + CapexIntLag2 + CapexIntLag3 + CapexIntLag4 + CapexIntLag5 +
  Skuldsättning| . -VinstMarginalLag+VinstMarginalLag2

jointProfit = plm(jointFormulaProfit, effect="twoways", model= "within", data=pdata)
summary(jointProfit,vcov=vcovHC(jointProfit, method="arellano", type="HC3"))
jointMargin = plm(jointFormulaMargin, effect="twoways", model= "within", data=pdata)
summary(jointMargin,vcov=vcovHC(jointMargin, method="arellano", type="HC3"))


#Residual analysis----

residualer = c(residuals(dynamicMargin))
summary(residualer)
residualer2 = c(residuals(dynamicProfit))
summary(residualer2)
#Model residuals

#Normalitet
library(moments)
JB = jarque.test(residualer)
JB2 = jarque.test(residualer2)
hist(residualer,breaks=200,main="Histogram för residulerna", 
     ylab="Frekvens", xlab = "Residual")
qqPlot(residualer,id=F, xlab = "Normalfördelning", ylab = "Modellresidualer")

#trash
# changeNames = marg
# rownames(changeNames) = paste("F",rownames(changeNames),sep="")


library(lmtest)
#Autokorrelation
BG = bgtest(lagFormulaMarginNoIV,10,data=marg)
BG2 = bgtest(lagFormulaProfitNoIV,10,data=marg)

#Heteroskedasticitet
BP = bptest(lagFormulaMarginNoIV,data=marg)
residualsAndNetMargin = data.frame(residualer,dynamicMargin$model$VinstMarginal)
xy = residualsAndNetMargin[residualsAndNetMargin[,2]>-96,]
plot(xy$residualer,xy$dynamicMargin.model.VinstMarginal,
     xlab = "Residualer", ylab="VinstMarginal")
BP2 = bptest(lagFormulaProfitNoIV,data=marg)



library(DescTools)
outRemoved = Winsorize(marg,probs = c(0.01, 0.99), na.rm=T)
plot(VinstMarginal,FoUIntLag1)
hist(VinstMarginal, breaks=100)

heteroCheck = data.frame(VinstMarginal,FoUIntLag1)
#heteroCheck = allNoDupes[allNoDupes[,"VinstMarginal"]>,]
outRemoved = heteroCheck[heteroCheck < quantile(heteroCheck,0.95,na.rm=T) & heteroCheck > quantile(heteroCheck,0.05,na.rm=T),]
outRemoved = heteroCheck[abs(heteroCheck[,1])<200&abs(heteroCheck[,2])<200,]
plot(unlist(outRemoved["FoUIntLag1"]), unlist(outRemoved["VinstMarginal"]))


#Multikollinearitet
library(car)
vifMarg = data.frame(round(vif(dynamicMarginPooled),2))
vifProf = data.frame(round(vif(dynamicProfitPooled),2))
colnames(vifMarg)=c("VIF")
#Vif rule of thumb: if vif>10, we have multicollinearity
write.csv(vifMarg, "vif.csv")

#Multicollinearitet
corrNivå = round(cor(levels,use = "complete.obs"),2)
corrMarg = round(cor(marg[,-1],use = "complete.obs"),2)
corrMarg[upper.tri(corrMarg,diag=T)] = ""
write.csv(corrMarg, "corr.csv")

corrMargAll = round(cor(marg,use = "complete.obs"),2)
write.csv(corrMargAll, "corr2.csv")



#Diagnostics reporting ----

diagnostics = rbind(c(JB$p.value, JB$statistic),
                    c(BP$p.value, BP$statistic),
                    c(BG$p.value, BG$statistic))
diagnostics = round(diagnostics,3)
diagReport = data.frame(c("Jarque-Bera", "Breusch-Pagan", "Breusch-Godfrey"),
                        diagnostics)

diagnostics = list(c("Jarque-Bera", round(JB$p.value,3), round(JB2$p.value,3),JB$statistic),
                         c("Breusch-Pagan", round(BP$p.value,3),round(BP2$p.value,3), BP$statistic),
                         c("Breusch-Godfrey", round(BG$p.value,3),round(BG2$p.value,3), BG$statistic))

#Robust errors
library("sandwich")
cov1 = vcovHC(dynamicMargin, type = "HC3")
robustSE1 = sqrt(diag(cov1))
cov2 = vcovHC(dynamicProfit,method="arellano")
robustSE2 = sqrt(diag(cov2))


#Reporting----
library(stargazer)
report = stargazer(dynamicMargin,dynamicProfit,
  type = "text",
  out = "dynamiskModell3.html",
  title = "",
  style = "default",
  omit=c("Företag","Sektor"),
  digits = 2,
  #digits.extra = 0,
  add.lines = diagnostics,
  se = list(robustSE1,robustSE2), 
  no.space = T,
  report=("vc*sp"),
  notes.label ="",
  single.row = T
)
library(stargazer)
report2 = stargazer(jointMargin,jointProfit,
  type = "text",
  out = "dynamiskModell3.html",
  title = "",
  style = "default",
  omit=c("Företag","Sektor"),
  digits = 2,
  #digits.extra = 0,
  add.lines = diagnostics,
  se = list(robustSE1,robustSE2), 
  no.space = T,
  report=("vc*sp"),
  notes.label ="",
  single.row = T
)


abline(regFixed)
library(ggplot2)
ggplot(df, aes())




library(stargazer);
stargazer(firmFixedVinstMarginal,
          type="text", 
          dep.var.caption = "", 
          dep.var.labels = NULL, 
          intercept.bottom = FALSE,
          model.names=FALSE, 
          model.numbers=FALSE, 
          keep.stat = c("n","f","rsq","adj.rsq"), 
          notes.align = "l",
          notes.append = TRUE, 
          report = "vc*st",se=robustHCse);




companiesWithData = names(dynamicMargin$model$VinstMarginal)
companiesWithoutEnd = gsub("\\d+$", "", companiesWithData)
noMinus = gsub("-$", "", companiesWithoutEnd)
freqTable = data.frame(table(noMinus,dnn = c("Name")))
withCountryAndSector = merge(df,freqTable,by.x = "Company Name",by.y = "Name")

sectr = table(withCountryAndSector["Primary Sector"])
sectr = as.data.frame(sectr)
colnames(sectr) = c("Sektor","Företag")
sectr[,1] = c("Telekom", "Diskretionär konsumtion", "Dagvarukonsumtion","Energi","Finans","Hälsa","Industri","IT","Råvaror","Fastigheter","Tjänster")
write.csv(sectr, file = "sectors.csv")

countries = table(withCountryAndSector["Country/Region of Incorporation"])
countries=as.data.frame(countries)
colnames(countries) = c("Land","Företag")
countries[,1] = c("Japan", "Sydkorea", "Taiwan")
write.csv(countries, file = "countries.csv")

#number of time periods
sectorFreq = aggregate(withCountryAndSector$Freq, by=list(Category=withCountryAndSector$`Primary Sector`), FUN=sum)
countryFreq = aggregate(withCountryAndSector$Freq, by=list(Category=withCountryAndSector$`Country/Region of Incorporation`), FUN=sum)


cNs = table(withCountryAndSector[c("Primary Sector","Country/Region of Incorporation")])
colnames(cNs) = c("Japan", "Sydkorea", "Taiwan")
rownames(cNs) = c("Telekom", "Diskretionär konsumtion", "Dagvarukonsumtion","Energi","Finans","Hälsa","Industri","IT","Råvaror","Fastigheter","Tjänster")
write.csv(cNs, file = "cNs.csv")


library(moments)
skew2 = skewness(withCountryAndSector)
kurt2 = kurtosis(withCountryAndSector)
stargazer(withCountryAndSector,
          type = "text",
          out = "desc2.html",
          title = "Deskriptiv statistik",
          flip = T,
          digits = 2,
          digits.extra = 0,
          median=T,
          align	=T,
          add.lines = list(skew2,kurt2)
)

