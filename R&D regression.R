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
RD = as.matrix(df[filterRD])

#Revenue
filterRev = sapply(names(df), grepl, pattern="Total Revenue")
revenue = as.matrix(df[filterRev])
revenue[revenue < 0] = NA

#Net income
filterNI = sapply(names(df), grepl, pattern="Net Income")
netIncome = as.matrix(df[filterNI])

#Changes
firstDropped = netIncome[,1:(ncol(netIncome)-1)]
lastDropped = netIncome[,2:ncol(netIncome)]
netIncomeChange = mapply(function(x,y) y-x, firstDropped,lastDropped)
#Too long = something wrong

#Employees
#filterEmploy = sapply(names(df), grepl, pattern="Total Employees")
#employees = as.matrix(df[filterEmploy])

#Capex
filterCapex = sapply(names(df), grepl, pattern="Capital Expenditure")
capex = as.matrix(df[filterCapex])

#Depreciation
Depreciation
filterDepr = sapply(names(df), grepl, pattern="Depreciation")
Depreciation = as.matrix(df[filterDepr])

#Liabilties
filterLiab = sapply(names(df), grepl, pattern="Total Liabilities ")
liabilities = as.matrix(df[filterLiab])

#Equity
filterEquity = sapply(names(df), grepl, pattern="Market Cap")
equity = as.matrix(df[filterEquity])
equity = equity[,1:21]

#Leverage
leverage = liabilities/(liabilities+equity)
leverage[mapply(is.infinite, leverage)] = NA
leverage = as.matrix(leverage)

#Net margin
netMargin = netIncome/revenue
netMargin[mapply(is.infinite, netMargin)] = NA
netMargin = as.matrix(netMargin)

#RD Intensity 
RDIntensity = RD/revenue
RDIntensity[mapply(is.infinite, RDIntensity)] = NA
RDIntensity[RDIntensity<=0] = NA
RDIntensity = as.matrix(RDIntensity) #rowMeans(RDIntensity[,-1])

#Capex Intensity 
CapexIntensity = capex/revenue
CapexIntensity[mapply(is.infinite, CapexIntensity)] = NA
CapexIntensity[CapexIntensity<=0] = NA
CapexIntensity = as.matrix(CapexIntensity) #rowMeans(CapexIntensity[,-1])

#firm, industry and country fixed effects for panel data
#"factor" creates dummy variables. Automatically omits one dummy to avoid collinearity problem
sectorDummies = factor(as.matrix(df["Primary Sector"])) #12 sectors
geoDummies = factor(as.matrix(df["Geographic Region"])) #5 regions
firmDummies = factor(as.matrix(df["Company Name"]))

#Panel format----
startYear = 6
VinstMarginal = c(netMargin[,startYear:21])
NettoVinst = c(netIncome[,startYear:21])

FoU20 = c(RDIntensity[,startYear:21])
FoU19 = c(RDIntensity[,(startYear-1):20])
FoU18 = c(RDIntensity[,(startYear-2):19])
FoU17 = c(RDIntensity[,(startYear-3):18])
FoU16 = c(RDIntensity[,(startYear-4):17])
FoU15 = c(RDIntensity[,(startYear-5):16])
FoU = c(RDIntensity[,1:21])

Capex20 = c(CapexIntensity[,startYear:21])
Capex19 = c(CapexIntensity[,(startYear-1):20])
Capex18 = c(CapexIntensity[,(startYear-2):19])
Capex17 = c(CapexIntensity[,(startYear-3):18])
Capex16 = c(CapexIntensity[,(startYear-4):17])
Capex15 = c(CapexIntensity[,(startYear-5):16])
Capex = c(CapexIntensity[,1:21])
Avskriv = c(Depreciation[,1:21])

`D/A` = c(leverage[,startYear:21])
Företag = rep(firmDummies, 22-startYear)
# Sektor = rep(sectorDummies,startYear)
# GeografisktOmråde = rep(geoDummies,startYear)
# `Vinstmarginal 2010-2020` = c(VinstMarginal[,(21-startYear):21])
#levels(Sektor) = c("-", "Telekom", "Utökad konsumtion", "Dagligvarukonsumtion","Energi","Finans","Hälsa","Industri","IT","Råvaror","Fastigheter","Tjänster")
#https://sp500.se/sp500-och-de-olika-sektorerna/
# Försäljning20 = c(revenue[,startYear:21])
# Försäljning19 = c(revenue[,(startYear-1):20])
# Försäljning18 = c(revenue[,(startYear-2):19])
# Försäljning17 = c(revenue[,(startYear-3):18])
# Försäljning16 = c(revenue[,(startYear-4):17])


#all


# all = cbind(NettoVinst,FoU20,FoU19,FoU18,FoU17,FoU16,
#             Capex,`D/A`)

n = max(length(NettoVinst), 
         length(VinstMarginal),
         length(FoUAll),
         length(CapexAll),
         length(`D/A`))

length(NettoVinst) = n
length(VinstMarginal) = n
length(FoUAll) = n
length(CapexAll) = n
length(`D/A`) = n
allNoDupes = cbind(NettoVinst,VinstMarginal,FoU,
            Capex,`D/A`)

#Tests on data----

#Descriptives
summary(NettoVinst)
summary(VinstMarginal)
summary(FoUAll)
summary(CapexAll)
summary(`D/A`)
a = summary(allNoDupes)
View(a)


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



#Unit root (modify for panel data)
library(fUnitRoots)
adfTest(Nettovinst, lags = 252, type = c("nc"), title = NULL, description = NULL)

library(urca)
panelRoots <- ca.jo(all, type = "trace", ecdet = "const", K = 3)
summary(panelRoots)

#Test for cointegration




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
hist(FoU20)
hist(NettoVinst)
plot(FoU)
plot(FoU20)
plot(NettoVinst)
plot(VinstMarginal)
quantile(round(NettoVinst,3),na.rm=T,prob = seq(0, 1, length = 101))
qqnorm(FoU20)
qqnorm(NettoVinst)

#RD logarithm
hist(FoU20)
test = as.vector(na.omit(FoU20))
jarque.test(test)
logTest = log(FoU20)
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

# library(plm)
# pdata = pdata.frame(df, index = c("Company Name","t"))
# regFixed = plm(netIncome[,11:21] ~ RDIntensity[,10:20] + RDIntensity[,9:19] + RDIntensity[,8:18] + RDIntensity[,11:21] + revenue[,10:20] + revenue[,11:21] +  sectorDummies + geoDummies, na.action=na.omit)

#Regression models----
standardModel = lm(NettoVinst ~ FoU20 + FoU19 + FoU18 + Sektor + GeografisktOmråde, na.action=na.omit)
logModel = lm(NettoVinstTransform ~ FoU20 + FoU19 + FoU18 + Sektor + GeografisktOmråde, na.action=na.omit)
rootModel = lm(rootTransform ~ FoU20 + FoU19 + FoU18 + Sektor + GeografisktOmråde, na.action=na.omit)
negLogModel = lm(negLogTransform ~ FoU20 + FoU19 + FoU18 + Sektor + GeografisktOmråde, na.action=na.omit)
robinRootModel = lm(robinTransform ~ FoU20 + FoU19 + FoU18 + Sektor + GeografisktOmråde, na.action=na.omit)

#FoU logged
standardModelFoUlog = lm(NettoVinst ~ log(FoU20) + log(FoU19) + log(FoU18) + Sektor + GeografisktOmråde, na.action=na.omit)
negLogModelFoUlog = lm(negLogTransform ~ log(FoU20) + log(FoU19) + log(FoU18) + Sektor + GeografisktOmråde, na.action=na.omit)
robinRootModelFoUlog = lm(robinTransform ~ log(FoU20) + log(FoU19) + log(FoU18) + Sektor + GeografisktOmråde, na.action=na.omit)


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

#Simple slopes
simpleFirmFixed = lm(NettoVinstTransform ~ FoU20 + FoU19 + FoU18 + FoU17 + FoU16 + Företag, na.action=na.omit)
summary(simpleFirmFixed)

#slope effects
regSlopes = lm(NettoVinst ~ FoU20 + FoU19 + FoU18 + FoU17 + FoU16 + Sektor + Sektor*FoU19, na.action=na.omit)
summary(regSlopes)


#Firm-fixed effects (borde kanske byta ut Capex 20 mot avskrivningar)
firmFixedVinstMarginal = lm(VinstMarginal ~ FoU20 + FoU19 + FoU18 + FoU17 + FoU16 + 
                        Capex20 + Capex19 + Capex18 + Capex17 + Capex16 +
                        `D/A` + Företag, na.action=na.omit)



library(SparseM)
library(MatrixModels)
library(Matrix)
faster <- glm4(VinstMarginal ~ FoU20 + FoU19 + FoU18 + FoU17 + FoU16 + 
                        Capex20 + Capex19 + Capex18 + Capex17 + Capex16 +
                        `D/A` + Företag, 
                      na.action=na.omit,sparse=TRUE)


firmFixedResiduals = residuals(firmFixedVinstMarginal)
summary(fixedEffectResiduals)


#Residual analysis----
#Model residuals

#Normalitet
library(moments)
JB = jarque.test(fixedEffectResiduals)
hist(fixedEffectResiduals,breaks=100)

#Autokorrelation och heteroskedasticitet
library(lmtest)
BG = bgtest(firmFixedVinstMarginal,10)
BP = bptest(firmFixedVinstMarginal)

#Multikollinearitet
library(car)
vif(firmFixedVinstMarginal)
#Vif rule of thumb: if vif>10, we have multicollinearity

#Slumpmässig eller fasteffekt
#phtest(random,fixed)



#Reporting----
library(stargazer)
stargazer(
  firmFixedVinstMarginal,
  type = "text",
  #out = "model.html",
  title = "Modell",
  style = "default",
  omit=c("Företag","Sektor"),
  add.lines(JB,BG,BP)
  #flip = T,
  #digits = 2,
  #digits.extra = 0,
)


abline(regFixed)
library(ggplot2)
ggplot(df, aes())


#robust standard errors
library(lmtest); library(sandwich);
res.robustHC  = coeftest(reg1, vcov=vcovHC(reg1, type=c("HC0")));   # HC0: Claasic White (1980), available: HC0, HC1, HC2, HC3, HC4, HC4m, HC5, const (OLS)
robustHCse    = sqrt(diag(vcovHC(reg1, type=c("HC0"))));


library(stargazer);
stargazer(reg1,
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





indus = table(df[,3])
countries = table(df[,5])
table(df[,5])


#write.table(indus, file = "industries.csv")





# m=3
# for(n in 2012:2020){
#   names(df)[m] = paste("RD", n, sep="")
#   names(df)[m+9] = paste("Revenue", n, sep="")
#   names(df)[m+9*2] = paste("return", n, sep="")
#   m=m+1
# }
# names(df)[30] = paste("return", 2021, sep="")
# rm(m,n)





