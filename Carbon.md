Drivers of carbon recovery in a secondary forested landscape of West Africa
================
Anny E. N'Guessan, Justin Kassi N'Dja, Roselyne G. Z. Gouli, Camille Piponiot, Olivier N. Yao , Irie C Zo-Bi & Bruno Hérault
4/17/2018

-   [Introduction](#introduction)
    -   [Current state and drivers of deforestation/degradation in West Africa](#current-state-and-drivers-of-deforestationdegradation-in-west-africa)
    -   [What do we know about the carbon recovery rates in secondary forests ?](#what-do-we-know-about-the-carbon-recovery-rates-in-secondary-forests)
    -   [What are the possible drivers of these rates?](#what-are-the-possible-drivers-of-these-rates)
    -   [Research Questions](#research-questions)
-   [Materials & Methods](#materials-methods)
    -   [Site Description](#site-description)
    -   [Data Computation](#data-computation)
    -   [Statistical Analyses](#statistical-analyses)
-   [Results](#results)
    -   [Comparing secondary forests to logged and natural forests](#comparing-secondary-forests-to-logged-and-natural-forests-1)
    -   [Forest recovery rates](#forest-recovery-rates)
    -   [Environmental control on recovery rates](#environmental-control-on-recovery-rates)
-   [Tuning for the paper](#tuning-for-the-paper)

Introduction
------------

### Current state and drivers of deforestation/degradation in West Africa

### What do we know about the carbon recovery rates in secondary forests ?

### What are the possible drivers of these rates?

### Research Questions

What are the current levels of carbon stocks in young secondary forests relatively to undisturbed (logged or not) natural forests? At which rates the carbon stocks are reconstituting? What are the main drivers of these rates?

Materials & Methods
-------------------

``` r
library(BIOMASS)
data1<-read.csv2("arbrestotals.csv")
```

``` r
######## Taxonomic correction
# Taxo<-correctTaxo(genus = data1$genus, species = data1$species)
# data1$genusCorr <- Taxo$genusCorrected
# table(data1$genus==data1$genusCorr)
# data1$speciesCorr <- Taxo$speciesCorrected
# table(data1$species==data1$speciesCorr)
# # correction des familles avec APG III
# APG <- getTaxonomy(data1$genusCorr, findOrder = T)
# data1$familyAPG <- APG$family
# data1$orderAPG <- APG$order
# save(data1, file="data1.Rdata")
load(file="data1.Rdata")
######### Wood density
dataWD<-getWoodDensity(genus=data1$genusCorr,
                       species=data1$speciesCorr,stand= data1$plot) 
```

    ## The reference dataset contains 16467 wood density values 
    ## Your taxonomic table contains 336 taxa

``` r
data1$WD = dataWD$meanWD
data1$WDsd=dataWD$sdWD
########## Diameter
data1$circ<-as.numeric(as.character(data1$circ))
data1$H<-as.numeric(as.character(data1$H))
data1$D<-data1$circ/pi
########## Biomass computation per individual
AGBtree<-computeAGB(D=data1$D, WD=data1$WD, H=data1$H)
data1$AGB=AGBtree
########## Biomass computation per plot
AGBPlot1<-tapply(data1$AGB,data1$plot,sum)
AGBPlot1ha=AGBPlot1/0.2
#head(AGBPlot1ha)
########## Stand Density per plot
nplot<-table(data1$plot) 
dplotha<-nplot/0.2
#head(dplotha)
########## Basal Area per plot
data1$S<-(data1$D/100)^2*pi/4
SPlot<-tapply(data1$S,data1$plot,sum)
SPlot<-SPlot/0.2
#head(SPlot)
########## Quadratic Diameter per plot
DG<-sqrt(tapply((data1$D)^2,data1$plot,mean))
#head(DG)
########## Biomass Computation without crop trees
data2=data1[data1$rem==0,]
AGBPlot2<-tapply(data2$AGB,data2$plot,sum)
AGBPlot2ha=AGBPlot2/0.2
#head(AGBPlot2ha)
########## creating a new dataframe per plot
data3<-read.csv2("variables.csv", dec=".")
data3$AGB.Kg.ha._avec.rem<-NULL
data3$AGB.Kg.ha._sans.rem<-NULL
data3$densit.<-NULL
data3$surface.terri.re<-NULL
data3$AGB<-as.numeric(as.character(AGBPlot2ha))
data3$N<-as.numeric(as.character(dplotha))
data3$BA<-as.numeric(as.character(SPlot))
data3$DG<-as.numeric(as.character(DG))
```

### Site Description

### Data Computation

### Statistical Analyses

#### Comparing secondary forests to logged and natural forests

In order to campare the forest structure betwenn secondary, logged and natural forests in our ladnscape, we first used the Kruskall-Wallis non-parametrical tests.

Results
-------

### Comparing secondary forests to logged and natural forests

``` r
data3$categorie<-as.character(data3$categorie)
data3[data3$categorie=="J 1-10 ans",]$categorie<-"a. Secondary < 11yr"
data3[data3$categorie=="J 11-20 ans",]$categorie<-"b. Secondary 11-20 yr"
data3[data3$categorie=="J 21-30 ans",]$categorie<-"c. Secondary 21-30 yr"
data3[data3$categorie=="J 31-40 ans",]$categorie<-"d. Secondary > 30yr"
data3[data3$categorie=="foretanci ",]$categorie<-"e. Old Growth"
data3[data3$categorie=="foretexpl ",]$categorie<-"e. Logged"
#### dropping logged forests
data3<-data3[!data3$categorie=="e. Logged",]
data3$categorie<-as.factor(data3$categorie)
# kruskal.test(data3$AGB,data3$categorie)
# kruskal.test(data3$BA,data3$categorie)
# kruskal.test(data3$DG,data3$categorie)
# kruskal.test(data3$N,data3$categorie)
```

``` r
library(ggplot2)
data_str<-data.frame(var=c(rep("AboveGround Biomass",dim(data3)[1]),rep("Tree Density",dim(data3)[1]),rep("Quadratic Diameter",dim(data3)[1]), rep("Basal area",dim(data3)[1])),value=c(data3$AGB,data3$N,data3$DG,data3$BA),Forest_Age=rep(data3$categorie,4))
data_str<-na.omit(data_str)
var<-levels(data_str$var)
data_str$var<-as.character(data_str$var)
for (i in var){
data_str[data_str$var== i,]$var<-paste(i," (P<", 0.001, ")", sep="")}
p <- ggplot(data = data_str, aes(x=var, y=value)) + 
  geom_boxplot(aes(fill=Forest_Age)) +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank()) #+
  #coord_flip()
p + facet_wrap( ~ var, scales="free") + scale_fill_brewer("Forest_Age", palette="YlOrRd")
```

![](Carbon_files/figure-markdown_github/Fig1-1.png)

### Forest recovery rates

``` r
agbmax<-quantile(data3[data3$categorie %in% c("e. Old Growth"),]$AGB, 0.5)
data4<-data3[!data3$categorie %in% c("e. Old Growth"),]
data4$Precedentcultural<-as.character(data4$Precedentcultural)
data4[data4$Precedentcultural=="cacao",]$Precedentcultural<-"3-Cocoa"
data4[data4$Precedentcultural=="igname",]$Precedentcultural<-"5-Yam"
data4[data4$Precedentcultural=="manioc",]$Precedentcultural<-"4-Cassava"
data4[data4$Precedentcultural=="ma\357s",]$Precedentcultural<-"2-Maize"
data4[data4$Precedentcultural=="riz",]$Precedentcultural<-"1-Rice"
data5<-data3[data3$categorie %in% c("e. Old Growth"),]
data5$age<-200
data4$lambda<- -log(1-(data4$AGB/agbmax))/((data4$age/10)^2)
q<-quantile(data4$lambda, probs=c(0.05, 0.5, 0.95))
```

``` r
library(rstan)
```

    ## Loading required package: StanHeaders

    ## rstan (Version 2.17.3, GitRev: 2e1f913d3ca3)

    ## For execution on a local, multicore CPU with excess RAM we recommend calling
    ## options(mc.cores = parallel::detectCores()).
    ## To avoid recompilation of unchanged Stan programs, we recommend calling
    ## rstan_options(auto_write = TRUE)

``` r
# rate <- stan(file="rate.stan", 
#                      data=list(N=length(data4$N), AGB=data4$AGB, t=data4$age), 
#                      pars=c("alpha", "beta", "lambda", "k", "sigma"), 
#                      chains=1, 
#                      iter=2500, 
#                      warmup=1000)
# save(rate, file="rate.Rdata")
load(file="rate.Rdata")
traceplot(rate, pars=c("alpha", "beta", "k", "lambda", "sigma"), nrow=2)
```

![](Carbon_files/figure-markdown_github/stan-1.png)

``` r
plot(rate)
```

    ## ci_level: 0.8 (80% intervals)

    ## outer_level: 0.95 (95% intervals)

![](Carbon_files/figure-markdown_github/stan-2.png)

``` r
par<-extract(rate)
age<-1:200
med<-numeric()
q05<-numeric()
q95<-numeric()
for (i in age){
med<-c(med,median(
  par$alpha*(1-exp(-par$lambda*((i/par$k)^par$beta)))
  ))
q05<-c(q05,quantile(
  par$alpha*(1-exp(-par$lambda*((i/par$k)^par$beta)))
  , probs=0.001))
q95<-c(q95,quantile(
 par$alpha*(1-exp(-par$lambda*((i/par$k)^par$beta)))
  , probs=0.999))
}

data4$lambda<--log(1-(data4$AGB/par$alpha[which(par$lp__ ==max(par$lp__))]))/((data4$age/par$k[which(par$lp__ == max(par$lp__))])^par$beta[which(par$lp__ == max(par$lp__))])
```

``` r
library(ggplot2)
newdata <- data.frame(age=age, med=med, q05=q05,
                    q95= q95)
ggplot() +
  geom_ribbon(data=newdata, aes(x=age, ymin = q05, ymax = q95), alpha = .25)+
  geom_point(data=data4, aes(x = age, y=AGB))+
  geom_line(data=newdata, aes(x=age, y=med))+
  geom_boxplot(data=data5, aes(x=age, y=AGB), width=0.1)+  
  scale_y_sqrt()+
  scale_x_log10()+
  xlab("Age of Secondary Forest (yr)") + ylab("Aboveground Biomass (T/ha)") + theme(legend.position=" none")
```

![](Carbon_files/figure-markdown_github/fig2-1.png)

### Environmental control on recovery rates

``` r
str(data4)
```

    ## 'data.frame':    89 obs. of  17 variables:
    ##  $ plot                : int  1 2 4 6 7 8 10 12 17 18 ...
    ##  $ categorie           : Factor w/ 5 levels "a. Secondary < 11yr",..: 2 1 3 3 1 4 2 4 2 4 ...
    ##  $ age                 : int  11 3 26 21 5 32 17 34 20 35 ...
    ##  $ nbr_remanent        : int  5 7 16 1 7 2 1 2 6 0 ...
    ##  $ shannon             : num  2.42 1.84 3.16 3.52 1.88 ...
    ##  $ Proximiteforesti.re.: int  300 500 150 10 500 10 50 10 210 10 ...
    ##  $ Densiteforesti.re.  : int  3 0 1 5 1 5 4 7 4 4 ...
    ##  $ Altitude            : int  135 96 106 118 124 120 122 99 129 119 ...
    ##  $ Annedeculture       : int  2 2 3 10 3 3 3 6 10 10 ...
    ##  $ Typedesol           : Factor w/ 2 levels "solferralitique",..: 1 2 2 1 1 1 1 1 1 1 ...
    ##  $ Topographie         : Factor w/ 7 levels "bas fond","bas pente",..: 7 1 6 5 5 5 7 5 5 5 ...
    ##  $ Precedentcultural   : chr  "4-Cassava" "1-Rice" "3-Cocoa" "3-Cocoa" ...
    ##  $ AGB                 : num  12.729 0.753 146.648 52.435 3.846 ...
    ##  $ N                   : num  1285 285 1765 1790 1485 ...
    ##  $ BA                  : num  7.71 2.12 48.26 17.61 4.35 ...
    ##  $ DG                  : num  8.74 9.73 18.66 11.19 6.11 ...
    ##  $ lambda              : num  0.0632 0.0343 0.2091 0.0909 0.0731 ...

``` r
summary(lm(lambda~log(nbr_remanent+1), data=data4, weights=data4$age))
```

    ## 
    ## Call:
    ## lm(formula = lambda ~ log(nbr_remanent + 1), data = data4, weights = data4$age)
    ## 
    ## Weighted Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.3453 -0.1618 -0.0411  0.1080  0.7816 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)           0.086094   0.006972  12.349  < 2e-16 ***
    ## log(nbr_remanent + 1) 0.020080   0.005409   3.712 0.000362 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1984 on 87 degrees of freedom
    ## Multiple R-squared:  0.1367, Adjusted R-squared:  0.1268 
    ## F-statistic: 13.78 on 1 and 87 DF,  p-value: 0.0003617

``` r
#summary(lm(lambda~Proximiteforesti.re., data=data4, weights=data4$age))
summary(lm(lambda~Precedentcultural, data=data4, weights=data4$age))
```

    ## 
    ## Call:
    ## lm(formula = lambda ~ Precedentcultural, data = data4, weights = data4$age)
    ## 
    ## Weighted Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.34518 -0.12246 -0.04562  0.09137  0.74705 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                 0.03817    0.02868   1.331   0.1869  
    ## Precedentcultural2-Maize    0.09331    0.04076   2.290   0.0246 *
    ## Precedentcultural3-Cocoa    0.06619    0.02926   2.262   0.0263 *
    ## Precedentcultural4-Cassava  0.06922    0.03615   1.915   0.0589 .
    ## Precedentcultural5-Yam      0.09426    0.04314   2.185   0.0317 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2088 on 84 degrees of freedom
    ## Multiple R-squared:  0.07667,    Adjusted R-squared:  0.0327 
    ## F-statistic: 1.744 on 4 and 84 DF,  p-value: 0.148

``` r
#summary(lm(lambda~log(Densiteforesti.re.+1), data=data4, weights=data4$age))
#summary(lm(lambda~Typedesol, data=data4, weights=data4$age))
#summary(lm(lambda~Altitude, data=data4, weights=data4$age))
#summary(lm(lambda~log(Annedeculture+1), data=data4, weights=data4$age))
model<-lm(lambda~log(nbr_remanent+1)+Precedentcultural, data=data4)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = lambda ~ log(nbr_remanent + 1) + Precedentcultural, 
    ##     data = data4)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.09105 -0.03492 -0.00680  0.03175  0.30592 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)                 0.01671    0.02509   0.666  0.50747   
    ## log(nbr_remanent + 1)       0.02107    0.00699   3.014  0.00342 **
    ## Precedentcultural2-Maize    0.05731    0.03090   1.855  0.06718 . 
    ## Precedentcultural3-Cocoa    0.06640    0.02544   2.610  0.01074 * 
    ## Precedentcultural4-Cassava  0.08175    0.03113   2.626  0.01029 * 
    ## Precedentcultural5-Yam      0.07903    0.03740   2.113  0.03760 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05922 on 83 degrees of freedom
    ## Multiple R-squared:  0.2185, Adjusted R-squared:  0.1714 
    ## F-statistic: 4.641 on 5 and 83 DF,  p-value: 0.0008752

``` r
anova(model)
```

    ## Analysis of Variance Table
    ## 
    ## Response: lambda
    ##                       Df   Sum Sq  Mean Sq F value    Pr(>F)    
    ## log(nbr_remanent + 1)  1 0.052092 0.052092 14.8515 0.0002284 ***
    ## Precedentcultural      4 0.029302 0.007325  2.0885 0.0896098 .  
    ## Residuals             83 0.291125 0.003508                      
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
remnant<-data.frame(Age=rep(seq(1,200,1),6))
remnant$remnants<-as.factor(c(rep("000",200),rep("001",200),rep("005",200), rep("020",200), rep("050",200), rep("100",200)))
remnant$AGB<- -2
remnant$AGBmin<- -2
remnant$AGBmax<- -2
remnant$dAGB<- -2
remnant$dAGBmin<- -2
remnant$dAGBmax<- -2

alpha<-par$alpha[which(par$lp__ == max(par$lp__))]
beta<-par$beta[which(par$lp__ == max(par$lp__))]
k<-par$k[which(par$lp__ == max(par$lp__))]
lambda<-mean(model$coefficients[1]+model$coefficients[c(1,3:6)])
mu_rem<-model$coefficients[2]
sd_rem<-summary(model)[["coefficients"]][2,2]
remnant[remnant$remnants=="000",]$AGB<-alpha*(1-exp(-(lambda+log(0+1)*(mu_rem))*((remnant[remnant$remnants=="000",]$Age/k)^beta)))
remnant[remnant$remnants=="000",]$AGBmin<-alpha*(1-exp(-(lambda+log(0+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="000",]$Age/k)^beta)))
remnant[remnant$remnants=="000",]$AGBmax<-alpha*(1-exp(-(lambda+log(0+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="000",]$Age/k)^beta)))
remnant[remnant$remnants=="001",]$AGB<-alpha*(1-exp(-(lambda+log(0.2+1)*(mu_rem))*((remnant[remnant$remnants=="001",]$Age/k)^beta)))
remnant[remnant$remnants=="001",]$AGBmin<-alpha*(1-exp(-(lambda+log(0.2+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="001",]$Age/k)^beta)))
remnant[remnant$remnants=="001",]$AGBmax<-alpha*(1-exp(-(lambda+log(0.2+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="001",]$Age/k)^beta)))
remnant[remnant$remnants=="005",]$AGB<-alpha*(1-exp(-(lambda+log(1+1)*(mu_rem))*((remnant[remnant$remnants=="005",]$Age/k)^beta)))
remnant[remnant$remnants=="005",]$AGBmin<-alpha*(1-exp(-(lambda+log(1+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="005",]$Age/k)^beta)))
remnant[remnant$remnants=="005",]$AGBmax<-alpha*(1-exp(-(lambda+log(1+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="005",]$Age/k)^beta)))
remnant[remnant$remnants=="020",]$AGB<-alpha*(1-exp(-(lambda+log(4+1)*(mu_rem))*((remnant[remnant$remnants=="020",]$Age/k)^beta)))
remnant[remnant$remnants=="020",]$AGBmin<-alpha*(1-exp(-(lambda+log(4+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="020",]$Age/k)^beta)))
remnant[remnant$remnants=="020",]$AGBmax<-alpha*(1-exp(-(lambda+log(4+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="020",]$Age/k)^beta)))
remnant[remnant$remnants=="050",]$AGB<-alpha*(1-exp(-(lambda+log(10+1)*(mu_rem))*((remnant[remnant$remnants=="050",]$Age/k)^beta)))
remnant[remnant$remnants=="050",]$AGBmin<-alpha*(1-exp(-(lambda+log(10+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="050",]$Age/k)^beta)))
remnant[remnant$remnants=="050",]$AGBmax<-alpha*(1-exp(-(lambda+log(10+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="050",]$Age/k)^beta)))
remnant[remnant$remnants=="100",]$AGB<-alpha*(1-exp(-(lambda+log(25+1)*(mu_rem))*((remnant[remnant$remnants=="100",]$Age/k)^beta)))
remnant[remnant$remnants=="100",]$AGBmin<-alpha*(1-exp(-(lambda+log(25+1)*(mu_rem-2*sd_rem))*((remnant[remnant$remnants=="100",]$Age/k)^beta)))
remnant[remnant$remnants=="100",]$AGBmax<-alpha*(1-exp(-(lambda+log(25+1)*(mu_rem+2*sd_rem))*((remnant[remnant$remnants=="100",]$Age/k)^beta)))

remnant[remnant$remnants=="000",]$dAGB<-(c(remnant[remnant$remnants=="000",]$AGB,0)-c(0, remnant[remnant$remnants=="000",]$AGB))[1:200]
remnant[remnant$remnants=="001",]$dAGB<-(c(remnant[remnant$remnants=="001",]$AGB,0)-c(0, remnant[remnant$remnants=="001",]$AGB))[1:200]
remnant[remnant$remnants=="005",]$dAGB<-(c(remnant[remnant$remnants=="005",]$AGB,0)-c(0, remnant[remnant$remnants=="005",]$AGB))[1:200]
remnant[remnant$remnants=="020",]$dAGB<-(c(remnant[remnant$remnants=="020",]$AGB,0)-c(0, remnant[remnant$remnants=="020",]$AGB))[1:200]
remnant[remnant$remnants=="050",]$dAGB<-(c(remnant[remnant$remnants=="050",]$AGB,0)-c(0, remnant[remnant$remnants=="050",]$AGB))[1:200]
remnant[remnant$remnants=="100",]$dAGB<-(c(remnant[remnant$remnants=="100",]$AGB,0)-c(0, remnant[remnant$remnants=="100",]$AGB))[1:200]
remnant[remnant$remnants=="000",]$dAGBmin<-(c(remnant[remnant$remnants=="000",]$AGBmin,0)-c(0, remnant[remnant$remnants=="000",]$AGBmin))[1:200]
remnant[remnant$remnants=="001",]$dAGBmin<-(c(remnant[remnant$remnants=="001",]$AGBmin,0)-c(0, remnant[remnant$remnants=="001",]$AGBmin))[1:200]
remnant[remnant$remnants=="005",]$dAGBmin<-(c(remnant[remnant$remnants=="005",]$AGBmin,0)-c(0, remnant[remnant$remnants=="005",]$AGBmin))[1:200]
remnant[remnant$remnants=="020",]$dAGBmin<-(c(remnant[remnant$remnants=="020",]$AGBmin,0)-c(0, remnant[remnant$remnants=="020",]$AGBmin))[1:200]
remnant[remnant$remnants=="050",]$dAGBmin<-(c(remnant[remnant$remnants=="050",]$AGBmin,0)-c(0, remnant[remnant$remnants=="050",]$AGBmin))[1:200]
remnant[remnant$remnants=="100",]$dAGBmin<-(c(remnant[remnant$remnants=="100",]$AGBmin,0)-c(0, remnant[remnant$remnants=="100",]$AGBmin))[1:200]
remnant[remnant$remnants=="000",]$dAGBmax<-(c(remnant[remnant$remnants=="000",]$AGBmax,0)-c(0, remnant[remnant$remnants=="000",]$AGBmax))[1:200]
remnant[remnant$remnants=="001",]$dAGBmax<-(c(remnant[remnant$remnants=="001",]$AGBmax,0)-c(0, remnant[remnant$remnants=="001",]$AGBmax))[1:200]
remnant[remnant$remnants=="005",]$dAGBmax<-(c(remnant[remnant$remnants=="005",]$AGBmax,0)-c(0, remnant[remnant$remnants=="005",]$AGBmax))[1:200]
remnant[remnant$remnants=="020",]$dAGBmax<-(c(remnant[remnant$remnants=="020",]$AGBmax,0)-c(0, remnant[remnant$remnants=="020",]$AGBmax))[1:200]
remnant[remnant$remnants=="050",]$dAGBmax<-(c(remnant[remnant$remnants=="050",]$AGBmax,0)-c(0, remnant[remnant$remnants=="050",]$AGBmax))[1:200]
remnant[remnant$remnants=="100",]$dAGBmax<-(c(remnant[remnant$remnants=="100",]$AGBmax,0)-c(0, remnant[remnant$remnants=="100",]$AGBmax))[1:200]

crop<-data.frame(Age=rep(seq(1,200,1),5))
crop$crops<-as.factor(c(rep("1-Rice",200),rep("2-Maize",200),rep("3-Cocoa",200), rep("4-Cassava",200), rep("5-Yam",200)))
crop$AGB<- -2
crop$AGBmin<- -2
crop$AGBmax<- -2
crop$dAGB<- -2
crop$dAGBmin<- -2
crop$dAGBmax<- -2

lambda<-model$coefficients[2]*log(mean(data4$nbr_remanent)+1)
mu_crop<-c(model$coefficients[1], model$coefficients[1]+model$coefficients[3:6])
sd_crop<-summary(model)[["coefficients"]][c(1, 3:6),2]
crop[crop$crops=="1-Rice",]$AGB<-alpha*(1-exp(-(lambda+mu_crop[1])*((crop[crop$crops=="1-Rice",]$Age/k)^beta)))
crop[crop$crops=="1-Rice",]$AGBmin<-alpha*(1-exp(-(lambda+mu_crop[1]-2*sd_crop[1])*((crop[crop$crops=="1-Rice",]$Age/k)^beta)))
crop[crop$crops=="1-Rice",]$AGBmax<-alpha*(1-exp(-(lambda+mu_crop[1]+2*sd_crop[1])*((crop[crop$crops=="1-Rice",]$Age/k)^beta)))
crop[crop$crops=="2-Maize",]$AGB<-alpha*(1-exp(-(lambda+mu_crop[2])*((crop[crop$crops=="2-Maize",]$Age/k)^beta)))
crop[crop$crops=="2-Maize",]$AGBmin<-alpha*(1-exp(-(lambda+mu_crop[2]-2*sd_crop[2])*((crop[crop$crops=="2-Maize",]$Age/k)^beta)))
crop[crop$crops=="2-Maize",]$AGBmax<-alpha*(1-exp(-(lambda+mu_crop[2]+2*sd_crop[2])*((crop[crop$crops=="2-Maize",]$Age/k)^beta)))
crop[crop$crops=="3-Cocoa",]$AGB<-alpha*(1-exp(-(lambda+mu_crop[3])*((crop[crop$crops=="3-Cocoa",]$Age/k)^beta)))
crop[crop$crops=="3-Cocoa",]$AGBmin<-alpha*(1-exp(-(lambda+mu_crop[3]-2*sd_crop[3])*((crop[crop$crops=="3-Cocoa",]$Age/k)^beta)))
crop[crop$crops=="3-Cocoa",]$AGBmax<-alpha*(1-exp(-(lambda+mu_crop[3]+2*sd_crop[3])*((crop[crop$crops=="3-Cocoa",]$Age/k)^beta)))
crop[crop$crops=="4-Cassava",]$AGB<-alpha*(1-exp(-(lambda+mu_crop[4])*((crop[crop$crops=="4-Cassava",]$Age/k)^beta)))
crop[crop$crops=="4-Cassava",]$AGBmin<-alpha*(1-exp(-(lambda+mu_crop[4]-2*sd_crop[4])*((crop[crop$crops=="4-Cassava",]$Age/k)^beta)))
crop[crop$crops=="4-Cassava",]$AGBmax<-alpha*(1-exp(-(lambda+mu_crop[4]+2*sd_crop[4])*((crop[crop$crops=="4-Cassava",]$Age/k)^beta)))
crop[crop$crops=="5-Yam",]$AGB<-alpha*(1-exp(-(lambda+mu_crop[5])*((crop[crop$crops=="5-Yam",]$Age/k)^beta)))
crop[crop$crops=="5-Yam",]$AGBmin<-alpha*(1-exp(-(lambda+mu_crop[5]-2*sd_crop[5])*((crop[crop$crops=="5-Yam",]$Age/k)^beta)))
crop[crop$crops=="5-Yam",]$AGBmax<-alpha*(1-exp(-(lambda+mu_crop[5]+2*sd_crop[5])*((crop[crop$crops=="5-Yam",]$Age/k)^beta)))

crop[crop$crops=="1-Rice",]$dAGB<-(c(crop[crop$crops=="1-Rice",]$AGB,0)-c(0, crop[crop$crops=="1-Rice",]$AGB))[1:200]
crop[crop$crops=="2-Maize",]$dAGB<-(c(crop[crop$crops=="2-Maize",]$AGB,0)-c(0, crop[crop$crops=="2-Maize",]$AGB))[1:200]
crop[crop$crops=="3-Cocoa",]$dAGB<-(c(crop[crop$crops=="3-Cocoa",]$AGB,0)-c(0, crop[crop$crops=="3-Cocoa",]$AGB))[1:200]
crop[crop$crops=="4-Cassava",]$dAGB<-(c(crop[crop$crops=="4-Cassava",]$AGB,0)-c(0, crop[crop$crops=="4-Cassava",]$AGB))[1:200]
crop[crop$crops=="5-Yam",]$dAGB<-(c(crop[crop$crops=="5-Yam",]$AGB,0)-c(0, crop[crop$crops=="5-Yam",]$AGB))[1:200]
crop[crop$crops=="1-Rice",]$dAGBmin<-(c(crop[crop$crops=="1-Rice",]$AGBmin,0)-c(0, crop[crop$crops=="1-Rice",]$AGBmin))[1:200]
crop[crop$crops=="2-Maize",]$dAGBmin<-(c(crop[crop$crops=="2-Maize",]$AGBmin,0)-c(0, crop[crop$crops=="2-Maize",]$AGBmin))[1:200]
crop[crop$crops=="3-Cocoa",]$dAGBmin<-(c(crop[crop$crops=="3-Cocoa",]$AGBmin,0)-c(0, crop[crop$crops=="3-Cocoa",]$AGBmin))[1:200]
crop[crop$crops=="4-Cassava",]$dAGBmin<-(c(crop[crop$crops=="4-Cassava",]$AGBmin,0)-c(0, crop[crop$crops=="4-Cassava",]$AGBmin))[1:200]
crop[crop$crops=="5-Yam",]$dAGBmin<-(c(crop[crop$crops=="5-Yam",]$AGBmin,0)-c(0, crop[crop$crops=="5-Yam",]$AGBmin))[1:200]
crop[crop$crops=="1-Rice",]$dAGBmax<-(c(crop[crop$crops=="1-Rice",]$AGBmax,0)-c(0, crop[crop$crops=="1-Rice",]$AGBmax))[1:200]
crop[crop$crops=="2-Maize",]$dAGBmax<-(c(crop[crop$crops=="2-Maize",]$AGBmax,0)-c(0, crop[crop$crops=="2-Maize",]$AGBmax))[1:200]
crop[crop$crops=="3-Cocoa",]$dAGBmax<-(c(crop[crop$crops=="3-Cocoa",]$AGBmax,0)-c(0, crop[crop$crops=="3-Cocoa",]$AGBmax))[1:200]
crop[crop$crops=="4-Cassava",]$dAGBmax<-(c(crop[crop$crops=="4-Cassava",]$AGBmax,0)-c(0, crop[crop$crops=="4-Cassava",]$AGBmax))[1:200]
crop[crop$crops=="5-Yam",]$dAGBmax<-(c(crop[crop$crops=="5-Yam",]$AGBmax,0)-c(0, crop[crop$crops=="5-Yam",]$AGBmax))[1:200]
```

Effect of the number of residual trees on Lambda

``` r
library(ggplot2)
library(grid)
library(gridExtra)
plot1<-ggplot(data4, aes(nbr_remanent*4,lambda)) + geom_point() + geom_smooth(col="red") +
    xlab("Number of Remnants") + ylab(c(expression(lambda~"obs"))) + scale_x_sqrt()

plot2<-ggplot(remnant, aes(x=Age, y=dAGB, color=remnants)) +
labs(x="Age of Secondary Forest (yr)", y=bquote('Biomass Flux (T.ha'^-1~.yr^-1~')'))+
geom_ribbon(aes(x=Age, ymin = dAGBmin, ymax = dAGBmax,fill = remnants), alpha=0.15, colour=NA)+
geom_line(alpha=1, size=1) +
scale_color_brewer("remnants", palette="YlOrRd")+
scale_fill_brewer("remnants", palette="YlOrRd")
plot3 <- grid.arrange(plot1, plot2, widths=c(2.1/5, 2.9/5), ncol=2, nrow=1)
```

    ## `geom_smooth()` using method = 'loess'

![](Carbon_files/figure-markdown_github/residual-1.png)

Effect of the precedent crop

``` r
library(ggplot2)
library(grid)
library(gridExtra)

plot4<-ggplot(data4, aes(x=Precedentcultural,y=lambda, fill=Precedentcultural)) + geom_boxplot() +
    xlab("Preceding Crop") + ylab(c(expression(lambda~"obs"))) + theme(legend.position=" none") + scale_fill_brewer(palette="YlOrRd")+
  theme(axis.text.x = element_text(size=rel(0.8)))

plot5<-ggplot(crop, aes(x=Age, y=dAGB, color=crops)) +
labs(x="Age of Secondary Forest (yr)", y=bquote('Biomass Flux (T.ha'^-1~.yr^-1~')'))+
geom_ribbon(aes(x=Age, ymin = dAGBmin, ymax = dAGBmax,fill = crops), alpha=0.15, colour=NA)+
geom_line(alpha=1, size=1) +
scale_color_brewer("crops", palette="YlOrRd")+
scale_fill_brewer("crops", palette="YlOrRd")

plot6 <- grid.arrange(plot4, plot5, widths=c(2.1/5, 2.9/5), ncol=2, nrow=1)
```

![](Carbon_files/figure-markdown_github/crop-1.png)

Tuning for the paper
--------------------
