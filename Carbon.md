Drivers of carbon recovery in a secondary forested landscape of West Africa
================
Anny E. N'Guessan, Bruno Hérault & Justin Kassi
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
-   [Discussion](#discussion)

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
########## Biomass Computation without remnant trees
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
data3[data3$categorie=="foretanci ",]$categorie<-"f. Old Growth"
data3[data3$categorie=="foretexpl ",]$categorie<-"e. Logged"
data3$categorie<-as.factor(data3$categorie)

# kruskal.test(data3$AGB,data3$categorie)
# kruskal.test(data3$BA,data3$categorie)
# kruskal.test(data3$DG,data3$categorie)
# kruskal.test(data3$N,data3$categorie)
```

``` r
library(ggplot2)
data_str<-data.frame(var=c(rep("AGB",dim(data3)[1]),rep("N trees",dim(data3)[1]),rep("Quad Diam",dim(data3)[1]), rep("Basal area",dim(data3)[1])),value=c(data3$AGB,data3$N,data3$DG,data3$BA),type=rep(data3$categorie,4))
data_str<-na.omit(data_str)
var<-levels(data_str$var)
data_str$var<-as.character(data_str$var)
for (i in var){
data_str[data_str$var== i,]$var<-paste(i," (P<", 0.001, ")", sep="")}
p <- ggplot(data = data_str, aes(x=var, y=value)) + 
  geom_boxplot(aes(fill=type)) +
  theme(axis.text.x=element_blank(), axis.title.x=element_blank()) #+
  #coord_flip()
p + facet_wrap( ~ var, scales="free")
```

![](Carbon_files/figure-markdown_github/Fig1-1.png)

### Forest recovery rates

``` r
agbmax<-quantile(data3[data3$categorie %in% c("e. Logged","f. Old Growth"),]$AGB, 0.75)
data4<-data3[!data3$categorie %in% c("e. Logged","f. Old Growth"),]
data4$lambda<- -log(1-(data4$AGB/agbmax))/((data4$age/10)^2)
q<-quantile(data4$lambda, probs=c(0.05, 0.5, 0.95))
```

``` r
library(ggplot2)
age<-1:200
newdata <- data.frame(age=age, med=agbmax*(1-exp(-q[2]*((age/10)^2))), q05=agbmax*(1-exp(-q[1]*((age/10)^2))),
                    q95= agbmax*(1-exp(-q[3]*((age/10)^2))))

ggplot() +
  geom_ribbon(data=newdata, aes(x=age, ymin = q05, ymax = q95), alpha = .25)+
  geom_point(data=data4, aes(x = age, y=AGB))+
  scale_y_sqrt()+
  scale_x_log10()+
  xlab("Age of Secondary Forest (yr)") + ylab("Aboveground Biomass (T/ha)")
```

![](Carbon_files/figure-markdown_github/fig2-1.png)

### Environmental control on recovery rates

Discussion
----------
