#' ---
#' title: Structural equation modelling
#' ---

library(lavaan)
library(lavaanPlot)
library(tidySEM)
library(ggplot2)
library(data.table)

if(!exists("dat")) {
  source("makeData.R")
  dat <- getData()
  }

## SELF EFFICAY IS MADE OF:
# CON = confidence (if I wanted to I could...)
# PD = perceived difficulty

## PC = perceived control
# PC = how much control do I have over the behaviour
# LOC = it is mostly up to me whether I...


smallDat <- with(dat,data.frame(AApleasant=as.numeric(AApleasant),
                        AAconvenient=as.numeric(AAconvenient),
                        AAenjoyable=as.numeric(AAenjoyable), 
                        AAinline=as.numeric(AAinline), 
                        AAimportant=as.numeric(AAimportant), 
                        EvalAttitude=-as.numeric(EvalAttitude),  
                        PBCeatingeasy=-as.numeric(PBCeatingeasy),
                        PBCbuyingcheap=-as.numeric(PBCbuyingcheap),  
                        PBCbehaviour=-as.numeric(PBCbehaviour),  
                        PBCpreparingeasy=-as.numeric(PBCpreparingeasy),  
                        PBCfindingtimeeasy=-as.numeric(PBCfindingtimeeasy),
                        SNfamily=-as.numeric(SNfamily),
                        SNfriends=-as.numeric(SNfriends),
                        SNdoctor=-as.numeric(SNdoctor),
                        SNadvice=-as.numeric(SNadvice),
                        RISKcancer = -as.numeric(RISKcancer),
                        RISKobesity = -as.numeric(RISKobesity),
                        RISKheart = -as.numeric(RISKheart),
                        RISKbaby = -as.numeric(RISKbaby),
                        HSCIconscious = -as.numeric(HSCIconscious),
                        HSCIconcerned = -as.numeric(HSCIconcerned),
                        FruitAndVeg
                        
                        ))

cor(smallDat,method="spearman",use="complete.obs") |> 
  corrplot::corrplot(method = "number",order = "hclust")

cor(smallDat,method="spearman",use="complete.obs") |> 
  corrplot::corrplot(method = "number")




#' ## Structural equation modelling 


#' ## CFA model only
#' 
#' To compare the SEM with the regression models we can estimate latent variables and compare them with the simple sums of the items.

### Components suggested:
CFAmodel0 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + HSCIconcerned + HSCIconscious
  PBC =~ NA*PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  
        
  #SN =~ NA*SNfamily + SNfriends + SNdoctor
  #RISK =~ NA*RISKheart + RISKcancer

  AA ~~ 1*AA
  #SN ~~ 1*SN
  #RISK ~~ 1*RISK
  PBC ~~ 1*PBC

  #AA ~~ PBC  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  #AA ~~ SN    ## suggested by lavaan
  #AA ~~ RISK  ## suggested by lavaan
  #SN ~~ RISK  ## For completeness
  #SN ~~ PBC  ## For completeness
  #PBC ~~ RISK  ## For completeness
  
  '

CFAfit0 <- sem(CFAmodel0 , data=dat, missing="pairwise")
CFAfit0 |> summary()




CFAmodel1 <- '

  #measurement model
  AAPBC =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + 
  HSCIconcerned + HSCIconscious + PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  
        
  #SN =~ NA*SNfamily + SNfriends + SNdoctor
  #RISK =~ NA*RISKheart + RISKcancer

  #AA ~~ 1*AA
  #SN ~~ 1*SN
  #RISK ~~ 1*RISK
  AAPBC ~~ 1*AAPBC

  #AA ~~ PBC  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  #AA ~~ SN    ## suggested by lavaan
  #AA ~~ RISK  ## suggested by lavaan
  #SN ~~ RISK  ## For completeness
  #SN ~~ AAPBC  ## For completeness
  #PBC ~~ RISK  ## For completeness
  
  '

CFAfit1 <- sem(CFAmodel1 , data=dat, missing="pairwise")
CFAfit1 |> summary()


anova(CFAfit0, CFAfit1)


### Two factors is a substantially better model than one..



### It wants convinient to be with PBS
### And HSCI to be separated
modificationIndices(CFAfit0, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)

modificationIndices(CFAfit1, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)



CFAmodel2 <- '

  #measurement model
  AA =~ NA*AApleasant + AAinline + AAimportant + EvalAttitude 
  HSCI =~ NA*HSCIconcerned + HSCIconscious
  PBC =~ NA*PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  + AAconvenient 
        
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  #RISK =~ NA*RISKheart + RISKcancer

  AA ~~ 1*AA
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  #RISK ~~ 1*RISK
  PBC ~~ 1*PBC

  #AA ~~ PBC  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  #AA ~~ SN    ## suggested by lavaan
  #AA ~~ RISK  ## suggested by lavaan
  #SN ~~ RISK  ## For completeness
  #SN ~~ PBC  ## For completeness
  #PBC ~~ RISK  ## For completeness
  
  '

CFAfit2 <- sem(CFAmodel2 , data=dat, missing="pairwise")
CFAfit2 |> summary()

anova(CFAfit0, CFAfit2)

modificationIndices(CFAfit2, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)

### So this is an adequate model.

predict(CFAfit2, newdata=dat)

dat2 <- cbind(dat, predict(CFAfit2, newdata=dat))


lm(data=dat2 , FruitAndVeg ~ AA + HSCI + PBC) |> summary()

lm(data=dat2 , FruitAndVeg ~ AA ) |> summary()
lm(data=dat2 , FruitAndVeg ~ PBC ) |> summary()
lm(data=dat2 , FruitAndVeg ~ HSCI ) |> summary()


 modaa <- lm(data=dat2 , FruitAndVeg ~ AA )
modpbc <- lm(data=dat2 , FruitAndVeg ~ PBC )
modadd <- lm(data=dat2 , FruitAndVeg ~ AA+PBC )

## This might suggest a non-linear relationship with AA
modinteract <- lm(data=dat2 , FruitAndVeg ~ AA*PBC )

### As PBC goes up, the effect of AA goes down (remember PBC is reversed in this analysis)
modinteract |> summary()


modaa2 <- lm(data=dat2, FruitAndVeg ~ AA + I(AA^2))
### Similarly, the squared term is negative
### Could reflect a ceiling effect possibly or be removable, don't read too much into it.
modaa2 |> summary()

modaa22 <- lm(data=dat2, FruitAndVeg ~ AA + I(AA^2) + PBC + age)
modaa22 |> summary()

modaa22_cheap <- lm(data=dat2, FruitAndVeg ~ AA + I(AA^2) + PBC + imd10*as.numeric(PBCbuyingcheap) + age)
modaa22_cheap |> summary()

anova(modpbc,modaa,modadd , modinteract)
anova(modaa22_cheap)


dat2$preds <- modaa2 |> predict()
ggplot(dat2) + aes(y=FruitAndVeg, x=AA) + geom_point() + geom_line(aes(y=preds))

ggplot(dat2) + aes(y=PBC, x=AA) + geom_point() 

### Yes, there is a significant interaction..

nrow(dat)

CFAmodel0 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant 
  
        EvalAttitude + PCBeatingeasy + PCBfindingtimeeasy + PCBpreparingeasy  
        #HSCIconcerned + HSCIconscious
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity

  AA ~~ 1*AA
  SN ~~ 1*SN
  RISK ~~ 1*RISK

  #AA ~~ PCB  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  AA ~~ SN    ## suggested by lavaan
  AA ~~ RISK  ## suggested by lavaan
  SN ~~ RISK  ## For completeness
  '

CFAfit <- sem(CFAmodel0 , data=dat, missing="pairwise")

CFAfit |> summary()
CFApreds <- CFAfit |> predict() |> as.data.frame()

dat2 <- cbind(dat, CFApreds)

plot(as.numeric(dat2$AAtotal), dat2$AA)
cor.test(as.numeric(dat2$AAtotal), dat2$AA)

## SN
plot(as.numeric(dat2$SNtotal), dat2$SN)
cor.test(as.numeric(dat2$SNtotal), dat2$SN)

## RISK
plot(as.numeric(dat2$RISKheart) + as.numeric(dat2$RISKobesity), dat2$RISK)
cor.test(as.numeric(dat2$SNtotal), dat2$SN)

#### The latent variables are bascially the same as just adding up values!


#' ## Preliminary Results 
#' 
#' These are the parameters for the regression model that comes from the SEM:

parameterEstimates(AAfit14b) |> subset(lhs=="FruitAndVeg")




