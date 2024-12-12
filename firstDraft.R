#' 
#' title: Structural equation modelling
#' 
#' 

library(polycor)
library(lavaan)
library(lavaanPlot)
library(tidySEM)
library(ggplot2)
library(data.table)
library(ggcorrplot)
library(readxl)
if(!exists("dat")) {
  source("makeData.R")
  dat < getData()
  }

## SELF EFFICAY IS MADE OF:
# CON = confidence (if I wanted to I could...)
# PD = perceived difficulty

## PC = perceived control
# PC = how much control do I have over the behaviour
# LOC = it is mostly up to me whether I...

# This is a version of the dataset which is set to numeric and reverse coded where needed (so that positive is 'good')

# Function to reverse the levels of a factor
reverse_levels <- function(f) {
  if (!is.factor(f)) {
    stop("Input must be a factor.")
  }
  
  # Reverse levels
  reversed_levels <- rev(levels(f))
  
  # Return a new factor with reversed levels, preserving the ordered status
  factor(f, levels = reversed_levels, ordered = is.ordered(f))
}


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

smallDat2 <- with(dat,data.frame(AApleasant=(AApleasant),
                                AAconvenient=(AAconvenient),
                                AAenjoyable=(AAenjoyable), 
                                AAinline=(AAinline), 
                                AAimportant=(AAimportant), 
                                EvalAttitude=reverse_levels(EvalAttitude),  
                                PBCeatingeasy=reverse_levels(PBCeatingeasy),
                                PBCbuyingcheap=reverse_levels(PBCbuyingcheap),  
                                PBCbehaviour=reverse_levels(PBCbehaviour),  
                                PBCpreparingeasy=reverse_levels(PBCpreparingeasy),  
                                PBCfindingtimeeasy=reverse_levels(PBCfindingtimeeasy),
                                SNfamily=reverse_levels(SNfamily),
                                SNfriends=reverse_levels(SNfriends),
                                SNdoctor=reverse_levels(SNdoctor),
                                SNadvice=reverse_levels(SNadvice),
                                RISKcancer = reverse_levels(RISKcancer),
                                RISKobesity = reverse_levels(RISKobesity),
                                RISKheart = reverse_levels(RISKheart),
                                RISKbaby =reverse_levels (RISKbaby),
                                HSCIconscious = reverse_levels(HSCIconscious),
                                HSCIconcerned = reverse_levels(HSCIconcerned),
                                FruitAndVeg
))


## Make a clustered Spearman correlation matrix
cor(smallDat,method="spearman",use="complete.obs") |> 
  corrplot::corrplot(method = "number",order = "hclust")

cor(smallDat,method="spearman",use="complete.obs") |> 
  corrplot::corrplot(method = "number")

cor(smallDat,method="kendall",use="complete.obs") |> 
  corrplot::corrplot(method = "number", order="hclust")

cor(smallDat,method="kendall",use="complete.obs") |> 
  ggcorrplot(hc.order = TRUE, colors = c("red", "white", "blue"))

cor(smallDat,method="spearman",use="complete.obs") |> 
  ggcorrplot(hc.order = TRUE, colors = c("red", "white", "blue"), lab=TRUE) + ggtitle("Kendall's tau")

hc <- hetcor(smallDat2)
hc$correlations |> ggcorrplot( colors = c("red", "white", "blue"), lab=TRUE) + ggtitle("Polychoric")
hc$correlations |> ggcorrplot( hc.order=TRUE,colors = c("red", "white", "blue"), lab=TRUE) + ggtitle("Polychoric")

hc$correlations |> ggcorrplot( colors = c("red", "white", "blue"), lab=TRUE) + ggtitle("Polychoric")


#' RISKbaby only has two negative responses, so it doesn't give us any information really!


#' ## Structural equation modelling 


#' ## CFA model only
#' 
#' To compare the SEM with the regression models we can estimate latent variables and compare them with the simple sums of the items.


## First we fit a model with all of the affective attitude, HCSI and percieved behavioural control into a single latent factor:
CFAmodel0 <- '

  #measurement model
  
  # Factor structure
  AAPBC =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + 
           HSCIconcerned + HSCIconscious + PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKcancer
  
  # Set variance to 1 for each factor for scaling
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  AAPBC ~~ 1*AAPBC
  '

CFAfit0 <- sem(CFAmodel0 , data=dat, missing="pairwise", ordered=TRUE)
CFAfit0 |> summary()

## This model is not identified, it also complains that RISKcancer and RISKheart are perfectly correlated.
## Actually we do see this from the data.  It would probably be best to make a three-level variable which is the sum of the RISK variables.
dat$RISKtotal <- (as.numeric(smallDat2$RISKheart) + as.numeric(smallDat2$RISKobesity) + as.numeric(smallDat2$RISKcancer)-3) |> factor(ordered=TRUE)
dat$RISKtotal2 <- (as.numeric(smallDat2$RISKheart) + as.numeric(smallDat2$RISKcancer)-3) |> factor(ordered=TRUE)
table(smallDat2$RISKcancer, smallDat2$RISKheart)


CFAmodel1 <- '

  #measurement model
  
  # Factor structure
  AAPBC =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + 
           HSCIconcerned + HSCIconscious + PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  
  # Set variance to 1 for each factor for scaling
  SN ~~ 1*SN
  AAPBC ~~ 1*AAPBC
  '

CFAfit1 <- sem(CFAmodel1 , data=dat, missing="pairwise", ordered=TRUE)
CFAfit1 |> summary()

#' Now we have a model that will run, without the RISK items included.

CFAfit1 |> lavaanExtra::nice_fit()
CFAfit1 |> lavaanExtra::nice_modindices() |> head()

#' The CFI is very good, but the RMSEA and modification indices suggest that HSCI elements should be separated out:
CFAmodel2 <- '

  #measurement model
  
  # Factor structure
  AAPBC =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + 
           PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  
  HSCI =~ NA*HSCIconcerned + HSCIconscious 
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  
  # Set variance to 1 for each factor for scaling
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  AAPBC ~~ 1*AAPBC
  '

CFAfit2 <- sem(CFAmodel2 , data=dat, missing="pairwise", ordered=TRUE)
CFAfit2 |> summary()
CFAfit2 |> lavaanExtra::nice_fit()
CFAfit2 |> lavaanExtra::nice_modindices() |> head()

## This suggests the HSCI was significantly separate factor.
anova(CFAfit1, CFAfit2)


CFAmodel3 <- '

  #measurement model
  
  # Factor structure
  AAPBC =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude
  PBC =~   NA*PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour  
  HSCI =~  NA*HSCIconcerned + HSCIconscious 
  SN =~    NA*SNfamily + SNfriends + SNdoctor
  
  # Set variance to 1 for each factor for scaling
  PBC ~~ 1*PBC
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  AAPBC ~~ 1*AAPBC
  '

CFAfit3 <- sem(CFAmodel3 , data=dat, missing="pairwise", ordered=TRUE)
CFAfit3 |> summary()
CFAfit3 |> lavaanExtra::nice_fit()
CFAfit3 |> lavaanExtra::nice_modindices() |> head()


## This suggests having a separate PBC factor is an improvement
anova(CFAfit3,CFAfit2)


## Should AAconvenient go with PBC?
CFAmodel3b <- '

  #measurement model
  
  # Factor structure
  AA =~ NA*AApleasant + AAinline + AAimportant + EvalAttitude
  PBC =~   NA*PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour +  AAconvenient 
  HSCI =~  NA*HSCIconcerned + HSCIconscious 
  SN =~    NA*SNfamily + SNfriends + SNdoctor
  
  # Set variance to 1 for each factor for scaling
  PBC ~~ 1*PBC
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  AA ~~ 1*AA
  
  # Correlations
  PBC ~~ AA
  '

CFAfit3b <- sem(CFAmodel3b , data=dat, missing="pairwise", ordered=TRUE)
CFAfit3b |> summary()
CFAfit3b |> fitMeasures()
CFAfit3b |> lavaanExtra::nice_fit()
CFAfit3b |> lavaanExtra::nice_modindices() |> head()
CFAfit3b |> lavaanExtra::nice_lavaanPlot(covs = TRUE,)

CFAfit3b_O <- sem(CFAmodel3b , data=dat, missing="pairwise", ordered = TRUE)
CFAfit3b |> summary()
CFAfit3b |> fitMeasures()
CFAfit3b_O |> lavaanExtra::nice_fit()
CFAfit3b |> lavaanExtra::nice_modindices() |> head()



## This suggests having a separate PBC factor is an improvement
anova(CFAfit3,CFAfit3b)


## What happens if I try to make a normal underlying variable for the RISK and FV factors:
CFAmodel4 <- '

  #measurement model
  
  # Factor structure
  AA =~ NA*AApleasant + AAinline + AAimportant + EvalAttitude
  PBC =~   NA*PBCeatingeasy + PBCfindingtimeeasy + PBCpreparingeasy + PBCbehaviour +  AAconvenient 
  HSCI =~  NA*HSCIconcerned + HSCIconscious 
  SN =~    NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKtotal2 + RISKobesity
  
  
  # Set variance to 1 for each factor for scaling
  PBC ~~ 1*PBC
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  AA ~~ 1*AA
  RISK ~~ 1*RISK
  
  '

CFAfit4 <- sem(CFAmodel4 , data=dat, missing="pairwise")
CFAfit4 |> summary()
CFAfit4 |> lavaanExtra::nice_modindices() |> head(10)
CFAfit4 |> lavaanExtra::nice_fit() 


#### So model 4 is probably the best.


## Add the predictions to the dataset
dat2 <- cbind(dat, predict(CFAfit4, newdata=dat))
setDT(dat2)
## Now we can make pairs plot with the predictions:

library(GGally)
GGally::ggpairs(dat2[ , .(FruitAndVeg, AA, PBC=-PBC, RISK, SN=-SN, HSCI=-HSCI)], 
                upper = list(continuous = wrap("cor", method = "spearman")))


### Now we can build regression models:

### Just throwing in all the factors none is linked, but 
lm(data=dat2 , FruitAndVeg ~ AA + HSCI + PBC + RISK + SN) |> summary()

lm(data=dat2 , FruitAndVeg ~ AA + age + imd10 ) |> summary()
lm(data=dat2 , FruitAndVeg ~ PBC + age + imd10 ) |> summary()
lm(data=dat2 , FruitAndVeg ~ HSCI  + age + imd10) |> summary()
lm(data=dat2 , FruitAndVeg ~ SN + age + imd10) |> summary()
lm(data=dat2 , FruitAndVeg ~ RISK  + age + imd10) |> summary()

### Univariately these are all linked.

#### If we only include AA and PBC, then PBC is not correlated.
#### But we shouldn't discount that AA and PBC are 
lm(data=dat2 , FruitAndVeg ~ AA+PBC ) |> summary()

## This might suggest a nonlinear relationship with AA
modinteract < lm(data=dat2 , FruitAndVeg ~ AA*PBC )

### As PBC goes up, the effect of AA goes down (remember PBC is reversed in this analysis)
modinteract |> summary()


modaa2 < lm(data=dat2, FruitAndVeg ~ AA + I(AA^2))
### Similarly, the squared term is negative
### Could reflect a ceiling effect possibly or be removable, don't read too much into it.
modaa2 |> summary()

modaa22 < lm(data=dat2, FruitAndVeg ~ AA + I(AA^2) + PBC + age)
modaa22 |> summary()

modaa22_cheap < lm(data=dat2, FruitAndVeg ~ AA + I(AA^2) + PBC + imd10*as.numeric(PBCbuyingcheap) + age)
modaa22_cheap |> summary()

anova(modpbc,modaa,modadd , modinteract)
anova(modaa22_cheap)


dat2$preds < modaa2 |> predict()
ggplot(dat2) + aes(y=FruitAndVeg, x=AA) + geom_point() + geom_line(aes(y=preds))

ggplot(dat2) + aes(y=PBC, x=AA) + geom_point() 

### Yes, there is a significant interaction..

nrow(dat)

CFAmodel0 < '

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

CFAfit < sem(CFAmodel0 , data=dat, missing="pairwise")

CFAfit |> summary()
CFApreds < CFAfit |> predict() |> as.data.frame()

dat2 < cbind(dat, CFApreds)

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




