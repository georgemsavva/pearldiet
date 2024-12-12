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


#' ## Structural equation modelling 

## This is the definition of the model.
## We have a single factor for many of the questions.
## Another for the 'advice' questions,
## A third for the risk questions.

## A lot of model development work is not shown but this model seems to fit well.

AAmodel14b <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + 
        EvalAttitude + PCBeatingeasy + PCBfindingtimeeasy + PCBpreparingeasy  
        #HSCIconcerned + HSCIconscious
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity
  
  AA ~~ 1*AA
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  
  AA ~~ SN    ## suggested by lavaan
  AA ~~ RISK  ## suggested by lavaan
  SN ~~ RISK  ## For completeness
  
  #structural model
  AA ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10 
  RISK ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10
  
  # Final regression model, analogous to the final regression model in the last analysis
  FruitAndVeg ~ AA + SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability + 
    SNadvice + imd10
    '

AAfit14b <- sem(AAmodel14b , data=dat, missing="listwise")
summary(AAfit14b, fit.measures=TRUE)

#' This is a graphical representation of the model but it's not very neat!
lavaanPlot(AAfit14b, coefs=TRUE, stars="regress",graph_options = list(layout = "circo"))
lavaanPlot(AAfit14b, coefs=TRUE, stars="regress",graph_options = list(rankdir = "LR") )

parameterEstimates(AAfit14b) |> subset(lhs=="FruitAndVeg")

modificationIndices(AAfit14b, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)


#' This suggests SN and AA are both linked to fruitandveg, as is agreeableness. 'Buyingcheap' is not.
#'
#' As a way to test the model fit, we can compare the observed vs the modelled correlations:

## 
inspect(AAfit14b, what = "cor.ov")|> corrplot::corrplot()
lavCor(AAfit14b) |> corrplot::corrplot()



#' ## CFA model only
#' 
#' To compare the SEM with the regression models we can estimate latent variables and compare them with the simple sums of the items.

CFAmodel <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + 
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

CFAfit <- sem(CFAmodel , data=dat, missing="pairwise")

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




