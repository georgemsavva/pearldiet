library(lavaan)
library(lavaanPlot)
library(tidySEM)
library(ggplot2)
library(data.table)

if(!exists("dat")) {
  source("makeData.R")
  dat <- getData()
  }


AAmodel <- '
  #measurement model
  
  AA =~ AApleasant + AAconvenient + AAenjoyable + AAinline + AAimportant
  
  healthiness =~ Fruit + Veg
  
  #structural model
  
  healthiness ~ AA + age
  
  #residual correlations
  Fruit ~~ Veg
  '
AAfit <- sem(AAmodel , data=dat)
summary(AAfit)
graph_sem(AAfit)
lavaanPlot(AAfit, coefs=TRUE)

# It looks like enjoyable and pleasant are the same, so drop one of them

AAmodel2 <- '
  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  healthiness =~ Fruit + Veg
  
  #structural model
  healthiness ~ AA + age
  
  #residual correlations
  #Fruit ~~ Veg
  '
AAfit2 <- sem(AAmodel2 , data=dat)
summary(AAfit2)
lavaanPlot(AAfit2, coefs=TRUE, stars="regress")

### Here, AA is very correlated with healthiness (0.65, p<1)
### Age is not associated.
### Fruit is more strongly linked to this concept than veg.

## For the next model, ditch the healthiness latent variable:

AAmodel3 <- '

  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  
  #structural model
  FruitAndVeg ~ AA + age
  '

AAfit3 <- sem(AAmodel3 , data=dat)
summary(AAfit3)
lavaanPlot(AAfit3, coefs=TRUE, stars="regress")

### We still have a significant effect of AA on fruitandveg

### Model 4 now adds PCB
AAmodel4 <- '

  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ PCBeatingeasy + PCBbuyingcheap + PCBfindingtimeeasy + PCBbehaviour
  
  #structural model
  FruitAndVeg ~ AA + PCB + age
  '

AAfit4 <- sem(AAmodel4 , data=dat)
summary(AAfit4)
lavaanPlot(AAfit4, coefs=TRUE, stars="regress")

#### There appears to be no effect of PCB on the fruitandveg outcome here


## But 'cheap' is not highly correlated with the other elements of the construct..
AAmodel5 <- '

  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
  
  #structural model
  FruitAndVeg ~ AA + PCB + age 
  '

AAfit5 <- sem(AAmodel5 , data=dat, missing="pairwise")
summary(AAfit5)
lavaanPlot(AAfit5, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
graph_sem(AAfit5)


## Now if we try to put 'cheap' back in as an extra regression coefficient..

AAmodel6 <- '

  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
  
  #structural model
  FruitAndVeg ~ AA + PCB + age + PCBbuyingcheap 
  '

AAfit6 <- sem(AAmodel6 , data=dat, missing="pairwise")
summary(AAfit6)
lavaanPlot(AAfit6, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))

#### This gives me a warning, and also reverses all of the other effects!
#### OK now its the same as every other effect, what's going on?

AAfit6b <- sem(AAmodel6 , data=dat)
summary(AAfit6b)

## Changing the missing values method completely changes all of the answers!!
## There is only one missing value I think, so what is happening?
lavaanPlot(AAfit6b, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit6, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))

setDT(dat)
dat[, .(AApleasant,AAconvenient,AAinline,AAimportant,PCBeatingeasy, PCBfindingtimeeasy, PCBbehaviour,
        age, FruitAndVeg, PCBbuyingcheap)]
View(dat[, .(AApleasant,AAconvenient,AAinline,AAimportant,PCBeatingeasy, PCBfindingtimeeasy, PCBbehaviour,
             age, FruitAndVeg, PCBbuyingcheap)])



### Only one element has one missing value (#26)
### This does look like an odd data point, because all of the other responses are '3'.

### Let's estimate without this point at all.


AAfit6c <- sem(AAmodel6 , data=dat[!is.na(AAimportant)])
summary(AAfit6c)
lavaanPlot(AAfit6c, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
## This has identical result to the initial model with the listwise deletion.

AAfit6d <- sem(AAmodel6 , data=dat[!is.na(AAimportant)], missing="pairwise")
summary(AAfit6d)
lavaanPlot(AAfit6d, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
## As does d when you manually take out the problematic point.



## So why should one point completely change the results like this?






## Why?
AAfit6b |> summary()

dat$PCBbuyingcheap2 = as.numeric(dat$PCBbuyingcheap)


AAmodel7 <- '

  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
  
  #structural model
  FruitAndVeg ~ AA + PCB + age + PCBbuyingcheap2
  '

AAfit7 <- sem(AAmodel7 , data=dat)
summary(AAfit7)
lavaanPlot(AAfit7, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))

## OK it's not the 'ordered' issue, these two are exactly the same.




predictions6 = lavPredict(AAfit6,newdata = dat)|> as.data.frame()
predictions5 = lavPredict(AAfit5, newdata=dat) |> as.data.frame()

## The latent constructs are not being affected in the different models.
## So I guess it's about the inclusion of the 'buying cheap' variable in the main regression model?
plot(predictions5[,1], predictions6[,1])
plot(predictions5[,2], predictions6[,2])

names(predictions5) <- c("AA5","PCB5")
names(predictions6) <- c("AA6","PCB6")

dat_preds <- cbind(dat, predictions5,predictions6)

lm(data = dat_preds , FruitAndVeg ~ AA5 + PCB5 + age) |> summary()
lm(data = dat_preds , FruitAndVeg ~ AA6 + PCB6 + PCBbuyingcheap2+age) |> summary()

### Do the AA and PCB prediced values have fruitandveg built in because this informs their likely value as much as the indicators do?
### Possibly!


## Should we include correlation between the latent variables as well?
## Actually this is included in the models by default it seems
## AA and PCB are negatively correlated.

setDT(dat_preds)
dat_preds[, AAtotal := 
            as.numeric(AApleasant) + 
            as.numeric(AAconvenient) + 
            as.numeric(AAimportant) + 
            as.numeric(AAinline)]

dat_preds[, PCBtotal := 
            as.numeric(PCBeatingeasy) + 
            as.numeric(PCBfindingtimeeasy) + 
            as.numeric(PCBbehaviour) ]


# Most people score very high on this!
hist(dat_preds$AAtotal)


## lm does remove this data point!
## so we have solutions close to the complete data SEM solution and not like the one point missing SEM solution
lm(data = dat_preds , FruitAndVeg ~ AAtotal + PCB6 + PCBbuyingcheap2+age) |> summary()


lm(data = dat_preds , FruitAndVeg ~ AAtotal + PCB6 + PCBbuyingcheap2+age) |> summary()

lm(data = dat_preds , FruitAndVeg ~ AAtotal + PCBtotal + PCBbuyingcheap2+age) |> summary()
lm(data = dat_preds , FruitAndVeg ~ AAtotal + PCBtotal + age) |> summary()

lm(data = dat_preds , Fruit ~ AAtotal + PCBtotal + age) |> summary()
lm(data = dat_preds , Veg ~ AAtotal + PCBtotal + age) |> summary()

cor(dat_preds$Fruit, dat_preds$Veg)
## OK this makes sense now.



## I wonder if it's actually a single factor, since the correlation is so high between AA and PCB




## But 'cheap' is not highly correlated with the other elements of the construct..


AAmodel8 <- '

  #measurement model
  AA_PCB =~ AApleasant + AAconvenient + AAinline + AAimportant + PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
  
  #structural model
  FruitAndVeg ~ AA_PCB + age + PCBbuyingcheap
  '

## We're using the marker method for model identification here, that is, one item gets a loading of 1.
AAfit8 <- sem(AAmodel8 , data=dat, missing="pairwise")
summary(AAfit8)
lavaanPlot(AAfit8, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit8, coefs=TRUE, stars="regress")
graph_sem(AAfit8)


### This is the fixed variance method
AAmodel8b <- '

  #measurement model
  AA_PCB =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
  AA_PCB ~~ 1*AA_PCB
  
  #structural model
  FruitAndVeg ~ AA_PCB + age + PCBbuyingcheap
  '

## I think I prefer this.
## I wonder if it helps with the identification problem above??
AAfit8b <- sem(AAmodel8b , data=dat, missing="pairwise")
summary(AAfit8b)
lavaanPlot(AAfit8, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit8b, coefs=TRUE, stars="regress")
graph_sem(AAfit8)


## OK so 6 is a significantly better fit than 8, which suggests two factors?
## 8 and 8b are recognised as the same model here.
library(semTools)
cf86 <- compareFit(AAfit8, AAfit6)
summary(cf86)

summary(AAfit8,fit.measures=TRUE)
summary(AAfit6,fit.measures=TRUE)



AAmodel8CFAonly <- '
  #measurement model
  AA_PCB =~ AApleasant + AAconvenient + AAinline + AAimportant + PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
    '
CFAfit8 <- sem(AAmodel8CFAonly , data=dat, missing="pairwise")
summary(CFAfit8,fit.measures=TRUE)

AAmodel6CFAonly <- '
  #measurement model
  AA =~ AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ PCBeatingeasy +  PCBfindingtimeeasy + PCBbehaviour
    '
CFAfit6 <- sem(AAmodel6CFAonly , data=dat, missing="pairwise")
summary(CFAfit6,fit.measures=TRUE)

cf86cfa <- compareFit(CFAfit8,CFAfit6)
summary(cf86cfa)

## Both models are OK but 6 fits better than 8.
## Could be something to do with the negative coding I guess?



## Model 9 removes 'control' from the PCB scale
AAmodel9 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  #structural model
  FruitAndVeg ~ AA + PCB + age + PCBbuyingcheap + PCBbehaviour 
  '

## lol.  now both factors are independently important
AAfit9 <- sem(AAmodel9 , data=dat, missing="pairwise")
summary(AAfit9)
lavaanPlot(AAfit9, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit9, coefs=TRUE, stars="regress")


## The correlation between AA and PCB is -0.844 in this model
## So it doesn't make sense to ask which one is the important factor.
## They map very closely.




polychoric <- psych::polychoric(dat[, .(AApleasant,AAconvenient,AAenjoyable,AAinline,AAimportant,
                                        PCBeatingeasy,PCBbuyingcheap,PCBpreparingeasy,PCBfindingtimeeasy,PCBbehaviour)])

rho = polychoric$rho
rownames(rho) <- names(dat[, .(AApleasant,AAconvenient,AAenjoyable,AAinline,AAimportant,PCBeatingeasy,PCBbuyingcheap,PCBpreparingeasy,PCBfindingtimeeasy,PCBbehaviour)])
colnames(rho) <- names(dat[, .(AApleasant,AAconvenient,AAenjoyable,AAinline,AAimportant,PCBeatingeasy,PCBbuyingcheap,PCBpreparingeasy,PCBfindingtimeeasy,PCBbehaviour)])

#polychoric1 <- psych::polychoric(dat[, .(AApleasant,AAconvenient,AAenjoyable,AAinline,AAimportant)],
#polychoric2 <- psych::polychoric(dat[, .(PCBeatingeasy,PCBbuyingcheap,PCBpreparingeasy,PCBfindingtimeeasy,PCBbehaviour)])

# the efa suggests everything except 'PCB buying cheap' is correlated into a single factor. 
# this is highly linked to fruit and veg

efa1 <- psych::fa(rho,nfactors = 5,rotate = "varimax")



## Let's do a big polychoric analysis
datsmall <- dat[, .(AApleasant,AAconvenient,AAenjoyable,AAinline,AAimportant,
                    PCBeatingeasy,PCBbuyingcheap,PCBpreparingeasy,PCBfindingtimeeasy,PCBbehaviour,
                    RISKcancer, RISKheart, RISKdiabetes, RISKobesity, RISKbaby)]

polychoric <- psych::polychoric(datsmall)

rho = polychoric$rho
rownames(rho) <- names(datsmall)
colnames(rho) <- names(datsmall)

efa2 <- psych::fa(rho,nfactors = 5,rotate = "varimax")
efa2


#the hsci variables are very highly correlated with each other as well.




AAmodel10 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  HSCI =~ NA*HSCIconcerned + HSCIconscious
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity + RISKdiabetes
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  #structural model
  FruitAndVeg ~ AA + PCB + HSCI + SN + RISK +  age + PCBbuyingcheap + PCBbehaviour
  '
AAfit10 <- sem(AAmodel10 , data=dat, missing="pairwise")
summary(AAfit10)
lavaanPlot(AAfit10, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit10, coefs=TRUE, stars="regress")

# Again HSCI is very closely correlated with AA and PCB.
# Doesn't independently explain more variance after AA and PCB are included.

#### The SEM model may not make theoretical sense, but it is modelling the observed relationships well enough.
#### Reflecting the very high correlations between factors.



table(dat$FruitAndVeg, dat$RISKheart)


table(dat$AAconvenient)



### This is good.

### So how do we incorporate the personality scales?

### They should be added as independent covariates I think?

## dropping HSCI because it's messing something up
AAmodel11 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  HSCI =~ NA*HSCIconcerned + HSCIconscious
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity + RISKdiabetes
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  HSCI ~~ 1*HSCI
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  #structural model
  FruitAndVeg ~ AA + PCB +  SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability
  '

AAfit11 <- sem(AAmodel11 , data=dat, missing="pairwise")
summary(AAfit11)
lavaanPlot(AAfit11, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit11, coefs=TRUE, stars="regress")

## Happy with this model.



## reintroduce HSCI as independent covariates
## still messes up
## So add them to the AA factor?
AAmodel11b <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + HSCIconcerned + HSCIconscious
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity + RISKdiabetes
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  #structural model
  FruitAndVeg ~ AA + PCB +  SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability  
      '
AAfit11b <- sem(AAmodel11b , data=dat, missing="pairwise")
summary(AAfit11b)
lavaanPlot(AAfit11b, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit11b, coefs=TRUE, stars="regress")


## Personality measures might underlie the attitudes?



AAmodel12 <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + HSCIconcerned + HSCIconscious
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity + RISKdiabetes
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  #structural model
  AA ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability
  PCB ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability  
  RISK ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability  
  
  FruitAndVeg ~ AA + PCB +  SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2  
    
      '
AAfit12 <- sem(AAmodel12 , data=dat, missing="pairwise")
summary(AAfit12)
lavaanPlot(AAfit12, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit12, coefs=TRUE, stars="regress")

## This is the closest thing to the model that Stephanie proposed.

## Would be nice to have a psychologist look at it.

### The fit indices are much better for 11 than 12.
### Not sure what this means, I guess its because personality is a poor predictor of attitude?
summary(AAfit12, fit.measures=TRUE)
summary(AAfit11b, fit.measures=TRUE)


AAmodel12b <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude
  PCB =~ NA*PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity
  AA ~~ 1*AA
  PCB ~~ 1*PCB
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  
  AA ~~ PCB  ##  This bit allows AA to be correlated with PCB
  AA ~~ SN ## suggested by lavaan
  AA ~~ RISK ## suggested by lavaan
  
  #structural model
  AA ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10 
  PCB ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10  
  RISK ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10
  
  FruitAndVeg ~ AA +  PCB + SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability  
    
      '

## This also has relatively poor CFI
AAfit12b <- sem(AAmodel12b , data=dat, missing="pairwise")
summary(AAfit12b, fit.measures=TRUE)
lavaan::vcov(AAfit12b)
lavaanPlot(AAfit12b, coefs=TRUE, stars="regress",graph_options = list(layout = "neato"))
lavaanPlot(AAfit12b, coefs=TRUE, stars="regress")

## 
fitMeasures(AAfit12b)
performance::model_performance(AAfit12b)

modificationIndices(AAfit12b, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)

#### OK, probably enough for now before I speak to the team.

#' ## Final model
#' 
#' This model strongly suggests a single latent factor with both AA and PCB.
#' 
#' 


AAmodel13b <- '

  #measurement model
  AA =~ NA*AApleasant + AAconvenient + AAinline + AAimportant + EvalAttitude + PCBeatingeasy +  PCBfindingtimeeasy + PCBpreparingeasy
  SN =~ NA*SNfamily + SNfriends + SNdoctor
  RISK =~ NA*RISKheart + RISKobesity
  
  
  AA ~~ 1*AA
  SN ~~ 1*SN
  RISK ~~ 1*RISK
  
  #AA ~~ PCB  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  AA ~~ SN ## suggested by lavaan
  AA ~~ RISK ## suggested by lavaan
  
  #structural model
  AA ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10 
  #PCB ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10  
  RISK ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10
  
  FruitAndVeg ~ AA + SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability  
    
      '

AAfit13b <- sem(AAmodel13b , data=dat, missing="pairwise")
summary(AAfit13b, fit.measures=TRUE)

lavaanPlot(AAfit13b, coefs=TRUE, stars="regress",graph_options = list(layout = "circo"))
lavaanPlot(AAfit12b, coefs=TRUE, stars="regress")

lavTestLRT(AAfit13b, AAfit12b)

## 
fitMeasures(AAfit12b)
performance::model_performance(AAfit13b)

modificationIndices(AAfit13b, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)





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
  
  #AA ~~ PCB  ##  This bit allows AA to be correlated with PCB.  Not needed now because they are all on one scale,
  AA ~~ SN    ## suggested by lavaan
  AA ~~ RISK  ## suggested by lavaan
  SN ~~ RISK  ## For completeness
  
  #structural model
  AA ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10 
  #PCB ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10  
  RISK ~ Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability+imd10
  
  FruitAndVeg ~ AA + SN + RISK +
    PCBbuyingcheap + PCBbehaviour + age + housing2 + 
    Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability + 
    SNadvice + imd10
    
      '

AAfit14b <- sem(AAmodel14b , data=dat, missing="listwise")
summary(AAfit14b, fit.measures=TRUE)
summary(AAfit14b )
lavaanPlot(AAfit14b, coefs=TRUE, stars="regress",graph_options = list(layout = "circo"))
lavaanPlot(AAfit14b, coefs=TRUE, stars="regress")

lavTestLRT(AAfit13b, AAfit12b)

## 

modificationIndices(AAfit13b, 
                    standardized = TRUE,
                    sort = TRUE,
                    minimum.value = 10)

parameterEstimates(AAfit14b) |> subset(lhs=="FruitAndVeg")

## This suggests SN and AA are both linked to fruitandveg, as is agreeableness.

## Buying_cheap is not.

mod5 |> summary() |> coef()

inspect(AAfit14b, what = "cor.ov")|> corrplot::corrplot()
lavCor(AAfit14b) |> corrplot::corrplot()



#### Latent model only

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

mod6 <- lm(data=dat2 , 
FruitAndVeg ~ AA + SN + RISK +
  as.numeric(PCBbuyingcheap) + as.numeric(PCBbehaviour) + age + housing2 + 
  Extroversion + Openness + Agreeableness + Concientiousness + EmotionalStability + 
  as.numeric(SNadvice) + imd10) 


modelsummary::modelsummary(list(mod5,mod6), estimate="{estimate} p={p.value}")


## These are essentially the same variable, so why are the models so different?
plot(as.numeric(dat2$AAtotal), dat2$AA)
cor.test(as.numeric(dat2$AAtotal), dat2$AA)

## SN
plot(as.numeric(dat2$SNtotal), dat2$SN)
cor.test(as.numeric(dat2$SNtotal), dat2$SN)

## RISK
plot(as.numeric(dat2$RISKheart) + as.numeric(dat2$RISKobesity), dat2$RISK)
cor.test(as.numeric(dat2$SNtotal), dat2$SN)


#### The latent variables are bascially the same as just adding up values..

#### So why are the regression models so different??



