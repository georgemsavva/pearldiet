library(gtsummary)
library(ggplot2)
library(data.table)


if(!exists("dat")) {
  source("makeData.R")
  dat <- getData()
}


#' ## Distribution of fruit and vegetable outcomes

#' There are surprisingly high numbers for both questions.

table(dat$Fruit)

table(dat$Veg)

#' Summing both and turning into categories for tabulation
table(dat$FruitAndVegCat)

#' ## Table of number of fruit and veg by each question / item
#' 
#' This table is intended just for a quick look at distributions.  The tests are likely inappropriate.  It could be adapted into a 'table 1'
predictorvarnames <- names(dat)[40:67]
## Very quick descriptive analysis with chi-square p-values
## Contingency tables with each variable and the fruit and veg categories
setDT(dat)
dat[,.SD,.SDcols = c("age","diet","imd10","ethnicity","housing",
                     predictorvarnames, "FruitAndVegCat")] |> 
  tbl_summary(by="FruitAndVegCat") |> 
  add_p(test=list(all_categorical() ~ "chisq.test"))


#' ## Index of multiple deprivation

#' There is no evidence that the average number of friut and veg is linked to IMD.

ggplot(dat) + aes(x=imd10, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$imd10, dat$FruitAndVeg, method="spearman")


#cor.test(dat$Concientiousness, dat$PCBtotal, method="spearman")

#' ## Regression modelling

#' As well as SEM I did some traditional regression modelling because it's easier to understand, and to validate the SEM results.

#' Here I added the AA and PCB scores together into a single variable each, and treated every outcome as numeric.
#' 
#' I just threw in everything together, so the results should be interpreted as the effect of each variable, for fixed values of the others.

form1 <- FruitAndVeg ~ 
  AAtotal + 
  PCBtotal +  
  as.numeric(SNadvice) + as.numeric(SNdoctor) + as.numeric(SNfamily)  + as.numeric(SNfriends) + 
  RISKtotal +
  age + housing2 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  + 
  imd10

(mod1 <- lm(data=dat, form1)) |> summary()

#' The model fit is OK surprisingly.  Not too much multicollinearity is indicated and the residual distributions are not too bad.
performance::check_model(mod1)

#' This model suggests that the only independent effect is for the sum of the 'affective attitude' variables.  So we can plot this to see a clear relationship:
ggplot(dat) + aes(x=AAtotal, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$AAtotal, dat$FruitAndVeg, method="spearman")

#' There is a correlation of 0.6 (p<0.0001) between AAtotal and Fruit and Veg consumption

#' The model suggests that given AA, there is no effect of any other variable in the model.

## Model without affective attitude

## Run it without AA
form2 <- FruitAndVeg ~ 
  #AAtotal + 
  PCBtotal +  
  as.numeric(SNadvice) + as.numeric(SNdoctor) + as.numeric(SNfamily)  + as.numeric(SNfriends) + 
  RISKtotal +
  age + housing2 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  + 
  imd10

(mod2 <- lm(data=dat, form2)) |> summary()

#' Without AA in the model, PCB takes over as 'very significant'.  We also have some effect of 'agreeableness'

#' Does this suggest that the AA is the mediator of the PCB variable?


library(ggplot2)
ggplot(dat) + aes(x=PCBtotal, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$PCBtotal, dat$FruitAndVeg, method="spearman")

#' Yes, univariately a very significant effect of PCB, but this disappears when we adjust for AA

#' ## Model without AA or PCB
 
#' With everything included in the model, 
 
form3 <- FruitAndVeg ~ 
  #AAtotal + 
  #PCBtotal +  
  as.numeric(SNadvice) + 
  as.numeric(SNdoctor) + 
  as.numeric(SNfamily)  + as.numeric(SNfriends) + 
  RISKtotal +
  age + housing2 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  + 
  imd10

(mod3 <- lm(data=dat, form3)) |> summary()

#' Without only personality and exogenous variables, Agreeableness is borderline significant but nothing else is.
form4 <- FruitAndVeg ~ 
  # AAtotal + 
  # PCBtotal +  
  # as.numeric(SNadvice) + as.numeric(SNdoctor) + as.numeric(SNfamily)  + as.numeric(SNfriends) + 
  # RISKtotal +
  age + housing2 + 
  imd10 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  

(mod4 <- lm(data=dat, form4)) |> summary()

#' ## There is just about a significant correlation there.
ggplot(dat) + aes(x=Agreeableness, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$Agreeableness, dat$FruitAndVeg, method="spearman")

## Those other factors don't really explain a lot of the 'agreeableness' effect

#' ## Individual items

#' So if AA is the only independent scale, and appears to mediate the PCB scale, what happens to the individual items?

form5 <- FruitAndVeg ~ 
  as.numeric(AApleasant) + 
  as.numeric(AAconvenient) +
  #as.numeric(AAenjoyable) + 
  as.numeric(AAinline) + 
  as.numeric(AAimportant) + 
  as.numeric(EvalAttitude) + 
  as.numeric(PCBeatingeasy) +
  as.numeric(PCBbuyingcheap) +  
  as.numeric(PCBbehaviour) +  
  as.numeric(PCBpreparingeasy) +  
  as.numeric(PCBfindingtimeeasy) +  
  
  
  as.numeric(SNadvice) + as.numeric(SNdoctor) + as.numeric(SNfamily)  + as.numeric(SNfriends) + 
  RISKtotal +
  age + housing2 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  + 
  imd10

(mod5 <- lm(data=dat, form5)) |> summary()
table(dat$AApleasant, dat$AAenjoyable)

#' We have some items going off in different directions, which suggests some collinearity.

performance::check_collinearity(mod5)

with(dat,cor(data.frame(as.numeric(AApleasant),
    as.numeric(AAconvenient),
    as.numeric(AAenjoyable), 
    as.numeric(AAinline), 
    as.numeric(AAimportant), 
    as.numeric(EvalAttitude),  
    as.numeric(PCBeatingeasy),
    as.numeric(PCBbuyingcheap),  
    as.numeric(PCBbehaviour),  
    as.numeric(PCBpreparingeasy),  
    as.numeric(PCBfindingtimeeasy) ), method="spearman",use="complete.obs")) |> 
  corrplot::corrplot(method = "number")

#' The affective attitude items are very concordant, with many Spearman's correlations greater than 0.5
#' The 'attitute' question is similar but reverse coded.
#' 
#' We can see the questions around 'pleasant' and 'enjoyable' are the same thing, so remove one of those.
#' 
#' The perceived behavioural control questions are similar except for 'behaviour' (..entirely my choice) and 'cheap' seem to correlate reasonably well with these.  
#' These two questions correlate less well with any of the others.


form5 <- FruitAndVeg ~ 
  I(AAtotal - PCBtotal) +
  #as.numeric(EvalAttitude) + 
  #as.numeric(PCBeatingeasy) +
  as.numeric(PCBbuyingcheap) +  
  as.numeric(PCBbehaviour) +  
  #as.numeric(PCBpreparingeasy) +  
  #as.numeric(PCBfindingtimeeasy) +  
  as.numeric(SNadvice) +
  I( as.numeric(SNdoctor) + as.numeric(SNfamily)  + as.numeric(SNfriends)) + 
  RISKtotal +
  age + housing2 + 
  Extroversion + 
  Openness + 
  Agreeableness + 
  Concientiousness + 
  EmotionalStability  + 
  imd10

(mod5 <- lm(data=dat, form5)) |> summary()
table(dat$AApleasant, dat$AAenjoyable)


lm(data=dat , FruitAndVeg ~ as.numeric(AApleasant)) |> summary()
lm(data=dat , FruitAndVeg ~ as.numeric(AAconvenient)) |> summary()
lm(data=dat , FruitAndVeg ~ as.numeric(AAenjoyable)) |> summary()
lm(data=dat , FruitAndVeg ~ as.numeric(AAinline)) |> summary()
lm(data=dat , FruitAndVeg ~ as.numeric(AAimportant)) |> summary()

full_model <- lm(data=remove_missing(dat) , FruitAndVeg ~ as.numeric(AApleasant) + 
                      as.numeric(AAconvenient) + 
                      as.numeric(AAenjoyable) + 
                      as.numeric(AAinline) + 
                      as.numeric(AAimportant))


bm <- step(full_model , direction="backward", scope = formula(~ .))
summary(bm)

bothm <- step(full_model , direction="both", scope = formula(~ .))
summary(bothm)


## Any one of these questions removes the need for the others to be in the model?



### All the AA variables explain about the same amount of variance in the main outcome when considered on their own...

