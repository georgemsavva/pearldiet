#' ---
#' title: Descriptive and regression analysis
#' ---

library(gtsummary)
library(ggplot2)
library(data.table)


if(!exists("dat")) {
  source("makeData.R")
  dat <- getData()
}

knitr::opts_chunk$set(warning = FALSE, message = FALSE)


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
dat[,.SD,.SDcols = c("age","diet","imd10","ethnicity","housing",
                     predictorvarnames, "FruitAndVegCat")] |> 
  tbl_summary(by="FruitAndVegCat") |> 
  add_p(test=list(all_categorical() ~ "chisq.test"))


#' ## Index of multiple deprivation

#' There is no evidence that the average number of friut and veg is linked to IMD.

ggplot(dat) + aes(x=imd10, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$imd10, dat$FruitAndVeg, method="spearman")


#' ## Regression modelling

#' As well as SEM I did some traditional regression modelling because it's easier to understand, and to validate the SEM results.

#' Here I added the AA and PCB scores together into a single variable each, and treated every outcome as numeric.
#' 
#' I just threw in everything together in a single model, so the results should be interpreted as the effect of each variable, for fixed values of the others.

#' This model suggests that the only independent effect is for the sum of the 'affective attitude' variables.  So we can plot this to see a clear relationship:
ggplot(dat) + aes(x=AAtotal, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$AAtotal, dat$FruitAndVeg, method="spearman")

#' There is a correlation of 0.6 (p<0.0001) between AAtotal and Fruit and Veg consumption

#' The model suggests that given AA, there is no effect of any other variable in the model.

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
#' 
#' Does this suggest that the AA is the mediator of the PCB variable?
#' 


library(ggplot2)
ggplot(dat) + aes(x=PCBtotal, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$PCBtotal, dat$FruitAndVeg, method="spearman")

#' Yes, univariately a very significant effect of PCB, but this disappears when we adjust for AA
#'
#' If we look at the correlation between the PCB and AA totals, there is a strong negative correlation

ggplot(dat) + aes(x=PCBtotal, y=AAtotal) + geom_point(position="jitter")
cor.test(dat$PCBtotal, dat$AAtotal, method="spearman")

#' ## Model without AA or PCB
 
#' With everything included in the model, 
 
form3 <- FruitAndVeg ~ 
  #AAtotal + 
  PCBtotal +  
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

#' There is just about a significant between 'agreeableness' and fruit and veg intake, but not very strong
ggplot(dat) + aes(x=Agreeableness, y=FruitAndVeg) + geom_point(position="jitter")
cor.test(dat$Agreeableness, dat$FruitAndVeg, method="spearman")

#' Those other factors don't really explain a lot of the 'agreeableness' effect

#' ## Individual items
#'
#' So if AA is the only independent scale, and appears to mediate the PCB scale, what happens to the individual items?
#'
# NOT DONE YET
#

# 
# 
# 
# #' The affective attitude items are very concordant, with many Spearman's correlations greater than 0.5
# #' The 'attitute' question is similar but reverse coded.
# #' 
# #' We can see the questions around 'pleasant' and 'enjoyable' are the same thing, so remove one of those.
# #' 
# #' The perceived behavioural control questions are similar except for 'behaviour' (..entirely my choice) and 'cheap' seem to correlate reasonably well with these.  
# #' These two questions correlate less well with any of the others.


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

performance::check_model(mod5)

#' ## Final model

#' The final model has strong effects of the affective attitutde and perceived self-efficacy questions, but not much evidence for anything else.

tbl_regression(mod5)
