library(haven)
library(data.table)
library(readxl)



getData <- function() {
  
  dat <- read_excel("T2 dietary preferences 08July24 -clean data.xlsx", sheet = "FOR R")
  
  names(dat)[1] <- "ID"
  
  # 1.	Affective attitude – 5-point scale
  # A.	For me, eating 5 portions of fruit and vegetables per day is pleasant:
  #   B.	For me, eating 5 portions of fruit and vegetables per day is convenient:
  #   C.	For me, eating 5 portions of fruit and vegetables per day is enjoyable:
  #   D.	For me, eating 5 portions of fruit and vegetables per day is in line with my food choice:
  #   E.	For me, eating 5 portions of fruit and vegetables per day is important:
  
  dat$AApleasant = dat$`Pleasant;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  dat$AAconvenient = dat$`Convenient;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  dat$AAenjoyable = dat$`Enjoyable;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  dat$AAinline = dat$`Food choice;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  dat$AAimportant = dat$`Important;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  
  #   PCB – self-efficacy– 5-point scale
  #   1.	For me, eating 5 portions of fruit and vegetables per day is easy:
  #   2.	For me, buying fruit and vegetables is cheap:
  #   3.	For me, preparing and cooking fruit and vegetables is easy:
  #   4.	For me, finding time to eat 5 portions of fruit and vegetables a day is easy:
  
  dat$PBCeatingeasy = dat$`Easy;For me, eating 5 portions of fruit and vegetables per day is:`|> as.numeric() |> ordered()
  dat$PBCbuyingcheap = dat$`Cheap;For me, buying fruit and vegetables is:`|> as.numeric() |> ordered()
  dat$PBCpreparingeasy = dat$`Easy;For me, preparing and cooking fruit and vegetables is:`|> as.numeric() |> ordered()
  dat$PBCfindingtimeeasy = dat$`Easy;For me, finding time to eat 5 portions of fruit and vegetables a day is:`|> as.numeric() |> ordered()
  
  # PBC - control
  # PBC – behavioural control– 5-point scale
  
  dat$PBCbehaviour = dat$`Agree;It is entirely my choice to eat 5 portions of fruit and vegetables a day:`|> as.numeric() |> ordered()
  
  # Evaluative attitude– 5-point scale
  
  dat$EvalAttitude = dat$`Response;My attitude towards eating 5 portions of fruit and vegetables a day:`|> as.numeric() |> ordered()
  
  # HCSI
  
  dat$HSCIconscious = dat$`Response;I think of myself as health conscious`|> as.numeric() |> ordered()
  dat$HSCIconcerned = dat$`Response;I think of myself as someone who is concerned about the consequences of what I eat`|> as.numeric() |> ordered()
  
  # SN - social norms
  
  dat$SNfamily = dat$`Response;My family think I should eat 5 portions of fruit and vegetables per day`|> as.numeric() |> ordered()
  dat$SNfriends = dat$`Response;My friends think I should eat 5 portions of fruit and vegetables per day`|> as.numeric() |> ordered()
  dat$SNdoctor = dat$`Response;My doctor and midwife think I should eat 5 portions of fruit and vegetables per day`|> as.numeric() |> ordered()
  
  # Risk theory
  
  dat$RISKcancer = dat$`Cancer;Eating 5 portions of fruit and vegetable per day will reduce the chances of me getting`|> as.numeric() |> ordered()
  dat$RISKheart = dat$`Heart disease;`|> as.numeric() |> ordered()
  dat$RISKdiabetes = dat$`Diabetes;`|> as.numeric() |> ordered()
  dat$RISKobesity = dat$`Obesity;`|> as.numeric() |> ordered()
  dat$RISKbaby = dat$`Response;Eating 5 portions of fruit and vegetables per day while I am pregnant is good for my baby.`|> as.numeric() |> ordered()
  
  # For these, 'don't know' is an interesting outcome
  ## I had coded these don't knows as 3 but it doesn't make sense so I'm recoding with the 'no'.
  # dat$RISKcancer |> levels() <- c("1","2","3")
  # dat$RISKcancer[is.na(dat$RISKcancer)] <- 3
  # 
  # dat$RISKheart |> levels() <- c("1","2","3")
  # dat$RISKheart[is.na(dat$RISKheart)] <- 3
  # 
  # dat$RISKdiabetes |> levels() <- c("1","2","3")
  # dat$RISKdiabetes[is.na(dat$RISKdiabetes)] <- 3
  # 
  # dat$RISKobesity |> levels() <- c("1","2","3")
  # dat$RISKobesity[is.na(dat$RISKobesity)] <- 3
  # 
  # dat$RISKbaby |> levels() <- c("1","2","3")
  # dat$RISKbaby[is.na(dat$RISKbaby)] <- 3
  
  dat$RISKcancer[is.na(dat$RISKcancer)] <- 2
  dat$RISKheart[is.na(dat$RISKheart)] <- 2
  dat$RISKdiabetes[is.na(dat$RISKdiabetes)] <- 2
  dat$RISKobesity[is.na(dat$RISKobesity)] <- 2
  dat$RISKbaby |> levels() <- c("1","2")
  dat$RISKbaby[is.na(dat$RISKbaby)] <- 2
  
  
  # Perceived Health
  
  dat$SRH = dat$`Response;How would you rate your health today?`|> as.numeric() |> ordered()
  
  # Advice and information
  
  dat$SNadvice = dat$`Response;Have you been given any advice about your diet since you became pregnant?`|> as.numeric() |> ordered()
  
  dat$Extroversion= dat$`Extroversion score;`|> as.numeric() 
  dat$Openness = dat$`Openness score;`|> as.numeric() 
  dat$Agreeableness = dat$`Agreeableness score;`|> as.numeric() 
  dat$Concientiousness = dat$`Concientiousness score;`|> as.numeric() 
  dat$EmotionalStability = dat$`Emotional stability score;`|> as.numeric() 
  
  # Demographic etc
  
  dat$ethnicity <- factor(dat$`What is your ethnicity?;`)
  dat$diet <- factor(dat$`What type of diet do you usually eat?;`)
  dat$age <- dat$`What age are you now?;` |> as.numeric()
  dat$housing <- factor(dat$`What is your housing arrangement?;`)
  dat$housing2 <- factor(dat$housing=="Owner-occupier")
  
  
  predictorvarnames <- names(dat)[40:67]
  
  ## Outcomes
  
  dat$Fruit <- dat$`;How many portions of fruit (fresh, tinned and frozen) did you eat yesterday? An apple, or an orange, a banana or a cupful of grapes counts as a portion`
  dat$Veg <- dat$`;How many portions of vegetables (fresh, tinned or frozen) did you eat yesterday? 80 grams or a cupful of vegetables counts as a portion. Beans, peas, tomatoes, seeds and nuts should be counted but not potatoes`
  dat$FruitAndVeg <- dat$Fruit + dat$Veg
  
  ### For deprivation index, higher is better.
  ### file was received on Friday, September 27, 2024 8:09 AM
  # Merge the deprivation index file
  townsend_data <- fread(file =  "2019-deprivation-by-postcode - Locations removed.csv", skip = 1)
  names(townsend_data)[1] <- "ID"
  setDT(dat)
  dat <-townsend_data[,.(ID,imd10=`Index of Multiple Deprivation Decile`, income10 = `Income Decile`)][dat, on="ID"]
  
  
  # There's a lot of liars in this dataset
  table(dat$FruitAndVeg)
  
  # Stratify by quartiles of the outcome
  dat$FruitAndVegCat <- cut(dat$FruitAndVeg , breaks = c(0,2,4,6,12), labels = c("0-2","3-4","5-6","6+"))
  dat$Fruit |> table()
  dat$Veg |> table()

  # Make an ordered version of fruit and veg variables
  dat$FruitAndVego <- ordered(dat$FruitAndVeg)
  dat$Fruito <- ordered(dat$Fruit)
  dat$Vego <- ordered(dat$Veg)
  

  ## Make sum scales for some of the constructs
  dat[ , AAtotal := as.numeric(AApleasant) + 
         as.numeric(AAconvenient) + 
         as.numeric(AAenjoyable) + 
         as.numeric(AAinline) + 
         as.numeric(AAimportant)]
    
  dat[ , PBCtotal := as.numeric(PBCpreparingeasy) + 
         as.numeric(PBCbehaviour) + 
         as.numeric(PBCeatingeasy) + 
         as.numeric(PBCfindingtimeeasy) +
         as.numeric(PBCbuyingcheap)]
  
  dat[ , RISKtotal := as.numeric(RISKheart) + 
         as.numeric(RISKdiabetes) + 
         as.numeric(RISKcancer)]
  
  dat[ , SNtotal := as.numeric(SNfamily) + 
         as.numeric(SNfriends) + 
         as.numeric(SNdoctor) + 
         as.numeric(SNadvice)  ]
  
  
  
  dat
  
}

