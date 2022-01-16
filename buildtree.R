# decision tree basics
library(tidyverse)
library(readr)
micro_dataset <- read_csv("data/micro-dataset.csv")
micro_dataset$X1 <- NULL
micro_dataset$...1 <- NULL

df <- micro_dataset

# verwijderen deelletters
df <- df %>% select(-starts_with('deelletter_'))
# verwijderen 
df <- df %>% select(-starts_with('deelscore_'))
df <- df %>% select(-starts_with('spreiding_score_'))
df <- df %>% select(-starts_with('spreiding_deelscore_'))
df <- df %>% select(-contains('_met_score_'))
df <- df %>% select(-contains('_2018'))
df <- df %>% select(-contains('_2016'))


vars <- names(df)
# eerste colom is case_id
case_id <- vars[1]
# laatste colom is target
target <- vars[2] #tail(vars)[1]
inputs <- vars[c(3:15)]

f <- as.formula(paste(target, " ~ ", paste(inputs, collapse="+") ))
#f <- as.formula(paste(target, " ~ ."))

library(rpart)

m <- rpart(f,data = df, control = rpart.control(cp = 0.015), method="class")


summary(m)
printcp(m)
plotcp(m)

require(rpart.plot)

rpart.plot(m ,
           branch.type = 5, 
           type = 1
           )

rpart.rules(m)
