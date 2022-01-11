# decision tree basics
library(tidyverse)
library(readr)
micro_dataset <- read_csv("data/micro-dataset.csv")
micro_dataset$X1 <- NULL
df <- micro_dataset
vars <- names(df)
# eerste colom is case_id
case_id <- vars[1]
# laatste colom is target
target <- vars[2] #tail(vars)[1]
inputs <- vars[5:25]

f <- as.formula(paste(target, " ~ ", paste(inputs, collapse="+") ))

library(rpart)

m <- rpart(f,data = df, control = rpart.control(cp = 0.00001), method="class")

printcp(m)
plotcp(m)

require(rpart.plot)

rpart.plot(m, 
           branch.type = 5, 
           #type = 4
           )
