library(xgboost)
library(tidyverse)

Diamonds <- diamonds

y1 <- Diamonds$cut
var.levels <- levels(y1)
y = as.integer(y1) - 1


noOutcome <- Diamonds[,-2]
x = noOutcome[,c('depth', 'table', 'price', 'x', 'y')]
var.names <- names(x)
x = as.matrix(x)

params <- list(
  "objective" = "multi:softprob"
  ,"eval_metric" = "mlogloss"
  ,"num_class" = length(table(y))
  ,"eta" = .5
  ,"max_depth" = 3
  ,"nthread" = 8
)

cv.nround = 250

bst.cv <- xgb.cv(param = params, data = x, label = y
                 , nfold = 5, nrounds = cv.nround
                 , missing = NA, prediction = TRUE
                 )

nrounds = which.min(bst.cv$evaluation_log$test_mlogloss_mean)
bst.cv$evaluation_log[nrounds,]


DiamondClass <- xgboost(param = params, data = x, label = y
                        ,nrounds = nrounds, missing = NA)


xgb.importance(var.names, model = DiamondClass)

xgb.save(DiamondClass, "diamonds.model")

DiamondClassInfo <- list(
  var.names = var.names
  ,var.levels = var.levels
)

save(DiamondClassInfo, file = 'DiamondClassInfo.rda')


