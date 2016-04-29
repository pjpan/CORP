# 信息熵数值；
calDeviance = function(y, pred) {
  -2*sum(y*log(pred) + (1-y)*log(1-pred))
}

DeafultDeviance = function(y) {
  calDeviance(y, mean(y))
}

# stepwise logistic regression -forward variable
AddVar = function(xframe, y, current_vars, current_dev, candidate_vars) {
  best_dev = current_dev
  newvar = NULL
  for(var in candidate_vars) {
    active=c(current_vars, var)
    xf = xframe[,active]
    if(length(active) > 1) {
      model = glmnet(as.matrix(xf), y,
                     alpha=0, lambda=0.001, family="binomial")
    } else {
      model =glm.fit(xframe[,active], y, family=binomial(link="logit"))
    }
    moddev = deviance(model)
    if(moddev < best_dev) {
      newvar = var
      best_dev = moddev
    }
  }
  improvement = 1 - (best_dev/current_dev)
  list(current_vars= c(current_vars, newvar),
       current_dev = best_dev,
       improvement = improvement)
}

# 逐步logistic regression；
StepWiseRidge = function(data, vars, yVar, min_improve=1e-6) {
  current_vars=c()
  candidate_vars = vars
  devs = numeric(length(vars))
  improvement = numeric(length(vars))
  current_dev = DeafultDeviance(data[[yVar]])
  do_continue=TRUE
  while(do_continue) {
    iter = AddVar(data, data[[yVar]], current_vars, current_dev, candidate_vars)
    current_vars = iter$current_vars
    current_dev = iter$current_dev
    
    count = length(current_vars)
    devs[count] = current_dev
    improvement[count] = iter$improvement
    candidate_vars = setdiff(vars, current_vars)
    #  print(current_vars)
    do_continue = (length(candidate_vars) > 0) && (iter$improvement > min_improve)
  }
  list(current_vars = current_vars, deviances=devs, improvement=improvement)
}

# gbm funciotn的结果；
GBMPredict <- function(formula
                       , data
                       , interaction.depth = 2
                       , keep.data =F
                       , n.trees = 2500
                       , n.minobsinnode = 10
                       , shrinkage = 0.05
                       , distribution = "adaboost"
                       , cv.folds = 3
                       , bag.fraction = 0.6               
                       , train.fraction=0.7 
) { gbm <- gbm(formula
               ,data = data
               ,distribution = distribution
               ,keep.data = keep.data 
               ,interaction.depth = interaction.depth
               ,shrinkage = shrinkage
               ,n.trees = n.trees
               ,n.minobsinnode = n.minobsinnode
               ,cv.folds = cv.folds
               ,bag.fraction = bag.fraction
               ,train.fraction = train.fraction
)  

return(gbm)
}





