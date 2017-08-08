library(snowfall)

setwd("/home/pgrad2/2158844v")

seeds = 1:10000

sim.func <- function(N, seed)
{
  set.seed(seed)
  hunger = rnorm(N)
  wanting = rnorm(N)
  liking = rnorm(N)
  rrvfood = rnorm(N)
  rest.eating = rnorm(N)
  dhbn = rnorm(N)
  
  p1<-ifelse(hunger <= 1.66882020912573 & liking <= -0.274476950084893 & rrvfood <= -1.26102685858924 , -1.883154552371264, 0)
  p2<-ifelse(hunger <= 1.66882020912573 & liking <= -0.274476950084893 & rrvfood > -1.26102685858924 , -0.30493633248167, 0)
  p3<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & rrvfood <= -1.26102685858924 , -0.314120757471661, 0) 
  p4<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & rrvfood > -1.26102685858924 , 0.254995927719034, 0) 
  p5<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & dhbn <= -1.12102685858924 , -0.0864231757471661, 0) 
  p6<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & dhbn > -1.12102685858924 , 2.234995625719034, 0) 
  p7<-ifelse(hunger > 1.66882020912573 , 1.35130003798171, 0)
  mu<-p1+p2+p3+p4+p5+p6+p7
  
  y<-rnorm(N, mean = mu, sd  = 1)
  test.data<-as.data.frame(cbind(y,hunger,wanting,liking,rrvfood,rest.eating,dhbn))
  names(test.data)<-c("y","hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn") 
  
  set.seed(seed+100)
  hunger = rnorm(N)
  wanting = rnorm(N)
  liking = rnorm(N)
  rrvfood = rnorm(N)
  rest.eating = rnorm(N)
  dhbn = rnorm(N)
  
  p1<-ifelse(hunger <= 1.66882020912573 & liking <= -0.274476950084893 & rrvfood <= -1.26102685858924 , -1.883154552371264, 0)
  p2<-ifelse(hunger <= 1.66882020912573 & liking <= -0.274476950084893 & rrvfood > -1.26102685858924 , -0.30493633248167, 0)
  p3<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & rrvfood <= -1.26102685858924 , -0.314120757471661, 0) 
  p4<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & rrvfood > -1.26102685858924 , 0.254995927719034, 0) 
  p5<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & dhbn <= -1.12102685858924 , -0.0864231757471661, 0) 
  p6<-ifelse(hunger <= 1.66882020912573 & liking > -0.274476950084893 & dhbn > -1.12102685858924 , 2.234995625719034, 0) 
  p7<-ifelse(hunger > 1.66882020912573 , 1.35130003798171, 0)
  mu<-p1+p2+p3+p4+p5+p6+p7
  y<-rnorm(N, mean = mu, sd  = 1)
  train.data<-as.data.frame(cbind(y,hunger,wanting,liking,rrvfood,rest.eating,dhbn))
  names(train.data)<-c("y","hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn") 
  
  model.ctree<-ctree(y~., data = train.data, control = ctree_control(mincriterion = 0.95))
  model.tree<-rpart(y~., data = train.data, control = rpart.control(cp = 0.01))
  model.tree2<-rpartXse(y~., minsplit = 20, cp = 0.01, data = train.data) 
  model.lm<-lm(y~., data = train.data)
  
  # Linear Regression 
  
  pred.lm<-predict(model.lm, newdata = test.data, type = "response")
  MSE<-mean((pred.lm - test.data$y)^2)
  msece<- MSE
  
  # Continuous Y - MSE
  
  # Conditional Inference Tree
  pred.ctree<-predict(object = model.ctree,newdata = test.data)
  MSE<-mean((pred.ctree-test.data$y)^2)
  msecd<-(MSE)
  
  #Unpruned Tree
  pred.tree<-predict(object = model.tree,newdata = test.data,type = "vector")
  MSE<-mean((pred.tree-test.data$y)^2)
  mseca<-(MSE)
  
  # Pruned 1-SE rule
  pred.tree2<-predict(object = model.tree2,newdata = test.data,type = "vector")
  MSE<-mean((pred.tree2-test.data$y)^2)
  msecc<-(MSE)
  
  # Pruned Tree
  # Minimum cross validation error 
  mcv1<-min(model.tree$cptable[,"xerror"])
  # Location of minimum in CP table
  loc1<-which.min(model.tree$cptable[,"xerror"])
  term.ptree<-model.tree$cptable[loc1, "nsplit"] +1
  
  # The tree with the minimum cross validation error
  # Extract the cp choice
  cp.choice1<-model.tree$cptable[loc1,"CP"]
  ptree<-prune(model.tree, cp = cp.choice1)
  
  pred.ptree<-predict(object = ptree, newdata = test.data, type = "vector")
  MSE<-mean((pred.ptree-test.data$y)^2)
  msecb<-MSE
  
  tree.frame<-model.tree[1]
  # Number of Terminal nodes
  frame<-tree.frame$frame
  leaf<-ifelse(frame[1]=="<leaf>",1,0)
  term.tree<-sum(leaf)
  
  #Pruned Tree 1-SE
  library(rpart)
  
  # Number of Terminal nodes
  frame<-model.tree2$frame
  leaf<-ifelse(frame[1]=="<leaf>",1,0)
  term.ptree2<-sum(leaf)
  
  # Conditional Inference Tree
  term.ctree<-length(nodeapply(model.ctree, ids = nodeids(model.ctree, terminal = TRUE), FUN = function(x) info_node(x)))
  
  list(term.tree, term.ptree, term.ptree2, term.ctree, mseca, msecb, msecc, msecd, msece)
}

sfInit(parallel = TRUE, cpus = 24)
sfLibrary(rpart); sfLibrary(partykit); sfLibrary(DMwR)
sfExport("sim.func", "seeds")
result = sfSapply( 1:length(seeds), function(i) sim.func(250, seeds[i])   )
sfStop()

save(result, file="simRESULTS250s01.RData")