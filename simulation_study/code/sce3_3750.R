library(snowfall)

setwd("/home/pgrad2/2158844v")

seeds = 1:10000

ash.func <- function(N, seed)
{
  set.seed(seed)
  hunger = rnorm(N)
  wanting = rnorm(N)
  liking = rnorm(N)
  rrvfood = rnorm(N)
  rest.eating = rnorm(N)
  dhbn = rnorm(N)
  
  Z<-matrix(cbind(hunger,wanting,liking,rrvfood,rest.eating,dhbn), ncol = 6)
  
  Bl<-matrix(c(B1 = 1.5, B2 = 1.25, B3 = 1, B4 =  0.85, B5 = 0.75, B6 = 0), nrow = 6)
  Bh<-matrix(c(B1 = 1.5,B2 = 1,B3 = 0.75,B4 = 0,B5 = 0,B6 = 0), nrow  = 6)
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
  lin.predh<-rnorm(N,mean = Z%*%Bh,sd = 1)
  
  Yl<-cbind(Z,lin.predl)
  Y.trainl<-as.data.frame(Yl)
  names(Y.trainl)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predl")
  
  Yh<-cbind(Z,lin.predh)
  Y.trainh<-as.data.frame(Yh)
  names(Y.trainh)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predh")
  
  model.ctreel<-ctree(lin.predl~., data = Y.trainl, control= ctree_control(mincriterion = 0.95))
  model.treel<-rpart(lin.predl~., data = Y.trainl, control = rpart.control(cp = 0.001))
  model.treel2<-rpartXse(lin.predl~., minsplit = 20,cp = 0.001,data = Y.trainl)
  model.lml<-lm(lin.predl~., data = Y.trainl)
  model.ctreeh<-ctree(lin.predh~., data = Y.trainh, control = ctree_control(mincriterion = 0.95))
  model.treeh<-rpart(lin.predh~., data = Y.trainh, cp = 0.001)
  model.treeh2<-rpartXse(lin.predh~., minsplit = 20,cp = 0.001,data = Y.trainh)
  model.lmh<-lm(lin.predh~., data = Y.trainh)
  
  set.seed(seed+100)
  hunger = rnorm(N)
  wanting = rnorm(N)
  liking = rnorm(N)
  rrvfood = rnorm(N)
  rest.eating = rnorm(N)
  dhbn = rnorm(N)
  
  Z<-matrix(cbind(hunger,wanting,liking,rrvfood,rest.eating,dhbn), ncol = 6)	  

  Bl<-matrix(c(B1 = 1.5, B2 = 1.25, B3 = 1, B4 =  0.85, B5 = 0.75, B6 = 0), nrow = 6)
  Bh<-matrix(c(B1 = 1.5,B2 = 1,B3 = 0.75,B4 = 0,B5 = 0,B6 = 0), nrow  = 6)
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
  lin.predh<-rnorm(N,mean = Z%*%Bh,sd = 1)
  
  Yl<-cbind(Z,lin.predl)
  Y.testl<-as.data.frame(Yl)
  names(Y.testl)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predl")
  
  Yh<-cbind(Z,lin.predh)
  Y.testh<-as.data.frame(Yh)
  names(Y.testh)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predh")
  
  
  #Unpruned tree 
  #model.treel$frame
  frame<-model.treel$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.treel<-sum(leaf)
  
  #model.treeh$frame
  frame<-model.treeh$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.treeh<-sum(leaf)
  
  pred.tree<-predict(model.treel,newdata = Y.testl,type = "vector")
  MNTl<-mean((pred.tree-Y.testl$lin.predl)^2)
  
  pred.tree<-predict(model.treeh,newdata = Y.testh,type = "vector")
  MNTh<-mean((pred.tree-Y.testh$lin.predh)^2)
  
  ############################################################################################
  # Pruned tree 
  # cptable object from the rpart
  #model.treel$cptable
  #model.treeh$cptable
  #minimum cross validation error 
  mcv1<-min(model.treel$cptable[,"xerror"])
  mcv2<-min(model.treeh$cptable[,"xerror"])
  #location of minimum in CP table
  loc1<-which.min(model.treel$cptable[,"xerror"])
  term.ptreel<-loc1
  loc2<-which.min(model.treeh$cptable[,"xerror"])
  term.ptreeh<-loc2
  # the tree with the minimum cross validation error
  #model.treel$cptable[loc1,]
  #model.treeh$cptable[loc2,]
  # Extract the cp choice
  cp.choice1<-model.treel$cptable[loc1,"CP"]
  ptree1<-prune(model.treel, cp = cp.choice1)
  cp.choice2<-model.treeh$cptable[loc2,"CP"]
  ptree2<-prune(model.treeh, cp = cp.choice2)
  
  pred.ptree<-predict(ptree1, newdata = Y.testl)
  MNPl<-mean((pred.ptree-Y.testl$lin.predl)^2)
  
  pred.ptree<-predict(ptree2, newdata = Y.testh)
  MNPh<-mean((pred.ptree-Y.testh$lin.predh)^2)
  
  ######################################################################################
  
  # Pruned 1-SE rule Regression tree
  pred2.ptree<-predict(model.treel2, newdata = Y.testl)
  MNPl2<-mean((pred2.ptree - Y.testl$lin.predl)^2)
  
  pred2.ptree<-predict(model.treeh2, newdata = Y.testh)
  MNPh2<-mean((pred2.ptree - Y.testh$lin.predh)^2)
  
  frame<-model.treel2$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.ptreel2<-sum(leaf)
  
  frame<-model.treeh2$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.ptreeh2<-sum(leaf)
  
  # Conditional Inference tree
  term.ctreel<-length(nodeapply(model.ctreel, ids = nodeids(model.ctreel, terminal = TRUE), FUN = function(x) info_node(x)))
  term.ctreeh<-length(nodeapply(model.ctreeh, ids = nodeids(model.ctreeh, terminal = TRUE), FUN = function(x) info_node(x)))

  pred.ctree<-predict(model.ctreel,newdata = Y.testl,type = "response")
  MNCl<-mean((pred.ctree-Y.testl$lin.predl)^2)

  pred.ctree<-predict(model.ctreeh,newdata = Y.testh,type = "response")
  MNCh<-mean((pred.ctree-Y.testh$lin.predh)^2)
  ######################
  # Linear Regression 
  pred.lm<-predict(model.lml, newdata = Y.testl)
  MNLl<-mean((pred.lm - Y.testl$lin.predl)^2)
   
  pred.lm<-predict(model.lmh, newdata = Y.testh)
  MNLh<-mean((pred.lm - Y.testh$lin.predh)^2)
  
  ######################################################################################
  list(term.treel,  term.ptreel,  term.ptreel2, term.ctreel,  
       term.treeh,  term.ptreeh,  term.ptreeh2, term.ctreeh, 
       MNTl, MNPl, MNPl2, MNCl,
       MNTh, MNPh, MNPh2, MNCh, MNLl, MNLh)
}

sfInit(parallel = TRUE, cpus = 24)
sfLibrary(rpart); sfLibrary(partykit); sfLibrary(DMwR)
sfExport("ash.func", "seeds")
result = sfSapply( 1:length(seeds), function(i) ash.func(750, seeds[i])   )
sfStop()

save(result, file="simRESULTS3750.RData")