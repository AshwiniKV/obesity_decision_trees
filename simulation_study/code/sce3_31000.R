library(snowfall)

setwd("/home/pgrad2/2158844v")

seeds = 1:10000

ash.func <- function(N, seed)
{
  set.seed(seed)
  x1 = rnorm(N)
  x2 = rnorm(N)
  x3 = rnorm(N)
  x4 = rnorm(N)
  x5 = rnorm(N)
  x6 = rnorm(N)
  x7 = rnorm(N)
  x8 = rnorm(N)
  x9 = rnorm(N)
  x10 = rnorm(N)
  
  Z<-matrix(cbind(x1,x2,x3,x4,x5,x6,x7,x8, x9,x10), ncol = 10)
  
  Bl<-matrix(c(B1 = 1.5, B2 = 1.25, B3 = 1, B4 =  0.85, B5 = 0.75, B6 = 0.65, B7 = 0.5, B8 = 0, B9 = 0,B10 = 0), nrow = 10)
  Bh<-matrix(c(B1 = 1.5,B2 = 1,B3 = 0.75,B4 = 0,B5 = 0,B6 = 0,B7 = 0,B8 = 0, B9 = 0, B10 = 0), nrow  = 10)
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
  lin.predh<-rnorm(N,mean = Z%*%Bh,sd = 1)
  
  Yl<-cbind(Z,lin.predl)
  Y.trainl<-as.data.frame(Yl)
  names(Y.trainl)<-c("x1", "x2","x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10","lin.predl")
  
  Yh<-cbind(Z,lin.predh)
  Y.trainh<-as.data.frame(Yh)
  names(Y.trainh)<-c("x1", "x2","x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10","lin.predh")
  
  model.ctreel<-ctree(lin.predl~., data = Y.trainl, control = ctree_control(mincriterion = 0.95))
  model.treel<-rpart(lin.predl~., control = rpart.control(cp = 0.01), data = Y.trainl)
  model.treel2<-rpartXse(lin.predl~.,minsplit = 20 ,cp = 0.01,data = Y.trainl)
  model.lml<-lm(lin.predl~., data = Y.trainl)
  model.ctreeh<-ctree(lin.predh~., data = Y.trainh, control = ctree_control(mincriterion = 0.95))
  model.treeh<-rpart(lin.predh~., control = rpart.control(cp = 0.01), data = Y.trainh)
  model.treeh2<-rpartXse(lin.predh~.,minsplit = 20,cp = 0.01 ,data = Y.trainh)
  model.lmh<-lm(lin.predh~., data = Y.trainh)
  
  set.seed(seed+100)
  x1 = rnorm(N)
  x2 = rnorm(N)
  x3 = rnorm(N)
  x4 = rnorm(N)
  x5 = rnorm(N)
  x6 = rnorm(N)
  x7 = rnorm(N)
  x8 = rnorm(N)
  x9 = rnorm(N)
  x10 = rnorm(N)
  
  Z<-matrix(cbind(x1,x2,x3,x4,x5,x6,x7,x8, x9,x10), ncol = 10)
  
  Bl<-matrix(c(B1 = 1.5, B2 = 1.25, B3 = 1, B4 =  0.85, B5 = 0.75, B6 = 0.65, B7 = 0.5, B8 = 0, B9 = 0,B10 = 0), nrow = 10)
  Bh<-matrix(c(B1 = 1.5,B2 = 1,B3 = 0.75,B4 = 0,B5 = 0,B6 = 0,B7 = 0,B8 = 0, B9 = 0, B10 = 0), nrow  = 10)
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
  lin.predh<-rnorm(N,mean = Z%*%Bh,sd = 1)
  
  Yl<-cbind(Z,lin.predl)
  Y.testl<-as.data.frame(Yl)
  names(Y.testl)<-c("x1", "x2","x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10" ,"lin.predl") 
  Yh<-cbind(Z,lin.predh)
  Y.testh<-as.data.frame(Yh)
  names(Y.testh)<-c("x1", "x2","x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10" ,"lin.predh") 
  #Unpruned tree 
  
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
  
  # Linear Regression 
  
  pred.lm<-predict(model.lml, newdata = Y.testl, type = "response")
  MNLl<-mean((pred.lm-Y.testl$lin.predl)^2)
  
  pred.lm<-predict(model.lmh, newdata = Y.testh, type = "response")
  MNLh<-mean((pred.lm-Y.testh$lin.predh)^2)
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
  
  ################################################################# Pruned 1-SE rule Regression tree
  pred2.ptree<-predict(model.treel2, newdata = Y.testl)
  MNPl2<-mean((pred2.ptree - Y.testl$lin.predl)^2)
  
  pred2.ptree<-predict(model.treeh2, newdata = Y.testh)
  MNPh2<-mean((pred2.ptree-Y.testh$lin.predh)^2)
  
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
  #######################################################################################
  ######################################################################################
  list(term.treel,  term.ptreel,  term.ptreel2, term.ctreel,  
       term.treeh,  term.ptreeh,  term.ptreeh2, term.ctreeh, 
       MNTl, MNPl, MNPl2, MNCl,
       MNTh, MNPh, MNPh2, MNCh, MNLl, MNLh)
}

sfInit(parallel = TRUE, cpus = 24)
sfLibrary(rpart); sfLibrary(partykit); sfLibrary(DMwR)
sfExport("ash.func", "seeds")
result = sfSapply( 1:length(seeds), function(i) ash.func(1000, seeds[i])   )
sfStop()

save(result, file="simRESULTS31000cont01.RData")