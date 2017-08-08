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
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
 
  Yl<-cbind(Z,lin.predl)
  Y.trainl<-as.data.frame(Yl)
  names(Y.trainl)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predl")
  
  model.ctreel<-ctree(lin.predl~., data = Y.trainl, control= ctree_control(mincriterion = 0.95))
  model.treel<-rpart(lin.predl~., data = Y.trainl, control = rpart.control(cp = 0.001))
  model.treel2<-rpartXse(lin.predl~., minsplit = 20,cp = 0.001,data = Y.trainl)
  model.lml<-lm(lin.predl~., data = Y.trainl)
 
  set.seed(seed+100)
  hunger = rnorm(N)
  wanting = rnorm(N)
  liking = rnorm(N)
  rrvfood = rnorm(N)
  rest.eating = rnorm(N)
  dhbn = rnorm(N)
  
  Z<-matrix(cbind(hunger,wanting,liking,rrvfood,rest.eating,dhbn), ncol = 6)	  

  Bl<-matrix(c(B1 = 1.5, B2 = 1.25, B3 = 1, B4 =  0.85, B5 = 0.75, B6 = 0), nrow = 6)
  
  lin.predl<-rnorm(N,mean = Z%*%Bl,sd = 1)
  
  Yl<-cbind(Z,lin.predl)
  Y.testl<-as.data.frame(Yl)
  names(Y.testl)<-c("hunger", "wanting","liking", "rrvfood", "rest.eating", "dhbn", "lin.predl")
  
  #Unpruned tree 
  #model.treel$frame
  frame<-model.treel$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.treel<-sum(leaf)
  
  pred.tree<-predict(model.treel,newdata = Y.testl,type = "vector")
  MNTl<-mean((pred.tree-Y.testl$lin.predl)^2)
  
  ############################################################################################
  # Pruned tree 
  # cptable object from the rpart
  #model.treel$cptable
  #minimum cross validation error 
  mcv1<-min(model.treel$cptable[,"xerror"])
  #location of minimum in CP table
  loc1<-which.min(model.treel$cptable[,"xerror"])
  term.ptreel<-loc1
  # the tree with the minimum cross validation error
  #model.treel$cptable[loc1,]
  # Extract the cp choice
  cp.choice1<-model.treel$cptable[loc1,"CP"]
  ptree1<-prune(model.treel, cp = cp.choice1)
  
  pred.ptree<-predict(ptree1, newdata = Y.testl)
  MNPl<-mean((pred.ptree-Y.testl$lin.predl)^2)
  
  ######################################################################################
  
  # Pruned 1-SE rule Regression tree
  pred2.ptree<-predict(model.treel2, newdata = Y.testl)
  MNPl2<-mean((pred2.ptree - Y.testl$lin.predl)^2)
  
  frame<-model.treel2$frame
  levels<-frame[1]
  # Number of Terminal nodes
  leaf<-ifelse(levels=="<leaf>",1,0)
  term.ptreel2<-sum(leaf)
  
  # Conditional Inference tree
  term.ctreel<-length(nodeapply(model.ctreel, ids = nodeids(model.ctreel, terminal = TRUE), FUN = function(x) info_node(x)))
 
  pred.ctree<-predict(model.ctreel,newdata = Y.testl,type = "response")
  MNCl<-mean((pred.ctree-Y.testl$lin.predl)^2)

  ######################
  # Linear Regression 
  pred.lm<-predict(model.lml, newdata = Y.testl)
  MNLl<-mean((pred.lm - Y.testl$lin.predl)^2)
   
  ######################################################################################
  list(term.treel,  term.ptreel,  term.ptreel2, term.ctreel,  
      MNTl, MNPl, MNPl2, MNCl, MNLl)
}

sfInit(parallel = TRUE, cpus = 24)
sfLibrary(rpart); sfLibrary(partykit); sfLibrary(DMwR)
sfExport("ash.func", "seeds")
result = sfSapply( 1:length(seeds), function(i) ash.func(250, seeds[i])   )
sfStop()

save(result, file="simRESULTS3250cont01.RData")