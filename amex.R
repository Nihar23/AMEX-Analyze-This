
#DATA EXPLORATION

for(i in 1:60129){
  a=c(train$major1[i],train$major2[i],train$major3[i],train$major4[i],train$major5[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    train$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    train$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    train$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    train$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    train$pred[i]="Cosmos"
  }
}

for(i in 1:21207){
  a=c(leader$major1[i],leader$major2[i],leader$major3[i],leader$major4[i],leader$major5[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    leader$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    leader$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    leader$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    leader$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    leader$pred[i]="Cosmos"
  }
}


for(i in 1:60129){
  if(train$party_voted_past[i]==train$pred[i]){
    train$vote_hist[i]=1
  }
  else{
    train$vote_hist[i]=0
  }
}

for(i in 1:21207){
  if(leader$party_voted_past[i]==leader$pred[i]){
    leader$vote_hist[i]=1
  }
  else{
    leader$vote_hist[i]=0
  }
}

for(i in 1:60129){
  a=c(train$mvar6[i],train$mvar7[i],train$mvar8[i],train$mvar9[i],train$mvar10[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    train$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    train$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    train$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    train$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    train$pred[i]="Cosmos"
  }
}

for(i in 1:60129){
  a=c(train$mvar16[i],train$mvar17[i],train$mvar18[i],train$mvar19[i],train$mvar20[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    train$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    train$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    train$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    train$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    train$pred[i]="Cosmos"
  }
}

for(i in 1:60129){
  a=c(train$mvar11[i],train$mvar12[i],train$mvar13[i],train$mvar14[i],train$mvar15[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    train$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    train$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    train$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    train$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    train$pred[i]="Cosmos"
  }
}


for(i in 1:60129){
  a=c(train$mvar21[i],train$mvar22[i],train$mvar23[i],train$mvar24[i],train$mvar25[i])
  k=sort(a,decreasing = TRUE)
  if(k[1]==a[1]){
    train$pred[i]="Centaur"
  }
  if(k[1]==a[2]){
    train$pred[i]="Ebony"
  }
  if(k[1]==a[3]){
    train$pred[i]="Tokugawa"
  }
  if(k[1]==a[4]){
    train$pred[i]="Odyssey"
  }
  if(k[1]==a[5]){
    train$pred[i]="Cosmos"
  }
}
 
#NEW FEATURES
 
train$major1=train$mvar1+train$mvar6*0.8+train$mvar11*0.4+train$mvar16*0.5+train$mvar21*0.6
train$major2=train$mvar2+train$mvar7*0.8+train$mvar12*0.4+train$mvar17*0.5+train$mvar22*0.6
train$major3=train$mvar3+train$mvar8*0.8+train$mvar13*0.4+train$mvar18*0.5+train$mvar23*0.6
train$major4=train$mvar4+train$mvar9*0.8+train$mvar14*0.4+train$mvar19*0.5+train$mvar24*0.6
train$major5=train$mvar5+train$mvar10*0.8+train$mvar1*0.4+train$mvar20*0.5+train$mvar25*0.6

train$state_old=as.character(train$mvar32)
train$state_new=as.character(train$mvar33)
leader$state_old=as.character(leader$mvar32)
leader$state_new=as.character(leader$mvar33)

for(i in 1:60129){
  if(train$state_old[i]==train$state_new[i]){
    train$shift[i]=0
  }
  else{
    train$shift[i]=1
  }
}

for(i in 1:21207){
  if(leader$state_old[i]==leader$state_new[i]){
    leader$shift[i]=0
  }
  else{
    leader$shift[i]=1
  }
}

#Normalization

normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
} 

train_norm=as.data.frame(lapply(train[,37:41],normalize))
train[,37:41]=train_norm[,1:5]
leader_norm=leader
leader_norm=as.data.frame(lapply(leader[,36:40],normalize))
leader[,36:40]=train_norm
train_knn=train[1:40000,]
test_knn=train[40001:60129,]
train_knn$actual_vote=NULL
test_knn$actual_vote=NULL
train.labels=train$actual_vote[1:40000]
test.labels=train$actual_vote[40001:60129]

#boosting
library(adabag)
boosting(actual_vote~major1+major2+major3+major4+major5+mvar26+mvar27+mvar28+mvar29+mvar30+mvar31, train, boos = TRUE, mfinal = 100, coeflearn = 'Breiman')
a=boosting(actual_vote~major1+major2+major3+major4+major5+mvar26+mvar27+mvar28+mvar29+mvar30+mvar31, train, boos = TRUE, mfinal = 100, coeflearn = 'Breiman')
pred <- predict.boosting(a,newdata=leader)
pred <- predict.boosting(a,newdata=leader)
pred <- predict.boosting(a,newdata=leader)
pred$class=as.factor(pred$class)
levels(train$actual_vote)
levels(pred$class)
sub=data.frame(citi=leader$citizen_id,vote=pred$class)
