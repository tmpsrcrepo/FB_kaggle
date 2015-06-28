library(foreach)
library(doParallel)
library(data.table)
library(bit64)

setwd("~/Documents/fb challenge")
bids1<-fread(input='bids.csv',header=TRUE)
test=read.csv('test.csv')
train=read.csv('train.csv')
create_df<-function(df,bids){
  rows=foreach(i = 1:length(df),.combine=rbind) %do% {
    row=data.table(bidder_id=as.character(df[i,1]),auction_id='a',tot_auctions=rep(0,1),tot_bids=rep(0,1),num_bids=rep(0,1),num_countries=rep(0,1),num_ip=rep(0,1),num_url=rep(0,1),avg_time=rep(0,1),min_time=rep(0,1),max_time=rep(0,1),num_device=rep(0,1),num_merch=rep(0,1))
    temp=bids[which(bids$bidder_id==as.character(df[i,1])),]
    temp_by_auction=split(temp,temp$auction)
    n_ac=length(unique(temp$auction))
    n_bd=nrow(temp)
    df1<-foreach(j = 1:length(temp_by_auction),.combine=rbind) %do% {
      tmp_df=temp_by_auction[[j]]
      row$tot_auctions=n_ac
      row$num_bids=nrow(tmp_df)
      row$tot_bids=n_bd
      row$auction_id=tmp_df$auction[1]
      row$num_countries=length(unique(tmp_df$country))
      row$num_ip=length(unique(tmp_df$ip))
      row$num_url=length(unique(tmp_df$url))
      row$avg_time=mean(tmp_df$time)
      row$max_time=max(tmp_df$time)
      row$min_time=min(tmp_df$time)
      row$num_device=length(unique(tmp_df$device))
      row$num_merch=length(unique(tmp_df$merchandise))
      row
    }
    df1
  }
}

auction_avg<-function(auc_list,bids){
  rows=foreach(i = 1:length(auc_list),.combine=rbind) %do% {
    row=data.table(auction_id=auc_list[i],tot_bids=rep(0,1),num_countries=rep(0,1),num_ip=rep(0,1),num_url=rep(0,1),avg_time=rep(0,1),min_time=rep(0,1),max_time=rep(0,1),num_device=rep(0,1),num_merch=rep(0,1))
    temp=bids[which(bids$auction==as.character(auc_list[i])),]
    row$tot_bids=nrow(temp)
    row$num_countries=length(unique(temp$country))
    row$num_ip=length(unique(temp$ip))
    row$num_url=length(unique(temp$url))
    row$avg_time=mean(temp$time)
    row$max_time=max(temp$time)
    row$min_time=min(temp$time)
    row$num_device=length(unique(temp$device))
    row$num_merch=length(unique(temp$merchandise))
    row
  }
}


df_wrapper<-function(df,bids){
  cluster=makeCluster(3)
  registerDoParallel(cluster)
  train_df1=create_df(df,bids)
  stopCluster(cluster)
  train_df1
}

auction_wrapper<-function(bids){
  cluster=makeCluster(6)
  registerDoParallel(cluster)
  auc_list=unique(bids$auction)
  auctions__=auction_avg(auc_list,bids)
  stopCluster(cluster)
  auctions__
}


tmp_train<-df_wrapper(train,bids)
tmp_train

tmp_test<-df_wrapper(test,bids)
tmp_test

tmp_auction<-auction_wrapper(bids)
tmp_auction

attach(mtcars)
summary(mtcars)
aggdata = aggregate(mtcars,by=list(cyl,vs),FUN=mean,na.rm=TRUE)
print(aggdata)
detach(mtcars)

a<-bids[which(bids$bidder_id=='8dac2b259fd1c6d1120e519fb1ac14fbqvax8'),]
b<-as.data.frame(table(a$country))
t=b[which.max(b$Freq),]$Var1


cluster=makeCluster(3)
registerDoParallel(cluster)


country1<-foreach(i =1:nrow(train),.combine=rbind) %do% {
  a = bids[which(bids$bidder_id==as.character(train[i,1])),]
  row=c()
  if(nrow(a)==0){
      row=c(as.character(train[i,1]),'us')
  }
  else{
  b<-data.frame(table(a$country))
  t=b[which.max(b$Freq),]
  row =c(as.character(train[i,1]),as.character(t[1,1]))
  }
  row
}
country1


country_test<-foreach(i =1:nrow(test),.combine=rbind) %do% {
  a = bids[which(bids$bidder_id==as.character(test[i,1])),]
  row=c()
  if(nrow(a)==0){
    row=c(as.character(test[i,1]),'us')
  }
  else{
    b<-data.frame(table(a$country))
    t=b[which.max(b$Freq),]
    row =c(as.character(test[i,1]),as.character(t[1,1]))
  }
  row
}
country_test




#stopCluster(cluster)
