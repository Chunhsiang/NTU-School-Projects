more<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\4. ChargeRecord 1018分類好.csv", sep=",", header=T)

tuo.hao<- charge.data[charge.data[,2]>= 1000, ]
plot(log(tuo.hao[,2]), log(tuo.hao[,4]), ylim= c(5, 11), xlim = c(6.5, 16))
plot(no.crazy[,2], no.crazy[,4])


#少了瘋子
no.crazy<- charge.data[charge.data[,2]>= 1000 & charge.data[,2]<= 100000, ]
ttuo.hao<- ctime.plot(more, no.crazy)





#較理性
#占全部比例: sum(stuo.hao[,2])/ sum(charge.data[,2])
#stuo.hao<- tuo.hao[log(tuo.hao[,2])<=8.3 & log(tuo.hao[,4])<=6.1,]
stuo.hao<- tuo.hao[tuo.hao[,2]<= 5000 & tuo.hao[,4]<= 500,]
ttcain<- ctime.plot(more, stuo.hao)


#土豪們
#占全部比例: sum(tuo.hao[!(tuo.hao[,1]%in% stuo.hao[,1]),2])/ sum(charge.data[,2])
jtuo.hao<- no.crazy[!(no.crazy[,1] %in% stuo.hao[,1]) ,]
jtcain<- ctime.plot(more, jtuo.hao )

jtuo.dused<- diamond[diamond[,1]%in% jtuo.hao[,1],]
jtuo.fdused<- dataform(jtuo.dused)
jtuo.5d<- dcost.5p(jtuo.fdused)

pie.data5(jtuo.5d)

times.up<- c("26", "32")
currency.up<- c("24", "31", "64", "137")
energy.up<- c("30", "52", "125")
bundle.up<- c("10", "25", "150", "145", "170", "701", "180", "62", "140", "63", "143")
else.up<- c("6", "shop_2_12", "123")


j.event.matrix<- get.event.matrix( jtuo.5d,times.up, currency.up, energy.up, bundle.up, else.up)
summary(j.event.matrix)
jematrix.cbind<- 
pie(colSums(j.event.matrix), main = "rich.D_spending")



rich.bigcol<- big.col(rich, rich.fdused)





ctime.plot<- function(more, tuo.hao){
c.tuohao<- more[more[,1] %in% tuo.hao[,1], c(1,2,3,5,6)]
c.tuohao<- find.date(c.tuohao, 4,5)[,-c(4,5)]

tuohao.lg<- rbind(very.rich.profit, rich.profit)
c.tuohao.mlg<- c.tuohao

for( k in 1: length(c.tuohao[,1])){
 for( i in 1: length(tuohao.lg[,1])){
  if(c.tuohao.mlg[k,1]== tuohao.lg[i,1]){
    c.tuohao.mlg[k, 4]<- c.tuohao.mlg[k, 4]- tuohao.lg[i,3]
  }
}
}

#return(c.tuohao.mlg)
ag.afterlg<- aggregate(.~ mday, data= c.tuohao.mlg[,3:4], mean )
#plot(ag.afterlg[,1:2], xlim=c(0, 50), type="l")
return(ag.afterlg)
}

