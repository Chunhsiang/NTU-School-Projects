options(digits = 22)
charge.data<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\code\\儲值資料.csv", sep="," , header=T)
diamond.ddata<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\code\\鑽石資料.csv", sep=",", header=T)
diamond<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\3. DiamondCost 1018分類好.csv",header=T, sep=",")
#charge.event<- charge.event[,-1]

di.charge.cor<- merge(diamond.ddata[,1:2], charge.data[,1:2], all=T)
di.charge.cor[is.na(di.charge.cor)]<- 0

cor(di.charge.cor[,2], di.charge.cor[,3])

very.rich<- charge.data[charge.data[,2]>= 10000,]
rich<- charge.data[charge.data[,2]>= 1000 &charge.data[,2]<=10000 ,]
regular<- charge.data[charge.data[,2]> 300 & charge.data[,2]<1000,]
once<- charge.data[charge.data[,2]<= 300,]








ld<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\2. Login.csv",sep=",", header=T)
ce<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\4_charge_record.csv", sep="," , header=T)


more<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\4. ChargeRecord 1018分類好.csv", sep=",", header=T)
oclock<- more[,13]

find.date<- function(Ctd, mm, dd){
  N.Ctd<- length(Ctd[,1])
  mday<- rep(0, N.Ctd)
  for(i in 1:N.Ctd){
    if(Ctd[i,mm]==8){
      mday[i]<- Ctd[i,dd]
    }else if(Ctd[i,mm]==9){
      mday[i]<- Ctd[i,dd]+31
    }else if (Ctd[i,mm]==10){
      mday[i]<- Ctd[i,dd]+61
    }
  }
  done<- cbind(Ctd, mday)
  return(done)
}
ce1<- find.date(ce, 5, 6)
charge.event<- ce1[,c(2,3,4,10)]
charge.event<- cbind(charge.event, oclock)
head(ce1[,2])

ld1<- find.date(ld, 3,4)
login.data<- ld1[,c(1,10,11)]


#最土豪
very.rich.e<- charge.event[charge.event[,1] %in% very.rich[,1] ,]
very.rich.l<- login.data[login.data[,1]%in% very.rich[,1], ]

very.rich.profit<- profit.time(login.data, charge.event, very.rich)
summary(very.rich.profit)

#rich開始時間、相對於他人，現金會導向的事件
rich.e<- charge.event[charge.event[,1] %in% rich[,1] ,]
rich.l<- login.data[login.data[,1]%in% rich[,1], ]
rich.profit<- profit.time(login.data, charge.event, rich)
tbric<- table(as.character(rich.profit[,2]))
plot(names(tbric), as.vector(tbric)/sum(tbric))

#regular 第一次儲值時間
profit.time<- function(login, charge, type){
 regular.e<- charge[charge[,1] %in% type[,1] ,]
 rich.l<- login[login[,1]%in% type[,1], ]
 rich.e<- charge[charge[,1] %in% type[,1] ,]
 
 fst.l<- rich.l[diff(c(0,rich.l[,1])) != 0, ]
 fst.l<- fst.l[order(fst.l[,1]),]
 fst.e<- rich.e[diff(c(0,rich.e[,1])) != 0, ]
 fst.e<- fst.e[order(fst.e[,1]),]
 ###
 rich.profit<- cbind(fst.l[,1], c(fst.e[,4]-fst.l[,3]))
 ###
 all.info<- cbind(fst.l[,1],c(fst.e[,4]-fst.l[,3]), fst.l[,3],fst.e[,2:4])
 names(all.info)<- c("ID","回本時間", "首次登入", "首儲管道","首儲金額","首儲日" )
 return(all.info)
 #return(rich.profit)
}

regular.profit<- profit.time(login.data, charge.event, regular)
tbreg<- table(as.character(regular.profit[,2]))
plot(names(tbreg), as.vector(tbreg)/sum(tbreg))
#_______________________________________________________

#once 第一次儲值時間
once.profit<- profit.time(login.data, charge.event, once)
tbonc<- table(as.character(once.profit[,2]))
plot(names(tbonc), as.vector(tbonc)/sum(tbonc))
#______________________________________________________

write.csv(rbind(very.rich.profit, rich.profit, regular.profit, once.profit), "首儲日與首次登入日比較")
