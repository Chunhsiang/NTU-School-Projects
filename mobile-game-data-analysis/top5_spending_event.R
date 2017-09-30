options(digits = 22)
diamond.event<- read.csv("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\data used\\3. DiamondCost 1018分類好.csv",header=T, sep=",")

get.mostdevent<- function(diamond.event){
agg.data<- diamond.event[,2:3]
names(agg.data)<- c("event", "count")
agg.data<- aggregate(.~event, data = agg.data, sum)
agg.data<- agg.data[order(agg.data[,2], decreasing = T), ]
return(agg.data)
}



#total.devent<- data.frame(agg.data[1:86, ], first.two[1:86,], last.two)
#write.csv(total.devent, "三群人花鑽事件累計")




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
  #done<- cbind(Ctd, mday)
  return(mday)
}

dataform<- function(diamond.event){
  tempp<- diamond.event[,c(1:3, 10, 11)]
  d.date<- find.date(tempp, 4, 5 )
  tempp[,4]<- d.date
  tempp[,5]<- diamond.event[,6]
  
  d.used<- tempp
  return(d.used)
}




dcost.5p<- function(d.used){
  options(digits = 5)
  d.used[,1]<- as.character(d.used[,1])
  tb1<- names(table(d.used[,1]))
  top5event<-data.frame(matrix(0, ncol = 5, nrow = length(tb1))) #will be characters putted in--> data frame
  top5perecent<-data.frame(matrix(0, ncol = 5, nrow = length(tb1))) 
  top5spend<- as.data.frame(matrix(0, ncol = 5, nrow = length(tb1)))
  for(i in 1: length(tb1)){
    temp1<- d.used[d.used[,1]==tb1[i], ]
    temp1[,2] <- as.character(temp1[,2])
    
    event.str<- names(table(temp1[,2]))
    spend<- rep(0, length(event.str))
    event.time<- rep(0, length(event.str))
    event.str<- data.frame(event.str, spend, event.time)
    for( k in 1: length(temp1[,1])){
      for(l in 1:length(event.str[,1])){
        if( temp1[k,2]== event.str[l,1]){
          event.str[l,2]<- event.str[l,2]+ temp1[k,3]
          event.str[l,3]<- event.str[l,3]+1
        }
      }
      
    }
    event.str<- event.str[order(event.str[,2], decreasing = T), ]
    event.str[,1]<- as.character(event.str[,1])
    for (m in 1:5) {
      top5event[i,m]<- event.str[m,1]
      top5spend[i,m]<- event.str[m,2]
      top5perecent[i,m]<- (top5spend[i,m])/ sum(event.str[,2])
    }
  }
  
  for(ll in 1:5){
    temp.final<- data.frame(top5event[,ll], top5spend[,ll], top5perecent[,ll])
    if(ll==1){
      final<- data.frame(temp.final)
    }else{
      final<- data.frame(final, temp.final)
    }
    
  }
  return(final)
}

pie.data5<- function(final){
  options(digits = 5)
  ppp<- final[,c(3,6,9,12,15)]
  ppp[is.na(ppp)]<-0
  ppp2<- ppp[,1]+ppp[,2]+ppp[,3]+ppp[,4]+ppp[,5]
  
  t.ppp1<-table(ppp2)
  t.ppp2<- as.numeric(names(t.ppp1))
  ct<- rep(0, 6)
  
  for (i in 1:length(t.ppp2)){
    if( t.ppp2[i]==1){
      ct[1]<- ct[1]+t.ppp1[i]
    }else if( 0.9<=t.ppp2[i] &&t.ppp2[i]<=1){
      ct[2]<- ct[2]+t.ppp1[i]
    }else if( 0.8<=t.ppp2[i] && t.ppp2[i]<=0.9){
      ct[3]<- ct[3]+t.ppp1[i]
    }else if( 0.7<=t.ppp2[i] && t.ppp2[i]<=0.8){
      ct[4]<- ct[4]+t.ppp1[i]
    }else if( 0.6<=t.ppp2[i] && t.ppp2[i]<=0.7){
      ct[5]<- ct[5]+t.ppp1[i]
    }else{ 
      ct[6]<- ct[6]+t.ppp1[i]
    }
  }
  names(ct)<- c("100%", "90幾%", "80幾%",  "70幾%",  "60幾%", "少於60")
  return(ct)
}

d5p.big.column<- function(d.used){
  options(digits = 5)
  d.used[,1]<- as.character(d.used[,1])
  tb1<- names(table(d.used[,1]))
  top5event<-data.frame(matrix(0, ncol = 5, nrow = length(tb1))) #will be characters putted in--> data frame
  top5perecent<-data.frame(matrix(0, ncol = 5, nrow = length(tb1))) 
  top5spend<- as.data.frame(matrix(0, ncol = 5, nrow = length(tb1)))
  for(i in 1: length(tb1)){
    temp1<- d.used[d.used[,1]==tb1[i], ]
    temp1[,2] <- as.character(temp1[,2])
    
    event.str<- names(table(temp1[,2]))
    spend<- rep(0, length(event.str))
    event.time<- rep(0, length(event.str))
    event.str<- data.frame(event.str, spend, event.time)
    for( k in 1: length(temp1[,1])){
      for(l in 1:length(event.str[,1])){
        if( temp1[k,2]== event.str[l,1]){
          event.str[l,2]<- event.str[l,2]+ temp1[k,3]
          event.str[l,3]<- event.str[l,3]+1
        }
      }
      
    }
    event.str<- event.str[order(event.str[,2], decreasing = T), ]
    event.str[,1]<- as.character(event.str[,1])
    for (m in 1:5) {
      top5event[i,m]<- event.str[m,1]
      top5spend[i,m]<- event.str[m,2]
      top5perecent[i,m]<- (top5spend[i,m])/ sum(event.str[,2])
    }
  }
  
  for(ll in 1:5){
    temp.final<- data.frame(top5event[,ll], top5spend[,ll], top5perecent[,ll])
    if(ll==1){
      final<- data.frame(temp.final)
    }else{
      final<- rbind(final, temp.final)
    }
    
  }
  return(final)
}


time.transfer<- function(data, day, time){
  
  daytime<- rep(0, length(time))
  for(i in 1: length(time)){
    daytime[i]<- day[i]+round(time[i]*0.01, digits = 2)
  }
  return(cbind(data, daytime))
}

event.table<- function(rich.5d){
  rich.etype<- c(NULL)
  for( i in c(1,4,7,10,13)){
    rich.etype<- c(rich.etype, as.character(rich.5d[,i]))
  }
  
  e.table<-table(rich.etype)
  return(e.table)
}

big.col<- function(rich, rich.fdused){
  #rich.5d.id<- cbind(rich[,1],rich.5d)
  r.bigcol<- d5p.big.column(rich.fdused)
  r.bigcol<- cbind(rep(rich[,1], 5), r.bigcol)
  return(r.bigcol)
}

correlation.list<- function(r.bigcol, dd.event, rich){
  corr.list<- rep(0, length(dd.event))
  for(k in 1:length(dd.event)){
    being25<- r.bigcol[r.bigcol[,2]== dd.event[k], ]
    being25<-being25[!is.na(being25[,1]),]
    
    charge25<- rich[rich[,1] %in% being25[,1],]
    corr.list[k]<- cor(charge25[,2], being25[,4])
  }
  return(corr.list)
}


individuals.d<- read.table("C:\\Users\\User\\Desktop\\大四課程\\行銷研究\\期末data\\code\\鑽石資料.csv", sep=",", header=T)


reg.matrix<- function(r.bigcol,regular.bigcol, event, rich, regular){
  ree.event <- regular.bigcol[regular.bigcol[,2]==event ,]
  ree.event <- ree.event[!is.na(ree.event[,1]), ]
  rii.event <- r.bigcol[r.bigcol[,2]==event ,]
  rii.event <- rii.event[!is.na(rii.event[,1]), ]
  
  ree.spend <- regular[regular[,1]%in% ree.event[,1], 2]
  rii.spend <- rich[rich[,1]%in% rii.event[,1], 2]
  
  up<- cbind(rii.event, rii.spend)
  names(up)<- c("ID", "事件", "事件總額","事件比例", "儲值金額")
  down<- cbind(ree.event, ree.spend)
  names(down)<- c("ID", "事件", "事件總額","事件比例", "儲值金額")
  matrix<- rbind(up, down)
  ids<- individuals.d[individuals.d[,1]%in% matrix[,1], 2]
  
  matrix<- cbind(matrix[,1:4], ids,matrix[,5])
  names(matrix)[5:6]<- c("總花鑽", "儲值金額" )
  
  #return(matrix)
  return(summary(lm(matrix[,6]~ matrix[,3]+ matrix[,5])))
}



event.names<- c("times.up", "currency.up", "energy.up", "bundle.up", "growth.up")
get.event.matrix<- function( rich.5d,times.up, currency.up, energy.up, bundle.up, else.up){
  r.event.matrix<- data.frame(matrix(0, nrow = length(rich.5d[,1]), ncol= 5))
  names(r.event.matrix)<- event.names
  for( k in c(1,4,7,10,13)){
    for( i in 1: length(rich.5d[,1])){
      if(rich.5d[i,k] %in% times.up){
        r.event.matrix[i,1]<-r.event.matrix[i,1]+ rich.5d[i, k+1]
      }else if(rich.5d[i,k] %in% currency.up){
        r.event.matrix[i,2]<-r.event.matrix[i,2]+ rich.5d[i, k+1]
      }else if(rich.5d[i,k] %in% energy.up){
        r.event.matrix[i,3]<-r.event.matrix[i,3]+ rich.5d[i, k+1]
      }else if(rich.5d[i,k] %in% bundle.up){
        r.event.matrix[i,4]<-r.event.matrix[i,4]+ rich.5d[i, k+1]
      }else if(rich.5d[i,k] %in% else.up){
        r.event.matrix[i,5]<-r.event.matrix[i,5]+ rich.5d[i, k+1]
      }
    }
  }
  return(r.event.matrix)
}
