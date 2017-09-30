setwd("/home3/ntueconfbra1/group_like")
install.packages("ggplot2")
library(ggplot2)


whole_data <- read.csv("C:\\Users\\Chang`s\\Desktop\\final_ABT.csv", sep=",", header= T)


GetBoxValue<- function(page_data, group_bound, group_proportion){
  #sum_user_count<- sum(page_data$user_count)
  n <- length(page_data)
  iter <-1
  for(i in 1:length(group_bound)){
    while(iter< n){
      if(page_data[iter] <= group_bound[i] ){
        group_proportion[i] = group_proportion[i]+1#/ sum_user_count
        iter = iter+1
      }else{
        break
      }
    }
  }
  return(group_proportion)
}

typhoon_plot_pdf<- function(rain_data, x_lab, topic){
  
  box_point <- seq(from= min(rain_data), to= max(rain_data), by=(max(rain_data)- min(rain_data))/15 )
  box_point
  box_value <- rep(0, length(box_point))
  box_value
  rain_pdf<- GetBoxValue(rain_data, box_point, box_value)
  rain_pdf
  plot(x= box_point, y= round(rain_pdf/sum(rain_pdf), digits = 4), type= "l", xlab = x_lab , ylab = "density" , main=topic)
  
}
rain_data<- sort(whole_data$rainfall)
typhoon_plot_pdf(rain_data, "累積降雨量", "累積降雨量機率密度圖")

wind_data<- sort(whole_data$avg_wind)
typhoon_plot_pdf(wind_data, "平均最大陣風", "平均最大陣風機率密度圖")

rain_data<- sort(whole_data$rainfall)
typhoon_plot_pdf(log(rain_data), "對數累積降雨量", "對數累積降雨量機率密度圖")
