library("tidyverse")
library(ggplot2)

csv_data <- read.csv(file = 'ArticleLevel-RegData-ALLSA_Xc_1_NData_655386_LONGXCIP2.csv', header= TRUE)
#csv_data
years <- sort(unique(csv_data$Yp))
years <-years[35:length(years)]
#years

# Filtering yearly NSAp above z-score
yearlyCIP_above_z = c()
i = 1
while (i <= length(years)){
  filtered_data <- csv_data %>% filter(Zp >= 0 & Yp== years[i]) %>% select(NSAp)
  data_length <- length(filtered_data$NSAp)
  yearlySA_above_z <- c(yearlySA_above_z, data_length)
  i = i + 1
}

#yearlySA_above_z

# Filtering yearly NSAp below z-score
yearlySA_below_z = c()
i = 1
while (i <= length(years)){
  filtered_data <- csv_data %>% filter(Zp < 0 & Yp== years[i]) %>% select(NSAp)
  data_length <- length(filtered_data$NSAp)
  yearlySA_below_z <- c(yearlySA_below_z, data_length)
  i = i + 1
}

yearlySA_below_z


# Yearly citations above and below average for SA (min: 1, max:4)
oneAboveAvgSA <- c()
oneBelowAvgSA <- c()
twoAboveAvgSA <- c()
twoBelowAvgSA <- c()
threeAboveAvgSA <- c()
threeBelowAvgSA <- c()
fourAboveAvgSA <- c()
fourBelowAvgSA <- c()

j = 1
while (j <= length(years)){
  filtered_data <- csv_data %>% filter(Yp== years[j]) %>% select(NSAp,Zp)
  
  oneAboveSACount = 0
  oneBelowSACount = 0
  twoAboveSACount = 0
  twoBelowSACount = 0
  threeAboveSACount = 0
  threeBelowSACount = 0
  fourAboveSACount = 0
  fourBelowSACount = 0
  
  i = 1
  while (i <= length(filtered_data$NSAp)) {
    if (filtered_data$NSAp[i] == 1) {
      if(filtered_data$Zp[i] >= 0) { oneAboveSACount = oneAboveSACount + 1}
      else { oneBelowSACount = oneBelowSACount + 1}
    }
    else if (filtered_data$NSAp[i] == 2) {
      if(filtered_data$Zp[i] >= 0){ twoAboveSACount = twoAboveSACount + 1}
      else { twoBelowSACount = twoBelowSACount + 1}
    }
    else if (filtered_data$NSAp[i] == 3) {
      if(filtered_data$Zp[i] >= 0){ threeAboveSACount = threeAboveSACount + 1}
      else{ threeBelowSACount = threeBelowSACount + 1 }
    }
    else if (filtered_data$NSAp[i] >= 4){
      if(filtered_data$Zp[i]>=0){fourAboveSACount = fourAboveSACount + 1}
      else { fourBelowSACount = fourBelowSACount + 1}
    }
    
    i= i + 1
  }
  oneAboveAvgSA <- c(oneAboveAvgSA, oneAboveSACount/yearlySA_above_z[j])
  oneBelowAvgSA <- c(oneBelowAvgSA, oneBelowSACount/yearlySA_below_z[j])
  
  twoAboveAvgSA <- c(twoAboveAvgSA, twoAboveSACount/yearlySA_above_z[j])
  twoBelowAvgSA <- c(twoBelowAvgSA, twoBelowSACount/yearlySA_below_z[j])
  
  threeAboveAvgSA <- c(threeAboveAvgSA, threeAboveSACount/yearlySA_above_z[j])
  threeBelowAvgSA <- c(threeBelowAvgSA, threeBelowSACount/yearlySA_below_z[j])
  
  fourAboveAvgSA <- c(fourAboveAvgSA, fourAboveSACount/yearlySA_above_z[j])
  fourBelowAvgSA <- c(fourBelowAvgSA, fourBelowSACount/yearlySA_below_z[j])
  
  j = j + 1
}

df <- data.frame(years, oneAboveAvgSA, oneBelowAvgSA, twoAboveAvgSA, twoBelowAvgSA,
                 threeAboveAvgSA, threeBelowAvgSA, fourAboveAvgSA, fourBelowAvgSA)

colors <- c("1(M)" = "black", "2(X)" = "#b3c8f2", "3(X)"  = "#6693ed","4(X)"="royalblue" )
citations <- c("Above-average citations" = 1, "Below-average citations" = 2)

plot1 <- ggplot(data, aes(x=years)) +
  theme_bw() +
  geom_line(aes(y = oneAboveSA, color = "1(M)", linetype="Above-average citations")) +
  geom_line(aes(y = oneBelowSA, color = "1(M)", linetype="Below-average citations"))+
  geom_line(aes(y = twoAboveSA, color = "2(X)", linetype="Above-average citations")) +
  geom_line(aes(y = twoBelowSA, color = "2(X)", linetype="Below-average citations"))+
  geom_line(aes(y = threeAboveSA, color = "3(X)", linetype="Above-average citations")) +
  geom_line(aes(y = threeBelowSA, color = "3(X)", linetype="Below-average citations"))+
  geom_line(aes(y = fourAboveSA, color = "4(X)", linetype="Above-average citations")) +
  geom_line(aes(y = fourBelowSA, color = "4(X)", linetype="Below-average citations"))+
  scale_color_manual(values=colors)

scale_x_continuous(breaks = round(seq(1980, 2020, by = 5),1))+ 
  scale_y_continuous( breaks = round(seq(0, 1, by = 0.2),1)) +
  labs(tag='Cross - Topic (SA)',color = "# categories per publication", linetype= NULL)+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1),
        plot.tag=element_text(angle=-90),plot.tag.position=c(1, 0.5),axis.title.x=element_blank(),axis.title.y=element_blank())

plot1
