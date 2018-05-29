1
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(choroplethr)
library(rworldmap)
library(RColorBrewer)
nation103<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=25f64d5125016dcd6aed42e50c972ed0")
nation104<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=4d3e9b37b7b0fd3aa18a388cdbc77996")
nation105<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=19bedf88cf46999da12513de755c33c6")
nation106<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=50e3370f9f8794f2054c0c82a2ed8c91")

nation104105<-inner_join(nation104,nation105,by=c("洲別","國別"))
nation105106<-inner_join(nation104105,nation106,by=c("洲別","國別"))
nationTotal<-inner_join(nation105106,nation103,by=c("洲別","國別"))
nationTotal$NaTotalForeigner<- rowSums(nationTotal[, c(-1 ,-2 )], na.rm = TRUE)
nationTopTen<-nationTotal$國別[head(order(nationTotal$NaTotalForeigner,decreasing = T),10)]
nationTopTen
TotalNumTopTen<-nationTotal$NaTotalForeigner[head(order(nationTotal$NaTotalForeigner,decreasing = T),10)]
TotalNumTopTen

college103<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=a6d1469f39fe41fb81dbfc373aef3331")
college104<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=8baeae81cba74f35cf0bb1333d3d99f5")
college105<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=1a485383cf9995da679c3798ab4fd681")
college106<-read_csv("https://quality.data.gov.tw/dq_download_csv.php?nid=6289&md5_url=883e2ab4d5357f70bea9ac44a47d05cc")

college104$"非學位生-大陸研修生"<-as.numeric(gsub("…","NA",college104$"非學位生-大陸研修生"))
college103$"非學位生-大陸研修生"<-as.numeric(gsub("…","NA",college103$"非學位生-大陸研修生"))
college104105<-inner_join(college104,college105,by=c("學校類型","學校代碼", "學校名稱"))
college105106<-inner_join(college104105,college106,by=c("學校類型","學校代碼", "學校名稱"))
collegeTotal<-inner_join(college105106,college103,by=c("學校類型","學校代碼", "學校名稱"))

collegeTotal$coTotalForeigner<- rowSums(collegeTotal[, c(-1 ,-2, -3 )], na.rm = TRUE)
collegeTopTen<-collegeTotal$"學校名稱"[head(order(collegeTotal$coTotalForeigner,decreasing = T),10)]
collegeTopTen
cTotalNumTopTen<-collegeTotal$coTotalForeigner[head(order(collegeTotal$coTotalForeigner,decreasing = T),10)]
cTotalNumTopTen

2
nationTotalTopTen<-nationTotal%>%group_by(國別)%>%
  summarise(NaTotalForeigner=sum(NaTotalForeigner))%>%arrange(desc(NaTotalForeigner))

nationTotalTopTen<-rbind(top_n(nationTotalTopTen,10),slice(nationTotalTopTen,11:n())%>%summarise(國別="other",NaTotalForeigner=sum(NaTotalForeigner)))
 
ggplot()+geom_bar(data=nationTotalTopTen,aes(x=`國別`, y=`NaTotalForeigner`),stat = "identity")

3
CountriesComparisionTable <- read_csv("C:/Users/aaa/Desktop/CountriesComparisionTable.csv")
CountriesComparisionTable<-rename(CountriesComparisionTable,`國別`=`Taiwan`)
nationmerge<-inner_join(CountriesComparisionTable,nationTotal,by="國別")
d <- data.frame(
  country=nationmerge$ISO3,
  value=nationmerge$NaTotalForeigner)

n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")

numCats<-50
palette=colorRampPalette(brewer.pal(n=9,name='Greens'))(numCats)
mapCountryData(n, nameColumnToPlot="value", mapTitle="World",catMethod = 'fixedWidth',numCats=numCats,colourPalette =palette,addLegend = T)

4
StudentgoabroadEC <- read.csv("C:/Users/aaa/Desktop/Student_RPT_07_all.csv")
StudentgoabroadEC<-filter(StudentgoabroadEC,學年度>102)

StudyCountryEC<-StudentgoabroadEC$對方學校.機構.國別.地區.
StudyCountryCountEC<-as.data.frame(table(StudyCountryEC))
StudyCountryTopTenEC<-StudyCountryCountEC$StudyCountryEC[head(order(StudyCountryCountEC$Freq,decreasing = T),10)]
StudyCountryTopTenEC
StudyCountryTopTenFreqEC<-StudyCountryCountEC$Freq[head(order(StudyCountryCountEC$Freq,decreasing = T),10)]
StudyCountryTopTenFreqEC

TwCollegeEC<-StudentgoabroadEC$學校名稱
TwCollegeCountEC<-as.data.frame(table(TwCollegeEC))
TwCollegeTopTenEC<-TwCollegeCountEC$TwCollegeEC[head(order(TwCollegeCountEC$Freq,decreasing = T),10)]
TwCollegeTopTenEC
TwCollegeTopTenFreqEC<-TwCollegeCountEC$Freq[head(order(TwCollegeCountEC$Freq,decreasing = T),10)]
TwCollegeTopTenFreqEC

5
StudyCountryECTopTen<-arrange(StudyCountryCountEC,desc(Freq))
StudyCountryECTopTen<-rbind(top_n(StudyCountryECTopTen,10),slice(StudyCountryECTopTen,11:n())%>%
                           summarise(StudyCountryEC="other",Freq=sum(Freq)))

ggplot()+geom_bar(data=StudyCountryECTopTen,aes(x=`StudyCountryEC`, y=`Freq`),stat = "identity")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1,vjust = 0.5))

6
StudyCountryCountEC<-rename(StudyCountryCountEC,`國別`=`StudyCountryEC`)
Countriesmerge<-inner_join(CountriesComparisionTable,StudyCountryCountEC,by="國別")
dd <- data.frame(
  country=Countriesmerge$ISO3,
  value=Countriesmerge$Freq)

nn <- joinCountryData2Map(dd, joinCode="NAME", nameJoinColumn="country")

numCats<-50
palette=colorRampPalette(brewer.pal(n=9,name='Greens'))(numCats)
mapCountryData(nn, nameColumnToPlot="value", mapTitle="World",catMethod = 'fixedWidth',numCats=numCats,colourPalette =palette,addLegend = T)


7
X105studyabroad <- read_csv("C:/Users/aaa/Desktop/105studyabroad.csv")
X105studyabroadNaTopTen<-X105studyabroad$國別[head(order(X105studyabroad$總人數,decreasing = T),10)]
X105studyabroadNaTopTen
X105studyabroadNaTopTenFreq<-X105studyabroad$總人數[head(order(X105studyabroad$總人數,decreasing = T),10)]
X105studyabroadNaTopTenFreq

8
studyabroadmerge<-inner_join(CountriesComparisionTable,X105studyabroad,by="國別")
ddd <- data.frame(
  country=studyabroadmerge$ISO3,
  value=studyabroadmerge$總人數)

nnn <- joinCountryData2Map(ddd, joinCode="NAME", nameJoinColumn="country")

numCats<-50
palette=colorRampPalette(brewer.pal(n=9,name='Greens'))(numCats)
mapCountryData(nnn, nameColumnToPlot="value", mapTitle="World",catMethod = 'fixedWidth',numCats=numCats,colourPalette =palette,addLegend = T)

9
TWnessNumber<-NULL
TWness<-NULL
for(i in 1:10){
  for(ii in 1:nrow(X105studyabroad)){
     if(nationTopTen[i]==X105studyabroad$國別[ii]){
       temp<-X105studyabroad$總人數[ii]
       TWnessNumber<-rbind(TWnessNumber,temp)
       break
     }
  } 
}
for(i in 1:10){
  for(ii in 1:nrow(X105studyabroad)){
    if(nationTopTen[i]==X105studyabroad$國別[ii]){
      temp2<-X105studyabroad$國別[ii]
      TWness<-rbind(TWness,temp2)
      break
    }
  } 
}
PeopleComeTW<-data.frame(`來台念書的外籍生主要來源國`=nationTopTen ,`來台念書的外籍生總數`= TotalNumTopTen)
TWnessGoAbroad<-data.frame(`台灣人留學國家`=TWness[,1],`留學此國的台灣人數`=TWnessNumber[,1])
PeopleComeTW
TWnessGoAbroad













