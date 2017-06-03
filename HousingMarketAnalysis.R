source("config.R")
source("subarea.R")
df.housing<-read.csv("train.csv")
#9572/20899 = 45%

df.housing <-df.housing %>% select(-id)
df.housing$state[df.housing$state == 33] <- which.max(table(df.housing$state))
df.housing$build_year[df.housing$build_year == 20052009] <- 2007
df.housing[which(df.housing$build_year == 4965), "build_year"] = 1961


df.housing[is.na(df.housing[,154:292]),154:292]
df.missingvalues<- map_dbl(df.housing[,154:292], function(x){sum(is.na(x))})
df.missingvalues<- df.missingvalues[df.missingvalues>0]
as.data.frame(df.missingvalues)

names(df.missingvalues[grep("_500",names(df.housing),)])

colnames.500<-colnames(df.housing[,grep("_500$", names(df.housing))])

colnames.500 <- colnames.500[-grep("price_500",colnames.500)]
colnames.500 <- colnames.500[-grep("sqm_500",colnames.500)]

df.housing.500 <- df.housing[,c(colnames.500,"sub_area")]

#df.housing.500$quint <- bin(df.housing.500$green_part_500, method = "content")
#library(OneR)

df.housing.500<-df.housing.500 %>%
          #select(sub_area,office_count_500) %>%
          group_by(sub_area) %>%
          mutate(quantile = bin(green_part_500,nbins=10, method = "content"),mean= median(green_part_500)) %>%
          mutate(splitQuantile = as.numeric(sub(".*,(.*?)].*","\\1",quantile)))%>%
          #mutate(green_part_500_new=ifelse(splitQuantile== max(splitQuantile),mean,green_part_500))
          mutate(facLevel=as.numeric(factor(splitQuantile)),green_part_500=ifelse(is.na(splitQuantile),mean,green_part_500))


#Need to convert to function the above dplyr 
assignFac<- function(x,columns)
  {
  x<- "prom_part_500"
  y <- df.housing.500[,c(x,"sub_area")]
    temp.df<-y %>%
      group_by(sub_area) %>%
      mutate(x=ifelse(is.na(x),median(x),x)) %>%
      mutate(quantile = bin(x,nbins=10, method = "content")) %>%
      mutate(splitQuantile = as.numeric(sub(".*,(.*?)].*","\\1",quantile)))%>%
      mutate(facLevel=as.numeric(factor(splitQuantile)))
      return(temp.df$facLevel)
}


#assignFac("prom_part_500")

b<- df.housing.500[df.housing.500$sub_area=="Ajeroport",]

df.housing.500<- df.housing.500 %>% 
  group_by(sub_area) %>%
  mutate(green_part_500_new=ifelse(splitQuantile== max(splitQuantile),mean,green_part_500))


#str(df.housing.500$quantile)
#df.housing.500$quantile=as.character(df.housing.500$quantile)
#df.housing.500$splitQuantile<- as.numeric(sub(".*,(.*?)].*","\\1",df.housing.500$quantile))
#class(df.housing.500$splitQuantile)

#sum(is.na(df.housing.500$splitQuantile))          

#df.housing.500<-df.housing.500[!is.na(df.housing.500$maxQuantile),]
#df.housing.500$maxQuantile=as.numeric(df.housing.500$maxQuantile)
#df.housing.500$mean=as.numeric(df.housing.500$mean)


boxplot(df.housing.500$green_part_500_new)
boxplot(df.housing.500$green_part_500)


  #cut(my.df$x, breaks = quantile(my.df$x, probs = seq(0, 1, 0.25)), 
#    include.lowest = TRUE, labels = 1:4)
?rank

df.housing=merge(df.housing, sub.districts, by.x = "sub_area", by.y = "sub_area")
df.missingpct = data.frame();
unique.districts <- unique(sub.districts$district)

for ( i in 1: length(unique.districts))
{
  
  miss_pct <- map_dbl( df.housing[df.housing$district == unique.districts[i],], function(x) { round((sum(is.na(x)) / length(x)) * 100, 1) })
  
  miss_pct <- miss_pct[miss_pct > 0]
  
  df.missingpct = rbind(df.missingpct,data.frame(miss=miss_pct, var=names(miss_pct), district =unique.districts[i], row.names=NULL))
  
}



scale_this <- function(x){
  if (is.numeric(x) || is.integer(x))
  {
    (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE) 
  }
  else {x}
}

df.housing <-data.frame(lapply(df.housing, scale_this))


###inprogress
df.housing =df.housing%>% select(-grep("raion", names(df.housing)))

getfactorcolumns <-map_dbl(df.housing, function(x){is.factor(x)})
getfactorcolumns <-getfactorcolumns[getfactorcolumns>0]

names(a)

is(df.housing$product_type)


which(colnames(df.housing)=="timestamp")

which(colnames(df.housing)=="product_type")


housing.Matrix =model.matrix(~. + 0, df.housing[, -1])

df.housing = cbind(df.housing[,1], housing.Matrix)
### factor to model matrix  pending..
##1. sales by district

ggplot(data = df.housing, mapping = aes(district, price_doc))+geom_violin(scale = "area")

ggplot(data = df.housing, mapping = aes(district, price_doc))+geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1))

df.housing$timestamp = as.POSIXlt(df.housing$timestamp, "GMT")
df.housing$year = as.integer(df.housing$timestamp$year) +1900

district.count = df.housing %>% select(district,year) %>%group_by(district, year) %>% summarise(count = n()) %>%
  mutate( percentage = round(count/ sum(count)*100))

district.count = as.data.frame(district.count)


ggplot(data= district.count , mapping = aes(reorder(district, -percentage), count))+
  geom_bar(stat="identity", fill ="orangered")+
  coord_polar(theta = "x", direction=1 )+

theme(axis.text.x=element_text(angle=90, hjust=1))+
  ggtitle("sales transactions by district")

ggplot(district.count, mapping = aes(year, count, group = district, color = district))+geom_point()+geom_line()


### median price by district by year
dist.median = df.housing %>% select(district, price_doc, year)%>% group_by(district, year) %>% summarise(Median = median(price_doc))
dist.median = as.data.frame(dist.median)

ggplot(dist.median, mapping = aes(year, Median, group = district, color = district))+
  geom_point()+geom_line()+
  theme(axis.text.x=element_text(angle=90, hjust=1))
  

#########population by district

names(df.housing)
df.housing$raion_popul
df.popcount = df.housing %>% select(price_doc,raion_popul, district, year) %>%group_by(district, year)%>% summarize(pop.count = n())
df.popcount = as.data.frame(df.popcount)

df.merged = merge(df.popcount, dist.median)
names(df.merged)

b = ggplot(df.merged, mapping = aes(year, pop.count, group = district, color = district))+
  geom_jitter()+
  theme(axis.text.x=element_text(angle=90, hjust=1))


ggplot(df.merged, mapping = aes(year, Median, group = district, color = district))+
  geom_point()+geom_line()+
 geom_jitter(aes(size = pop.count, alpha = 0.3))+
    theme(axis.text.x=element_text(angle=90, hjust=1))


########################

ggplot(df.missingpct, aes(x=reorder(var, -miss), y=miss))+
  geom_bar(stat= "identity", fill  = "orangered")+
  facet_grid(district~.)+
theme(axis.text.x=element_text(angle=90, hjust=1))


ggplot(df.missingpct, aes(x=reorder(var, -miss), y=miss))+
  geom_boxplot()+
  facet_grid(district~.)+
    theme(axis.text.x=element_text(angle=90, hjust=1))


#df.housing[df.housing$district == unique.districts[1], 50:100] %>% correlate() %>% network_plot(min_cor=0.9)
chart.Correlation(df.housing[,50:100], histogram=TRUE, pch=19)



df.housing = df.housing[df.housing$build_year!=20052009, ]

# 1 rec --updated to 1961

a = df.housing[df.housing$build_year %in% c(0,1), ]
a<-a[!is.na(a$price_doc), ]



ggplot(data = a, mapping = aes( sub_area))+geom_histogram(stat = "count")+coord_flip()

b<-df.housing[df.housing$sub_area =="Poselenie Vnukovskoe",]

ggplot(b, aes(timestamp,price_doc ))+
  geom_point(aes(color = factor(full_sq)))+
  facet_grid(.~build_year)+
  theme(axis.text.x=element_text(angle=90, hjust=1))


ggplot(b, aes(full_sq,price_doc))+geom_point(aes(color = factor(build_year)))+
facet_grid(material~max_floor)

## We need to select features to assign the assumpted mean value 
for ( col in names(df.housing)){
  xmean =  mean(df.housing[,col], na.rm = TRUE)
  df.housing[is.na(df.housing[col]), col] = xmean
}


housing.Matrix <- as.data.frame(model.matrix(~. + 0 ,data=df.housing[, -c(1,2)],
                                  contrasts.arg = lapply(df.housing[,c(12,13)], 
                                  contrasts, contrasts=FALSE)))
b<-cor(housing.Matrix)
if(!require(caret))install.packages("caret")

highlyCorrelated <- findCorrelation(b, cutoff=0.8)

test.df = df.housing[,-c(1,highlyCorrelated)]

model <-glm(price_doc~., data = test.df, family = "gaussian")

summary(model)
predict.glm(model, newdata = "test.csv", type = "response")

read.csv("test.csv")
####################

if(!require(ggplot2) )install.packages("ggplot2")

ggplot( df.housing, mapping = aes(price_doc, build_year))+geom_boxplot()

ggplot(data = df.housing,mapping = aes(sub_area, price_doc, fill = product_type))+
  geom_boxplot()+
  facet_grid(big_market_raion~.)

a<-matrix(lapply(df.housing, unique))



df.housing$price_doc<-scale_this(df.housing$price_doc)
str(df.housing)
col <- c("mosque_count_5000")
df.housing[,c(5:8,11)] <- data.frame(apply(df.housing[c(5:8,11)], 2, as.factor))

df.housing[,20]



boxplot(df.housing)

write.csv(df.housing,file = "zhousingdata.csv")
###########
new_DF <- df.housing[rowSums(is.na(df.housing)) > 0,]

a<-data.frame(sapply(lapply(new_DF, is.na), table))

nt <-sapply(new_DF, function(y) sum(length(which(is.na(y)))))

length(sort(nt[nt>0], decreasing = T))

a<-df.housing[!is.na(df.housing$build_count_brick),]


##########
# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
if(!require(mlbench))install.packages("mlbench")
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(df.housing[,20:100])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
