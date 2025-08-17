install.packages("caTools")
library(caTools)
install.packages("rpart")
library(rpart)
install.packages("C50")
library(C50)
install.packages("gmodels")
library(gmodels)
install.packages("dendextend")
library(dendextend)
install.packages("xlsx")
library(xlsx)
install.packages("NbClust")
library(NbClust)
install.packages("factoextra")
library(factoextra)
install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
install.packages("caret")
library(caret)
install.packages('ISLR')
library(ISLR)
install.packages("PerformanceAnalytics")
library("PerformanceAnalytics")
install.packages("learningr")
library(learningr)
install.packages("dplyr")
library(dplyr)
install.packages("corrplot")
library(corrplot)
install.packages("arules")
library(arules)
install.packages("scales")
library(scales)
install.packages("xlsx")
library(xlsx)
install.packages("NbClust")
library(NbClust)
install.packages("factoextra")
library(factoextra)
install.packages("dendextend")
library(dendextend)


# give information on data set 'passportpower'
View(passportpower)


# the str() command provides a compact representation of the internal structure of the object, 

str(passportpower)

colnames(passportpower)


#The head() and tail() commands are used to display the first or last few rows
head(passportpower)
tail(passportpower)
nrow(passportpower)
ncol(passportpower)

# show rows and column
dim(passportpower)

typeof(passportpower$Countries)
class(passportpower$Countries)



# to show rows with free of missing values
complete.cases(passportpower)

# to show missing values
is.na(passportpower)
View(is.na(passportpower))



# to detect if a data frame has at least 1 NA
any(is.na(passportpower))



#to identify count of NAs in data frame
sum(is.na(passportpower))



# to obtain frequencies for categorical attributes
table(passportpower$HDI)
table(passportpower$'Visa Free')
table(passportpower$'Visa onArrival')
table(passportpower$'Visa Required')
table(passportpower$'Political Stability')



#******* Descriptive Statistics


summary(passportpower$HDI)
summary(passportpower$'Political Stability')
summary(passportpower$'Visa Free')
summary(passportpower$'Visa onArrival')
summary(passportpower$'Political Stability')
summary(passportpower$'Political Stability')



# Missing values will block the override the result:
mean(passportpower$HDI)
median(passportpower$HDI)
var(passportpower$HDI)
sd(passportpower$HDI)
range(passportpower$HDI)
IQR(passportpower$HDI)

#  0%   25%   50%   75%  100% g??sterir
quantile(passportpower$'Visa Free',prob=c(0,0.25,0.5,0.75,1))
quantile(passportpower$'Visa onArrival',prob=c(0,0.25,0.5,0.75,1))
quantile(passportpower$'Visa Required',prob=c(0,0.25,0.5,0.75,1))
quantile(passportpower$HDI,prob=c(0,0.25,0.5,0.75,1))
quantile(passportpower$'Political Stability',prob=c(0,0.25,0.5,0.75,1))

summary(passportpower)


#***********Subsesting Data / Data Manipulaton

#to get 4th row only 
passportpower4 = passportpower[4,]

View(passportpower4)

#to get first 5 row only 
passportpower_1_5<- passportpower[1:5,]


View(passportpower_1_5)

#to get last 6 row only 
passportpower_194_199<- passportpower[194:199,]
View(passportpower_194_199)




#to get 2nd and 4th row only 
passportpower2_4<- passportpower[c(2,4),]

View(passportpower2_4)


#to get 3rd column only 
passportpowerC3<- passportpower[,3]

View(passportpowerC3)


#Filtering the data
HighDevelopment <- passportpower$HDI > 0.80 & passportpower$HDI < 1
MidDevelopment <- passportpower$HDI > 0.50 & passportpower$HDI < 0.80
LowDevelopment <- passportpower$HDI > 0 & passportpower$HDI < 0.50

summary(HighDevelopment)
summary(MidDevelopment)
summary(LowDevelopment)


StabilCountry <- passportpower$'Political Stability' > 1 & passportpower$'Political Stability' < 2
HalfStabilCountry <- passportpower$'Political Stability' > -0.5 & passportpower$'Political Stability' < 1
NonStabilCountryt <- passportpower$'Political Stability' > -3 & passportpower$'Political Stability' < -0.5

summary(StabilCountry)
summary(HalfStabilCountry)
summary(NonStabilCountryt)


#Sorting the data

passportpowerVR <- order(passportpower$'Visa Required', decreasing ='TRUE' )
View(passportpower [passportpowerVR, ])

passportpowerVF <- order(passportpower$'Visa Free', decreasing ='TRUE' )
View(passportpower [passportpowerVF, ])


#To get unique values
unique(passportpower$'Visa Free')
unique(passportpower$'Visa onArrival')
unique(passportpower$'Visa Required')
unique(passportpower$'Political Stability')
unique(passportpower$'HDI')


#---------------------------------Basic Data Visulation-------------------------


cor(passportpower$'Mobility Rank', passportpower$HDI, method = "pearson")

# default method is pearson cor
cor(passportpower$'Visa onArrival', passportpower$HDI)
cor(passportpower$'Visa Free', passportpower$HDI)
cor(passportpower$'Visa Required', passportpower$HDI)

cor(passportpower$'Visa onArrival', passportpower$'Political Stability')
cor(passportpower$'Visa Free', passportpower$'Political Stability')
cor(passportpower$'Visa Required', passportpower$'Political Stability')

cor(passportpower$'Mobility Rank', passportpower$'Political Stability')
cor(passportpower$'Mobility Rank', passportpower$'HDI')
cor(passportpower$'Mobility Rank', passportpower$'Visa Free')
cor(passportpower$'Mobility Rank', passportpower$'Visa onArrival')
cor(passportpower$'Mobility Rank', passportpower$'Visa Required')



# If your data contain missing values, use the following R code 
# to handle missing values by case-wise deletion.
# cor(x, y,  method = "pearson", use = "complete.obs")

# Compute correlation matrix
passportpower_corrmat <- cor(passportpower[, c(2,2:7)], method = "pearson")
View(passportpower_corrmat)

# Visualize correlation matrix
install.packages("corrplot")
library(corrplot)

corrplot(corrmat_pearson, method="circle")
corrplot(corrmat_pearson, method="pie")
corrplot(corrmat_pearson, method="color")
corrplot(corrmat_pearson, method="number")

# ???upper??? (default) : display full correlation matrix
# ???lower??? (default) : display full correlation matrix

corrplot(corrmat_pearson, type = "upper")
corrplot(corrmat_pearson, type = "lower")

# The correlation matrix can be reordered according to the correlation coefficient. 
# This is important to identify the hidden structure and pattern in the matrix. 
# ???hclust??? for hierarchical clustering order is used in the following examples.

corrplot(corrmat_pearson, type="full", order="hclust", col=c("steelblue", "red4"),
         bg="grey90",tl.col = "black")

# tl.col (for text label color) and tl.srt (for text label string rotation) 
# are used to change text colors and rotations.
corrplot(corrmat_pearson, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# You can change the method to "square," "circle," or "number" 
# to change the way it represents the correlation matrix. 
corrplot(corrmat_pearson, method = "square", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# You can also create a mixed-type matrix using the following code.
corrplot.mixed(corrmat_pearson,
               upper = "square",
               lower = "number",
               addgrid.col = "black",
               tl.col = "black")

# The function chart.Correlation() in the package PerformanceAnalytics, 
# can be used to display a chart of a correlation matrix.


install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(passportpower[, c(2, 2:7)], histogram=TRUE, pch=19)


# cut function is used in R for cutting a numeric value 
# split the values in the points column into bins of five equal sizes
cut(passportpower$'Mobility Rank', breaks=7)

# to see how many values fall into different bins
table(cut(passportpower$'Visa Free', seq(0, 140, 10)))
table(cut(passportpower$'Visa Required', seq(0, 160, 10)))


#*********   Scatterplot  **************
#create a simple scatterplot
plot (passportpower$HDI, passportpower$'Visa Free', 
      main = "Visa Free depend on HDI",
      ylab = "Visa Free",
      xlab = "HDI",
      pch=0, col=c("blue"))

plot (passportpower$'Mobility Rank', passportpower$'Political Stability', 
      main = "Mobilty Rank depend on Political Stability",
      ylab = "Political Stability",
      xlab = "Mobilty Rank",
      pch=11, col=c("turquoise"))

plot (passportpower$'Visa onArrival', passportpower$HDI, 
      main = "Relationship between Visa on arrival and HDI",
      ylab = "HDI",
      xlab = "Visa on Arrival",
      pch=9, col=c("darkblue"))




#*********   Histograms  **************
hist(passportpower$HDI,
     main = "HDI per country",
     ylab = "Countries",
     xlab = "HDI",
     col = "aquamarine4", border ="black")


histogram(~  HDI, passportpower, breaks = 20)


hist(passportpower$'Political Stability',
     main = "HDI per country",
     ylab = "Countries",
     xlab = "HDI",
     col = "wheat4", border ="black")


hist(passportpower$'Political Stability', breaks = 10, 
     main = "Political Stability Histogram",
     xlab = "Political Stability")




hist(passportpower$'Mobility Rank',
     main = "Mobility rank of countries",
     ylab = "Countries",
     xlab = "Mobility Rank",
     col = "violetred3", border ="black")

hist(passportpower$'Mobility Rank', breaks = 20, 
     main = "Political Stability Histogram",
     xlab = "Political Stability")





#*******#*******#*******   Box Plots  *************

boxplot(HDI ~ `Mobility Rank`, data = passportpower,
        main = "HDI by Mobility Rank",
        xlab = "Mobility Rank",
        ylab = "HDI",
        col = "lemonchiffon")


boxplot(HDI ~ `Visa Free`, data = passportpower,
        main = "Visa Free by Mobility Rank",
        xlab = "Mobility Rank",
        ylab = "HDI",
        col = "skyblue")





#****** boxplots to detect potential outliers.

boxplot(passportpower$HDI,
        ylab = "HDI")

boxplot(passportpower$'Visa Free',
        ylab = "Visa Free")


boxplot(passportpower$'Visa onArrival',
        ylab = "Visa Onarrival")

boxplot(passportpower$'Visa Required',
        ylab = "Visa Required")

boxplot(passportpower$'Political Stability',
        ylab = "Politic Stabilty")



# to extract the values of the potential outliers based on the IQR criterion 
boxplot.stats(passportpower$HDI)$out

boxplot.stats(passportpower$'Visa Free')$out

boxplot.stats(passportpower$'Visa onArrival')$out

boxplot.stats(passportpower$'Visa Required')$out

boxplot.stats(passportpower$'Political Stability')$out

# to extract the row number corresponding to these outliers:

passportpower_out <- boxplot.stats(passportpower$'Political Stability')$out

passportpower_out_row <- which(passportpower$'Political Stability' %in% passportpower_out)

View(passportpower [passportpower_out_row, ])




#**** Z-scores for outliers detection 

passportpower.scaled.PS <- scale(passportpower$'Political Stability')
View(passportpower.scaled.PS)
summary(passportpower.scaled.PS)



out_row <- which(passportpower.scaled.PS <  -2.47)

#print all variables for these outliers:
View(passportpower.scaled.PS [out_row, ])




# Calculate the most frequent category 
passportpower_most_frequent.VF <- names(which.max(table(passportpower$'Visa Free')))
View(passportpower_most_frequent.VF)





#Rescaling a variable to [-3,3] to ???


passportpower.Scaled <- passportpower[, -1]


View(passportpower.Scaled)

#z-score standardization/transformation
passportpower.z <- scale(passportpower.Scaled)

View(passportpower.z)
summary(passportpower.z)


#**for k-means clustering
install.packages("factoextra")
library(factoextra)


fviz_nbclust(passportpower.z, kmeans, method = "wss", k.max=12)

fviz_nbclust(passportpower.z, kmeans, method = "wss", k.max=12) +
  geom_vline(xintercept = 4, linetype = 5)





#***Clustering Execution****

set.seed(2345)

passportpower_clustering <- kmeans(passportpower.z, 4, nstart = 20)


passportpower_clustering$size

passportpower_cluster <- passportpower_clustering$cluster
table(passportpower_cluster)


#*****for hierarchical clustering (Ward's clustering) 
set.seed(1234)
# Compute pairewise distance matrices
dist.res <- dist(passportpower.Scaled, method = "euclidean")
# Hierarchical clustering results
passportpower_clustering <- hclust(dist.res, method = "ward.D")
# Visualization of hclust
plot(passportpower_clustering, labels = FALSE, hang = -1)


rect.hclust(passportpower_clustering, k = 4, border = 2:5)







install.packages("rnaturalearth")
install.packages("ggplot2")        
install.packages("sf")  
install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
install.packages("ggthemes")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")

library(rnaturalearth)
library(ggplot2)
library(sf)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggthemes)



world <- ne_countries(scale = "medium", returnclass = "sf")
mapped_data <- world %>%
  left_join(data, by = c("name" = "Countries"))
View(test_join)
anti_join(data, world, by = c("Countries" = "name"))
anti_join(data, world %>% select(-geometry), by = c("Countries" = "name"))


ggplot(data = mapped_data) +
  geom_sf(aes(fill = HDI), color = "white") +
  labs(
    title = "World Map by Mobility Rank",
    fill = "Mobility Rank Value"
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_map() +
  theme(
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(size = 20, color = "black", hjust = 0.5, face = "bold")
  )
View(mapped_data)
