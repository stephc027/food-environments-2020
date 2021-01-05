#Food Environment Analysis 2020

library(tidyverse)
library(readxl)
library(plyr)

#setwd("C:/Users/sscha/Documents")
#Use/modify setwd function if needed, or keep commented out

#Load FEA file (main dataset) and examine categories of data (for now we will ignore the supplemental data):
fea_file<-"FoodEnvironmentAtlas 2020 revised.xls"
categories<-excel_sheets(fea_file)
categories<-categories[5:13]
categories<-as.array(categories)
categories

access<-as.data.frame(read_xls(fea_file,sheet="ACCESS"))
stores<-as.data.frame(read_xls(fea_file,sheet="STORES"))
restaurants<-as.data.frame(read_xls(fea_file,sheet="RESTAURANTS"))
assistance<-as.data.frame(read_xls(fea_file,sheet="ASSISTANCE"))
insecurity<-as.data.frame(read_xls(fea_file,sheet="INSECURITY"))
taxes<-as.data.frame(read_xls(fea_file,sheet="TAXES"))
local<-as.data.frame(read_xls(fea_file,sheet="LOCAL"))
health<-as.data.frame(read_xls(fea_file,sheet="HEALTH"))
socioeconomic<-as.data.frame(read_xls(fea_file,sheet="SOCIOECONOMIC"))

#create dataframes of county ID columns (1-3) to compare across sheets to see if all counties are listed the same*:
access.cty<-access[1:3]
stores.cty<-stores[1:3]
restaurants.cty<-restaurants[1:3]
assistance.cty<-assistance[1:3]
insecurity.cty<-insecurity[1:3]
taxes.cty<-taxes[1:3]
local.cty<-local[1:3]
health.cty<-health[1:3]
socioeconomic.cty<-socioeconomic[1:3]

#compare counties to make sure dataframes will match up:

library(arsenal)
comparedf(access.cty,stores.cty)
comparedf(access.cty,restaurants.cty)
comparedf(access.cty,assistance.cty)
comparedf(access.cty,insecurity.cty)
comparedf(access.cty,taxes.cty)
comparedf(access.cty,local.cty)

#note: originally, local tab had 2 duplicate rows.  More research showed USDA had already published a revision, thus new file was downloaded and re-imported.  Now the comparedf() function shows no differences, but there is an extra column.
#further inspection revealed that one column had been averaged, creating an extra row at the bottom.  This can be removed:
local<-local[-3144,]

comparedf(access.cty,health.cty)
comparedf(access.cty,socioeconomic.cty) 

#We need to examine what the variables are. Import variable list:
key<-as.data.frame(read_xls(fea_file,sheet=" Variable List"))
key

#Notice that many variables are derived (61 total) - Percentage points and %change indicates this.  We will remove these to a separate DF:

derived<-(key$Units=='% change'| key$Units== 'Percentage points')
key$Derived<-derived
head(key)
variables.derived<-key$`Variable Code`[derived]
#*
der.df<-filter(data.frame(key$Derived,key$`Category Code`,key$`Variable Code`),key$Derived==T)

ac.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='ACCESS')
st.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='STORES')
r.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='RESTAURANTS')
as.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='ASSISTANCE')
in.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='INSECURITY')
tx.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='TAXES')
l.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='LOCAL')
h.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='HEALTH')
sc.der.lst<-filter(data.frame(key$Derived),key$`Category Code`=='SOCIOECONOMIC')

#Now that we have a boolean list marking every derived category in each tab, we can create new lists for our model, excluding derived columns:

ac.der.lst
ac.der.lst<-c(F,F,F,as.vector(ac.der.lst[1:nrow(ac.der.lst),]))              
access.mod<-access[,!ac.der.lst]

st.der.lst
st.der.lst<-c(F,F,F,as.vector(st.der.lst[1:nrow(st.der.lst),]))  
stores.mod<-stores[,!st.der.lst]

r.der.lst
r.der.lst<-c(F,F,F,as.vector(r.der.lst[1:nrow(r.der.lst),]))  
restaurants.mod<-restaurants[,!r.der.lst]

as.der.lst
as.der.lst<-c(F,F,F,as.vector(as.der.lst[1:nrow(as.der.lst),])) 
assistance.mod<-assistance[,!as.der.lst]

in.der.lst
in.der.lst<-c(F,F,F,as.vector(in.der.lst[1:nrow(in.der.lst),])) 
insecurity.mod<-insecurity[,!in.der.lst]

tx.der.lst
#No derived fields in taxes, but we will make a mod version of the df to avoid confusion:
taxes.mod<-taxes

l.der.lst
l.der.lst<-c(F,F,F,as.vector(l.der.lst[1:nrow(l.der.lst),])) 
local.mod<-local[,!l.der.lst]

h.der.lst
h.der.lst<-c(F,F,F,as.vector(h.der.lst[1:nrow(h.der.lst),])) 
health.mod<-health[,!h.der.lst]

sc.der.lst
#No derived fields in socioeconomic. 
socioeconomic.mod<-socioeconomic

#load secondary dataset (Identifying food deserts):
deserts<-as.data.frame(read_xlsx("food research atlas.xlsx",sheet="Food Desert Calculations"))
head(deserts)

#load another secondary dataset (Census info on education and labor force participation rates - pre-cleaned in Excel):
edulab<-read.csv("labor participation 2018.csv")
index <-3:ncol(edulab)
edulab[index] <- lapply(edulab[index], function(x) as.numeric(as.character(x)))
names(edulab)

head(edulab)
cor(edulab$HS.or.Equivalent...25.64.yr,health$PCT_DIABETES_ADULTS13,use="complete.obs")
cor(edulab$Less.than.HS...25.64.yr,health$PCT_DIABETES_ADULTS13,use="complete.obs")
cor(edulab$Labor.PR..20.to.64..Bach.degree.or.higher,health$PCT_DIABETES_ADULTS13,use="complete.obs")


#county IDs and food desert markers were pre-processed in Excel.  Use comparedf again to check that they are the same as existing DFs:
access$FIPS<-as.numeric(access$FIPS)
access.fips<-access[1]
edulab.fips<-edulab[1]
comparedf(access.fips,edulab.fips)
deserts.fips<-deserts[1]
comparedf(access.fips,deserts.fips)
#both lists match up with current dfs


#Creating a 'combined' dataframe that will be useful for unsupervised learning:
df.ul<-data.frame(access.mod,stores.mod[4:ncol(stores.mod)],restaurants.mod[4:ncol(restaurants.mod)],assistance.mod[4:ncol(assistance.mod)],insecurity.mod[4:ncol(insecurity.mod)],taxes.mod[4:ncol(taxes.mod)],local.mod[4:ncol(local.mod)],socioeconomic.mod[4:ncol(socioeconomic.mod)],edulab[4:ncol(edulab)])


#EDA:
#What variables may we want to watch closely based on food desert correlation?  Comparison prior to fixing imputers, requires removal of NA:
#compare correlation of food desert vs. other variables:
df.init<-data.frame(deserts$`County >1/3 Food Desert?`,df.ul[,4:ncol(df.ul)])


cor.init<-cor(df.init$deserts..County..1.3.Food.Desert,df.init[2:62],use="complete.obs",method="spearman")
cor.init2<-cor(df.init$deserts..County..1.3.Food.Desert,df.init[63:123],use="complete.obs"method="spearman")
cor.init3<-cor(df.init$deserts..County..1.3.Food.Desert,df.init[124:184],use="complete.obs")
cor.init4<-cor(df.init$deserts..County..1.3.Food.Desert..,df.init[185:235],use="complete.obs"method="spearman")


library(ggcorrplot)
library(gridExtra)

a<-ggcorrplot(cor.init,tl.cex=5,show.legend=F)
b<-ggcorrplot(cor.init2,tl.cex=5,show.legend=F)
c<-ggcorrplot(cor.init3,tl.cex=5,show.legend=F)
d<-ggcorrplot(cor.init4,tl.cex=5)

grid.arrange(a,b,c,ncol=3)
d

library(corrplot)
#Let's separately examine health markers vs. food deserts:
df.hlth.fd<-data.frame(deserts$`County >1/3 Food Desert?`,health.mod[4:ncol(health.mod)])
head(df.hlth.fd)
#obesity rate only goes by state so this is not useful and may be misleading.  We should omit this:
df.hlth.fd<-df.hlth.fd[,-c(4,5)]
cor.hlth<-cor(df.hlth.fd,use="complete.obs")
cor.hlth

col1<-colorRampPalette(c("purple","lightblue","white","pink","red"))
corrplot(cor.hlth,tl.cex=.6,col=col1(100),tl.col="black")
?corrplot

#Some summary statistics and plots:


#major food desert indicators:
par(mfrow=c(1,2))
summary(access$PCT_LACCESS_POP15)
summary(socioeconomic$povrate15)

ac<-boxplot(access$PCT_LACCESS_POP15, main="% Low Access to Store 2015")
pr<-boxplot(socioeconomic$POVRATE15, main="Poverty Rate 2015")

#major health indicators:
summary(health)
db<-boxplot(health$PCT_DIABETES_ADULTS13, main="% Diabetic Adults 2013")
ob<-boxplot(health$PCT_OBESE_ADULTS17,main="% Obese Adults 2017")


#mapping:

library(choroplethr)
library(choroplethrMaps)
head(access)

map_fn<-function(x,title) {
  names(x)[1] <- "region"
  names(x)[2] <- "value"
  x$region<-as.numeric(x$region)
  county_choropleth(x) + labs(title=as.character(title))
}

map.df.lap15<-access[,c(1,8)]
#map_fn(map.df.lap15,"% Pop. Low Access to Store, 2015")

map.df.pov15<-socioeconomic[,c(1,13)]
#map_fn(map.df.pov15,"Poverty Rate 2015")

map.df.fds<-deserts[,c(1,5)]
#map_fn(map.df.fds,"Counties with >1/3 Tracts as Food Deserts")

map.dia<-health[,c(1,5)]
#map_fn(map.dia,"Adult Diabetes Rate 2013")

map.ob<-health[,c(1,7)]
#map_fn(map.ob,"Adult Obesity Rate 2017")

map.active<-health[,c(1,13)]
map_fn(map.active,"Recreational facilities per capita 2016")

library(purrr)
#inspecting df.ul:
dim(df.ul)
missinginfo<-map(df.ul, ~sum(is.na(.)))
missinginfo
missinginfo2<-map(df.ul, ~sum(is.na(.))/3143*100)
mi<-as.data.frame(matrix(unlist(missinginfo2),nrow=length(missinginfo2),byrow=TRUE))
mi2<-matrix(unlist(missinginfo2),nrow=length(missinginfo2),byrow=TRUE)
par(mfrow=c(1,1))
hist(mi2, main="Missing Data in Unsupervised Learning Set",xlab='Data Missing (%)')

#There are several columns of the 240 variables with NA values.  Especially prevalent in the local farm data.
#What % of each variable is missing?

p.mis<-function(x) {sum(is.na(x))/length(x)*100}
#There are several categories with up to 40% NAs.  These must be dealt with.
max(apply(df.ul,2,p.mis))
#FOODHUB18 has the highest with 94% NAs.  Confirmed this means no food hubs, so replace with 0.  That is the only such variable:
df.ul$'FOODHUB18'[is.na(df.ul$'FOODHUB18')] <- 0




#We can use pcaMethods for automated handling of missing values for PCA:
library(pcaMethods)

#From prior testing and research, Bayesian PCA is the optimal and only reasonable method available for this data set, based on number of missing values it can handle and speed to run (Non-linear PCA may handle the high missing values well but could not perform at an efficient speed as too many NN iterations are needed.)
#We still need to choose # components, nPCs.  We will start with a high number, 15, and examine proportion of variance explained.
#We also need to scale and center the data, which pcaMethods does for us.
#Beginning with all counties:
b_pca <- pca(df.ul[,4:ncol(df.ul)], nPcs=15,cv="q2", method="bpca", scale="uv", center=T,set.seed=12)
screeplot (r2 and q2; more generally applicable for pca methods than traditional plot of explained variance):
  plot(b_pca,main="Scree plot")
cvstat(b_pca)

b_pca_all<-pca(df.ul[,4:ncol(df.ul)], nPcs=3, method="bpca", scale="uv", center=T,set.seed=12)
imputed_all <- completeObs(b_pca_all)

library(ggrepel)
#inspecting some of the PCs:
plot(b_pca_all@scores[,1],b_pca_all@scores[,2],col="lightblue", pch=19, cex=.5)
text(b_pca_all@scores[,1],b_pca_all@scores[,2], labels=df.ul[,3], cex=.5)

plot(b_pca_all@scores[,1],b_pca_all@scores[,3],col="lightblue", pch=19, cex=.5)
text(b_pca_all@scores[,1],b_pca_all@scores[,3], labels=df.ul[,3], cex=.5)

#layering in the loadings:
l.x<-b_pca_all@loadings[,1]*40
l.y<-b_pca_all@loadings[,2]*40

plot(b_pca_all@scores[,1],b_pca_11@scores[,2],col="lightblue", pch=19, cex=.5,xlim=c(-40,40),ylim=c(-40,40))
text(b_pca_all@scores[,1],b_pca_11@scores[,2], labels=df.ul[,3], cex=.5)
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="lightpink", length=0.15, lwd=1.5,lty=3)
text(l.x, l.y, labels=row.names(b_pca_11@loadings), col="black",cex=.3)


#Since the population seems to be overshadowing other attributes, stratifying by metro vs non metro:

met.bool<-socioeconomic.mod$METRO13 == 1
met.df.ul<-subset(df.ul,met.bool==T)
nm.df.ul<-subset(df.ul,met.bool==F)

dim(met.df.ul)
#METRO:
b_pca_met1 <- pca(met.df.ul[,4:ncol(met.df.ul)], nPcs=15,cv="q2", method="bpca", scale="uv", center=T,set.seed=12)
plot(b_pca_met1,main="Scree plot, Metro")

b_pca_met<-pca(met.df.ul[,4:ncol(met.df.ul)], nPcs=2, method="bpca", scale="uv", center=T,set.seed=12)
imputed_met <- completeObs(b_pca_met)

plot(b_pca_met@scores[,1],b_pca_met@scores[,2],col="lightblue", pch=19)
text(b_pca_met@scores[,1],b_pca_met@scores[,2],labels=met.df.ul[,3], cex=.5)
df.met.load<-data.frame(b_pca_met@loadings)
df.met.load <- df.met.load[order(-df.met.load[,1], -df.met.load[,2]),]
df.met.load
#NONMETRO:
b_pca_nm1 <- pca(nm.df.ul[,4:ncol(nm.df.ul)], nPcs=15,cv="q2", method="bpca", scale="uv", center=T,set.seed=12)
plot(b_pca_nm1,main="Scree plot, Non-Metro")

b_pca_nm<-pca(nm.df.ul[,4:ncol(nm.df.ul)], nPcs=4, method="bpca", scale="uv", center=T,set.seed=12)
imputed_nm <- completeObs(b_pca_nm)

df.nm.load<-data.frame(b_pca_nm@loadings[,1:2])
df.nm.load <- df.nm.load[order(-df.nm.load[,1], -df.nm.load[,2]),]
df.nm.load


plot(b_pca_nm@scores[,1],b_pca_nm@scores[,2],col="lightgreen", pch=19)
text(b_pca_nm@scores[,1],b_pca_nm@scores[,2],labels=df.ul[,3], cex=.5)

plot(b_pca_nm@scores[,1],b_pca_nm@scores[,3],col="lightgreen", pch=19)
text(b_pca_nm@scores[,1],b_pca_nm@scores[,3],labels=df.ul[,3], cex=.5)

plot(b_pca_nm@scores[,1],b_pca_nm@scores[,4],col="lightgreen", pch=19)
text(b_pca_nm@scores[,1],b_pca_nm@scores[,4],labels=df.ul[,3], cex=.5)

plot(b_pca_nm@scores[,2],b_pca_nm@scores[,3],col="lightgreen", pch=19)
text(b_pca_nm@scores[,2],b_pca_nm@scores[,3],labels=df.ul[,3], cex=.5)

plot(b_pca_nm@scores[,2],b_pca_nm@scores[,4],col="lightgreen", pch=19)
text(b_pca_nm@scores[,2],b_pca_nm@scores[,4],labels=df.ul[,3], cex=.5)

plot(b_pca_nm@scores[,3],b_pca_nm@scores[,4],col="lightgreen", pch=19)
text(b_pca_nm@scores[,3],b_pca_nm@scores[,4],labels=df.ul[,3], cex=.5)


#layering in the loadings:

l.x<-b_pca_all@loadings[,1]*40
l.y<-b_pca_all@loadings[,2]*40

plot(b_pca_all@scores[,1],b_pca_11@scores[,2],col="lightblue", pch=19, cex=.5,xlim=c(-40,40),ylim=c(-40,40))
text(b_pca_all@scores[,1],b_pca_11@scores[,2], labels=df.ul[,3], cex=.5)
arrows(x0=0, x1=l.x, y0=0, y1=l.y, col="lightpink", length=0.15, lwd=1.5,lty=3)
text(l.x, l.y, labels=row.names(b_pca_11@loadings), col="black",cex=.3)

#METRO:
metscore<-as.data.frame(b_pca_met@scores)
metld<-as.data.frame(b_pca_met@loadings)

#ggplot(metscore,aes(metscore[,1],metscore[,2])) + geom_point(color="black")+ geom_segment(data=metld, aes(x=0, y=0, xend=metld[,1]*20, yend=metld[,2]*20), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="lightpink") + geom_text_repel(data=metld,aes(metld[,1]*20,metld[,2]*20),label=rownames(metld),size=1.75,segment.color = 'grey50')+ labs(title="Metro PC1 vs. PC2")


#NONMETRO:
nmscore<-as.data.frame(b_pca_nm@scores)
nmld<-as.data.frame(b_pca_nm@loadings)

#ggplot(nmscore,aes(nmscore[,1],nmscore[,2])) + geom_point(color="black")+ geom_segment(data=nmld, aes(x=0, y=0, xend=nmld[,1]*20, yend=nmld[,2]*20), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="lightpink") + geom_text_repel(data=nmld,aes(nmld[,1]*20,nmld[,2]*20),label=rownames(nmld),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro PC1 vs. PC2")

#We can see there are too many loadings for easy interpretability.


#Clustering:
#We will perform k-means clustering on the PCA results.

#Find #clusters
#All data
pos_k <- seq(1, 10)
i<- c()

for (n in pos_k) {
  model <- kmeans(x=b_pca_all@scores[,1:3],centers=n)
  ss<-mean(model$withinss)
  i<-append(i,ss)
}

plot(pos_k, i, color='black',xlab="# clusters",ylab="Within SS",main="All counties- finding k")
lines(pos_k,i)


km_s1<-kmeans(x=b_pca_all@scores[,1:3],centers=3)
km_s1
plot(km_s1$centers)
tb1<-table(km_s1$cluster)
barplot(tb1,main="Cluster sizes, S1",xlab="Cluster",ylab="Counties")

#METRO:
pos_k <- seq(1, 10)
i<- c()

for (n in pos_k) {
  model <- kmeans(x=b_pca_met@scores[,1:2],centers=n)
  ss<-mean(model$withinss)
  i<-append(i,ss)
}
plot(pos_k, i, color='black',xlab="# clusters",ylab="Within SS",main="Metros- finding k")
lines(pos_k,i)

set.seed(151)
km_s2<-kmeans(x=b_pca_met@scores[,1:2],centers=3)
km_s2$centers

plot(km_s2$centers)
tb2<-table(km_s2$cluster)
barplot(tb2,main="Cluster sizes, Metro",xlab="Cluster",ylab="Counties")

#NONMETRO:

pos_k <- seq(1, 10)
i<- c()

for (n in pos_k) {
  model <- kmeans(x=b_pca_nm@scores[,1:4],centers=n)
  ss<-mean(model$withinss)
  i<-append(i,ss)
}

plot(pos_k, i, color='black',xlab="# clusters",ylab="Within SS",main="Non-metros- finding k")
lines(pos_k,i)

set.seed(532)
km_s3<-kmeans(x=b_pca_nm@scores[,1:4],centers=3)
km_s3$centers
plot(km_s3$centers)
tb3<-table(km_s3$cluster)
barplot(tb3,main="Cluster sizes, Non-Metro",xlab="Cluster",ylab="Counties")

#Overlaying clusters from kmeans clustering onto biplots:

colors<-c("plum","steelblue","seagreen1")
#classcol<-as.data.frame(km_s1$cluster)
#classcol$colors <- colors[classcol[,1]]
#colors1<-classcol$colors

#plot(b_pca_all@scores[,1],b_pca_all@scores[,2],col=colors1, pch=19, cex=.5,main="K-means Clustering, All counties",xlab="PC1",ylab="PC2")
#points(km_s1$centers)
#plot(b_pca_all@scores[,1],b_pca_all@scores[,3],col=colors1, pch=19, cex=.5,main="K-means Clustering, All counties",xlab="PC1",ylab="PC3")
#points(km_s1$centers)
#plot(b_pca_all@scores[,2],b_pca_all@scores[,3],col=colors1, pch=19, cex=.5,main="K-means Clustering, All counties",xlab="PC2",ylab="PC3")
#points(km_s1$centers)


#METRO:
par(mfrow=c(1,1))

classcol2<-as.data.frame(km_s2$cluster)
classcol2$colors <- colors[classcol2[,1]]
colors2<-classcol2$colors

plot(b_pca_met@scores[,1],b_pca_met@scores[,2],col=colors2, pch=19, cex=.5,main="K-means Clustering, Metros",xlab="PC1",ylab="PC2")  
points(km_s2$centers)

#NONMETRO:
par(mfrow=c(2,3))

classcol3<-as.data.frame(km_s3$cluster)
classcol3$colors <- colors[classcol3[,1]]
colors3<-classcol3$colors


plot(b_pca_nm@scores[,1],b_pca_nm@scores[,2],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC1",ylab="PC2")
points(km_s3$centers)
plot(b_pca_nm@scores[,1],b_pca_nm@scores[,3],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC1",ylab="PC3")
points(km_s3$centers)
plot(b_pca_nm@scores[,1],b_pca_nm@scores[,4],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC1",ylab="PC4")
points(km_s3$centers)
plot(b_pca_nm@scores[,2],b_pca_nm@scores[,3],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC2",ylab="PC3")
points(km_s3$centers)
plot(b_pca_nm@scores[,2],b_pca_nm@scores[,4],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC2",ylab="PC4")
points(km_s3$centers)
plot(b_pca_nm@scores[,3],b_pca_nm@scores[,4],col=colors3, pch=19, cex=.5,main="K-means Clustering, Non-Metros",xlab="PC3",ylab="PC4")
points(km_s3$centers)


#CLUSTER GEO MAPPING:
map_cl_fn<-function(x,title) {
  names(x)[1] <- "region"
  names(x)[2] <- "value"
  x$region<-as.numeric(x$region)
  county_choropleth(x) + labs(title=as.character(title))
}
?county_choropleth
km_s2$cluster
map.df.km_s2<-as.data.frame(cbind(met.df.ul[,1],km_s2$cluster))

map_fn(map.df.km_s2,"Metro Clusters")


map.df.km_s3<-as.data.frame(cbind(nm.df.ul[,1],km_s3$cluster))
map_fn(map.df.km_s3,"Nonmetro Clusters")

cbind(nm.df.ul[,3],km_s3$cluster,nm.df.ul[,2])



#Let's examine the loadings with top scores (in order of absolute value of loading) in each PC to simplify the biplot:
#metro:
ordmetld1<-as.data.frame(b_pca_met@loadings[order(-(abs(b_pca_met@loadings[,1]))),])
ordmetld1.78<-ordmetld1[1:78,]
ordmetld1.156<-ordmetld1[79:156,]
ordmetld1.234<-ordmetld1[157:232,]

ordmetld1<-ordmetld1[1:40,]
ordmetld2<-as.data.frame(b_pca_met@loadings[order(-(abs(b_pca_met@loadings[,2]))),])
ordmetld2<-ordmetld2[1:15,]

#non-metro:
ordnmld1<-as.data.frame(b_pca_nm@loadings[order(-(abs(b_pca_nm@loadings[,1]))),])
ordnmld1<-ordnmld1[1:15,]
ordnmld2<-as.data.frame(b_pca_nm@loadings[order(-(abs(b_pca_nm@loadings[,2]))),])
ordnmld2<-ordnmld2[1:15,]
ordnmld3<-as.data.frame(b_pca_nm@loadings[order(-(abs(b_pca_nm@loadings[,3]))),])
ordnmld3<-ordnmld3[1:15,]
ordnmld4<-as.data.frame(b_pca_nm@loadings[order(-(abs(b_pca_nm@loadings[,4]))),])
ordnmld4<-ordnmld4[1:25,]

ordnmld<-as.data.frame(b_pca_nm@loadings[order(-(abs(b_pca_nm@loadings[,1]))),])


ordnmld.78<-ordnmld[1:78,]
ordnmld.156<-ordnmld[79:156,]
ordnmld.234<-ordnmld[157:232,]
ordnm<-ordnmld[1:10,]
corrplot(t(ordnm), is.corr=FALSE,tl.cex=.7,tl.col="black")

corrplot(t(ordnmld.78), is.corr=FALSE,tl.cex=.3,tl.col="black")
corrplot(t(ordnmld.156), is.corr=FALSE,tl.cex=.7,tl.col="black",cl.pos="n")
corrplot(t(ordnmld.234), is.corr=FALSE,tl.cex=.7,tl.col="black",cl.pos="n")



#Updating PCA biplots with selected loadings:
#METRO:

shapes<-c(16,4)
classshape<-as.data.frame(met.df.sl$FD)
classshape$shapes<- shapes[classshape[,1]]
FDclass<-classshape$shapes
library(ggplot2)
library(ggrepel)
ggplot(metscore,aes(x=metscore[,1],y=metscore[,2]))+ geom_point(color=colors2,shape=FDclass,show.legend=T)+ geom_segment(data=ordmetld1, aes(x=0, y=0, xend=ordmetld1[,1]*10, yend=ordmetld1[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordmetld1,aes(ordmetld1[,1]*10,ordmetld1[,2]*10),label=rownames(ordmetld1),size=1.75,segment.color = 'grey50')+ labs(title="Metro: Modified PCA Biplot",subtitle="strongest loadings PC1",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())+ scale_colour_hue(name = "Cluster",labels = c("1", "2","3"), l = 40)+ scale_shape(name = "FD Status", labels = c("Non-FD", "FD"))
ggplot(metscore,aes(metscore[,1],metscore[,2]))+ geom_point(color=colors2,shape=FDclass)+ geom_segment(data=ordmetld2, aes(x=0, y=0, xend=ordmetld2[,1]*10, yend=ordmetld2[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordmetld2,aes(ordmetld2[,1]*10,ordmetld2[,2]*10),label=rownames(ordmetld2),size=1.75,segment.color = 'grey50')+ labs(title="Metro: Modified PCA Biplot",subtitle="strongest loadings PC2",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())

ggplot(metscore,aes(metscore[,1],metscore[,2]))+ geom_point(color=colors2,shape=FDclass)+ geom_segment(data=ordmetld2, aes(x=0, y=0, xend=ordmetld2[,1]*10, yend=ordmetld2[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordmetld2,aes(ordmetld2[,1]*10,ordmetld2[,2]*10),label=rownames(ordmetld2),size=1.75,segment.color = 'grey50')+ labs(title="Metro: Modified PCA Biplot",subtitle="strongest loadings PC2",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) + geom_text(aes(metscore[,1],metscore[,2]),label=met.df.ul[,3],size=1.75)

#NONMETRO:
nmld2<-as.data.frame(b_pca_nm@loadings)
nmld2<-nmld2[(rownames(b_pca_nm@loadings)) %in% best.load.nm,]

classshape2<-as.data.frame(nm.df.sl$FD)
classshape2$shapes<- shapes[classshape2[,1]]
FDclass2<-classshape2$shapes


ggplot(nmscore,aes(nmscore[,1],nmscore[,2])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld1, aes(x=0, y=0, xend=ordnmld1[,1]*10, yend=ordnmld1[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordnmld1,aes(ordnmld1[,1]*10,ordnmld1[,2]*10),label=rownames(ordnmld1),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC1",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-7.5,5) +geom_text(aes(nmscore[,1],nmscore[,2]),label=km_s3$cluster,size=1.75)
ggplot(nmscore,aes(nmscore[,1],nmscore[,2])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld2, aes(x=0, y=0, xend=ordnmld2[,1]*10, yend=ordnmld2[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordnmld2,aes(ordnmld2[,1]*10,ordnmld2[,2]*10),label=rownames(ordnmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC2",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-7.5,5)

ggplot(nmscore,aes(nmscore[,1],nmscore[,3])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld3, aes(x=0, y=0, xend=ordnmld3[,1]*20, yend=ordnmld3[,3]*20), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordnmld3,aes(ordnmld3[,1]*10,ordnmld3[,3]*10),label=rownames(ordnmld3),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC3",x="PC1",y="PC3")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())
ggplot(nmscore,aes(nmscore[,2],nmscore[,3])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld3, aes(x=0, y=0, xend=ordnmld3[,2]*10, yend=ordnmld3[,3]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=ordnmld3,aes(ordnmld3[,2]*10,ordnmld3[,3]*10),label=rownames(ordnmld3),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC3",x="PC2",y="PC3")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 

ggplot(nmscore,aes(nmscore[,1],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld4, aes(x=0, y=0, xend=ordnmld4[,1]*10, yend=ordnmld4[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30")+ geom_text_repel(data=ordnmld4,aes(ordnmld4[,1]*10,ordnmld4[,4]*10),label=rownames(ordnmld4),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC4",x="PC1",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-10,5)

#ggplot(nmscore,aes(nmscore[,1],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld4, aes(x=0, y=0, xend=ordnmld4[,1]*10, yend=ordnmld4[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30")+ geom_text(aes(nmscore[,1],nmscore[,4]),label=nm.df.ul[,3],size=1.75) + geom_text_repel(data=ordnmld4,aes(ordnmld4[,1]*10,ordnmld4[,4]*10),label=rownames(ordnmld4),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC4",x="PC1",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-10,5)
ggplot(nmscore,aes(nmscore[,1],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=ordnmld4, aes(x=0, y=0, xend=ordnmld4[,1]*10, yend=ordnmld4[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30")+ geom_text(aes(nmscore[,1],nmscore[,4]),label=nm.df.ul[,2],size=1.75) + geom_text_repel(data=ordnmld4,aes(ordnmld4[,1]*10,ordnmld4[,4]*10),label=rownames(ordnmld4),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with strongest loadings, PC4",x="PC1",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-10,5)

#ggplot(nmscore,aes(nmscore[,2],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,2]*10, yend=nmld2[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,2]*10,nmld2[,4]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC2",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 

#ggplot(nmscore,aes(nmscore[,3],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,3]*10, yend=nmld2[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,3]*10,nmld2[,4]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC3",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 



library("corrplot")
par(mfrow=c(1,1))

corrplot(t(ordmetld1.78), is.corr=FALSE,tl.cex=.7,tl.col="black",cl.pos="n")
corrplot(t(ordmetld1.156), is.corr=FALSE,tl.cex=.7,tl.col="black",cl.pos="n")
corrplot(t(ordmetld1.234), is.corr=FALSE,tl.cex=.8,tl.col="black",cl.pos="n")




#corrplot#cos2 comparisons:
#sd.met<-b_pca_met@sDev
#sdPC1.met<-sd.met[1]
#sdPC2.met<-sd.met[2]
#met.cor<-data.frame(metld)
#met.cor$PC1cor<-met.cor$PC1*sdPC1.met
#met.cor$PC2cor<-met.cor$PC2*sdPC2.met
#met.cor$cos2PC1<-met.cor$PC1cor^2
#met.cor$cos2PC2<-met.cor$PC2cor^2
#met.cor$labels<-row.names(met.cor)
#head(met.cor)

library("corrplot")
corrplot(t(met.cor$cos2PC1), is.corr=FALSE)
barplot(sort(met.cor$cos2PC1, decreasing = TRUE),col="lightblue",las=2,names.arg=(met.cor$labels),cex.names=.5)




#Supervised learning exploration: other variables vs. food desert classification
#Removing variables that directly determine food desert status - ACCESS variables (cols 4:39), directly poverty/income measures (poverty rate,median hh inc, child pov. rate, free and reduced lunch eligibility)
#Random Forest

library(caret)
library(randomForest)

FD.met<-subset(deserts$`County >1/3 Food Desert?`,met.bool==T)
FD.nm<-subset(deserts$`County >1/3 Food Desert?`,met.bool==F)

#All variables:
met.sl<-cbind(met.df.ul[,4:ncol(met.df.ul)],'FD'=as.factor(FD.met))
nm.sl<-cbind(nm.df.ul[,4:ncol(nm.df.ul)],'FD'=as.factor(FD.nm))

#Eliminating access and poverty rate variables:
met.df.sl <- cbind(met.df.ul[,40:ncol(met.df.ul)], 'FD'=FD.met)
dlt.pov<-c("POVRATE15","PERPOV10","CHILDPOVRATE15","PERCHLDPOV10","MEDHHINC15","PCT_FREE_LUNCH10","PCT_FREE_LUNCH15","PCT_REDUCED_LUNCH10","PCT_REDUCED_LUNCH15")
met.df.sl<-met.df.sl[ , !(names(met.df.sl)) %in% dlt.pov]
met.df.sl$FD<-as.factor(met.df.sl$FD)

nm.df.sl <- cbind(nm.df.ul[,40:ncol(met.df.ul)], 'FD'=FD.nm)
nm.df.sl<-nm.df.sl[ , !(names(nm.df.sl)) %in% dlt.pov]
nm.df.sl$FD<-as.factor(nm.df.sl$FD)

#Run RF:
set.seed(37)
tlog<-sample(nrow(met.df.sl), .7*(nrow(met.df.sl)), replace = FALSE)
train_met1<-met.sl[tlog,]
test_met1<-met.sl[-tlog,]

train_met<-met.df.sl[tlog,]
test_met<-met.df.sl[-tlog,]

set.seed(89)
tlog<-sample(nrow(nm.df.sl), .7*(nrow(nm.df.sl)), replace = FALSE)
train_nm1<-nm.sl[tlog,]
test_nm1<-nm.sl[-tlog,]

train_nm<-nm.df.sl[tlog,]
test_nm<-nm.df.sl[-tlog,]

#RANDOM FOREST CLASSIFIER
#METRO:

#Impute:
set.seed(112)
met.train.imp<-rfImpute(FD~.,train_met)
set.seed(3)
met.test.imp<-rfImpute(FD~.,test_met)


set.seed(76)
met.rf1<-randomForest(FD~.,data=met.train.imp)

#Let's examine the predictive power of the models:

pred.rf1<-predict(met.rf1,newdata=met.test.imp)
table(pred.rf1)
3/(348+3)
#Only 3 metro counties were classified as FDs (.85%). This is likely due to class imbalance.

#Changing the threshold for clasification may help:

table(train_met$FD)
89/(89+727)
#89/727 of metro counties (~10.9%) are FDs

#To optimize cutoff level, we can compare ROC curves for a list of possible cutoff levels:
library(pROC)

try.i<-c(.5,.6,.7,.8,.9)
auc.list<-c()

set.seed(155)
for(i in try.i){
  met.rf<-randomForest(FD~.,data=met.train.imp,cutoff=c(i,1-i))
  auc.roc<-auc(roc(met.train.imp$FD,met.rf$votes[,2]))
  auc.list<-append(auc.list,auc.roc)}

plot(try.i,auc.list,type='b',main="Metro FD decision cutoff vs. ROC AUC")
#.7 seems to give the maximum AUC for the ROC plot of the metro area classifier.

#Re-run RF, METRO:
set.seed(76)
met.rf<-randomForest(FD~.,data=met.train.imp,cutoff=c(.7,1-.7),importance=T)
inp.met<-round(importance(met.rf), 2)
inp.met<-inp.met[order(-inp.met[,1]),]

#head(inp.met,40)
#met.rf$importance
#best.load.met<-names(head(inp.met,40))

library(pdp)
varImpPlot(met.rf,cex=.6,main="Variable Importance, Metro Food Desert Classifier")

par(mfrow=c(2,2))
partialPlot(met.rf,x.var="PC_SNAPBEN12",pred.data=met.train.imp,ylab="Partial dependence",which.class=1)
partialPlot(met.rf,x.var="Labor.PR..45.to.54.years",pred.data=met.train.imp,ylab="Partial dependence",which.class=1)
partialPlot(met.rf,x.var="CONVSPTH16",pred.data=met.train.imp,ylab="Partial dependence",which.class=1)
partialPlot(met.rf,x.var="Labor.PR..20.to.64.years..Male",pred.data=met.train.imp,ylab="Partial dependence",which.class=1)

partialPlot(met.rf,x.var="SNAPSPTH12",pred.data=met.train.imp,which.class=1,ylab="Partial dependence",main="Food Desert P.D. on SNAPSPTH12")
partialPlot(met.rf,x.var="SNAPSPTH17",pred.data=met.train.imp,which.class=1,ylab="Partial dependence",main="Food Desert P.D. on SNAPSPTH17")


#Revisiting test set prediction:
pred.rf<-predict(met.rf,newdata=met.test.imp)
table(pred.rf)
35/(35+316)

#Now we get 35 counties classified as food deserts in the metro category.
#This is ~10%, which is significantly closer to the training set proportion of 12.24%.
library(caret)
confusionMatrix(pred.rf,met.test.imp$FD)


#NONMETRO:
set.seed(672)
nm.train.imp<-rfImpute(FD~.,train_nm)
head(nm.train.imp)
set.seed(90)
nm.test.imp<-rfImpute(FD~.,test_nm)


table(pred.rf.nm)
90/(503+90)
15% of test data labeled as food desert
table(train_nm$FD)
417/(966+417)
30% of original non-metro data is for food desert
#For the non-metro data, class imbalance is still present but less severe.


try.i<-c(.5,.6,.7,.8,.9)
auc.list<-c()

set.seed(57)
for(i in try.i){
  nm.rf<-randomForest(FD~.,data=nm.train.imp,cutoff=c(i,1-i))
  auc.roc<-auc(roc(nm.train.imp$FD,nm.rf$votes[,2]))
  auc.list<-append(auc.list,auc.roc)}

plot(try.i,auc.list,type='b',main="Non-metro FD decision cutoff vs. ROC AUC")
#.6 is ideal for the non-metro data cutoff.

#Re-run RF:
set.seed(852)
nm.rf<-randomForest(FD~.,data=nm.train.imp,cutoff=c(.6,1-.6),importance =T)
inp.nm<-round(importance(nm.rf), 2)
inp.nm<-inp.nm[order(-inp.nm[,1]),]
inp.nm
par(mfrow=c(1,1))

varImpPlot(nm.rf,cex=.6,main="Variable Importance, Nonmetro Food Desert Classifier")

par(mfrow=c(2,2))
partialPlot(nm.rf,x.var="PC_SNAPBEN17",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence")
partialPlot(nm.rf,x.var="PC_SNAPBEN12",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence")
partialPlot(nm.rf,x.var="PCT_NHWHITE10",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence")
partialPlot(nm.rf,x.var="Labor.PR..20.to.64.years..Male",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence")

#partialPlot(nm.rf,x.var="SNAPSPTH12",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence",main="Non-metro 2012")
#partialPlot(nm.rf,x.var="SNAPSPTH17",pred.data=nm.train.imp,which.class=1,ylab="Partial dependence",main="Non-metro 2017")


head(inp.nm,40)
best.load.nm<-names(head(inp.nm,40))

#Revisiting test set prediction:
pred.rf.nm<-predict(nm.rf,newdata=nm.test.imp)
table(pred.rf.nm)
179/(179+414)
#This level arrives at ~30% food deserts which mirrors the training set.

confusionMatrix(pred.rf.nm,nm.test.imp$FD)
#Accuracy ~74%

print(best.load.met)
print(best.load.nm)



#Updating PCA biplots with selected loadings:
#METRO:
metld2<-as.data.frame(b_pca_met@loadings)
metld2<-metld2[(rownames(b_pca_met@loadings)) %in% best.load.met,]

shapes<-c(16,4)
classshape<-as.data.frame(met.df.sl$FD)
classshape$shapes<- shapes[classshape[,1]]
FDclass<-classshape$shapes

ggplot(metscore,aes(metscore[,1],metscore[,2]))+ geom_point(color=colors2,shape=FDclass)+ geom_segment(data=metld2, aes(x=0, y=0, xend=metld2[,1]*10, yend=metld2[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=metld2,aes(metld2[,1]*10,metld2[,2]*10),label=rownames(metld2),size=1.75,segment.color = 'grey50')+ labs(title="Metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank())

#NONMETRO:
nmld2<-as.data.frame(b_pca_nm@loadings)
nmld2<-nmld2[(rownames(b_pca_nm@loadings)) %in% best.load.nm,]

classshape2<-as.data.frame(nm.df.sl$FD)
classshape2$shapes<- shapes[classshape2[,1]]
FDclass2<-classshape2$shapes


ggplot(nmscore,aes(nmscore[,1],nmscore[,2])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,1]*10, yend=nmld2[,2]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,1]*10,nmld2[,2]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC1",y="PC2") + theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-7.5,5)

ggplot(nmscore,aes(nmscore[,1],nmscore[,3])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,1]*20, yend=nmld2[,3]*20), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,1]*20,nmld2[,3]*20),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC1",y="PC3")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-7.5,5) + ylim(-5,7.5)

ggplot(nmscore,aes(nmscore[,1],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,1]*10, yend=nmld2[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,1]*10,nmld2[,4]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC1",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) +xlim(-10,5)

ggplot(nmscore,aes(nmscore[,2],nmscore[,3])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,2]*10, yend=nmld2[,3]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,2]*10,nmld2[,3]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC2",y="PC3")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 

ggplot(nmscore,aes(nmscore[,2],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,2]*10, yend=nmld2[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,2]*10,nmld2[,4]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC2",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 

ggplot(nmscore,aes(nmscore[,3],nmscore[,4])) + geom_point(color=colors3,shape=FDclass2)+ geom_segment(data=nmld2, aes(x=0, y=0, xend=nmld2[,3]*10, yend=nmld2[,4]*10), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="gray30") + geom_text_repel(data=nmld2,aes(nmld2[,3]*10,nmld2[,4]*10),label=rownames(nmld2),size=1.75,segment.color = 'grey50')+ labs(title="Non-metro: Modified PCA Biplot",subtitle="with 40 best food desert predictors",x="PC3",y="PC4")+ theme(axis.line = element_line(colour = "black"),panel.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.border=element_blank()) 

#It appears that most of the food deserts in the metro category are clustered into the bottom portion of the graph.  Non-metro seems more complicated.  

#Assessing clusters in relation to food desert status.
#Chi-Square:
library(formattable)

#metro:
length(km_s2$cluster)
chisq.test(km_s2$cluster,met.df.sl$FD)
table(km_s2$cluster,met.df.sl$FD)
library(rcompanion)
cramerV(km_s2$cluster,met.df.sl$FD)


#non-metro:
chisq.test(km_s3$cluster,nm.df.sl$FD)
table(km_s3$cluster,nm.df.sl$FD)
cramerV(km_s3$cluster,nm.df.sl$FD)
#In both cases, food desert status is not independent of cluster, but this is unsurprising since there are related variables at play in the clustering.
#We can, however, examine strength of association for added information.


#Assessing cluster vs. diabetes rate:
#Data is non-normal so we can't perform ANOVA on the raw data.  We could transform, or as below, perform a Kruskal Wallis test:

kruskal.test(x=dia.met[,1],g=km_s2$cluster,formula=dia.met[,1]~km_s2$cluster)

library(FSA)

multiVDA(x = dia.met[,1],
         g = km_s2$cluster)
dunnTest(x = dia.met[,1],
         g = km_s2$cluster)

#Metro cluster diabetes rate medians are significantly different.

#metro DR table:
medmet1<-median(dia.met[km_s2$cluster==1,])
medmet2<-median(dia.met[km_s2$cluster==2,])
medmet3<-median(dia.met[km_s2$cluster==3,],na.rm=T)
mm1<-mean(dia.met[km_s2$cluster==1,])
mm2<-mean(dia.met[km_s2$cluster==2,])
mm3<-mean(dia.met[km_s2$cluster==3,],na.rm=T)

#nonmetro DR table:
medn1<-median(dia.nm[km_s3$cluster==1,])
medn2<-median(dia.nm[km_s3$cluster==2,])
medn3<-median(dia.nm[km_s3$cluster==3,])
mn1<-mean(dia.nm[km_s3$cluster==1,])
mn2<-mean(dia.nm[km_s3$cluster==2,])
mn3<-mean(dia.nm[km_s3$cluster==3,])


clusters<-c(1,2,3)
DR_med_metro<-c(medmet1,medmet2,medmet3)
DR_means_metro<-c(mm1,mm2,mm3)
data.frame(clusters,DR_med_metro,DR_means_metro)
table(data.frame(clusters,DR_med_metro,DR_means_metro))


DR_med_nonmetro<-c(medn1,medn2,medn3)
DR_means_nonmetro<-c(mn1,mn2,mn3)
data.frame(clusters,DR_med_nonmetro,DR_means_nonmetro)

kruskal.test(x=dia.nm[,1],g=km_s3$cluster,formula=dia.nm[,1]~km_s3$cluster)
#Non-metro cluster diabetes rate medians are significantly different.
dunnTest(x = dia.nm[,1],
         g = km_s3$cluster)

multiVDA(x = dia.nm[,1],
         g = km_s3$cluster)




#Random forest: variable effect on health outcomes
#Random forest regression
head(df.met.impute)
nm.train.imp

dia.met<-as.data.frame(subset(health$PCT_DIABETES_ADULTS13, met.bool==T))
dia.nm<-as.data.frame(subset(health$PCT_DIABETES_ADULTS13, met.bool==F))


set.seed(37)
tlog<-sample(nrow(met.df.sl), .7*(nrow(met.df.sl)), replace = FALSE)
train_met.dia<-dia.met[tlog,]
test_met.dia<-dia.met[-tlog,]
train.health.met<-cbind(met.train.imp,train_met.dia)
test.health.met<-cbind(met.test.imp,test_met.dia)
table(is.na(train.health.met))




#there is 1 NA in the training set.  We can remove it:
train.health.met<-na.omit(train.health.met)

rf_health_met<-randomForest(train_met.dia~.,data=train.health.met,importance=T)
varImpPlot(rf_health_met,cex=.7,main="RF Regression - Metro Var. Importance")

#MSE:
mean((test.health.met$test_met.dia - predict(rf_health_met, newdata=test.health.met)) ^ 2)


par(mfrow=c(2,2))
partialPlot(rf_health_met,x.var="HS.or.Equivalent...25.64.yr",pred.data=train.health.met,ylab="Partial dependence")
partialPlot(rf_health_met,x.var="PCT_NHBLACK10",pred.data=train.health.met,ylab="Partial dependence")
partialPlot(rf_health_met,x.var="Bachelors.or.higher...25.64.yr",pred.data=train.health.met,ylab="Partial dependence")
partialPlot(rf_health_met,x.var="PCT_SNAP12",pred.data=train.health.met,ylab="Partial dependence")

partialPlot(rf_health_met,x.var="SNAPSPTH12",pred.data=train.health.met,ylab="Partial dependence",main="Diabetes Rate P.D. on SNAPSPTH12")
partialPlot(rf_health_met,x.var="SNAPSPTH17",pred.data=train.health.met,ylab="Partial dependence",main="Diabetes Rate P.D. on SNAPSPTH17")

set.seed(89)
tlog<-sample(nrow(nm.df.sl), .7*(nrow(nm.df.sl)), replace = FALSE)
train_nm.dia<-dia.nm[tlog,]
test_nm.dia<-dia.nm[-tlog,]
train.health.nm<-cbind(nm.train.imp,train_nm.dia)
test.health.nm<-cbind(nm.test.imp,test_nm.dia)

rf_health_nm<-randomForest(train_nm.dia~.,data=train.health.nm,importance=T)
summary(rf_health_nm)

#MSE:
mean((test.health.nm$test_nm.dia - predict(rf_health_nm, newdata=test.health.nm)) ^ 2)


varImpPlot(rf_health_nm,cex=.7,main="RF Regression - Non-Metro Var. Importance")
par(mfrow=c(2,2))

partialPlot(rf_health_nm,x.var="PCT_65OLDER10",pred.data=train.health.nm,ylab="Partial dependence")
partialPlot(rf_health_nm,x.var="PCT_NHBLACK10",pred.data=train.health.nm,ylab="Partial dependence")
partialPlot(rf_health_nm,x.var="PCT_SNAP12",pred.data=train.health.nm,ylab="Partial dependence")
partialPlot(rf_health_nm,x.var="PC_SNAPBEN12",pred.data=train.health.nm,ylab="Partial dependence")

#See PPT, video, or article for full analysis.


