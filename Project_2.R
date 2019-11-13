# Load data and remove observations with erroneous values
setwd("C:/Users/laurenfo/Documents/Courses/FYS-STK4155/Project 2")	#Set working directory
project.2.data = read.table("Project_2_data.tsv", header=T, check.names=F)	#Load data
unique(project.2.data[,2])	#No wrong values
unique(project.2.data[,3])	#No wrong values
unique(project.2.data[,4])	#Wrong values: 5, 6 present when there should be only 1, 2 ,3 ,4
nrow(project.2.data[project.2.data[,4] %in% c(5,6),])	#331 observations to remove
project.2.data = project.2.data[project.2.data[,4] %in% c(1,2,3,4),]	#Keep observations with education levels 1-4
unique(project.2.data[,5])	#Wrong values: 0 present when there should be only 1, 2 ,3
nrow(project.2.data[project.2.data[,5]==0,])	#54 observations to remove
project.2.data = project.2.data[project.2.data[,5] %in% c(1,2,3),]	#Keep observations with marriage levels 1-3

#Write filtered dataset to file
write.table(project.2.data, "Project_2_data_filtered.tsv", append = FALSE, sep = "\t", dec = ".", row.names = TRUE, col.names = TRUE)

sort(unique(project.2.data[,6]))	#No wrong values
sort(unique(project.2.data[,7]))	#Wrong values: -2, 0.
project.2.data = project.2.data[project.2.data[,7] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
project.2.data = project.2.data[project.2.data[,8] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
project.2.data = project.2.data[project.2.data[,9] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
project.2.data = project.2.data[project.2.data[,10] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
project.2.data = project.2.data[project.2.data[,11] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
project.2.data = project.2.data[project.2.data[,12] %in% c(-1,1,2,3,4,5,6,7,8),]	#Keep observations with payment levels -1,1,2,3,4,5,6,7,8
nrow(project.2.data[project.2.data[,20:24]<0,])	#No wrong values
nrow(project.2.data)	#4030 observations remaining
nrow(project.2.data[project.2.data[,25]==1,])/nrow(project.2.data)	#36 % of remaining observations have defaulted

# Scale data
project.2.data.scaled = project.2.data
project.2.data.scaled[,2] = scale(project.2.data.scaled[,2], scale=T, center=T)	#Divide values by standar deviation and set column means to 0
project.2.data.scaled[,13:24] =  scale(project.2.data[,13:24], scale=T, center=T)	#Divide values by standar deviation and set column means to 0
project.2.data.credit = project.2.data[,2]
project.2.data.socioeconomic = project.2.data[,c(3:6)]
project.2.data.past_payment = project.2.data[,c(7:12)]
project.2.data.bill_statement = project.2.data.scaled[,c(13:18)]
project.2.data.previous_payment = project.2.data.scaled[,c(19:24)]
project.2.data.default = project.2.data[,25]

#Set categorical variables to factor
for (column in c(3,5,25)){
	project.2.data[,column] = as.factor(project.2.data[,column])
	project.2.data.scaled[,column] = as.factor(project.2.data.scaled[,column])
	}
for (column in c(4,7:12)){
	project.2.data[,column] = as.ordered(project.2.data[,column])
	project.2.data.scaled[,column] = as.factor(project.2.data.scaled[,column])
	}
project.2.data[,2] = as.numeric(project.2.data[,2])
project.2.data.scaled[,2] = as.numeric(project.2.data.scaled[,2])
project.2.data[,6] = as.numeric(project.2.data[,6])
project.2.data.scaled[,6] = as.numeric(project.2.data.scaled[,6])
project.2.data[,25] = as.factor(project.2.data[,25])
project.2.data.scaled[,25] = as.factor(project.2.data.scaled[,25])


# Compute distance matrix
library(cluster)
project.2.data.dist = daisy(project.2.data[,2:25], metric="gower")
library(vegan)
data.mds = metaMDS(project.2.data.dist, autotransform =FALSE)
ef = envfit(data.mds, project.2.data, permu=9, na.rm=T)
ordiplot(data.mds)
plot(ef, p.max = 0.05, cex=0.6)

# PCA
rda.out = rda(project.2.data.dist, scale=F)	#Distance matrix provided as input, gower distance metric scaled numeric variables already anyway. PCA does not work in this case (mixed types of variables)

#RDA
project.2.data.rda = rda(X=as.numeric(project.2.data[,25]), Y=project.2.data[,2:24])	#X is the response data (default), Y is the data selected as explanatory to X, Y is the data selected as explanatory to X
anova(project.2.data.rda)
RsquareAdj(project.2.data.rda)


part=varpart(as.numeric(project.2.data[,25]), project.2.data.socioeconomic, project.2.data.past_payment, project.2.data.bill_statement)
plot(part, bg = c("red", "blue", "green"), alpha=70, Xnames=c("Socioeconomic", "Past payment", "Bill statement"), cutoff=0, digits=4)

#CCA
project.2.data.cca=cca(X=as.numeric(project.2.data[,25]), Y=project.2.data.scaled[,2:24])	#X is the response data (default), Y is the data selected as explanatory to X
#, Z=phytoplankton.hel.no.na.df[,2])	#, Z is the data to be partialled out.
anova(project.2.data.cca)
RsquareAdj(project.2.data.cca)


project.2.data.cca=cca(X=project.2.data.socioeconomic, Y=project.2.data[,25])
project.2.data.cca=cca(X=as.numeric(project.2.data.scaled[,25]), Y=project.2.data[,2])
anova(project.2.data.cca)
RsquareAdj(project.2.data.cca)


