#socal_Cm_lms
library(ggplot2)

setwd("C:/Users/shrey/Box/MEC_LAB_DOCS/MEC lab Projects/SDBay_Chmy_health")
data<-read.csv("SoCal_Cm_all.data.csv")
data<-data[,-1] #remove index column
str(data)

#predictors
summary(data)
#scl.std, straight carapace length
#SDmeantemp - indexes seasonality
ggplot(data,aes(x=scl.std,y=SDmeantemp,color=Sex))+geom_point()
cor(data$SDmeantemp,data$scl.std) #scl.std and temp are not correlated

#data prep
data$temp.std<-scale(data$SDmeantemp,center=T,scale=T)
mean(data$temp.std)
sd(data$temp.std)
data$scl.std<-scale(data$SCL,center=T,scale=T)
mean(data$scl.std)
sd(data$scl.std)
data$BCI.std <- scale(data$BCI, center=T, scale=T)
mean(data$BCI.std, na.rm=T)
sd(data$BCI.std)

#subset data
data.chem <- subset(data, Glucose != "NA")         #39 obs
data.hema <- subset(data, Hematocrit_ave != "NA")  #20 obs

#first variable, glucose
m1<-lm(data=data.chem, Glucose~scl.std*temp.std*BCI.std)
summary(m1)
vif(m1) #VIF is no issue
vif(data$Glucose, data[,c(47:49)])
usdm::vif(data[,c(47:49)])

#BUN
m2<-lm(data=data.chem, BUN~scl.std*temp.std*BCI.std) 
summary(m2)

#Creatinine
m3<-lm(data=data.chem, Creatinine~scl.std*temp.std*BCI.std) 
summary(m3)

#Cholesterol
m4<-lm(data=data.chem, Cholesterol~scl.std*temp.std*BCI.std) 
summary(m4)


#Triglycerides
m5<-lm(data=data.chem, Triglycerides~scl.std*temp.std*BCI.std) 
summary(m5)

#Total_Protein
m6<-lm(data=data.chem, Total_Protein~scl.std*temp.std*BCI.std) 
summary(m6)
ggplot(data.chem,aes(x=temp.std,y=Total_Protein))+geom_point()

#Albumin
m7<-lm(data=data.chem, Albumin~scl.std*temp.std*BCI.std) 
summary(m7)
ggplot(data.chem,aes(x=temp.std,y=Albumin))+geom_point()
#NO effect

#Globulin
m8<-lm(data=data.chem, Globulin~scl.std*temp.std*BCI.std) 
summary(m8)
ggplot(data.chem,aes(x=scl.std,y=Globulin))+geom_point()
ggplot(data.chem,aes(x=temp.std,y=Globulin))+geom_point()

#AST
m9<-lm(data=data.chem, AST~scl.std*temp.std*BCI.std) 
summary(m9)

#CK
m10<-lm(data=data.chem, CK~scl.std*temp.std*BCI.std) 
summary(m10)

#LD
m11<-lm(data=data.chem, LD~scl.std*temp.std*BCI.std) 
summary(m11)


#Calcium
m12<-lm(data=data.chem, Calcium~scl.std*temp.std*BCI.std) 
summary(m12)

#Phosphorous
m13<-lm(data=data.chem, Phosphorous~scl.std*temp.std*BCI.std) 
summary(m13)

#Iron
m14<-lm(data=data.chem, Iron~scl.std*temp.std*BCI.std) 
summary(m14)

#Sodium
m15<-lm(data=data.chem, Sodium~scl.std*temp.std*BCI.std) 
summary(m15)

#Potassium
m16<-lm(data=data.chem, Potassium~scl.std*temp.std*BCI.std) 
summary(m16)
ggplot(data.chem,aes(x=scl.std,y=Potassium))+geom_point()

#Chloride
m17<-lm(data=data.chem, Chloride~scl.std*temp.std*BCI.std) 
summary(m17)
ggplot(data.chem,aes(x=scl.std,y=Chloride))+geom_point()

#CO2
m18<-lm(data=data.chem, CO2~scl.std*temp.std*BCI.std) 
summary(m18)

#Uric.Acid
m19<-lm(data=data.chem, Uric.Acid~scl.std*temp.std*BCI.std) 
summary(m19)

#Hematocrit
m20<-lm(data=data.hema, Hematocrit_ave~scl.std*temp.std*BCI.std) 
summary(m20)



