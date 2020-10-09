
# Include Foreign Package To Read SAS Transport Files
library(foreign)
library(dummies)
library(tidyr)
library(dplyr)

# Create Temporary File To Store Downloaded SAS Transport Files
tf <- tempfile()

# Download NHANES 2015-2016 Demographic Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
DEMO <- read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH3")]
head(DEMO)
DEMO$Gender <- factor(DEMO$RIAGENDR, labels=c("Male","Female"))
#DEMO$AgeGroup <- cut(DEMO$RIDAGEYR, breaks=c(-1,19,39,59,86), 
#                              labels=c("0-19","20-39","40-59","60+"))

DEMO$Race <- factor(DEMO$RIDRETH3, labels=c("Mexican American","Other Hispanic","Non-Hispanic White","Non-Hispanic Black","6	Non-Hispanic Asian","Other Race - Including Multi-Racial"))
#Age = dummy(DEMO$AgeGroup)
Race = dummy(DEMO$Race)

demo = as.data.frame(cbind(SEQN=DEMO$SEQN,Gender=DEMO$Gender,Age=DEMO$RIDAGEYR,Race))
demo$Gender=case_when(demo$Gender==1 ~ 0, demo$Gender==2 ~ 1)



# Download NHANES 2015-2016 Diabetes Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
diabetes <- read.xport(tf)[,c("SEQN","DIQ010")]
diabetes<-diabetes[with(diabetes,diabetes$DIQ010<=3),]
diabetes$DIQ010 <- factor(diabetes$DIQ010, labels=c("Yes","No","Borderline"))
dia = dummy(diabetes$DIQ010)
diabetes = as.data.frame(cbind(SEQN=diabetes$SEQN,dia))
diabetes<-diabetes[1:3]


# Download NHANES 2015-2016 BMI Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
BMI <- read.xport(tf)[,c("SEQN","BMXBMI")]
#BMI<-na.omit(BMI)
#overweight$ow[overweight$BMXBMI<25] = 0
#overweight$ow[overweight$BMXBMI>=25] = 1
#overweight1 <- overweight[,c("SEQN","ow")]

# Download NHANES 2015-2016 Blood Pressure Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPX_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
bp <- read.xport(tf)[,c("SEQN","BPXPLS","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")]
bp1<-bp[complete.cases(bp[,2]),]
bp1[is.na(bp1)]<-0
bp1$avgSY <- apply(bp1[,3:6], 1, mean)
bp1$avgDI <- apply(bp1[,7:10], 1, mean)
bp2<-bp1[,c("SEQN","BPXPLS","avgSY","avgDI")]



# Download NHANES 2015-2016 Medical Conditions Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
medic <- read.xport(tf)[,c("SEQN","MCQ300A","MCQ300B","MCQ300C","MCQ365A","MCQ365B","MCQ365C","MCQ365D","MCQ160C","MCQ160B","MCQ160D","MCQ160E")]
medic<-na.omit(medic)
medic1<-medic[with(medic,medic$MCQ300A<=2),]
medic1$MCQ300A[with(MCQ300A,medic1$MCQ300A==1)] = 1
medic1$MCQ300A[with(MCQ300A,medic1$MCQ300A==2)] = 0
MCQ300A<-medic1[1:2]

medic2<-medic[with(medic,medic$MCQ300B<=2),]
medic2$MCQ300B[with(MCQ300B,medic2$MCQ300B==1)] = 1
medic2$MCQ300B[with(MCQ300B,medic2$MCQ300B==2)] = 0
MCQ300B<-medic2[c(1,3)]


medic3<-medic[with(medic,medic$MCQ300C<=2),]
medic3$MCQ300C[with(MCQ300C,medic3$MCQ300C==1)] = 1
medic3$MCQ300C[with(MCQ300C,medic3$MCQ300C==2)] = 0
MCQ300C<-medic3[c(1,4)]

medic4<-medic[with(medic,medic$MCQ365A<=2),]
medic4$MCQ365A[with(MCQ365A,medic4$MCQ365A==1)] = 1
medic4$MCQ365A[with(MCQ365A,medic4$MCQ365A==2)] = 0
MCQ365A<-medic4[c(1,5)]

medic5<-medic[with(medic,medic$MCQ365B<=2),]
medic5$MCQ365B[with(MCQ365B,medic5$MCQ365B==1)] = 1
medic5$MCQ365B[with(MCQ365B,medic5$MCQ365B==2)] = 0
MCQ365B<-medic5[c(1,6)]


medic6<-medic[with(medic,medic$MCQ365C<=2),]
medic6$MCQ365C[with(MCQ365C,medic6$MCQ365C==1)] = 1
medic6$MCQ365C[with(MCQ365C,medic6$MCQ365C==2)] = 0
MCQ365C<-medic6[c(1,7)]

medic7<-medic[with(medic,medic$MCQ365D<=2),]
medic7$MCQ365D[with(MCQ365D,medic7$MCQ365D==1)] = 1
medic7$MCQ365D[with(MCQ365D,medic7$MCQ365D==2)] = 0
MCQ365D<-medic7[c(1,8)]



med <- Reduce(function(x,y) merge(x = x, y = y, by = "SEQN"), 
              list(MCQ300A,MCQ300B,MCQ300C,MCQ365A,MCQ365B,MCQ365C,MCQ365D))


ang<-medic[,c("SEQN","MCQ160D")]
ang$MCQ160D=case_when(ang$MCQ160D==1 ~ 1, ang$MCQ160D==2 ~ 0)



# Download NHANES 2015-2016 Cholesterol - Total Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TCHOL_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
tc <- read.xport(tf)[,c("SEQN","LBDTCSI")]
#tc<-na.omit(tc)

# Download NHANES 2015-2016 Cholesterol - High-Density Lipoprotein (HDL) Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HDL_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
hdl <- read.xport(tf)[,c("SEQN","LBDHDDSI")]
#hdl<-na.omit(hdl)
tc_hdl <- merge(tc,hdl, by = "SEQN")
tc_hdl<-mutate(tc_hdl,AI=tc_hdl$LBDTCSI/tc_hdl$LBDHDDSI)
AI<-tc_hdl[,c(1,4)]

# Download NHANES 2015-2016 Complete Blood Count with 5-Part Differential - Whole Blood Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CBC_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
mcv <- read.xport(tf)[,c("SEQN","LBXMCVSI")]
#mcv<-na.omit(mcv)


# Download NHANES 2015-2016 Standard Biochemistry Profile Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BIOPRO_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
cr_ua <- read.xport(tf)[,c("SEQN","LBXSCR","LBXSUA")]
#cr_ua<-na.omit(cr_ua)



# Merge all tables
alltable<-Reduce(function(x,y) merge(x = x, y = y, by = "SEQN"), 
                 list(demo, diabetes, BMI,bp2,tc,AI,mcv,cr_ua,ang,med))

write.csv(alltable,"/Users/tmh/Desktop/MedBlob/all_heart.csv")

##### Tareget
heart <- medic[medic$MCQ160C==1|medic$MCQ160B==1|medic$MCQ160E==1,] # 得病

Records_heart_disease <-heart %>% mutate(y = 1) # 产生得病y
Records_heart_disease <-Records_heart_disease[c("SEQN","y")]
nrow(Records_heart_disease)

Healthy_Records <- alltable[-Records_heart_disease$SEQN,] %>% mutate(y = 0)
Healthy_Records <-Healthy_Records[c("SEQN","y")]
table = rbind(Healthy_Records,Records_heart_disease)
table<-table[c("SEQN","y")]

table_final = merge(alltable,table, by = "SEQN")
table_final[is.na(table_final)]<-0


length(table_final$y[table_final$y==1])

table_final1<-na.omit(table_final)
table_final1$Gender<- as.factor(table_final1$Gender)
table_final1$`RaceMexican American`<-as.factor(table_final1$`RaceMexican American`)
table_final1$`RaceOther Hispanic`<-as.factor(table_final1$`RaceOther Hispanic`)
table_final1$`RaceNon-Hispanic White`<-as.factor(table_final1$`RaceNon-Hispanic White`)
table_final1$`RaceNon-Hispanic Black`<-as.factor(table_final1$`RaceNon-Hispanic Black`)
table_final1$`Race6	Non-Hispanic Asian`<-as.factor(table_final1$`Race6	Non-Hispanic Asian`)
table_final1$`RaceOther Race - Including Multi-Racial`<-as.factor(table_final1$`RaceOther Race - Including Multi-Racial`)
table_final1$DIQ010Yes<-as.factor(table_final1$DIQ010Yes)
table_final1$DIQ010No<-as.factor(table_final1$DIQ010No)
table_final1$y<-as.factor(table_final1$y)

colnames(table_final1)
write.csv(table_final,"/Users/tmh/Desktop/MedBlob/all_heart.csv")


# normalize input variables
table_final1<-as.data.frame(table_final)
selected<- table_final1[,c(3,12:20 )]
normalization<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
norm<-normalization(selected)
table_final2<-cbind(table_final1[,c(1:2,4:11,21:28)],norm,y=table_final1[,29])

write.csv(table_final2,"/Users/tmh/Desktop/MedBlob/all_heart(normalized).csv")

scale<-scale(selected)
table_final4<-cbind(table_final1[,c(1:2,4:11,21:28)],scale,y=table_final1[,29])
write.csv(table_final4,"/Users/tmh/Desktop/MedBlob/all_heart(standard).csv")
#######
library(glmnet)


# divide the dataset into training and testing parts
a<-table_final4[,2:29]
length_dataset=dim(a)[1]*0.8
train.index=sample(c(1:length_dataset),555)
train.df=a[train.index,]
valid.df=a[-train.index,]


train.x = as.matrix(train.df[2:28])
train.y = train.df[29]
train.y<- as.numeric(train.df$y)
valid.x = as.matrix(valid.df[2:28])
valid.y = valid.df[29]
valid.y<- as.numeric(valid.df$y)
r2 <- glmnet(train.x,train.y,family = "binomial",alpha = 1)
plot(r2)
r2.cv <- cv.glmnet(train.x,train.y,family = "multinomial",alpha = 1,nfolds = 10)
plot(r2.cv)
r2.cv$lambda.min
r2.cv$lambda.1se
# get the coefficients of lasso lambda.1se
r2.1se <- glmnet(train.x,train.y, family = "multinomial", alpha = 1, lambda = r2.cv$lambda.1se)
r2.min_coef <- coef(r2.1se)
print(r2.min_coef)

# evaluation, chosing lambda.1se：largest value of lambda such that error is within 1 standard error of the minimum.

# training data
lasso.pred <- predict(r2, s = r2.cv$lambda.1se, newx = train.x,type = "class")

# accuracy
(length(which(lasso.pred==train.y)==TRUE))/length(train.y)*100


# validation data

lasso.pred <- predict(r2, s = r2.cv$lambda.1se, newx = valid.x,type = "class")

# accuracy
(length(which(lasso.pred==valid.y)==TRUE))/length(valid.y)*100




#####
# Model 1-3: Random forest

library(randomForest)
train.df$y=as.factor(train.df$y)
valid.df$y=as.factor(valid.df$y)
train.df$y=as.factor(train.df$y)
forest= randomForest(y~.,data = train.df,ntree=500,mtry=20,importance=TRUE)
forest$importance
varImpPlot(forest, main = "variable importance")

pre_ran=predict(forest,newdata = valid.df)
pre_ran2=predict(forest,newdata = train.df)
obs_p_ran=data.frame(prob=pre_ran,obs=valid.df$y)
conf_nat=table(valid.df$y,pre_ran)
conf_nat_2=table(train.df$y,pre_ran2)

(sum(diag(conf_nat_2))/sum(conf_nat_2)*100)
(Accuracy <-sum(diag(conf_nat))/sum(conf_nat)*100)

#matrix 3
matrix1=confusionMatrix(pre_ran, as.factor(valid.df$y))
matrix1$byClass
