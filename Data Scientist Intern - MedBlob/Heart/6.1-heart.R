
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
medic <- read.xport(tf)[,c("SEQN","MCQ300A","MCQ300B","MCQ300C","MCQ365A","MCQ365B","MCQ365C","MCQ365D","MCQ160C","MCQ160B","MCQ160D","MCQ160E","MCQ160F")]
medic<-na.omit(medic)

medic$MCQ160C = case_when(medic$MCQ160C == 1 ~ 1, medic$MCQ160C == 2 ~ 0)
medic$MCQ160C = as.factor(medic$MCQ160C)

medic$MCQ160B = case_when(medic$MCQ160B == 1 ~ 1, medic$MCQ160B == 2 ~ 0)
medic$MCQ160B = as.factor(medic$MCQ160B)

medic$MCQ160D = case_when(medic$MCQ160D == 1 ~ 1, medic$MCQ160D == 2 ~ 0)
medic$MCQ160D = as.factor(medic$MCQ160D)

medic$MCQ160E = case_when(medic$MCQ160E == 1 ~ 1, medic$MCQ160E == 2 ~ 0)
medic$MCQ160E = as.factor(medic$MCQ160E)

medic$MCQ160F = case_when(medic$MCQ160F == 1 ~ 1, medic$MCQ160F == 2 ~ 0)
medic$MCQ160F = as.factor(medic$MCQ160F)


medic$MCQ300A = case_when(medic$MCQ300A == 1 ~ 1, medic$MCQ300A == 2 ~ 0)
medic$MCQ300A = as.factor(medic$MCQ300A)

medic$MCQ300B = case_when(medic$MCQ300B == 1 ~ 1, medic$MCQ300B == 2 ~ 0)
medic$MCQ300B = as.factor(medic$MCQ300B)

medic$MCQ300C = case_when(medic$MCQ300C == 1 ~ 1, medic$MCQ300C == 2 ~ 0)
medic$MCQ300C = as.factor(medic$MCQ300C)

medic$MCQ365A = case_when(medic$MCQ365A == 1 ~ 1, medic$MCQ365A == 2 ~ 0)
medic$MCQ365A = as.factor(medic$MCQ365A)

medic$MCQ365B = case_when(medic$MCQ365B == 1 ~ 1, medic$MCQ365B == 2 ~ 0)
medic$MCQ365B = as.factor(medic$MCQ365B)


medic$MCQ365C = case_when(medic$MCQ365C == 1 ~ 1, medic$MCQ365C == 2 ~ 0)
medic$MCQ365C = as.factor(medic$MCQ365C)

medic$MCQ365D = case_when(medic$MCQ365D == 1 ~ 1, medic$MCQ365D == 2 ~ 0)
medic$MCQ365D = as.factor(medic$MCQ365D)



med <- medic %>% select(-c("MCQ160C","MCQ160B","MCQ160E")) %>% drop_na()




# Download NHANES 2015-2016 Cholesterol - Total Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TCHOL_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
tc <- read.xport(tf)[,c("SEQN","LBXTC")]
#tc<-na.omit(tc)

# Download NHANES 2015-2016 Cholesterol - High-Density Lipoprotein (HDL) Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HDL_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
hdl <- read.xport(tf)[,c("SEQN","LBDHDD")]
#hdl<-na.omit(hdl)
tc_hdl <- merge(tc,hdl, by = "SEQN")
tc_hdl<-mutate(tc_hdl,AI=tc_hdl$LBXTC/tc_hdl$LBDHDD)
AI<-tc_hdl[,c(1,4)]


# Download NHANES 2015-2016 Cholesterol - low-Density Lipoprotein (LDL) Data 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/TRIGLY_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
ldl <- read.xport(tf)[,c("SEQN","LBDLDL")]
#hdl<-na.omit(hdl)



# Download NHANES 2015-2016smokeData 
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT", tf, mode="wb")
# Create Data Frame From Temporary File & Keep Variables Of Interest                                              
smoke <- read.xport(tf)[,c("SEQN","SMQ040")]
#hdl<-na.omit(hdl)

smoke$SMQ040<- case_when(smoke$smokenow==1 ~ 1, smoke$smokenow==2 ~ 2,smoke$smokenow==3 ~ 0)
smoke$SMQ020<- as.factor(smoke$SMQ020)
smoke<- na.omit(smoke)


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
                 list(demo, diabetes, BMI,bp2,tc,hdl,ldl,smoke,AI,mcv,cr_ua,med))

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




# normalize input variables
table_final1<-as.data.frame(table_final)
selected<- table_final1[,c(3,12:18,20:23)]
normalization<-function(x){
  return((x-min(x))/(max(x)-min(x)))}
norm<-normalization(selected)
table_final2<-cbind(table_final1[,c(1:2,4:11,19,24:32)],norm,y=table_final1[,33])


colnames(table_final2) = c('SEQN','Gender','RaceMexican_American','RaceOther_Hispanic','RaceNonHispanic_White','RaceNonHispanic_Black','RaceNonHispanic_Asian','RaceOther Race - Including Multi-Racial','Diabetes','No_(Pre)_Diabetes','Smoke','Close_Relative_Heart_Attack','Close_Relative_Asthma','Close_Relative_Diabetes','Overweight','Lack_Exercise','High_Salt_in_Diet','Hight_Fat/Calories','Angina','Stroke','Age','BMI','Pulse','AVG_Systolic_Blood_Pressure','AVG_Diastolic_Blood_Pressure','Total_Cholesterol','HDL','LDL','Atherosclerosis_Index','Complete_Blood_Count_with_5-part_Differential','Standard_Biochemistry_Profile','Uric_Acid','y')
         


write.csv(table_final2,"/Users/tmh/Desktop/MedBlob/all_heart(normalized).csv")





