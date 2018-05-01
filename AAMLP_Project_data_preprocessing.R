library("dplyr")
library("RNHANES")

##################JW
LACR = nhanes_load_data("ALB_CR_H", "2013-2014")
QSMOKING = nhanes_load_data("SMQ_H", "2013-2014")%>%
  transmute(SEQN=SEQN, Smoke100=SMQ020)
###############convert to # of drinks/week in the past 12 months##################
QALCOHOL = nhanes_load_data("ALQ_H", "2013-2014")%>%
  transmute(SEQN=SEQN, AlcoQ=ALQ120Q, AlcoU=ALQ120U)
QALCOHOL$DrinkperWeek = ifelse(QALCOHOL$AlcoU==1,QALCOHOL$AlcoQ, 
                               ifelse(QALCOHOL$AlcoU==2,QALCOHOL$AlcoQ/4, 
                                      ifelse(QALCOHOL$AlcoU==3,QALCOHOL$AlcoQ/52,NA)))
QKIDNEY = nhanes_load_data("KIQ_U_H", "2013-2014")%>%
  transmute(SEQN=SEQN, DiagFallingKidney=KIQ022, KidneyStone=KIQ026, UrinaryLeakage=KIQ005)
QDIET1day = nhanes_load_data("DR1TOT_H", "2013-2014")%>%
  transmute(SEQN=SEQN, Energy_kcal=DR1TKCAL, Protein_gm=DR1TPROT, Carbohydrate_gm=DR1TCARB,
            Sugars_gm=DR1TSUGR,DietaryFiber_gm=DR1TFIBE, TotalFat_gm=DR1TTFAT,saturatedfattyacids_gm=DR1TSFAT,
            Cholesterol_mg=DR1TCHOL,Retinol_mcg=DR1TRET, Lycopene_mcg=DR1TLYCO,ThiaminVitaminB1_mg=DR1TVB1, Niacin_mg=DR1TNIAC,
            VitaminB6_mg=DR1TVB6, choline_mg=DR1TCHL, VitaminB12_mcg=DR1TVB12,VitaminC_mg=DR1TVC,VitaminD_mcg=DR1TVD,
            VitaminK_mcg=DR1TVK,Calcium_mg=DR1TCALC,Phosphorus_mg=DR1TPHOS, Magnesium_mg=DR1TMAGN,Iron_mg=DR1TIRON,Zinc_mg=DR1TZINC,
            Copper_mg=DR1TCOPP, Sodium_mg=DR1TSODI,Potassium_mg=DR1TPOTA,Selenium_mcg=DR1TSELE,Caffeine_mg=DR1TCAFF,Theobromine_mg=DR1TTHEO,
            Alcohol_gm=DR1TALCO,Moisture_gm=DR1TMOIS)
QMENTAL = nhanes_load_data("DPQ_H", "2013-2014")%>%
  transmute(SEQN=SEQN, Depressed=DPQ020, TroubleSleep=DPQ030)

################eliminate pregnant & age < 18 participants####################
DEMOGRAPHIC = nhanes_load_data("DEMO_H", "2013-2014") %>% 
  transmute(SEQN=SEQN, Gender=RIAGENDR, Age=RIDAGEYR, Race=RIDRETH1,
            Education19=DMDEDUC2,Pregnancy=RIDEXPRG,HHIncome=INDHHIN2) %>%
  filter(Pregnancy %in% c(NA, 2,3)) %>% filter(Age >= 18)
EBLOODPRESSURE = nhanes_load_data("BPX_H", "2013-2014") %>%
  transmute(SEQN=SEQN, Systolic_mmHg=BPXSY1, Diastolic_mmHg=BPXDI1)
EBODY = nhanes_load_data("BMX_H", "2013-2014") %>%
  transmute(SEQN=SEQN, BMI=BMXBMI)
SBioChem =  nhanes_load_data("BIOPRO_H", "2013-2014") %>%
  transmute(SEQN=SEQN, SCreatinine_mg_dL=LBXSCR)


########################joining non-lab tables#######################
combine = DEMOGRAPHIC %>% inner_join(SBioChem,by="SEQN") %>%
  inner_join(EBODY, by="SEQN")%>%
  inner_join(EBLOODPRESSURE,by="SEQN")%>%
  inner_join(QALCOHOL, by="SEQN")%>%
  inner_join(QSMOKING, by="SEQN")%>%
  inner_join(QKIDNEY, by="SEQN")%>%
  inner_join(QMENTAL, by="SEQN")%>%
  inner_join(QDIET1day, by="SEQN")


################calculate eGFR for every participant##########################
combine$black = ifelse(combine$Race == 4, 1.21, 1)
combine$female = ifelse(combine$Gender == 2, 0.742, 1)
combine$eGFR = 186*combine$SCreatinine_mg_dL^(-1.154)*combine$Age^(-0.203)*combine$black*combine$female
combine <- combine %>% select(-Pregnancy, -AlcoQ, -AlcoU, -black, -female,
                              -SCreatinine_mg_dL,-Age,-Race,-Gender,
                              -DiagFallingKidney, -KidneyStone)
#delete some really rare level, for the convenience of future modeling.
combine <- combine %>% filter(Education19 != 9)



##################shuyi
#Standard Biochemistry Profile
BIOPRO_H <- nhanes_load_data("BIOPRO_H", "2013-2014") %>% 
  select(-file_name, -cycle, -begin_year, -end_year) %>%
  select(-LBDSALSI, -LBDSBUSI, -LBDSCASI, -LBDSCHSI, -LBDSCRSI, -LBDSGBSI, -LBDSGLSI
         -LBDSIRSI , -LBDSPHSI, -LBDSTBSI , -LBDSTPSI, -LBXSCR) 
BIOPRO_H <- BIOPRO_H[!is.na(BIOPRO_H$LBXSAL),]
colnames(BIOPRO_H)

#urine flow rate
UCFLOW_H <- nhanes_load_data("UCFLOW_H", "2013-2014") %>% 
  select(-file_name, -cycle, -begin_year, -end_year) %>%
  select(SEQN, URXVOL1) %>% na.omit()

#urine metals
UM_H <- nhanes_load_data("UM_H", "2013-2014") %>% 
  select(-file_name, -cycle, -begin_year, -end_year) %>%
  select(-WTSA2YR, -URXUBA, -URDUBALC, -URDUCDLC, -URDUCOLC, -URDUCSLC, -URDUMOLC,
         -URDUMNLC, -URDUPBLC, -URDUSBLC, -URDUSNLC, -URDUSRLC, -URDUTLLC, 
         -URDUTULC, -URDUURLC)


##################################
#### Join All Tables Together ####
##################################

data_lab <- BIOPRO_H %>% inner_join(UCFLOW_H, by="SEQN") %>%inner_join(UM_H, by="SEQN")
dataset <- data_lab %>% inner_join(combine, by="SEQN")
dataset$eGFR <- ifelse(dataset$eGFR>90,1,0)

#convert to factor
col <- c("eGFR","TroubleSleep", "Depressed", "Smoke100", "Education19", "HHIncome" )
dataset[col] <- lapply(dataset[col], factor)

#missing imputation
library(mice)
mdat <- mice(dataset%>%select(-SEQN,-eGFR),m = 12,maxit = 5)
#mdat <- mice(dataset%>%select(-SEQN,-eGFR),m = 5,maxit = 1)
final_data  <-  complete(mdat) %>% as_tibble()%>% cbind(dataset$eGFR, dataset$SEQN)
final_data <- final_data %>% na.omit()
names(final_data)[84]<-"SEQN"
names(final_data)[83]<-"eGFR"

#normalize data
library(caret)
#method = "scale"
pp <- preProcess(final_data %>% select(-eGFR, -SEQN), method = "scale")
final_data <- predict(pp, final_data %>% select(-eGFR, -SEQN)) %>% 
  cbind(final_data$eGFR, final_data$SEQN)
names(final_data)[84]<-"SEQN"
names(final_data)[83]<-"eGFR"

#shuffle the data
final_data <- final_data[sample(nrow(final_data)),]

#train test split
halfrow <- ceiling(nrow(final_data)*0.67)
train <- final_data %>% head(halfrow) 
test <- final_data %>% tail(nrow(final_data)-halfrow) 

write.csv(train, file = "train.csv",row.names=FALSE)
write.csv(test, file = "test.csv",row.names=FALSE)


##############################
#### Without Urine Metals ####
##############################
data_lab_withoutUM <- BIOPRO_H %>% inner_join(UCFLOW_H, by="SEQN")
dataset_withoutUM <- data_lab_withoutUM %>% inner_join(combine, by="SEQN")
dataset_withoutUM$eGFR <- ifelse(dataset_withoutUM$eGFR>90,1,0)
dataset_withoutUM[col] <- lapply(dataset_withoutUM[col], factor)
mdat_withoutUM <- mice(dataset_withoutUM%>%select(-SEQN,-eGFR),m = 12,maxit = 5)
#mdat_withoutUM <- mice(dataset_withoutUM%>%select(-SEQN,-eGFR),m = 5,maxit = 1)
final_data_withoutUM  <-  complete(mdat_withoutUM) %>% as_tibble()%>% 
  cbind(dataset_withoutUM$eGFR, dataset_withoutUM$SEQN)
final_data_withoutUM <- final_data_withoutUM %>% na.omit()
names(final_data_withoutUM)[71]<-"SEQN"
names(final_data_withoutUM)[70]<-"eGFR"

pp_withoutUM <- preProcess(final_data_withoutUM %>% select(-eGFR, -SEQN), method = "scale")
final_data_withoutUM <- predict(pp_withoutUM, final_data_withoutUM %>% select(-eGFR, -SEQN)) %>% cbind(final_data_withoutUM$eGFR,
                                                                                                       final_data_withoutUM$SEQN)
names(final_data_withoutUM)[71]<-"SEQN"
names(final_data_withoutUM)[70]<-"eGFR"
final_data_withoutUM <- final_data_withoutUM[sample(nrow(final_data_withoutUM)),]
halfrow_withoutUM <- ceiling(nrow(final_data_withoutUM)*0.67)
train_withoutUM <- final_data_withoutUM %>% head(halfrow_withoutUM) 
test_withoutUM <- final_data_withoutUM %>% tail(nrow(final_data_withoutUM)-halfrow_withoutUM) 
write.csv(train_withoutUM, file = "train_withoutUM.csv",row.names=FALSE)
write.csv(test_withoutUM, file = "test_withoutUM.csv",row.names=FALSE)
