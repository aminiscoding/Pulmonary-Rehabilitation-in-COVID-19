###################################### Introduction ######################################
library(foreign)
library(effsize)
library(stats)
library(writexl)

primary_data <- read.spss("File Address", to.data.frame = TRUE)

Result_Descriptive <- data.frame(Topic = c("Education", "Employment", "Children", "Smoking", "Marraige", "Sex", "Age", "BMI", "VAS_Pre", "VAS_Post", "PHQ15_Pre", "PHQ15_Post", "BI_Pre", "BI_Post", "GHQ28_Somatic_Pre", "GHQ28_Somatic_Post", "GHQ28_AnxietyInsomnia_Pre", "GHQ28_AnxietyInsomnia_Post", "GHQ28_SocialDysfunction_Pre", "GHQ28_SocialDysfunction_Post", "GHQ28_SevereDepression_Pre", "GHQ28_SevereDepression_Post", "GHQ28_Pre", "GHQ28_Post", "Alcohol"), Mean_Online = c(NA), SD_Online = c(NA), Mean_Brochure = c(NA), SD_Brochure = c(NA))
Result_Inferential <- data.frame(Topic = c("Education", "Employment", "Children", "Smoking", "Marraige", "Sex", "Age", "BMI", "VAS", "PHQ15", "BI", "GHQ28_Somatic","GHQ28_AnxietyInsomnia", "GHQ28_SocialDysfunction", "GHQ28_SevereDepression", "GHQ28", "Alcohol"), Between = NA, CI = NA, Effect_Size = NA, Within_Online = NA, CI_Online = NA, Effect_Size_Online = NA, Within_Brochure = NA, CI_Brochure = NA, Effect_Size_Brochure = NA)

Online <- subset(primary_data, Type_of_Intervention == "Online")
Brochure <- subset(primary_data, Type_of_Intervention == "Brochure")

###################################### Demographic variables ######################################
# Education
Result_Descriptive[1, 2] <- paste("Non-Academic", table(Online$Education)[1] , "Academic", table(Online$Education)[2])  
Result_Descriptive[1, 3] <- paste("Non-Academic", round(prop.table(table(Online$Education))[1] * 100, 2), "Academic", round(prop.table(table(Online$Education))[2] * 100, 2))

Result_Descriptive[1, 4] <- paste("Non-Academic", table(Brochure$Education)[1] , "Academic", table(Brochure$Education)[2])  
Result_Descriptive[1, 5] <- paste("Non-Academic", round(prop.table(table(Brochure$Education))[1] * 100, 2), "Academic", round(prop.table(table(Brochure$Education))[2] * 100, 2))

Result_Inferential[1, 2] <- round(chisq.test(primary_data$Education, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Employment
Result_Descriptive[2, 2] <- paste("Employed", table(Online$Employment)[1] , "Non-Emoloyed", table(Online$Employment)[2])  
Result_Descriptive[2, 3] <- paste("Employed", round(prop.table(table(Online$Employment))[1] * 100, 2), "Non-Emoloyed", round(prop.table(table(Online$Employment))[2] * 100, 2))

Result_Descriptive[2, 4] <- paste("Employed", table(Brochure$Employment)[1] , "Non-Emoloyed", table(Brochure$Employment)[2])  
Result_Descriptive[2, 5] <- paste("Employed", round(prop.table(table(Brochure$Employment))[1] * 100, 2), "Non-Emoloyed", round(prop.table(table(Brochure$Employment))[2] * 100, 2))

Result_Inferential[2, 2] <- round(chisq.test(primary_data$Employment, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Children
Result_Descriptive[3, 2] <- paste("NO", table(Online$Children)[1] , "Yes", table(Online$Children)[2])  
Result_Descriptive[3, 3] <- paste("No", round(prop.table(table(Online$Children))[1] * 100, 2), "Yes", round(prop.table(table(Online$Children))[2] * 100, 2))

Result_Descriptive[3, 4] <- paste("No", table(Brochure$Children)[1] , "Yes", table(Brochure$Children)[2])  
Result_Descriptive[3, 5] <- paste("No", round(prop.table(table(Brochure$Children))[1] * 100, 2), "Yes", round(prop.table(table(Brochure$Children))[2] * 100, 2))

Result_Inferential[3, 2] <- round(chisq.test(primary_data$Children, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Smoking
Result_Descriptive[4, 2] <- paste("No", table(Online$Smoking)[1] , "Yes", table(Online$Smoking)[2])  
Result_Descriptive[4, 3] <- paste("No", round(prop.table(table(Online$Smoking))[1] * 100, 2), "Yes", round(prop.table(table(Online$Smoking))[2] * 100, 2))

Result_Descriptive[4, 4] <- paste("No", table(Brochure$Smoking)[1] , "Yes", table(Brochure$Smoking)[2])  
Result_Descriptive[4, 5] <- paste("No", round(prop.table(table(Brochure$Smoking))[1] * 100, 2), "Yes", round(prop.table(table(Brochure$Smoking))[2] * 100, 2))

Result_Inferential[4, 2] <- round(chisq.test(primary_data$Smoking, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Marriage
Result_Descriptive[5, 2] <- paste("Yes", table(Online$Marriage)[1] , "No", table(Online$Marriage)[2], "Others", table(Online$Marriage)[3])  
Result_Descriptive[5, 3] <- paste("Yes", round(prop.table(table(Online$Marriage))[1] * 100, 2), "No", round(prop.table(table(Online$Marriage))[2] * 100, 2), "Others", round(prop.table(table(Online$Marriage))[3] * 100, 2))

Result_Descriptive[5, 4] <- paste("Yes", table(Brochure$Marriage)[1] , "No", table(Brochure$Marriage)[2], "Others", table(Brochure$Marriage)[3])  
Result_Descriptive[5, 5] <- paste("Yes", round(prop.table(table(Brochure$Marriage))[1] * 100, 2), "No", round(prop.table(table(Brochure$Marriage))[2] * 100, 2), "Others", round(prop.table(table(Brochure$Marriage))[3] * 100, 2))

Result_Inferential[5, 2] <- round(chisq.test(primary_data$Marriage, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Sex
Result_Descriptive[6, 2] <- paste("Female", table(Online$Sex)[1] , "Male", table(Online$Sex)[2])  
Result_Descriptive[6, 3] <- paste("Female", round(prop.table(table(Online$Sex))[1] * 100, 2), "Male", round(prop.table(table(Online$Sex))[2] * 100, 2))

Result_Descriptive[6, 4] <- paste("Female", table(Brochure$Sex)[1] , "Male", table(Brochure$Sex)[2])  
Result_Descriptive[6, 5] <- paste("Female", round(prop.table(table(Brochure$Sex))[1] * 100, 2), "Male", round(prop.table(table(Brochure$Sex))[2] * 100, 2))

Result_Inferential[6, 2] <- round(chisq.test(primary_data$Sex, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

# Age
Result_Descriptive[7, 2] <- round(mean(Online$Age), 2)
Result_Descriptive[7, 3] <- round(sd(Online$Age), 2)

Result_Descriptive[7, 4] <- round(mean(Brochure$Age), 2)
Result_Descriptive[7, 5] <- round(sd(Brochure$Age), 2)

Ttest_Age <- t.test(Online$Age, Brochure$Age, na.rm = TRUE)
Result_Inferential[7, 2] <- round(Ttest_Age$p.value, 3)

# BMI
Result_Descriptive[8, 2] <- round(mean(Online$BMI), 2)
Result_Descriptive[8, 3] <- round(sd(Online$BMI), 2)

Result_Descriptive[8, 4] <- round(mean(Brochure$BMI) , 2)
Result_Descriptive[8, 5] <- round(sd(Brochure$BMI) , 2) 

Result_Inferential[8, 2] <- round(t.test(Online$BMI, Brochure$BMI, na.rm = TRUE)$p.value, 3)

# Alcohol
Result_Descriptive[25, 2] <- paste("Non-Academic", table(Online$AlcoholUse)[1] , "Academic", table(Online$AlcoholUse)[2])  
Result_Descriptive[25, 3] <- paste("Non-Academic", round(prop.table(table(Online$AlcoholUse))[1] * 100, 2), "Academic", round(prop.table(table(Online$AlcoholUse))[2] * 100, 2))

Result_Descriptive[25, 4] <- paste("Non-Academic", table(Brochure$AlcoholUse)[1] , "Academic", table(Brochure$AlcoholUse)[2])  
Result_Descriptive[25, 5] <- paste("Non-Academic", round(prop.table(table(Brochure$AlcoholUse))[1] * 100, 2), "Academic", round(prop.table(table(Brochure$AlcoholUse))[2] * 100, 2))

Result_Inferential[17, 2] <- round(chisq.test(primary_data$AlcoholUse, primary_data$Type_of_Intervention, correct = FALSE)$p.value , 3)

###################################### VAS ######################################
#Combining all data into Pre and Post
primary_data$VAS_Pre <- primary_data$VASHeadachePRE + primary_data$VASBackachePRE + primary_data$VASABDpinPRE + primary_data$VASBodypainPRE + primary_data$VASEXTpainPRE + primary_data$VASMensturationPainPRE + primary_data$VASChestPainPRE
primary_data$VAS_Pre
primary_data$VAS_Post <- primary_data$VASHeadachePOST + primary_data$VASBackachePOST + primary_data$VASABDPainPOST + primary_data$VASBodypainPOST + primary_data$VASEXTpainPOST + primary_data$VASMensPainPOST + primary_data$VASChestPainPOST
primary_data$VAS_Post

# separating Online and Brochure regarding Pre
primary_data$Online_VAS_Pre <- primary_data[primary_data$Type_of_Intervention == "Online", "VAS_Pre"]
is.na(primary_data$Online_VAS_Pre)
New_Online_VAS_Pre <- na.omit(primary_data$Online_VAS_Pre)
Result_Descriptive[9, 2] <- round(mean(New_Online_VAS_Pre), 2)
Result_Descriptive[9, 3] <- round(sd(New_Online_VAS_Pre), 2)

primary_data$Brochure_VAS_Pre <- primary_data[primary_data$Type_of_Intervention == "Brochure", "VAS_Pre"]
is.na(primary_data$Brochure_VAS_Pre)
New_Brochure_VAS_Pre <- na.omit(primary_data$Brochure_VAS_Pre)
Result_Descriptive[9, 4] <- round(mean(New_Brochure_VAS_Pre), 2)
Result_Descriptive[9, 5] <- round(sd(New_Brochure_VAS_Pre), 2)

# separating Online and Brochure regarding Post
primary_data$Online_VAS_Post <- primary_data[primary_data$Type_of_Intervention == "Online", "VAS_Post"]
is.na(primary_data$Online_VAS_Post)
New_Online_VAS_Post <- na.omit(primary_data$Online_VAS_Post)
Result_Descriptive[10, 2] <- round(mean(New_Online_VAS_Post), 2)
Result_Descriptive[10, 3] <- round(sd(New_Online_VAS_Post), 2)

primary_data$Brochure_VAS_Post <- primary_data[primary_data$Type_of_Intervention == "Brochure", "VAS_Post"]
is.na(primary_data$Brochure_VAS_Post)
New_Brochure_VAS_Post <- na.omit(primary_data$Brochure_VAS_Post)
Result_Descriptive[10, 4] <- round(mean(New_Brochure_VAS_Post), 2)
Result_Descriptive[10, 5] <- round(sd(New_Brochure_VAS_Post), 2)

# Finding Difference
VAS_Online_Difference <- New_Online_VAS_Post - New_Online_VAS_Pre
VAS_Brochure_Difference <- New_Brochure_VAS_Post - New_Brochure_VAS_Pre

# Between Inferential
Result_Inferential[9, 2] <- round(t.test(VAS_Online_Difference, VAS_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[9, 3] <- paste(round(t.test(VAS_Online_Difference, VAS_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(VAS_Online_Difference, VAS_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[9, 4] <- round(cohen.d(VAS_Online_Difference, VAS_Brochure_Difference, pooled = TRUE)$estimate, 3)

# Within Online Inferential
Result_Inferential[9, 5] <- round(t.test(New_Online_VAS_Pre, New_Online_VAS_Post, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[9, 6] <- paste(round(t.test(New_Online_VAS_Pre, New_Online_VAS_Post, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(New_Online_VAS_Pre, New_Online_VAS_Post, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[9, 7] <- round(cohen.d(New_Online_VAS_Pre, New_Online_VAS_Post, paired = TRUE)$estimate, 3)

# Within Brochure Inferential
Result_Inferential[9, 8] <- round(t.test(New_Brochure_VAS_Pre, New_Brochure_VAS_Post, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[9, 9] <- paste(round(t.test(New_Brochure_VAS_Pre, New_Brochure_VAS_Post, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(New_Brochure_VAS_Pre, New_Brochure_VAS_Post, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[9, 10] <- round(cohen.d(New_Brochure_VAS_Pre, New_Brochure_VAS_Post, paired = TRUE)$estimate, 3)

###################################### PHQ15 ######################################
#Descriptive
PHQ15_Pre_Online <- na.omit(primary_data$PHQ15_Pre_Online)
Result_Descriptive[11, 2] <- round(mean(PHQ15_Pre_Online), 2)
Result_Descriptive[11, 3] <- round(sd(PHQ15_Pre_Online), 2)

PHQ15_Pre_Brochure <- na.omit(primary_data$PHQ15_Pre_Brochure)
Result_Descriptive[11, 4] <- round(mean(PHQ15_Pre_Brochure), 2)
Result_Descriptive[11, 5] <- round(sd(PHQ15_Pre_Brochure), 2)

PHQ15_Post_Online <- na.omit(primary_data$PHQ15_Post_Online)
Result_Descriptive[12, 2] <- round(mean(PHQ15_Post_Online), 2)
Result_Descriptive[12, 3] <- round(sd(PHQ15_Post_Online), 2)

PHQ15_Post_Brochure <- na.omit(primary_data$PHQ15_Post_Brochure)
Result_Descriptive[12, 4] <- round(mean(PHQ15_Post_Brochure), 2)
Result_Descriptive[12, 5] <- round(sd(PHQ15_Post_Brochure), 2)

# Finding Difference
PHQ15_Online_Difference <- PHQ15_Post_Online - PHQ15_Pre_Online 
PHQ15_Brochure_Difference <- PHQ15_Post_Brochure - PHQ15_Pre_Brochure 

#Between Inferential
Result_Inferential[10, 2] <- round(t.test(PHQ15_Online_Difference, PHQ15_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[10, 3] <- paste(round(t.test(PHQ15_Online_Difference, PHQ15_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(PHQ15_Online_Difference, PHQ15_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[10, 4] <- round(cohen.d(PHQ15_Online_Difference, PHQ15_Brochure_Difference, pooled = TRUE)$estimate, 3)

# Within Online Inferential
Result_Inferential[10, 5] <- round(t.test(PHQ15_Pre_Online, PHQ15_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[10, 6] <- paste(round(t.test(PHQ15_Pre_Online, PHQ15_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(PHQ15_Pre_Online, PHQ15_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[10, 7] <- round(cohen.d(PHQ15_Pre_Online, PHQ15_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential
Result_Inferential[10, 8] <- round(t.test(PHQ15_Pre_Brochure, PHQ15_Post_Brochure, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[10, 9] <- paste(round(t.test(PHQ15_Pre_Brochure, PHQ15_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(PHQ15_Pre_Brochure, PHQ15_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[10, 10] <- round(cohen.d(PHQ15_Pre_Brochure, PHQ15_Post_Brochure, paired = TRUE)$estimate, 3)

###################################### BI ######################################
#Descriptive
BI_Pre_Online <- na.omit(primary_data$BI_Pre_Online)
Result_Descriptive[13, 2] <- round(mean(BI_Pre_Online), 2)
Result_Descriptive[13, 3] <- round(sd(BI_Pre_Online), 2)

BI_Pre_Brochure <- na.omit(primary_data$BI_Pre_Brochure)
Result_Descriptive[13, 4] <- round(mean(BI_Pre_Brochure), 2)
Result_Descriptive[13, 5] <- round(sd(BI_Pre_Brochure), 2)

BI_Post_Online <- na.omit(primary_data$BI_Post_Online)
Result_Descriptive[14, 2] <- round(mean(BI_Post_Online), 2)
Result_Descriptive[14, 3] <- round(sd(BI_Post_Online), 2)

BI_Post_Brochure <- na.omit(primary_data$BI_Post_Brochure)
Result_Descriptive[14, 4] <- round(mean(BI_Post_Brochure), 2)
Result_Descriptive[14, 5] <- round(sd(BI_Post_Brochure), 2)

#Finding Difference
BI_Online_Difference <- BI_Post_Online - BI_Pre_Online
BI_Brochure_Difference <- BI_Post_Brochure - BI_Pre_Brochure

#Between Inferential
Result_Inferential[11, 2] <- round(t.test(BI_Online_Difference, BI_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[11, 3] <- paste(round(t.test(BI_Online_Difference, BI_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(BI_Online_Difference, BI_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[11, 4] <- round(cohen.d(BI_Online_Difference, BI_Brochure_Difference, pooled = TRUE)$estimate, 3)

# Within Online Inferential
Result_Inferential[11, 5] <- round(t.test(BI_Pre_Online, BI_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[11, 6] <- paste(round(t.test(BI_Pre_Online, BI_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(BI_Pre_Online, BI_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[11, 7] <- round(cohen.d(BI_Pre_Online, BI_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential
Result_Inferential[11, 8] <- round(t.test(BI_Pre_Brochure, BI_Post_Brochure, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[11, 9] <- paste(round(t.test(BI_Pre_Brochure, BI_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(BI_Pre_Brochure, BI_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[11, 10] <- round(cohen.d(BI_Pre_Brochure, BI_Post_Brochure, paired = TRUE)$estimate, 3)

###################################### GHQ-28 ###################################### 
#Descriptive Somatic
GHQ28_Somatic_Pre_Online <- na.omit(primary_data$GHQ28_Somatic_Pre_Online)
Result_Descriptive[15, 2] <- round(mean(GHQ28_Somatic_Pre_Online), 2)
Result_Descriptive[15, 3] <- round(sd(GHQ28_Somatic_Pre_Online), 2)

GHQ28_Somatic_Pre_Brochure <- na.omit(primary_data$GHQ28_Somatic_Pre_Brochure)
Result_Descriptive[15, 4] <- round(mean(GHQ28_Somatic_Pre_Brochure), 2)
Result_Descriptive[15, 5] <- round(sd(GHQ28_Somatic_Pre_Brochure), 2)

GHQ28_Somatic_Post_Online <- na.omit(primary_data$GHQ28_Somatic_Post_Online)
Result_Descriptive[16, 2] <- round(mean(GHQ28_Somatic_Post_Online), 2)
Result_Descriptive[16, 3] <- round(sd(GHQ28_Somatic_Post_Online), 2)

GHQ28_Somatic_Post_Brochure <- na.omit(primary_data$GHQ28_Somatic_Post_Brochure)
Result_Descriptive[16, 4] <- round(mean(GHQ28_Somatic_Post_Brochure), 2)
Result_Descriptive[16, 5] <- round(sd(GHQ28_Somatic_Post_Brochure), 2)

#Descriptive AnxietyInsomnia
GHQ28_AnxietyInsomnia_Pre_Online <- na.omit(primary_data$GHQ28_AnxietyInsomnia_Pre_Online)
Result_Descriptive[17, 2] <- round(mean(GHQ28_AnxietyInsomnia_Pre_Online), 2)
Result_Descriptive[17, 3] <- round(sd(GHQ28_AnxietyInsomnia_Pre_Online), 2)

GHQ28_AnxietyInsomnia_Pre_Bruchore <- na.omit(primary_data$GHQ28_AnxietyInsomnia_Pre_Bruchore)
Result_Descriptive[17, 4] <- round(mean(GHQ28_AnxietyInsomnia_Pre_Bruchore), 2)
Result_Descriptive[17, 5] <- round(sd(GHQ28_AnxietyInsomnia_Pre_Bruchore), 2)

GHQ28_AnxietyInsomnia_Post_Online <- na.omit(primary_data$GHQ28_AnxietyInsomnia_Post_Online)
Result_Descriptive[18, 2] <- round(mean(GHQ28_AnxietyInsomnia_Post_Online), 2)
Result_Descriptive[18, 3] <- round(sd(GHQ28_AnxietyInsomnia_Post_Online), 2)

GHQ28_AnxietyInsomnia_Post_Bruchore <- na.omit(primary_data$GHQ28_AnxietyInsomnia_Post_Bruchore)
Result_Descriptive[18, 4] <- round(mean(GHQ28_AnxietyInsomnia_Post_Bruchore), 2)
Result_Descriptive[18, 5] <- round(sd(GHQ28_AnxietyInsomnia_Post_Bruchore), 2)

#Descriptive SocialDysfunction
GHQ28_SocialDysfunction_Pre_Online <- na.omit(primary_data$GHQ28_SocialDysfunction_Pre_Online)
Result_Descriptive[19, 2] <- round(mean(GHQ28_SocialDysfunction_Pre_Online), 2)
Result_Descriptive[19, 3] <- round(sd(GHQ28_SocialDysfunction_Pre_Online), 2)

GHQ28_SocialDysfunction_Pre_Bruchore <- na.omit(primary_data$GHQ28_SocialDysfunction_Pre_Bruchore)
Result_Descriptive[19, 4] <- round(mean(GHQ28_SocialDysfunction_Pre_Bruchore), 2)
Result_Descriptive[19, 5] <- round(sd(GHQ28_SocialDysfunction_Pre_Bruchore), 2)

GHQ28_SocialDysfunction_Post_Online <- na.omit(primary_data$GHQ28_SocialDysfunction_Post_Online)
Result_Descriptive[20, 2] <- round(mean(GHQ28_SocialDysfunction_Post_Online), 2)
Result_Descriptive[20, 3] <- round(sd(GHQ28_SocialDysfunction_Post_Online), 2)

GHQ28_SocialDysfunction_Post_Bruchore <- na.omit(primary_data$GHQ28_SocialDysfunction_Post_Bruchore)
Result_Descriptive[20, 4] <- round(mean(GHQ28_SocialDysfunction_Post_Bruchore), 2)
Result_Descriptive[20, 5] <- round(sd(GHQ28_SocialDysfunction_Post_Bruchore), 2)

#Descriptive SevereDepression
GHQ28_SevereDepression_Pre_Online <- na.omit(primary_data$GHQ28_SevereDepression_Pre_Online)
Result_Descriptive[21, 2] <- round(mean(GHQ28_SevereDepression_Pre_Online), 2)
Result_Descriptive[21, 3] <- round(sd(GHQ28_SevereDepression_Pre_Online), 2)

GHQ28_SevereDepression_Pre_Brochure <- na.omit(primary_data$GHQ28_SevereDepression_Pre_Brochure)
Result_Descriptive[21, 4] <- round(mean(GHQ28_SevereDepression_Pre_Brochure), 2)
Result_Descriptive[21, 5] <- round(sd(GHQ28_SevereDepression_Pre_Brochure), 2)

GHQ28_SevereDepression_Post_Online <- na.omit(primary_data$GHQ28_SevereDepression_Post_Online)
Result_Descriptive[22, 2] <- round(mean(GHQ28_SevereDepression_Post_Online), 2)
Result_Descriptive[22, 3] <- round(sd(GHQ28_SevereDepression_Post_Online), 2)

GHQ28_SevereDepression_Post_Brochure <- na.omit(primary_data$GHQ28_SevereDepression_Post_Brochure)
Result_Descriptive[22, 4] <- round(mean(GHQ28_SevereDepression_Post_Brochure), 2)
Result_Descriptive[22, 5] <- round(sd(GHQ28_SevereDepression_Post_Brochure), 2)

#Descriptive Total
GHQ28_Pre_Online <- na.omit(primary_data$GHQ28_Pre_Online)
Result_Descriptive[23, 2] <- round(mean(GHQ28_Pre_Online), 2)
Result_Descriptive[23, 3] <- round(sd(GHQ28_Pre_Online), 2)

GHQ28_Pre_Brochure <- na.omit(primary_data$GHQ28_Pre_Brochure)
Result_Descriptive[23, 4] <- round(mean(GHQ28_Pre_Brochure), 2)
Result_Descriptive[23, 5] <- round(sd(GHQ28_Pre_Brochure), 2)

GHQ28_Post_Online <- na.omit(primary_data$GHQ28_Post_Online)
Result_Descriptive[24, 2] <- round(mean(GHQ28_Post_Online), 2)
Result_Descriptive[24, 3] <- round(sd(GHQ28_Post_Online), 2)

GHQ28_Post_Brochure <- na.omit(primary_data$GHQ28_Post_Brochure)
Result_Descriptive[24, 4] <- round(mean(GHQ28_Post_Brochure), 2)
Result_Descriptive[24, 5] <- round(sd(GHQ28_Post_Brochure), 2)

#Finding Difference 
# Somatic
GHQ28_Somatic_Online_Difference <- GHQ28_Somatic_Post_Online - GHQ28_Somatic_Pre_Online
GHQ28_Somatic_Brochure_Difference <- GHQ28_Somatic_Post_Brochure - GHQ28_Somatic_Pre_Brochure

# AnxietyInsomnia
GHQ28_AnxietyInsomnia_Online_Difference <- GHQ28_AnxietyInsomnia_Post_Online - GHQ28_AnxietyInsomnia_Pre_Online
GHQ28_AnxietyInsomnia_Brochure_Difference <- GHQ28_AnxietyInsomnia_Post_Bruchore - GHQ28_AnxietyInsomnia_Pre_Bruchore


# SocialDysfunction
GHQ28_SocialDysfunction_Online_Difference <- GHQ28_SocialDysfunction_Post_Online - GHQ28_SocialDysfunction_Pre_Online
GHQ28_SocialDysfunction_Brochure_Difference <- GHQ28_SocialDysfunction_Post_Bruchore - GHQ28_SocialDysfunction_Pre_Bruchore


# SevereDepression
GHQ28_SevereDepression_Online_Difference <- GHQ28_SevereDepression_Post_Online - GHQ28_SevereDepression_Pre_Online
GHQ28_SevereDepression_Brochure_Difference <- GHQ28_SevereDepression_Post_Brochure - GHQ28_SevereDepression_Pre_Brochure

# Total
GHQ28_Total_Online_Difference <- GHQ28_Post_Online - GHQ28_Pre_Online
GHQ28_Total_Brochure_Difference <- GHQ28_Post_Brochure - GHQ28_Pre_Brochure


#Between Inferential Somatic
Result_Inferential[12, 2] <- round(t.test(GHQ28_Somatic_Online_Difference, GHQ28_Somatic_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[12, 3] <- paste(round(t.test(GHQ28_Somatic_Online_Difference, GHQ28_Somatic_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Somatic_Online_Difference, GHQ28_Somatic_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[12, 4] <- round(cohen.d(GHQ28_Somatic_Online_Difference, GHQ28_Somatic_Brochure_Difference, pooled = TRUE)$estimate, 3)

#Between Inferential AnxietyInsomnia
Result_Inferential[13, 2] <- round(t.test(GHQ28_AnxietyInsomnia_Online_Difference, GHQ28_AnxietyInsomnia_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[13, 3] <- paste(round(t.test(GHQ28_AnxietyInsomnia_Online_Difference, GHQ28_AnxietyInsomnia_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_AnxietyInsomnia_Online_Difference, GHQ28_AnxietyInsomnia_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[13, 4] <- round(cohen.d(GHQ28_AnxietyInsomnia_Online_Difference, GHQ28_AnxietyInsomnia_Brochure_Difference, pooled = TRUE)$estimate, 3)

#Between Inferential SocialDysfunction
Result_Inferential[14, 2] <- round(t.test(GHQ28_SocialDysfunction_Online_Difference, GHQ28_SocialDysfunction_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[14, 3] <- paste(round(t.test(GHQ28_SocialDysfunction_Online_Difference, GHQ28_SocialDysfunction_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SocialDysfunction_Online_Difference, GHQ28_SocialDysfunction_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[14, 4] <- round(cohen.d(GHQ28_SocialDysfunction_Online_Difference, GHQ28_SocialDysfunction_Brochure_Difference, pooled = TRUE)$estimate, 3)

#Between Inferential SevereDepression
Result_Inferential[15, 2] <- round(t.test(GHQ28_SevereDepression_Online_Difference, GHQ28_SevereDepression_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[15, 3] <- paste(round(t.test(GHQ28_SevereDepression_Online_Difference, GHQ28_SevereDepression_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SevereDepression_Online_Difference, GHQ28_SevereDepression_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[15, 4] <- round(cohen.d(GHQ28_SevereDepression_Online_Difference, GHQ28_SevereDepression_Brochure_Difference, pooled = TRUE)$estimate, 3)

#Between Inferential Total
Result_Inferential[16, 2] <- round(t.test(GHQ28_Total_Online_Difference, GHQ28_Total_Brochure_Difference, na.rm = TRUE)$p.value, 3)
Result_Inferential[16, 3] <- paste(round(t.test(GHQ28_Total_Online_Difference, GHQ28_Total_Brochure_Difference, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Total_Online_Difference, GHQ28_Total_Brochure_Difference, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[16, 4] <- round(cohen.d(GHQ28_Total_Online_Difference, GHQ28_Total_Brochure_Difference, pooled = TRUE)$estimate, 3)

# Within Online Inferential Somatic
Result_Inferential[12, 5] <- round(t.test(GHQ28_Somatic_Pre_Online, GHQ28_Somatic_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[12, 6] <- paste(round(t.test(GHQ28_Somatic_Pre_Online, GHQ28_Somatic_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Somatic_Pre_Online, GHQ28_Somatic_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[12, 7] <- round(cohen.d(GHQ28_Somatic_Pre_Online, GHQ28_Somatic_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential Somatic
Result_Inferential[12, 8] <- round(t.test(GHQ28_Somatic_Pre_Brochure, GHQ28_Somatic_Post_Brochure, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[12, 9] <- paste(round(t.test(GHQ28_Somatic_Pre_Brochure, GHQ28_Somatic_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Somatic_Pre_Brochure, GHQ28_Somatic_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[12, 10] <- round(cohen.d(GHQ28_Somatic_Pre_Brochure, GHQ28_Somatic_Post_Brochure, paired = TRUE)$estimate, 3)

# Within Online Inferential AnxietyInsomnia
Result_Inferential[13, 5] <- round(t.test(GHQ28_AnxietyInsomnia_Pre_Online, GHQ28_AnxietyInsomnia_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[13, 6] <- paste(round(t.test(GHQ28_AnxietyInsomnia_Pre_Online, GHQ28_AnxietyInsomnia_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_AnxietyInsomnia_Pre_Online, GHQ28_AnxietyInsomnia_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[13, 7] <- round(cohen.d(GHQ28_AnxietyInsomnia_Pre_Online, GHQ28_AnxietyInsomnia_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential AnxietyInsomnia
Result_Inferential[13, 8] <- round(t.test(GHQ28_AnxietyInsomnia_Pre_Bruchore, GHQ28_AnxietyInsomnia_Post_Bruchore, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[13, 9] <- paste(round(t.test(GHQ28_AnxietyInsomnia_Pre_Bruchore, GHQ28_AnxietyInsomnia_Post_Bruchore, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_AnxietyInsomnia_Pre_Bruchore, GHQ28_AnxietyInsomnia_Post_Bruchore, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[13, 10] <- round(cohen.d(GHQ28_AnxietyInsomnia_Pre_Bruchore, GHQ28_AnxietyInsomnia_Post_Bruchore, paired = TRUE)$estimate, 3)

# Within Online Inferential SocialDysfunction 
Result_Inferential[14, 5] <- round(t.test(GHQ28_SocialDysfunction_Pre_Online, GHQ28_SocialDysfunction_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[14, 6] <- paste(round(t.test(GHQ28_SocialDysfunction_Pre_Online, GHQ28_SocialDysfunction_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SocialDysfunction_Pre_Online, GHQ28_SocialDysfunction_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[14, 7] <- round(cohen.d(GHQ28_SocialDysfunction_Pre_Online, GHQ28_SocialDysfunction_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential SocialDysfunction 
Result_Inferential[14, 8] <- round(t.test(GHQ28_SocialDysfunction_Pre_Bruchore, GHQ28_SocialDysfunction_Post_Bruchore, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[14, 9] <- paste(round(t.test(GHQ28_SocialDysfunction_Pre_Bruchore, GHQ28_SocialDysfunction_Post_Bruchore, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SocialDysfunction_Pre_Bruchore, GHQ28_SocialDysfunction_Post_Bruchore, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[14, 10] <- round(cohen.d(GHQ28_SocialDysfunction_Pre_Bruchore, GHQ28_SocialDysfunction_Post_Bruchore, paired = TRUE)$estimate, 3)

# Within Online Inferential SevereDepression
Result_Inferential[15, 5] <- round(t.test(GHQ28_SevereDepression_Pre_Online, GHQ28_SevereDepression_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[15, 6] <- paste(round(t.test(GHQ28_SevereDepression_Pre_Online, GHQ28_SevereDepression_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SevereDepression_Pre_Online, GHQ28_SevereDepression_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[15, 7] <- round(cohen.d(GHQ28_SevereDepression_Pre_Online, GHQ28_SevereDepression_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential SevereDepression
Result_Inferential[15, 8] <- round(t.test(GHQ28_SevereDepression_Pre_Brochure, GHQ28_SevereDepression_Post_Brochure, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[15, 9] <- paste(round(t.test(GHQ28_SevereDepression_Pre_Brochure, GHQ28_SevereDepression_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_SevereDepression_Pre_Brochure, GHQ28_SevereDepression_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[15, 10] <- round(cohen.d(GHQ28_SevereDepression_Pre_Brochure, GHQ28_SevereDepression_Post_Brochure, paired = TRUE)$estimate, 3)

# Within Online Inferential Total
Result_Inferential[16, 5] <- round(t.test(GHQ28_Pre_Online, GHQ28_Post_Online, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[16, 6] <- paste(round(t.test(GHQ28_Pre_Online, GHQ28_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Pre_Online, GHQ28_Post_Online, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[16, 7] <- round(cohen.d(GHQ28_Pre_Online, GHQ28_Post_Online, paired = TRUE)$estimate, 3)

# Within Brochure Inferential Total
Result_Inferential[16, 8] <- round(t.test(GHQ28_Pre_Brochure, GHQ28_Post_Brochure, paired = TRUE, na.rm = TRUE)$p.value, 3)
Result_Inferential[16, 9] <- paste(round(t.test(GHQ28_Pre_Brochure, GHQ28_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[1], 3), round(t.test(GHQ28_Pre_Brochure, GHQ28_Post_Brochure, paired = TRUE, na.rm = TRUE)$conf.int[2], 3)) 
Result_Inferential[16, 10] <- round(cohen.d(GHQ28_Pre_Brochure, GHQ28_Post_Brochure, paired = TRUE)$estimate, 3)


###################################### Exporting to Excel ######################################
write_xlsx(Result_Descriptive, "File Address")
write_xlsx(Result_Inferential, "file address")