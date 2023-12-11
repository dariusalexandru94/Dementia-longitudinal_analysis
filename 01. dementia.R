library("dplyr")
library("janitor")
library("skimr")
library("here")
library("tidyverse")
library("patchwork")
library("ggthemes")
library("ggplot2")
library("psych")
library("corrplot")
library("RColorBrewer")
library("readr")
library("randomForest")
library("pROC")
# loading
mydata = read_csv("oasis_longitudinal.csv")


#### data quality assessment & preliminary exploration
################################################################################
################################################################################
# info
glimpse(mydata)

# summary
skim_without_charts(mydata)

# checking for nans
sum(is.na(mydata))

# checking for duplicates
sum(duplicated(mydata)) 

#
head(mydata)
View(mydata)

# number of subjects
n_distinct(mydata$'Subject ID')

# 
View(mydata %>% arrange(cdr)) # by CDR
View(mydata %>% arrange(ses)) # by SES
View(mydata %>% arrange(educ)) # by EDUC
View(mydata %>% arrange(mmse)) # by MMSE
View(mydata %>% arrange(age)) # by AGE

#### data cleaning & transformation
################################################################################
################################################################################
mydata <- clean_names(mydata) # change columns to lowercase

mydata <- mydata %>% rename("gender" = "m_f") # rename gender column

# new columns for better visualization 
mydata <- mydata %>% mutate(clinical_dementia_rating =
                           case_when(cdr == 0 ~ "normal", 
                                     cdr == 0.5 ~ "very mild dementia",
                                     cdr == 1 ~ "mild demeentia",
                                     cdr == 2 ~ "dementia",
                                     cdr == 3 ~ "severe"))

mydata <- mydata %>% mutate(education =
                        case_when(educ <= 8 ~ "primary", 
                                  educ > 8 & educ <= 12 ~ "secondary",
                                  educ > 12 & educ <= 16 ~ "post-secondary",
                                  educ > 16 ~ "univ. and postgraduate"))

mydata <- mydata %>% mutate("mental state examination" =
                              case_when(mmse >= 24 ~ "normal cognition", 
                                        mmse > 19 & mmse <= 23 ~ "mild cognitive immpairment",
                                        mmse > 10 & mmse <= 18 ~ "moderate cognitive immpairment",
                                        mmse <= 9 ~ "severe cognitive immpairment"))

mydata <- mydata %>% mutate("socioeconomic status" =
                              case_when(ses == 1 ~ "very  high", 
                                        ses == 2  ~ "high",
                                        ses == 3  ~ "medium",
                                        ses == 4 ~ "low",
                                        ses == 5 ~ "very low"))

#
mydata <- na.omit(mydata)

converted = filter(mydata, group == "Converted")
demented = filter(mydata, group == "Demented")
nondemented = filter(mydata, group == "Nondemented")
View(converted)
View(demented)
View(nondemented)


#### correlation
################################################################################
################################################################################
df_corr = select(mydata, c('visit','age','educ', 'ses', 'mmse', 'cdr', 'e_tiv', 'n_wbv', 'asf'))
M <-cor(df_corr)

corrplot(M, method = 'number')
M

#### explanatory analysis
################################################################################
################################################################################
####################################################
###
subjects <- mydata[!duplicated(mydata$'subject_id'), ]
#################################################### count
m = ggplot(subjects, aes(gender, fill = gender))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray3", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Gender")+
  theme(plot.title = element_text(hjust = .5)) + theme_gdocs()
n = ggplot(subjects, aes(education, fill = education))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray2", "darkslategray3",  "darkslategray4", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Education status")+
  theme_gdocs() + theme(axis.text.x = element_text(size = rel(0.1), angle = 0))
n
p = ggplot(subjects, aes(`socioeconomic status`, fill = `socioeconomic status`))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray2", "darkslategray3",  "darkslategray4", "lightseagreen", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Socioeconomic status")+
  theme(plot.title = element_text(hjust = .5)) + theme_gdocs()

m/
  n/
  p

m + n/
  p

#### distribution

a = ggplot(mydata, aes(age))+
  geom_histogram(fill = "darkslategray4")+
  ggtitle("Distribution of age")+
  theme(plot.title = element_text(hjust = .5))  + theme_gdocs()

qqnorm(mydata$age,  xlab = 'Theoretical Dist', 
       ylab = 'Sample dist', col = "darkslategray4")
qqline(mydata$age, col = 'red', lwd = 2, lty = 2)


mydata %>% group_by(gender) %>% 
  summarise(mean(age), median(age), sd(age))

b = ggplot(mydata, aes(age, fill = gender))+
  geom_histogram()+
  facet_wrap(~gender)+
  scale_fill_manual(values = c("darkslategray4", "darkslategray"))+
  ggtitle("Distribution of age by sex")+
  theme(plot.title = element_text(hjust = .5))  + theme_gdocs()

c = ggplot(mydata, aes(age, fill = group))+
  geom_histogram()+
  facet_wrap(~group)+
  scale_fill_manual(values = c("darkslategray3", "darkslategray",  "darkslategray4")) +
  ggtitle("Distribution of age by group") +
  theme(plot.title = element_text(hjust = .5)) + theme_gdocs()

a + b/
  c

a

aa = ggplot(subjects, aes(group, fill = group))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray3",  "darkslategray4", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Groups")+
  theme_gdocs() + theme(axis.text.x = element_text(size = rel(0.01), angle = 0))
bb = ggplot(subjects, aes(clinical_dementia_rating, fill = clinical_dementia_rating))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray2", "darkslategray3",  "darkslategray4", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Clinical dementia rating")+
  theme_gdocs() + theme(axis.text.x = element_text(size = rel(0.01), angle = 0))
cc = ggplot(subjects, aes(`mental state examination`, fill = `mental state examination`))+
  geom_bar()+
  scale_fill_manual(values = c("darkslategray3",  "darkslategray4", "darkslategray"))+
  geom_text(stat = "count", aes(label = ..count..), y = 5, col = "white", fontface = "bold")+
  ggtitle("Mental states")+
  theme_gdocs() + theme(axis.text.x = element_text(size = rel(0.01), angle = 0))

aa + bb/
  cc

x = ggplot(subjects, aes(x = group, y = mmse, fill = group, shape = group)) +  
  geom_jitter(aes(colour = group), size = 3) + scale_colour_manual(values = c("darkslategray4", "darkslategray3", "darkslategray2"))
y = ggplot(subjects, aes(x=group, y=mmse, fill=group)) +
  geom_violin(trim=FALSE) + scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee")) + 
  labs(title = "Score of mmse test / group") + theme_gdocs()
z = ggplot(subjects, aes(x=group, y=mmse, fill=group)) +
  geom_boxplot() + scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee")) + 
  labs(title = "Score of mmse test / group") + theme_gdocs()

x + y/
  z
View(mydata)
####### CDR
a1 = ggplot(mydata, aes(x = group, y = cdr, fill = group, shape = group)) +  
  geom_jitter(aes(colour = group), size = 3) + scale_colour_manual(values = c("darkslategray4", "darkslategray3", "darkslategray2"))
a2 = ggplot(subjects, aes(x=group, y=cdr, fill=group)) +
  geom_violin(trim=FALSE) + scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee")) + 
  labs(title = "Score of cdr test / group") + theme_gdocs()
a3 = ggplot(mydata, aes(x=as.factor(cdr), y=age, fill=as.factor(cdr))) +
  geom_boxplot() + scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee", "#8deeee")) + 
  labs(title = "CDR by age") + theme_gdocs()

a1 + a3/
  a2

a11 = ggplot(mydata, aes(x = education, y = cdr, fill = education, shape = education)) +  
  geom_jitter(aes(colour = education), size = 3) + scale_colour_manual(values = c("darkslategray4", "darkslategray3", "darkslategray2", "darkslategray"))
a22 = ggplot(mydata, aes(x = `socioeconomic status`, y = cdr, fill = `socioeconomic status`, shape = `socioeconomic status`)) +  
  geom_jitter(aes(colour = `socioeconomic status`), size = 3) + scale_colour_manual(values = c("darkslategray4", "darkslategray3", "darkslategray2", "darkslategray", "darkslategray"))

filtered = filter(mydata, group != "Converted") # demented/nondemented

a33 = ggplot(filtered, aes(group, educ, fill = group)) +
  geom_boxplot(col = "black") +
  ggtitle("Education and Dementia") +
  scale_fill_manual(values=c("#8deeee", "#3b6363")) +
  theme(plot.title = element_text(hjust = .5)) + theme_gdocs()

View(mydata %>% arrange(educ)) # by EDUC .. < 10 years of education -> possible demention?
a11 /a22 #if you are poor and have a primary education, you might develop dementia?
## education and dementia
a33

### eTiv
z3 = ggplot(mydata, aes(x=group, y=e_tiv, fill=group)) +
  geom_boxplot() + geom_jitter(color = "red", alpha = .5) +
  scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee", "#8deeee")) + 
  labs(title = "eTiv by group") + theme_gdocs()
### nWBV
q3 = ggplot(mydata, aes(x=group, y=n_wbv, fill=group)) +
  geom_boxplot() + geom_jitter(color = "red", alpha = .5) +
  scale_fill_manual(values=c("#008080", "#3b6363", "#8deeee", "#8deeee")) + 
  labs(title = "nWBV by group") + theme_gdocs()

z3
q3

## Longitudinal

## converted
converted = filter(mydata, group == "Converted")


qqnorm(converted$mmse)
qqline(converted$mmse)



s1 <- filter(converted, subject_id == "OAS2_0018")
s2 <- filter(converted, subject_id == "OAS2_0020")
s3 <- filter(converted, subject_id == "OAS2_0031")
s4 <- filter(converted, subject_id == "OAS2_0041")
s5 <- filter(converted, subject_id == "OAS2_0054")
s6 <- filter(converted, subject_id == "OAS2_0092")
s7 <- filter(converted, subject_id == "OAS2_0103")
s8 <- filter(converted, subject_id == "OAS2_0118")
s9 <- filter(converted, subject_id == "OAS2_0127")
s10 <- filter(converted, subject_id == "OAS2_0131")
s11<- filter(converted, subject_id == "OAS2_0133")
s12 <- filter(converted, subject_id == "OAS2_0144")
s13 <- filter(converted, subject_id == "OAS2_0145")
s14 <- filter(converted, subject_id == "OAS2_0176")

s1p = ggplot(s1, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s2p = ggplot(s2, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s3p = ggplot(s3, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s4p = ggplot(s4, aes(x=visit, y=mmse)) +geom_line(size = 1.7, color = "#8deeee") + geom_point(size = 5, color = "#8deeee") + theme_gdocs()
s5p = ggplot(s5, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s6p =  ggplot(s6, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s7p = ggplot(s7, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#008080") + geom_point(size = 3, color = "#008080") + theme_gdocs()
s8p = ggplot(s8, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s9p = ggplot(s9, aes(x=visit, y=mmse)) +geom_line(size = 1.7, color = "#8deeee") + geom_point(size = 5, color = "#8deeee") + theme_gdocs()
s10p =  ggplot(s10, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s11p = ggplot(s11, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s12p = ggplot(s12, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#008080") + geom_point(size = 3, color = "#008080") + theme_gdocs()
s13p = ggplot(s13, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#3b6363") + geom_point(size = 3, color = "#3b6363") + theme_gdocs()
s14p = ggplot(s14, aes(x=visit, y=mmse)) +geom_line(size = 1, color = "#008080") + geom_point(size = 3, color = "#008080") + theme_gdocs()

(s1p + s2p + s3p)/
 ( s4p + s5p + s6p)/
  (s7p + s8p +s9p)/
  (s10p + s11p +s12p)/
  (s13p + s14p)
View(converted)

converted %>% summarise(mean(age), median(age), sd(age), max(age), min(age))
nondemented %>% summarise(mean(age), median(age), sd(age), max(age), min(age))

converted %>% summarise(mean(mmse), median(mmse), sd(mmse), max(mmse), min(mmse))
nondemented %>% summarise(mean(mmse), median(mmse), sd(mmse), max(mmse), min(mmse))

View(converted)


conv1 = converted %>% 
  group_by(visit) %>% 
  summarise(mean(age), max(age), min(age), median(age), sd(age))

converted %>%  count(visit)

conv11 = converted %>% 
  group_by(visit) %>% 
  summarise(mean(educ), sd(educ), mean(ses), sd(ses))

conv111 = converted %>% 
  group_by(visit) %>% 
  summarise(mean(cdr), median(cdr), sd(cdr), mean(mmse), median(mmse), sd(mmse))

conv1
conv11
conv111

conv2 = mydata %>% 
  group_by(visit) %>% 
  summarise(mean(age), max(age), min(age), median(age), sd(age), mean(educ), sd(educ), mean(ses), sd(ses))

conv3 = converted %>% 
  group_by(visit) %>% 
  summarise(mean(cdr), median(cdr), sd(cdr), mean(mmse), median(mmse), sd(mmse))

conv1 # only one patient with five visits

conv2

conv3

View(converted)

nondemented %>% count(visit)
nondemented %>% count(subject_id)

n_distinct(nondemented$subject_id)
n_distinct(demented$subject_id)
n_distinct(converted$subject_id)

View(demented)
demented %>% count(visit)
converted %>% count(visit)
mydata %>% count(visit)

mean(mydata$age)
mean(converted$age)

median(mydata$age)
median(converted$age)

### TEST TEST TEST TEST


###############################################################################
###############################################################################
################################      ML       ################################
# new df - > model data
# males = males...converted - > demented
mdata = mydata %>% 
  select("subject_id", "group", "visit", "gender", "age", "educ", "ses", "mmse", "cdr", "e_tiv", "n_wbv", 
         "asf")

View(mdata)

### Demented / Nondemented
mdata = mdata %>%
  mutate(group = case_when(
    cdr == 0 ~ "Nondemented",
    cdr > 0  ~ "Demented"
  ))
mdata

### 
mdata %>% 
  count(gender)

females <- mdata %>% filter(gender == "F")
males <- mdata %>% filter(gender == 'M')
f_sample <- sample_n(females, 148) #taking a 148 females samples 

mdata_eq <- rbind(males, f_sample)

mdata_eq %>% count(gender)

#shuffle
shuffled_data= mdata_eq[sample(1:nrow(mdata_eq)), ]
mdata_eq <- shuffled_data

View(mdata_eq)



### Scaling and centering
mdata_eq$group<-as.factor(mdata_eq$group)
mdata_eq$gender<-as.factor(mdata_eq$gender)
mdata_eq$cdr<-as.factor(mdata_eq$cdr)

factor_variables<-mdata_eq[, sapply(mdata_eq, is.factor)]
cdr<-factor_variables$cdr
dum_var<-dummyVars(~., factor_variables[,-3])
factor_variables<-as.data.frame(predict(dum_var, factor_variables[,-3]))
factor_variables<-as.data.frame(cbind(factor_variables, cdr))

numeric_variables<-mdata_eq[, sapply(mdata_eq, is.numeric)]
correlations<-cor(numeric_variables)
highCorr<-findCorrelation(correlations, cutoff = .75)
numeric_variables<-numeric_variables[,-highCorr]

numeric_variables$age<-scale(numeric_variables$age)
numeric_variables$educ<-scale(numeric_variables$educ)
numeric_variables$mmse<-scale(numeric_variables$mmse)
numeric_variables$n_wbv<-scale(numeric_variables$n_wbv)
numeric_variables$asf<-scale(numeric_variables$asf)

temp<-as.data.frame(cbind(numeric_variables, factor_variables))
temp<-temp[,c(-1,-8,-9,-10)]
train_set <- round(0.9 * nrow(temp)) ## 90 % training 10% testing
indices <- sample(1:nrow(temp), train_set)
train <- temp[indices,]
test <- temp[-indices,]
head(train)
View(train)
View(test)



#### SVM
set.seed(11) 
ctrl<-trainControl(method = "cv", number = 12)
svm.mod<-train(cdr~., data = train, method = "svmRadial", metric = "Accuracy", trControl = ctrl, tuneLength = 15)
pred1 <- predict(svm.mod, newdata = test[,-9], cost = 100, gamma = 1)
confusionMatrix(pred1, test$cdr)


svm.mod$results
plot(svm.mod$results$Accuracy,svm.mod$results$C, col="#008080", type="p", pch=17, cex = 1.5)


