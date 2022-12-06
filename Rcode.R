setwd("D:\\Rdata")
load('seer2.Rdata')
data <- read.table('Second_chondrosarcoma.txt'
                   ,sep = '\t'
                   ,header = T)
seer <- data

table(seer$Age.recode.with.single.ages.and.100.)
seer$age <- substr(x = seer$Age.recode.with.single.ages.and.100.,start = 1,stop = 2)
table(seer$age)
class(seer$age)
seer$age <- as.numeric(seer$age)



class(seer$age)
for (i in 1:nrow(seer)) {
  if(seer$age[i]<11){
    seer$age.10per[i]='0_10'
  }else if(seer$age[i]<21){
    seer$age.10per[i]='10_20'
  }else if(seer$age[i]<31){
    seer$age.10per[i]='20_30'
  }else if(seer$age[i]<41){
    seer$age.10per[i]='30_40'
  }else if(seer$age[i]<51){
    seer$age.10per[i]='40_50'
  }else if(seer$age[i]<61){
    seer$age.10per[i]='50-_60'
  }else if(seer$age[i]<71){
    seer$age.10per[i]='60_70'
  }else if(seer$age[i]<81){
    seer$age.10per[i]='70_80'
  }else if(seer$age[i]<91){
    seer$age.10per[i]='80_90'
  }else{
    seer$age.10per[i]='>90'
  }
}

table(seer$Sex)

table(seer$Race.recode..W..B..AI..API.)
for (i in 1:nrow(seer)) {
  if(seer$Race.recode..W..B..AI..API.[i]=='White'|
     seer$Race.recode..W..B..AI..API.[i]=='Black'){
    seer$Race.code[i]=seer$Race.recode..W..B..AI..API.[i]
  }else{
    seer$Race.code[i]='Other'
  }
}
table(seer$Race.code)
#
table(seer$ICD.O.3.Hist.behav)
table(seer$Histologic.Type.ICD.O.3)
#
table(seer$Derived.EOD.2018.T..2018..)
table(seer$Derived.AJCC.T..6th.ed..2004.2015.)
table(seer$T.value...based.on.AJCC.3rd..1988.2003.)

index <- which(seer$Derived.EOD.2018.T..2018..=='Blank(s)')
seer$Derived.EOD.2018.T..2018..[index]=''

index <- which(seer$Derived.AJCC.T..6th.ed..2004.2015.=='Blank(s)')
seer$Derived.AJCC.T..6th.ed..2004.2015.[index]=''

index <- which(seer$T.value...based.on.AJCC.3rd..1988.2003.=='Blank(s)')
seer$T.value...based.on.AJCC.3rd..1988.2003.[index]=''

seer$T.stage <- paste(seer$Derived.EOD.2018.T..2018..
                      ,seer$Derived.AJCC.T..6th.ed..2004.2015.
                      ,seer$T.value...based.on.AJCC.3rd..1988.2003.,sep = '')
table(seer$T.stage)
seer$T.stage <- substr(x = seer$T.stage,start = 1,stop = 2)
table(seer$T.stage)
seer$T.stage <- ifelse(seer$T.stage==''|seer$T.stage=='88'|seer$T.stage=='Re'|seer$T.stage=='NA','TX'
                       ,seer$T.stage)
table(seer$T.stage)
#
table(seer$Derived.EOD.2018.N..2018..)
table(seer$Derived.AJCC.N..6th.ed..2004.2015.)
table(seer$N.value...based.on.AJCC.3rd..1988.2003.)

index <- which(seer$Derived.EOD.2018.N..2018..=='Blank(s)')
seer$Derived.EOD.2018.N..2018..[index]=''

index <- which(seer$Derived.AJCC.N..6th.ed..2004.2015.=='Blank(s)')
seer$Derived.AJCC.N..6th.ed..2004.2015.[index]=''

index <- which(seer$N.value...based.on.AJCC.3rd..1988.2003.=='Blank(s)')
seer$N.value...based.on.AJCC.3rd..1988.2003.[index]=''

seer$N.stage <- paste(seer$Derived.EOD.2018.N..2018..
                      ,seer$Derived.AJCC.N..6th.ed..2004.2015.
                      ,seer$N.value...based.on.AJCC.3rd..1988.2003.,sep = '')
table(seer$N.stage)
seer$N.stage <- substr(seer$N.stage,start = 1,stop = 2)
seer$N.stage <- ifelse(seer$N.stage==''|
                         seer$N.stage=='88'|seer$N.stage=='Re'|
                         seer$N.stage=='NA','NX',seer$N.stage)
table(seer$N.stage)
#M
table(seer$Derived.EOD.2018.M..2018..)
table(seer$Derived.AJCC.M..6th.ed..2004.2015.)
table(seer$M.value...based.on.AJCC.3rd..1988.2003.)

index <- which(seer$Derived.EOD.2018.M..2018..=='Blank(s)')
seer$Derived.EOD.2018.M..2018..[index]=''

index <- which(seer$Derived.AJCC.M..6th.ed..2004.2015.=='Blank(s)')
seer$Derived.AJCC.M..6th.ed..2004.2015.[index]=''

index <- which(seer$M.value...based.on.AJCC.3rd..1988.2003.=='Blank(s)')
seer$M.value...based.on.AJCC.3rd..1988.2003.[index]=''

seer$M.stage <- paste(seer$Derived.EOD.2018.M..2018..,seer$Derived.AJCC.M..6th.ed..2004.2015.
                      ,seer$M.value...based.on.AJCC.3rd..1988.2003.,sep = '')
table(seer$M.stage)
seer$M.stage <- substr(seer$M.stage,start = 1,stop = 2)
table(seer$M.stage)
seer$M.stage <- ifelse(seer$M.stage==''|seer$M.stage=='88'|
                         seer$M.stage==''|seer$M.stage=='NA'|seer$M.stage=='Re','MX',seer$M.stage)
table(seer$M.stage)
#Grade
table(seer$Grade..thru.2017.)
seer$Grade.code='bk'
index <- which(seer$Grade..thru.2017.=='Well differentiated; Grade I')
seer$Grade.code[index]='I'

index <- which(seer$Grade..thru.2017.=='Moderately differentiated; Grade II')
seer$Grade.code[index]='II'

index <- which(seer$Grade..thru.2017.=='Poorly differentiated; Grade III')
seer$Grade.code[index]='III'

index <- which(seer$Grade..thru.2017.=='Undifferentiated; anaplastic; Grade IV')
seer$Grade.code[index]='IV'

index <- which(seer$Grade..thru.2017.=='Blank(s)')
seer$Grade.code[index]='Unknown'

index <- which(seer$Grade..thru.2017.=='Unknown')
seer$Grade.code[index]='Unknown'
table(seer$Grade.code)

#tumor size 
table(seer$Tumor.Size.Summary..2016..)
table(seer$CS.tumor.size..2004.2015.)
table(seer$EOD.10...size..1988.2003.)

index <- which(seer$Tumor.Size.Summary..2016..=='')
seer$Tumor.Size.Summary..2016..[index]='bk'
seer$Tumor.Size.Summary..2016..[seer$Tumor.Size.Summary..2016..=='bk']='1022'
seer$Tumor.Size.Summary..2016.. <- as.numeric(seer$Tumor.Size.Summary..2016..)
seer$size.2016.Group <- 'bk'
index <- which(seer$Tumor.Size.Summary..2016..>0&seer$Tumor.Size.Summary..2016..<55)
seer$size.2016.Group[index]='<55mm'
index <- which(seer$Tumor.Size.Summary..2016..>54&seer$Tumor.Size.Summary..2016..<151)
seer$size.2016.Group[index]='55-150mm'
index <- which(seer$Tumor.Size.Summary..2016..>150&seer$Tumor.Size.Summary..2016..<990)
seer$size.2016.Group[index]='>150mm'
table(seer$size.2016.Group)

index <- which(seer$CS.tumor.size..2004.2015.=='')
seer$CS.tumor.size..2004.2015.[index]='bk'
seer$CS.tumor.size..2004.2015.[seer$CS.tumor.size..2004.2015.=='bk']='1026'
seer$CS.tumor.size..2004.2015. <- as.numeric(seer$CS.tumor.size..2004.2015.)
seer$size.2004.2015.Group <- 'bk'
index <- which(seer$CS.tumor.size..2004.2015.>0&seer$CS.tumor.size..2004.2015.<55)
seer$size.2004.2015.Group[index]='<55mm'
index <- which(seer$CS.tumor.size..2004.2015.>54&seer$CS.tumor.size..2004.2015.<151)
seer$size.2004.2015.Group[index]='55-150mm'
index <- which(seer$CS.tumor.size..2004.2015.>150&seer$CS.tumor.size..2004.2015.<990)
seer$size.2004.2015.Group[index]='>150mm'
index <- which(seer$CS.tumor.size..2004.2015.>990&seer$CS.tumor.size..2004.2015.<996)
seer$size.2004.2015.Group[index]='<55mm'
table(seer$size.2004.2015.Group)

index <- which(seer$EOD.10...size..1988.2003.=='')
seer$EOD.10...size..1988.2003.[index]='bk'
seer$EOD.10...size..1988.2003.[seer$EOD.10...size..1988.2003.=='bk']='1026'
seer$EOD.10...size..1988.2003. <- as.numeric(seer$EOD.10...size..1988.2003.)
seer$size.1988.2003.Group <- 'bk'
index <- which(seer$EOD.10...size..1988.2003.>1&seer$EOD.10...size..1988.2003.<55)
seer$size.1988.2003.Group[index]='<55mm'
index <- which(seer$EOD.10...size..1988.2003.>54&seer$EOD.10...size..1988.2003.<151)
seer$size.1988.2003.Group[index]='55-150mm'
index <- which(seer$EOD.10...size..1988.2003.>150&seer$EOD.10...size..1988.2003.<991)
seer$size.1988.2003.Group[index]='>150mm'
table(seer$size.1988.2003.Group)
seer$Size.Group <- paste(seer$size.2016.Group,seer$size.2004.2015.Group,seer$size.1988.2003.Group,sep = '-')
table(seer$Size.Group)
for (i in 1:nrow(seer)) {
  if(seer$Size.Group[i]=='<55mm-bk-bk'|seer$Size.Group[i]=='bk-<55mm-bk'|seer$Size.Group[i]=='bk-bk-<55mm'){
    seer$Size.Group[i]='<55mm'
  }else if(seer$Size.Group[i]=='55-150mm-bk-bk'|
           seer$Size.Group[i]=='bk-55-150mm-bk'|
           seer$Size.Group[i]=='bk-bk-55-150mm'){
    seer$Size.Group[i]='55-150mm'
  }else if(seer$Size.Group[i]=='>150mm-bk-bk'|
           seer$Size.Group[i]=='bk->150mm-bk'|
           seer$Size.Group[i]=='bk-bk->150mm'){
    seer$Size.Group[i]='>150mm'
  }else{
    seer$Size.Group[i]='Unknown'
  }
}
table(seer$Size.Group)
#
table(seer$RX.Summ..Surg.Prim.Site..1998..)
class(seer$RX.Summ..Surg.Prim.Site..1998..)
index <- which(seer$RX.Summ..Surg.Prim.Site..1998..>9&seer$RX.Summ..Surg.Prim.Site..1998..<20)
seer$Surg.Group <- 'Unknown'
seer$Surg.Group[index]='Destruction'
index <- which(seer$RX.Summ..Surg.Prim.Site..1998..>19&seer$RX.Summ..Surg.Prim.Site..1998..<81)
seer$Surg.Group[index]='Resection'
index <- which(seer$RX.Summ..Surg.Prim.Site..1998..==90|seer$RX.Summ..Surg.Prim.Site..1998..==98)
seer$Surg.Group[index]='Surgery'
table(seer$Surg.Group)

#
table(seer$Survival.months)
class(seer$Survival.months)
seer <- subset(seer,seer$Survival.months!='Unknown')

#
table(seer$Vital.status.recode..study.cutoff.used.)
seer$status <- seer$Vital.status.recode..study.cutoff.used.
table(seer$status)
seer$status <- ifelse(seer$status=='Alive',0,1)

table(seer$Sequence.number)
table(seer$Total.number.of.benign.borderline.tumors.for.patient)
table(seer$Total.number.of.in.situ.malignant.tumors.for.patient)
seer <- subset(x = seer,subset = seer$Total.number.of.in.situ.malignant.tumors.for.patient==1)
table(seer$Total.number.of.in.situ.malignant.tumors.for.patient)

#
seer_1 <- subset(seer,seer$Sequence.number=='One primary only')

#
seer_2 <- subset(seer,seer$Sequence.number!='One primary only')
seer_2 <- subset(seer_2,subset = seer_2$Diagnostic.Confirmation=='Positive histology')
seer_2 <- seer_21
seer$P_S <- 'Second'
for (i in 1:length(seer$Patient.ID)) {
  index=which(seer$Patient.ID==seer_1$Patient.ID[i])
  seer$P_S[index]='Primary'
}
table(seer$P_S)

table(seer$Reason.no.cancer.directed.surgery)
seer$Surg.bio <- ifelse(seer$Reason.no.cancer.directed.surgery=='Surgery performed','Yes','No')
table(seer$Radiation.recode)
seer$Radio.bio <- ifelse(seer$Radiation.recode=='Refused (1988+)'|
                           seer$Radiation.recode=='None/Unknown','No','Yes')
seer$Surg.Radio <- paste(seer$Surg.bio,seer$Radio.bio,sep = '-')
table(seer$Surg.Radio)

seer$Radio.Chemo <- paste(seer$Radio.bio,seer$Chemotherapy,sep = '-')

seer$Surg.Chemo <- paste(seer$Surg.bio,seer$Chemotherapy,sep = '-')

table(seer$Chemotherapy.recode..yes..no.unk.)
seer$Chemotherapy[seer$Chemotherapy.recode..yes..no.unk.=='No/Unknown']='No'
table(seer$Chemotherapy)
seer$Surg.Radio.Chemo <- paste(seer$Surg.bio,seer$Radio.bio,seer$Chemotherapy,sep = '-')
table(seer$Surg.Radio.Chemo)

#
seer_2$Primary.Site.3F <- 'Other'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C14.0-Pharynx, NOS']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C30.0-Nasal cavity']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C32.0-Glottis']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C32.1-Supraglottis']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C32.2-Subglottis']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C32.3-Laryngeal cartilage']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C32.9-Larynx, NOS']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C33.9-Trachea']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C34.0-Main bronchus']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C40.9-Bone of limb, NOS']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.0-Bones of skull and face and associated joints']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.3-Rib, sternum, clavicle and associated joints']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C49.0-Conn, subcutaneous, other soft tis: head, face, neck']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C49.1-Conn, subcutaneous, other soft tis: upr limb, shoulder']='hyaline cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C49.2-Conn, subcutaneous, other soft tis: lower limb, hip']='hyaline cartilage'

seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.1-Mandible']='elastic cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.8-Overlap bones, joints, and art. cartilage']='elastic cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.9-Bone, NOS']='elastic cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C49.3-Conn, subcutaneous, other soft tis: thorax']='elastic cartilage'

seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C40.0-Long bones: upper limb, scapula, and associated joints']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C40.1-Short bones of upper limb and associated joints']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C40.2-Long bones of lower limb and associated joints']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C40.3-Short bones of lower limb and associated joints']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.2-Vertebral column']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C41.4-Pelvic bones, sacrum, coccyx and associated joints']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C49.5-Conn, subcutaneous, other soft tis: pelvis']='fibrous cartilage'
seer_2$Primary.Site.3F[seer_2$Primary.Site...labeled=='C72.0-Spinal cord']='fibrous cartilage'

table(seer_2$Primary.Site.3F)


library(survival)
library(survminer)

library(survival)
library(survminer)
fit <- survfit(Surv(Survival.months,status)~Sequence.number,data = seer)
ggsurvplot(fit =fit,data = seer,risk.table = T,pval = T)

fit <- survfit(Surv(Survival.months,status)~P_S,data = seer)
ggsurvplot(fit =fit,data = seer,risk.table = T,pval = T)




ggsurvplot(
  fit, # survfit object with calculated statistics.
  pval = TRUE, # show p-value of log-rank test.
  conf.int = TRUE, # show confidence intervals for
  # point estimaes of survival curves.
  conf.int.style = "ribbon", # customize style of confidence intervals
  xlab = "Time in months", # customize X axis label.
  break.time.by = 120, # break X axis in time intervals by 200.
  ggtheme = theme_light(), # customize plot and risk table with a theme.
  risk.table = "abs_pct", # absolute number and percentage at risk.
  risk.table.y.text.col = T,# colour risk table text annotations.
  risk.table.y.text = FALSE,# show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = T, # plot the number of censored subjects at time t
  surv.median.line = "hv", # add the median survival pointer.
  legend.labs =
    c("Primary only", "NPC"), # change legend labels.
  palette =
    c("#E7B800", "#2E9FDF")) # custom color palettes.
dev.off()









cox.multi <- coxph(Surv(Survival.months,status)~P_S,data = seer)#
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.95))
CI95


seer$P_S









fit <- survfit(Surv(Survival.months,status)~Surg.Radio.Chemo,data = seer_2)
ggsurvplot(fit =fit,data = seer,risk.table = T,pval = T)
ggsurvplot(fit
           ,pval = T
           ,conf.int = T
           ,linetype = c('solid','dashed','solid','dashed','solid','dashed','solid','dashed')
           ,surv.median.line = 'hv'
           ,ggtheme = theme_bw()
           ,palette = c('skyblue','yellow','forestgreen','orange','black','red','green','brown'))


fit <- survfit(Surv(Survival.months,status)~P_S,data = seer)
ggsurvplot(fit =fit,data = seer,risk.table = T,pval = T)

fit <- survfit(Surv(Survival.months,status)~Surg.Radio,data = seer_1)
ggsurvplot(fit =fit,data = seer_1,risk.table = T,pval = T)

fit <- survfit(Surv(Survival.months,status)~Surg.Radio,data = seer_2)
ggsurvplot(fit =fit,data = seer_2,risk.table = T,pval = T)


seer_Mes <- subset(seer_21,seer_21$ICD.O.3.Hist.behav=='9240/3: Mesenchymal chondrosarcoma')
fit <- survfit(Surv(Survival.months,status)~Surg.Radio,data = seer_Mes)
ggsurvplot(fit =fit,data = seer_Mes,risk.table = T,pval = T)

table(seer_2$ICD.O.3.Hist.behav)
seer2_Mes <- subset(seer_2,seer_2$ICD.O.3.Hist.behav=='9240/3: Mesenchymal chondrosarcoma')
fit <- survfit(Surv(Survival.months,status)~Surg.Radio,data = seer2_Mes)
ggsurvplot(fit =fit,data = seer2_Mes,risk.table = T,pval = T)




cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer_2)
summary(cox.multi)
#
cox.multi <- coxph(Surv(Survival.months,status)~Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+
                     Surg.Radio.Chemo,data =seer_2)
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~age+T.stage+N.stage+M.stage+Grade.code+Histologic.Type.+
                     Size.Group+Primary.Site.3F+
                     Surg.Radio.Chemo,data = Validation)
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95
#
library(regplot)
regplot(cox.multi
        ,odds = T
        ,interval = 'confidence'
        ,points = T)

regplot(reg = cox.multi
             ,title = 'Cox Regression,C-index=0.783'
             ,plots = c('density','boxes')
             ,failtime = c(12,36,60)
             ,center = T
             ,prfail = T
             ,showP = T
             ,droplines=F
             ,points = T,rank = 'range',subticks = T)
#



#
ddist <- datadist(seer_2)
options(datadist='ddist')
cox <- cph(Surv(Survival.months,status)~Age+Sex+M.stage+
             Grade.code+Histologic.Type+Cartilage.type+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,data =seer_2,surv=T,x=T,y=T)
cox
#

surv <- Survival(cox)
sur_1_year <- function(x)surv(1*12*1,lp=x)
sur_3_year<-function(x)surv(1*12*3,lp=x)#
sur_5_year<-function(x)surv(1*12*5,lp=x)#5
nom_sur <- nomogram(cox,fun=list(sur_1_year,sur_3_year,sur_5_year)
                    ,lp= F,funlabel=c('1-Year Survival','3-Year Survival','5-Year survival')
                    ,maxscale=100
                    ,fun.at=c('0.9','0.8','0.7','0.6','0.5','0.4','0.3','0.2','0.1'))




seer_2$Age <- seer_2$age.10per
seer_2$Histologic.Type <- seer_2$Histologic.Type.ICD.O.3
seer_2$Cartilage.type <- seer_2$Primary.Site.3F





library(mstate)
table(seer_2$Survival.months)
table(seer_2$compete_riskstatus)
table(seer_2$Patient.ID)

data_w <- crprep('Survival.months','compete_riskstatus'
                 ,data = seer_2
                 ,trans = c(1,2)#
                 ,id = 'Patient.ID'
                 ,keep = c('age','Sex.code.num','T.stage.num','M.stage.num','Grade.code.num','Histologic.Type_num',
                             'Size.Group.num','Primary.Site.3F.num','Surg.Radio.Chemo'))

class(seer_2$age)
class(seer_2$Sex.code.num)
class(seer_2$T.stage.num)
cox.multi <- coxph(Surv(Survival.months,status)~age+Sex+T.stage+M.stage+Grade.code+Histologic.Type.+
                     Size.Group+Primary.Site.3F+
                     Surg.Radio.Chemo,data =seer_2)
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95
#


data <- subset(data,data$time!=0)



data_w <- crprep(data$time,data$compete_riskstatus
                 ,data = data
                 ,trans = c(1,2)
                 ,id = 'Patient.ID'
                 ,keep = c('Regional.nodes.positive..1988..'
                           ,'Age_re'
                           ,'Laterality'))


data_w$Times <- data_w$Tstop-data_w$Tstart
table(data$compete_riskstatus)
attach(data_w)


data_crr <- coxph(Surv(Times,status==1)~age+Sex.code.num+T.stage.num+M.stage.num+Grade.code.num+Histologic.Type_num+
                    Size.Group.num+Primary.Site.3F.num
                  ,data = data_w[data_w$failcode==1,]
                  ,weights = weight.cens
                  ,subset = failcode==1)

library(regplot)
regplot(data_crr,failtime = c(-6,-2),prfail = T,droplines=T)

regplot(reg = data_crr
        ,title = 'Cox Regression,C-index=0.765'
        ,plots = c('density','boxes')
        ,failtime = c(12,36,60)
        ,center = T
        ,prfail = T
        ,showP = T
        ,droplines=F
        ,points = T)

table(seer$ICD.O.3.Hist.behav)
set.seed(20221106)
n <- nrow(seer_2)
trainindex <- sample(x = 1:n,size = 0.7*n)
seer_2$Diag.Group <- 'Validation'
seer_2$Diag.Group[trainindex]='Training'
table(seer_2$Diag.Group)
Training <- subset(x = seer_2,subset = seer_2$Diag.Group=='Training')
Validation <- subset(x = seer_2,subset = seer_2$Diag.Group=='Validation')



seer_2$Histologic.Type.ICD.O.3 <- as.character(seer_2$Histologic.Type.ICD.O.3)
seer_2$Histologic.Type. <- seer_2$Histologic.Type.ICD.O.3
library(table1)
seer$Sex <- factor(seer$Sex)
table1(~age+Sex+Race.code+Primary.Site.3F+T.stage+N.stage+M.stage+Size.Group+Grade.code+Histologic.Type.+
         Size.Group+Surg.bio+Radio.bio+Chemotherapy+Surg.Group+Surg.Radio.Chemo|Diag.Group,
       data=seer_2,topclass="Rtable1-zebra")
table(seer_2$Histologic.Type.ICD.O.3)
table(seer_2$Surg.bio)
table(seer_2$Radio.bio)
table(seer_2$Chemotherapy)
t.test(age~Diag.Group,seer_2)
chisq.test(seer_2$Sex,seer_2$Diag.Group)
chisq.test(seer_2$Race.code,seer_2$Diag.Group)
chisq.test(seer_2$Primary.Site.3F,seer_2$Diag.Group)
chisq.test(seer_2$T.stage,seer_2$Diag.Group)
chisq.test(seer_2$N.stage,seer_2$Diag.Group)
chisq.test(seer_2$M.stage,seer_2$Diag.Group)
chisq.test(seer_2$Size.Group,seer_2$Diag.Group)
chisq.test(seer_2$Grade.code,seer_2$Diag.Group)
chisq.test(seer_2$Histologic.Type.,seer_2$Diag.Group)
chisq.test(seer_2$Surg.bio,seer_2$Diag.Group)
chisq.test(seer_2$Radio.bio,seer_2$Diag.Group)
chisq.test(seer_2$Chemotherapy,seer_2$Diag.Group)
chisq.test(seer_2$Surg.Group,seer_2$Diag.Group)
chisq.test(seer_2$Surg.Radio.Chemo,seer_2$Diag.Group)



table(seer_2$Diag.Group)
seer_2$Diag.Group.num <- ifelse(seer_2$Diag.Group=='Training',0,1)
seer_2$Diag.Group.num <- factor(seer_2$Diag.Group.num,levels = c(0,1,2),labels = c("Training","Validation","P_value"))

x_value <- seer_2$Diag.Group.num
table(x_value)
rndr <- function(x,name,...){
  if(length(x)==0){
    y <- seer_2[[name]]
    s <- rep("",length(render.default(x=y,name=name,...)))
    if(is.numeric(y)){
      p <- wilcox.test(y ~ x_value,paired=F)$p.value
    }else{
      p <- chisq.test(table(y,droplevels(x_value)))$p.value
    }
    s[2] <- sub("<","&lt;",format.pval(p,digits = 3,eps=0.001))
    s
  }else{
    render.default(x=x,name=name,...)
  }
}
rndr.strat <- function(label, n, ...) {
  ifelse(n=0, label, render.strat.default(label, n, ...))
}#
library(table1)
table1(~age+Sex+Race.code+Primary.Site.3F+T.stage+N.stage+M.stage+Size.Group+Grade.code+Histologic.Type.+
         Size.Group+Surg.bio+Radio.bio+Chemotherapy+Surg.Group+Surg.Radio.Chemo|Diag.Group.num
       ,data = seer_2,render = rndr,droplevels = F,render.strat = rndr.strat
       ,overall = F,topclass="Rtable1-zebra")




ext.validation$Status <- as.character(ext.validation$Status)
library(table1)
table1(~Age+Sex+Primary.site.3F+Histologic.type+Grade+Size+Surge.method+Radiation+Chemotherapy+M.stage+Surg.Radio.Chemo
       +Survival.moths+Status,
       data=ext.validation,topclass="Rtable1-zebra")

chisq.test(seer_2$Primary.Site.3F,seer_2$Histologic.Type.)
xtabs(~seer_2$Primary.Site.3F+seer_2$Histologic.Type,data = seer_2)
DYdata <- read.table('sc.txt',sep = '\t',header = T,row.names = 1)
DYdata2 <- read.table('CA.txt',sep = '\t',header = T,row.names = 1)
require(FactoMineR)
require(factoextra)
data("housetasks")
head(housetasks)
res.car <- CA(X = DYdata,graph = F)
res.car
summary(res.car)
head(res.car$eig)[,1:2]
plot(res.car,invisible = "row")
plot(res.car,invisible = "col")
plot(res.car)
dev.off()

cox.multi <- coxph(Surv(Survival.months,status)~age,data = seer_2)#age
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.95))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Sex,data = seer_2)#Sex
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.95))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Race.code,data = Training)#race
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.95))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data = seer_2)#primary.ste.sF
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~T.stage,data = seer_2)#T.stage
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~N.stage,data = Training)#N.stage
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~M.stage,data = Training)#M.stage
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Size.Group,data = seer_2)#Size.Group
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Grade.code,data = seer_2)#Grade.code
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

library(forcats)
seer_2$Histologic.Type. <- relevel(seer_2$Histologic.Type.,ref = '9220')
class(seer_2$Histologic.Type.)
seer_2$Histologic.Type. <- as.factor(seer_2$Histologic.Type.)
table(seer_2$Histologic.Type.)

cox.multi <- coxph(Surv(Survival.months,status)~Histologic.Type.,data = seer_2)#Histologic.Type
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Surg.bio,data = Training)#Surg.bio
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Radio.bio,data = Training)#Radio.bio
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Surg.Group,data = seer_2)#Surg.Group
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

cox.multi <- coxph(Surv(Survival.months,status)~Chemotherapy,data = seer_2)#Chemotherapy
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95
Training$Chemotherapy

cox.multi <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data = seer_2)#Surg.radio.chemo
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

seer_2$Race.code



cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =Validation)
summary(cox.multi)
class(seer_2$status)
seer_2$status <- as.character(seer_2$status)
seer_2$status <- as.numeric(seer_2$status)



table(seer$Sequence.number)
table(seer_3$ICD.O.3.Hist.behav)
seer_3 <- subset(seer_2,seer_2$Sequence.number!='1st of 2 or more primaries')
seer_3 <- subset(seer_3,seer_3$Histologic.Type.=='9240/3: Mesenchymal chondrosarcoma'|
                   seer_3$Histologic.Type.=='9243/3: Dedifferentiated chondrosarcoma')
cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer_3)
summary(cox.multi)
seer_3$age.10per
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95


table(seer_2$Surg.Radio.Chemo)
class(seer_2$Surg.Radio.Chemo)
seer_2$Surg.Radio.Chemo <- relevel(seer_2$Surg.Radio.Chemo,ref = 'Yes-No-No')
cox.multi <- coxph(Surv(Survival.months,status)~age+Sex+T.stage+M.stage+Grade.code+Histologic.Type.+
                     Size.Group+Primary.Site.3F+
                     Surg.Radio.Chemo,data = seer_2)
summary(cox.multi)







cox.multi <- coxph(Surv(Survival.months,status)~Primary.Site,data = Training)
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

library(mstate)
library(cmprsk)
library(survival)
library(survminer)
table(seer_2$COD.to.site.rec.KM)

for (i in 1:nrow(seer_2)) {
  if(seer_2$COD.to.site.rec.KM[i]=='Alive'){
    seer_2$compete_riskstatus[i]=0
  }else if(seer_2$COD.to.site.rec.KM[i]=='Bones and Joints'|
           seer_2$COD.to.site.rec.KM[i]=='In situ, benign or unknown behavior neoplasm'){
    seer_2$compete_riskstatus[i]=1
  }else{
    seer_2$compete_riskstatus[i]=2
  }
}
table(seer_2$compete_riskstatus)


una_compe_risk <- cuminc(seer_2$Survival.months
                         ,seer_2$compete_riskstatus==1
                         ,seer_2$Sex
                         ,cencode = 0)
una_compe_risk
summary(una_compe_risk)

plot(una_compe_risk,xlab = 'year'
     ,lty = 1
     ,lwd = 2
     ,ylab = 'CIF'
     ,color = c('red','blue','orange','black'))

factor2ind <- function(x, baseline){ 
  xname <- deparse(substitute(x))
  n =length(x) 
  x =as.factor(x) 
  if(!missing(baseline)) x <- relevel(x, baseline) 
  X = matrix(0, n, length(levels(x))) 
  X[(1:n) + n*(unclass(x)-1)] <- 1 
  X[is.na(x),]=NA 
  dimnames(X) =list(names(x), paste(xname, levels(x), sep = ":")) 
  return(X[,-1,drop=FALSE])
}


comprisk <- crr(ftime = seer_2$Survival.months
                      ,fstatus = seer_2$compete_riskstatus
                      ,cov1 = seer_2$age
                      ,failcode = 1
                      ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$age.10per))) 
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)


#Sex
table(seer_2$Sex)
new_cov=data.frame(cbind(factor2ind(seer_2$Sex))) 
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov#sex
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Race
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Race.code.num#race
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)
new_cov=data.frame(cbind(factor2ind(seer_2$Race.code)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Primary site 3F
table(seer_2$Primary.Site.3F)
seer_2$Primary.Site.3F.num[seer_2$Primary.Site.3F=='fibrous cartilage']=1
seer_2$Primary.Site.3F.num[seer_2$Primary.Site.3F=='elastic cartilage']=2
seer_2$Primary.Site.3F.num[seer_2$Primary.Site.3F=='hyaline cartilage']=3
seer_2$Primary.Site.3F.num[seer_2$Primary.Site.3F=='Other']=4
table(seer_2$Primary.Site.3F.num)
class(seer_2$Primary.Site.3F.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = seer_2$Primary.Site.3F.num#primary.site 
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Primary.Site.3F)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#T.stage
table(seer_2$T.stage.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$T.stage.num#T stage
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$T.stage)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#N.stage
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$N.stage.num# N stage
                ,failcode = 1
                ,cencode = 0)
comprisk
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$N.stage)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#M.stage
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$M.stage.num# M stage
                ,failcode = 1
                ,cencode = 0)
comprisk
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$M.stage)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Size
table(seer_2$Size.Group)
seer_2$Size.Group.num[seer_2$Size.Group=='<55mm']=1
seer_2$Size.Group.num[seer_2$Size.Group=='55-150mm']=2
seer_2$Size.Group.num[seer_2$Size.Group=='>150mm']=3
seer_2$Size.Group.num[seer_2$Size.Group=='Unknown']=4
table(seer_2$Size.Group.num)
class(seer_2$Size.Group.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Size.Group.num#size
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Size.Group)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Grade
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Grade.code.num#Grade
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Grade.code)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)
table(seer_2$Histologic.Type.)


#Histologic type
table(seer_2$Histologic.Type.)
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9220]=1
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9200]=2
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9210]=3
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9221]=4
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9231]=5
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9240]=6
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9241]=7
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9242]=8
seer_2$Histologic.Type_num[seer_2$Histologic.Type.==9243]=9
table(seer_2$Histologic.Type_num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Histologic.Type_num
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
seer_2 <- subset(x = seer_2,subset = seer_2$ICD.O.3.Hist.behav!='9200/0: Osteoblastoma, NOS')
seer_2 <- subset(x = seer_2,subset = seer_2$ICD.O.3.Hist.behav!='9210/0: Osteochondroma')
seer_2 <- subset(x = seer_2,subset = seer_2$ICD.O.3.Hist.behav!='9210/1: Osteochondromatosis, NOS')
seer_2 <- subset(x = seer_2,subset = seer_2$ICD.O.3.Hist.behav!='9220/0: Chondroma, NOS')
seer_2 <- subset(x = seer_2,subset = seer_2$ICD.O.3.Hist.behav!='9241/0: Chondromyxoid fibroma')
seer_2 <- subset(x = seer_2,subset = seer_2$Histologic.Type.!='9200')
seer_2 <- subset(x = seer_2,subset = seer_2$Histologic.Type.!='9210')
seer_2 <- subset(x = seer_2,subset = seer_2$Histologic.Type.!='9241')
table(seer_2$Histologic.Type.)
table(seer_2$ICD.O.3.Hist.behav)
new_cov=data.frame(cbind(factor2ind(seer_2$ICD.O.3.Hist.behav)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)
table(seer_2$Histologic.Type.)


#Surg.bio
table(seer_2$Surg.bio)
seer_2$Surg.bio.num[seer_2$Surg.bio=='No']=1
seer_2$Surg.bio.num[seer_2$Surg.bio=='Yes']=2
table(seer_2$Surg.bio.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Surg.bio.num#surg.bio
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Surg.bio)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Radio.bio
table(seer_2$Radiation.recode.num)
table(seer_2$Radiation.recode)
table(seer_2$Radio.bio)
seer_2$Radio.bio.num[seer_2$Radio.bio=='No']=0
seer_2$Radio.bio.num[seer_2$Radio.bio=='Yes']=1
table(seer_2$Radio.bio.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Radio.bio.num#radiation  
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Radio.bio)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Chemo.bio
table(seer_2$Chemotherapy.num)
table(seer_2$Chemotherapy)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Chemotherapy.num#chemotherapy
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Chemotherapy)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)


#Surg.method
table(seer_2$Surg.Group)
seer_2$Surg.Group.num[seer_2$Surg.Group=='Destruction']=2
seer_2$Surg.Group.num[seer_2$Surg.Group=='Resection']=3
seer_2$Surg.Group.num[seer_2$Surg.Group=='Surgery']=1
seer_2$Surg.Group.num[seer_2$Surg.Group=='Unknown']=4
table(seer_2$Surg.Group.num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Surg.Group.num#surg.method
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
library(forcats)
class(seer_2$Surg.Group)
seer_2$Surg.Group <- as.factor(seer_2$Surg.Group)
seer_2$Surg.Group <- relevel(seer_2$Surg.Group,ref = "Resection")
m1$prog2 <- relevel(seer_2$Surg.Group,ref = "Surgery")

new_cov=data.frame(cbind(factor2ind(seer_2$Surg.Group)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)



#Surg.Radio.Chemo
table(seer_2$Surg.Radio.Chemo)
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='No-No-No']=2
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='No-No-Yes']=3
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='No-Yes-No']=4
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='No-Yes-Yes']=5
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='Yes-No-No']=1
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='Yes-No-Yes']=6
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='Yes-Yes-No']=7
seer_2$Surg.Radio.Chemo_num[seer_2$Surg.Radio.Chemo=='Yes-Yes-Yes']=8
table(seer_2$Surg.Radio.Chemo_num)
table(seer_2$surra)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,seer_2$Surg.Radio.Chemo_num
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.90)
new_cov=data.frame(cbind(factor2ind(seer_2$Surg.Radio.Chemo)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
summary(comprisk,conf.int = 0.95)



data_f <- data.frame(seer_2$age
                     ,seer_2$Histologic.Type_num
                     ,seer_2$Grade.code.num
                     ,seer_2$Primary.Site.3F.num
                     ,seer_2$T.stage.num
                     ,seer_2$N.stage.num
                     ,seer_2$M.stage.num
                     ,seer_2$Size.Group.num
                     ,seer_2$Radiation.recode.num
                     ,seer_2$Chemotherapy.num
                     ,seer_2$Sequence.number.num
                     ,seer_2$Surg.Radio.Chemo_num)
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,data_f
                ,failcode = 1
                ,cencode = 0)
comprisk
summary(comprisk,conf.int = 0.90)



cox.multi <- coxph(Surv(Survival.months,status)~age+Sex+T.stage+M.stage+Grade.code+Histologic.Type.+
                     Size.Group+Surg.Group+Primary.Site.3F+
                     Surg.Radio.Chemo,data =seer_2)
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95


new_cov=data.frame(cbind(factor2ind(seer_2$age)
                         ,factor2ind(seer_2$Histology)
                         ,factor2ind(seer_2$Radio.bio)
                         ,factor2ind(seer_2$Chemotherapy)
                         ,factor2ind(seer_2$T.stage)
                         ,factor2ind(seer_2$N.stage)
                         ,factor2ind(seer_2$Grade.code)
                         ,factor2ind(seer_2$Size.Group)
                         ,factor2ind(seer_2$Primary.Site.3F)
                         ,factor2ind(seer_2$Surg.Group)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
comprisk[["converged"]]=T
summary(comprisk,conf.int = 0.95)



#primary site 3f 
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Primary.Site.3F)
una_compe_risk

new_cov <- factor2ind(seer_2$Primary.Site.3F)
comprisk <- crr(ftime = seer_2$time,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov)
summary(comprisk)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray','red','yellow'))

plot(una_compe_risk$`fibrous cartilage 1`$time
     ,una_compe_risk$`fibrous cartilage 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,30)
     ,xlim = c(0,18)
     ,col='darkblue')
lines(una_compe_risk$`elastic cartilage 1`$time
      ,una_compe_risk$`elastic cartilage 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,30,c('fibrous cartilage','elastic cartilage'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='fibr vs elas,pval=0.001',bty = 'n')



par(mfrow=c(2,3))
#T.stage
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$T.stage)
plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray','red','yellow','seagreen','salmon'))

#T2 vs T1
plot(una_compe_risk$`T2 1`$time
     ,una_compe_risk$`T2 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,35)
     ,xlim = c(0,16)
     ,col='darkblue')
lines(una_compe_risk$`T1 1`$time
      ,una_compe_risk$`T1 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,35,c('T2','T1'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='T2 vs T1,pval=0.015',bty = 'n')


#M1 vs M0
una_compe_risk <- cuminc(ftime = seer_2$time
                        ,fstatus = seer_2$compete_riskstatus
                        ,group = seer_2$M.stage)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray'))


plot(una_compe_risk$`M1 1`$time
     ,una_compe_risk$`M1 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,50)
     ,xlim = c(0,12)
     ,col='darkblue')
lines(una_compe_risk$`M0 1`$time
      ,una_compe_risk$`M0 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,50,c('M1','M0'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='M1 vs M0,pval=0.001',bty = 'n')


#Size group
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Size.Group)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray','yellow','salmon'))

#55-150mm vs <50mm
plot(una_compe_risk$`55-150mm 1`$time
     ,una_compe_risk$`55-150mm 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,32)
     ,xlim = c(0,18)
     ,col='darkblue')
lines(una_compe_risk$`<55mm 1`$time
      ,una_compe_risk$`<55mm 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,32,c('55-150mm','<55mm'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='55-150mm vs <50mm,pval<0.001',bty = 'n')

#>150mm  vs <55mm
plot(una_compe_risk$`>150mm 1`$time
     ,una_compe_risk$`>150mm 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,32)
     ,xlim = c(0,19)
     ,col='darkblue')
lines(una_compe_risk$`<55mm 1`$time
      ,una_compe_risk$`<55mm 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,30,c('>150mm','<55mm'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='>150mm vs <50mm,pval<0.01',bty = 'n')

#Grade 
table(seer_2$Grade.code)
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Grade.code)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray','yellow','salmon','red','purple'))


#II  vs  I
plot(una_compe_risk$`II 1`$time
     ,una_compe_risk$`II 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,20)
     ,xlim = c(0,15)
     ,col='darkblue')
lines(una_compe_risk$`I 1`$time
      ,una_compe_risk$`I 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,20,c('GradeII','GradeI'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='GradeII vs GradeI,pval<0.01',bty = 'n')

#III  vs I
plot(una_compe_risk$`III 1`$time
     ,una_compe_risk$`III 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,30)
     ,xlim = c(0,15)
     ,col='darkblue')
lines(una_compe_risk$`I 1`$time
      ,una_compe_risk$`I 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(9,5,c('GradeIII','GradeI'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='GradeIII vs GradeI,pval<0.01',bty = 'n')

#IV  vs  I
plot(una_compe_risk$`IV 1`$time
     ,una_compe_risk$`IV 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,40)
     ,xlim = c(0,16)
     ,col='darkblue')
lines(una_compe_risk$`I 1`$time
      ,una_compe_risk$`I 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(9,7,c('GradeIV','GradeI'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='GradeIV vs GradeI,pval<0.01',bty = 'n')

#Unknown  vs  I
plot(una_compe_risk$`Unknown 1`$time
     ,una_compe_risk$`Unknown 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,25)
     ,xlim = c(0,18)
     ,col='darkblue')
lines(una_compe_risk$`I 1`$time
      ,una_compe_risk$`I 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,25,c('Unknown','GradeI'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='Unknown vs GradeI,pval<0.01',bty = 'n')

#Histologic type
table(seer_2$Histologic.Type.)
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Histologic.Type.)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen','skyblue','gray'
            ,'yellow','salmon','red','purple','navy','magenta'))


#9221  vs 9220
plot(una_compe_risk$`9221 1`$time
     ,una_compe_risk$`9221 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,20)
     ,xlim = c(0,17)
     ,col='darkblue')
lines(una_compe_risk$`9220 1`$time
      ,una_compe_risk$`9220 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(0,20,c('9221','9220'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='9221 vs 9220,pval<0.01',bty = 'n')

#9240  vs 9220
plot(una_compe_risk$`9240 1`$time
     ,una_compe_risk$`9240 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,42)
     ,xlim = c(0,17)
     ,col='darkblue')
lines(una_compe_risk$`9220 1`$time
      ,una_compe_risk$`9220 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(9,10,c('9240','9220'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='9240 vs 9220,pval<0.01',bty = 'n')

#9243  vs 9220
plot(una_compe_risk$`9243 1`$time
     ,una_compe_risk$`9243 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,42)
     ,xlim = c(0,12)
     ,col='darkblue')
lines(una_compe_risk$`9220 1`$time
      ,una_compe_risk$`9220 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(7,10,c('9243','9220'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='9243 vs 9220,pval<0.01',bty = 'n')



#Surg.bio
table(seer_2$Surg.bio)
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Surg.bio)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen'))

#surg  vs non_surg
plot(una_compe_risk$`Yes 1`$time
     ,una_compe_risk$`Yes 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,30)
     ,xlim = c(0,19)
     ,col='darkblue')
lines(una_compe_risk$`No 1`$time
      ,una_compe_risk$`No 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(10,7,c('Surg','Non_surg'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='Surg vs Non_surg,pval<0.01',bty = 'n')


#Radio.bio
table(seer_2$Radio.bio)
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Radio.bio)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen'))

#radia  vs Non_radia
plot(una_compe_risk$`Yes 1`$time
     ,una_compe_risk$`Yes 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,28)
     ,xlim = c(0,18)
     ,col='darkblue')
lines(una_compe_risk$`No 1`$time
      ,una_compe_risk$`No 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(9,7,c('Radia','Non_Radia'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='Radia vs Non_Radia,pval<0.01',bty = 'n')



#Chemotherapy
table(seer_2$Chemotherapy)
una_compe_risk <- cuminc(ftime = seer_2$time
                         ,fstatus = seer_2$compete_riskstatus
                         ,group = seer_2$Chemotherapy)

plot(una_compe_risk
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,0.55)
     ,xlim = c(0,12)
     ,col=c('darkblue','orange','black','forestgreen'))

plot(una_compe_risk$`Yes 1`$time
     ,una_compe_risk$`Yes 1`$est*100
     ,type = 's'
     ,xlab = 'month'
     ,lty=1
     ,lwd=5
     ,ylab = 'Cumulative incidence'
     ,ylim = c(0,28)
     ,xlim = c(0,16)
     ,col='darkblue')
lines(una_compe_risk$`No 1`$time
      ,una_compe_risk$`No 1`$est*100
      ,type = 's'
      ,lty=1
      ,lwd=5
      ,col='orange')
legend(7,7,c('Chemo','Non_Chemo'),lty=1,lwd=5,col = c('darkblue','orange')
       ,title='Chemo vs Non_Chemo,pval<0.01',bty = 'n')




#crr
library(cmprsk)
new_cov=data.frame(cbind(factor2ind(seer_2$Race.code)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
predict_crr <- predict.crr(comprisk,cov1 = new_cov)
plot(predict_crr
     ,xlim=c(0,200)
     ,ylim = c(0,2))





library(rms)

fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Training,time.inc = 12)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12, m= 265, B=800)
par(mar=c(8,5,3,2),cex=1.0)
plot(call,lwd=2,lty=2,col=c('red')
               ,xlab = 'Nomogram-Predicted 12-month OS'
               ,ylab = 'Actual 12-month OS'
               ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black") 
dev.off()





fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Training,time.inc = 36)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12*3, m= 265, B=800)
par(mar=c(8,5,3,2),cex=1.0)
plot(call,lwd=2,lty=1,col=c('red'),xlab = 'Nomogram-Predicted 36-month OS'
                                  ,ylab = 'Actual 36-month OS'
                                  ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black")
dev.off()




fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Training,time.inc = 60)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12*5, m= 265, B=800)
par(mar=c(8,5,3,2),cex=1.0,main='x')
plot(call,lwd=2,lty=1,col=c('red')
     ,xlab = 'Nomogram-Predicted 60-month OS'
     ,ylab = 'Actual 60-month OS'
     ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black")



fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Validation,time.inc = 12)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12, m= 95, B=340)
par(mar=c(8,5,3,2),cex=1.0)
plot(call,lwd=2,lty=1,col=c('red')
     ,xlab = 'Nomogram-Predicted 12-month OS'
     ,ylab = 'Actual 12-month OS'
     ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black")



fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Validation,time.inc = 36)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12*3, m= 95, B=340)
par(mar=c(8,5,3,2),cex=1.0)
plot(call,lwd=2,lty=1,col=c('red')
     ,xlab = 'Nomogram-Predicted 36-month OS'
     ,ylab = 'Actual 36-month OS'
     ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black")



fit <- cph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,surv=T,x=T,y=T,data = Validation,time.inc = 60)
call <- calibrate(fit, cmethod="KM", method="boot", u=1*12*5, m= 95, B=340)
par(mar=c(8,5,3,2),cex=1.0)
plot(call,lwd=2,lty=1,col=c('red')
     ,xlab = 'Nomogram-Predicted 60-month OS'
     ,ylab = 'Actual 60-month OS'
     ,subtitles = F,xlim=c(0,1),ylim=c(0,1))
abline(0,1,lty = 2,lwd = 1.5,col = "black")  




library(cmprsk)
library(survival)
library(riskRegression)

dd <- datadist(seer_2)
options(datadist='dd')
fit <- cph(Surv(Survival.months,compete_riskstatus==1)~Grade.code+
             Size.Group+Surg.Group,surv=T,x=T,y=T,data = seer_2,time.inc = 36)
comprisk <- crr.fit(fit,cencode = 0,failcode = 1)




library(glmnet)
library(survival)
attach(seer_2)
detach(seer_2)
table(seer_2$Sex)
seer_2$Sex.code.num <- ifelse(seer_2$Sex=='Female',0,1)
table(seer_2$Sex.code)
table(seer_2$Race.code)
seer_2$Race.code.num <- ifelse(seer_2$Race.code=='Black',0,ifelse(seer_2$Race.code=='White',1,2))
table(seer_2$Race.code.num)
table(seer_2$Primary.Site)
table(seer_2$Histologic.Type.)
table(seer_2$Grade.code)
seer_2$Grade.code.num <- ifelse(seer_2$Grade.code=='I'
                                ,1,ifelse(seer_2$Grade.code=='II',2
                                ,ifelse(seer_2$Grade.code=='III',3
                                ,ifelse(seer_2$Grade.code=='IV',4,5))))
table(seer_2$Grade.code.num)
table(seer_2$T.stage)
seer_2$T.stage.num <- ifelse(seer_2$T.stage=='T1',1
                            ,ifelse(seer_2$T.stage=='T2',2
                            ,ifelse(seer_2$T.stage=='T3',3
                            ,ifelse(seer_2$T.stage=='T4',4,5))))
table(seer_2$T.stage.num)
table(seer_2$N.stage)
seer_2$N.stage.num <- ifelse(seer_2$N.stage=='N0',0
                             ,ifelse(seer_2$N.stage=='N1',1,2))
table(seer_2$N.stage.num)

table(seer_2$M.stage)
seer_2$M.stage.num <- ifelse(seer_2$M.stage=='M0',0,ifelse(seer_2$M.stage=='M1',1,2))
table(seer_2$M.stage.num)

table(seer_2$EOD.10...size..1988.2003.)
table(seer_2$CS.tumor.size..2004.2015.)
table(seer_2$Tumor.Size.Summary..2016..)

table(seer_2$RX.Summ..Surg.Prim.Site..1998..)
table(seer_2$Radiation.recode)
seer_2$Radiation.recode.num <- seer_2$Radiation.recode
seer_2$Radiation.recode.num <- as.factor(seer_2$Radiation.recode.num)
seer_2$Radiation.recode.num <- as.numeric(seer_2$Radiation.recode.num)
table(seer_2$Radiation.recode.num)

table(seer_2$Chemotherapy)
seer_2$Chemotherapy.num <- ifelse(seer_2$Chemotherapy=='No',0,1)
table(seer_2$Chemotherapy.num)



table(seer_2$Sequence.number)
seer_2$Sequence.number.num <- seer_2$Sequence.number
seer_2$Sequence.number.num <- as.factor(seer_2$Sequence.number.num)
seer_2$Sequence.number.num <- as.numeric(seer_2$Sequence.number.num)
table(seer_2$Sequence.number.num)

table(seer_2$Total.number.of.benign.borderline.tumors.for.patient)
table(seer_2$Total.number.of.in.situ.malignant.tumors.for.patient)

data_f <- data.frame(seer_2$age
                     ,seer_2$Sex.code.num
                     ,seer_2$Race.code.num
                     ,seer_2$Primary.Site
                     ,seer_2$Histologic.Type_num
                     ,seer_2$Grade.code.num
                     ,seer_2$Primary.Site.3F.num
                     ,seer_2$T.stage.num
                     ,seer_2$N.stage.num
                     ,seer_2$M.stage.num
                     ,seer_2$EOD.10...size..1988.2003.
                     ,seer_2$CS.tumor.size..2004.2015.
                     ,seer_2$Tumor.Size.Summary..2016..
                     ,seer_2$Size.Group.num
                     ,seer_2$Year.of.diagnosis
                     ,seer_2$Surg.bio.num
                     ,seer_2$RX.Summ..Surg.Prim.Site..1998..
                     ,seer_2$Radiation.recode.num
                     ,seer_2$Chemotherapy.num
                     ,seer_2$Sequence.number.num
                     ,seer_2$Total.number.of.benign.borderline.tumors.for.patient
                     ,seer_2$Total.number.of.in.situ.malignant.tumors.for.patient)
seer_2=subset(seer_2,seer_2$Survival.months!=0)
table(seer_2$EOD.10...size..1988.2003.)
library(survival)
x=data_f
x <- as.matrix(data_f)
typeof(x)
table(data$status)
class(data$status)
table(data$status_code)
class(data$status_code)
table(data$Survival.months)
data$Survival.months <- as.numeric(data$Survival.months)
table(data$time)
set.seed(20221107)
fit <- glmnet(x,Surv(seer_2$Survival.months,seer_2$status),family = 'cox')
plot(fit,xvar = 'lambda')
set.seed(20221107)
cv_fit= cv.glmnet(x,Surv(seer_2$Survival.months,seer_2$status),family='cox')
plot(cv_fit)
coef(cv_fit,s = 'lambda.min')
coef(cv_fit,s = 'lambda.1se')




setwd("D:/Rdata")
library(SurvRegCensCov)
library(survival)

AFT=survreg(Surv(seer_2$Survival.months,seer_2$status) ~age+Sex+T.stage+M.stage+Grade.code+Histologic.Type.+
              Size.Group+Surg.Group+Primary.Site.3F+
              Surg.Radio.Chemo,data =seer_2,dist = 'weibull')

regplot(AFT)
regplot(Cox)

library(ggDCA)
library(ggsci)
library(rmda)
library(rms)
library(foreign)
library(survival)
library(survminer)
detach(data)
seer_2$time <- seer_2$Survival.months/12
class(seer_2$status)
table(seer_2$time)


data$time=data$time/12
data$status=as.numeric(data$status)
qt=quantile(data$time,c(0.25,0.5,0.75))
qt=round(qt,2)



seer_2$Histology
cox1=coxph(Surv(time, status) ~T.stage+N.stage+M.stage,Validation)
cox2=coxph(Surv(time, status) ~age.10per+Sex+M.stage+
             Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
             Size.Group+Surg.Group+
             Surg.Radio.Chemo,data = Validation)
cox3 <- coxph(Surv(time,status) ~Sex+Surg.bio+Size.Group+M.stage+Grade.code+age+Histology,Validation)

d1=dca(cox1,model.names ='TNM',times=5)
ggplot(d1)
ggplot(d1,linetype = F,color = c('darkblue','red','orange','forestgreen'))



d2=dca(cox2,model.names ='Nomogram')
ggplot(d2)

d3=dca(cox3,model.names='Other')
ggplot(d3)

d = dca(cox1,cox2,model.names = c('TNM','Nomogram'))
ggplot(d)

d = dca(cox1,cox2,model.names = c('TNM','Nomogram'))
ggplot(d,linetype = F,color = c('darkblue','red','orange','forestgreen'))



d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=1)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))


d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=3)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))



d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=5)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))



d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=1)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))


d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=3)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))



d = dca(cox1,cox2,cox3,model.names = c('TNM','Nomogram','Other'),times=5)
ggplot(d,linetype = F,color = c('black','darkblue','red','orange','forestgreen'))



ggplot(d,linetype = F,color = c('#699FCC','#963830','#68709C','#894BA6'))







library(forcats)
library(survival)
library(magrittr)
library(dplyr)
library(ggsci)
library(ggplot2)

#Age sex HIstologic_type grade size T M surbio radiationbio chemobio
seer$age.10per

cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+T.stage+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer_2)
summary(cox.multi)
coef(cox.multi)
exp(coef(cox.multi))
CI95 <- exp(confint(cox.multi,level = 0.90))
CI95

a <- summary(cox.multi)
a$conf.int
cox_dta <- data.frame(a$conf.int)
cox_dta$ID <- rownames(cox_dta)


seer_2$age.10per
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='T.stageTX')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='M.stageMX')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Grade.codeUnknown')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Size.GroupUnknown')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Histologic.Type.9200')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Histologic.Type.9210')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Surg.GroupUnknown')

cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='T.stageT2')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='T.stageT3')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='T.stageT4')
table(cox_dta$ID)

table(cox_dta$ID)
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Histologic.Type.ICD.O.39200')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Histologic.Type.ICD.O.39210')
cox_dta <- subset(x = cox_dta,subset = cox_dta$ID!='Histologic.Type.ICD.O.39241')




cox_dta <- cox_dta[order(cox_dta$exp.coef.,decreasing = T),]
cox_dta$seq <- c('1','2','3','4','5','6','7','8','9','10')
cox_dta$ID <- paste(cox_dta$seq,cox_dta$ID,sep = '')
library(dplyr)
cox_dta <- select(.data = cox_dta,-c(ID))
cox_dta$color <- c('age','age','age','age','age','age','age'
                   ,'Sex'
                   ,'M.stage'
                   ,'Grade.code','Grade.code','Grade.code'
                   ,'Histologic.Type.','Histologic.Type.','Histologic.Type.'
                   ,'Histologic.Type.','Histologic.Type.'
                   ,'T.stage','T.stage','T.stage'
                   ,'Primary.Site.3F','Primary.Site.3F','Primary.Site.3F'
                   ,'Size.Group','Size.Group'
                   ,'Surg.Group','Surg.Group'
                   ,'Surg.Radio.Chemo','Surg.Radio.Chemo'
                   ,'Surg.Radio.Chemo','Surg.Radio.Chemo'
                   ,'Surg.Radio.Chemo','Surg.Radio.Chemo','Surg.Radio.Chemo')



pd <- position_dodge(.2)
cox_dta%>% 
  mutate(Factor=fct_reorder(ID,exp.coef.)) %>% 
  ggplot(aes(x=ID
             ,y=exp.coef.
             ,color=color))+
  scale_x_discrete(limits=rev(levels(cox_dta$ID)))+
  scale_y_continuous(breaks = c(0:10),limits = c(0,9))+
  geom_point(shape=15,size=4,position = pd)+
  geom_errorbar(aes(ymin=lower..95
                    ,ymax=upper..95)
                ,width=0.2
                ,size=1.3
                ,position = pd)+
  theme_classic()+
  theme(axis.title = element_text(face = 'bold'))+
  geom_hline(yintercept = 1,linetype='dashed',color='black')+
  ylab('Hazard Ratio')+scale_color_jco()+
  coord_flip()




cox_dta[7,1]=5

for (i in 1:nrow(cox_dta)) {
  if(cox_dta$upper..95[i]>10){
    cox_dta$upper..95[i]=8.6
  }
}

cox_dta[17,4]=9
cox_dta <- subset(cox_dta,subset = cox_dta$ID!='M.stageMX')
cox_dta <- subset(cox_dta,subset = cox_dta$ID!='Size.GroupUnknown')
cox_dta <- subset(cox_dta,subset = cox_dta$ID!='Grade.codeUnknown')
cox_dta <- subset(cox_dta,subset = cox_dta$ID!='T.stageTX')

seer_2$Surg.Group <- as.factor(seer_2$Surg.Group)
seer_2$Surg.Group <- relevel(seer_2$Surg.Group,ref = 'Unknown')
seer_2$Surg.Radio.Chemo <- as.factor(seer_2$Surg.Radio.Chemo)
seer_2$Surg.Radio.Chemo <- relevel(seer_2$Surg.Radio.Chemo,ref = 'Yes-No-No')
seer$T.stage <- as.factor(seer$T.stage)
seer$T.stage <- relevel(seer$T.stage,ref = 'T4')
seer$Surg.Radio.Chemo <- as.factor(seer$Surg.Radio.Chemo)
seer$Surg.Radio.Chemo <- relevel(seer,ref = '')
seer$Surg.Group <- as.factor(seer$Surg.Group)
seer$Surg.Group <- relevel(seer$Surg.Group,ref = 'Unknown')
m1$prog2 <- relevel(m1$prog,ref = "academic")

#
class(seer_2$Histologic.Type.)
table(seer_2$Histologic.Type.)
seer_2$Histologic.Type. <- as.character(seer_2$Histologic.Type.)
table(seer_2$Histologic.Type.)
class(seer_2$Histologic.Type.)
seer_2$Histologic.Type. <- as.factor(seer_2$Histologic.Type.)
m1$prog2 <- relevel(m1$prog,ref = "academic")
library(forcats)
seer_2$Histologic.Type. <- relevel(seer_2$Histologic.Type.,ref = '9220')

#age10
table(seer_2$age)
class(seer_2$age)
for (i in 1:nrow(seer_2)) {
  if(seer_2$age[i]<21){
    seer_2$age.10per[i]='0-20'
  }else if(seer_2$age[i]<31){
    seer_2$age.10per[i]='20-30'
  }else if(seer_2$age[i]<41){
    seer_2$age.10per[i]='30-40'
  }else if(seer_2$age[i]<51){
    seer_2$age.10per[i]='40-50'
  }else if(seer_2$age[i]<61){
    seer_2$age.10per[i]='50-60'
  }else if(seer_2$age[i]<71){
    seer_2$age.10per[i]='60-70'
  }else if(seer_2$age[i]<81){
    seer_2$age.10per[i]='70-80'
  }else{
    seer_2$age.10per[i]='80-100'
  }
}
table(seer_2$age.10per)








path<-file.path("D:\\Rdata","ff.xls")
library(openxlsx)
write.xlsx(cox_dta,file = path)




table(seer_2$ICD.O.3.Hist.behav)
seer2_45 <- subset(seer_2,subset = seer_2$age>45)#
cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer2_45)
summary(cox.multi)



seer_2_x45 <- subset(x = seer_2,subset =seer_2$age<47)
cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer_2_x45)
summary(cox.multi)


cox.multi <- coxph(Surv(Survival.months,status)~age,data =seer2_45)#age
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Sex,data =seer2_45)#sex
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Race.code,data =seer2_45)#race
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =seer2_45)#site
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~T.stage,data =seer2_45)#T.stage
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~N.stage,data =seer2_45)#N.stage
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~M.stage,data =seer2_45)#M.stage
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Size.Group,data =seer2_45)#size
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Grade.code,data =seer2_45)#grade
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =seer2_45)#Histologic type
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Surg.bio,data =seer2_45)#surgery
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Radio.bio,data =seer2_45)#Radiatherapy
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Chemotherapy,data =seer2_45)#Chemotherapy
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Surg.Group,data =seer2_45)#Surg.method
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =seer2_45)#Surg.Radia.Chemo
summary(cox.multi)









table(seer_2$Survival.months)
class(seer_2$Survival.months)
table(seer_2$status)
class(seer_2$status)
library(survivalROC)
library(survival)
cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Validation)

cox.multi <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =Training)
summary(cox.multi)




cox2 <- coxph(Surv(Survival.months,status)~Grade.code+Histologic.Type.,data = seer_2)

cox_predic_score=predict(cox)
cox2_predic_score=predict(cox2)


roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
roc_cox2=survivalROC(seer_2$Survival.months, seer_2$status, cox2_predic_score,  predict.time=12, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)),type='o', 
     ylab="TP",main="cox roc,1,3,5-YEAR prediction")
lines(roc_cox2$FP, roc_cox2$TP, type="l", xlim=c(0,1), ylim=c(0,1),   
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      col='red',
      ylab="TP",main="cox2 roc,1-YEAR prediction")
abline(0,1)


roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
roc_cox2=survivalROC(seer_2$Survival.months, seer_2$status, cox2_predic_score,  predict.time=36, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
     ylab="TP",main="cox roc,3-YEAR prediction")
lines(roc_cox2$FP, roc_cox2$TP, type="l", xlim=c(0,1), ylim=c(0,1),   
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      col='red',
      ylab="TP",main="cox2 roc,3-YEAR prediction")
abline(0,1)



roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
roc_cox2=survivalROC(seer_2$Survival.months, seer_2$status, cox2_predic_score,  predict.time=60, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
     ylab="TP",main="cox roc,5-YEAR prediction")
lines(roc_cox2$FP, roc_cox2$TP, type="l", xlim=c(0,1), ylim=c(0,1),   
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      col='red',
      ylab="TP",main="cox2 roc,5-YEAR prediction")
abline(0,1)



cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Validation Group cox roc,1,3,5-YEAR prediction")
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
roc_cox=survivalROC(Validation$Survival.months,Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")


cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,1-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                                 ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)


cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,3-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                           ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)




cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,5-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Training)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Training$Survival.months, Training$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                           ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)

cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,1-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=12, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                           ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)



cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,3-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=36, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                           ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)


cox <- coxph(Surv(Survival.months,status)~age.10per+Sex+M.stage+
               Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
               Size.Group+Surg.Group+
               Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
plot(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
     col='blue',
     xlab="FP", 
     ylab="TP",main="Multi vs Uni variable cox roc,5-YEAR prediction")
cox <- coxph(Surv(Survival.months,status)~age.10per,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='red',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Sex,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='green',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~M.stage,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='forestgreen',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Grade.code,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='orange',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Histologic.Type.,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='black',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Primary.Site.3F,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='yellow',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='brown',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Size.Group,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='salmon',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")

cox <- coxph(Surv(Survival.months,status)~Surg.Radio.Chemo,data =Validation)
cox_predic_score=predict(cox)
roc_cox=survivalROC(Validation$Survival.months, Validation$status, cox_predic_score,  predict.time=60, method="KM")
lines(roc_cox$FP, roc_cox$TP, type="l", xlim=c(0,1), ylim=c(0,1), 
      col='grey',
      xlab=paste( "FP", "\n", "AUC = ",round(roc_cox$AUC,3)), 
      ylab="TP")
legend(0.6,0.6, legend = c("Full_model","Age","Sex","M.stage"
                           ,"Grade","Histology","Site","Surg.group","Size","Surg.Raio.Chemo") ,
       col = c('blue','red','green','forestgreen','orange','black','yellow','brown','salmon','grey') 
       ,bty = "n", pch=20 , pt.cex = 3, cex = 1,
       horiz = FALSE, inset = c(0.03, 0.1))
abline(0,1)








library(survival)
library(prodlim)
library(riskRegression)
new_cov=data.frame(cbind(factor2ind(seer_2$age)
                         ,factor2ind(seer_2$Histology)
                         ,factor2ind(seer_2$Radio.bio)
                         ,factor2ind(seer_2$Chemotherapy)
                         ,factor2ind(seer_2$T.stage)
                         ,factor2ind(seer_2$N.stage)
                         ,factor2ind(seer_2$Grade.code)
                         ,factor2ind(seer_2$Size.Group)
                         ,factor2ind(seer_2$Primary.Site.3F)
                         ,factor2ind(seer_2$Surg.Group)))
comprisk <- crr(ftime = seer_2$Survival.months
                ,fstatus = seer_2$compete_riskstatus
                ,cov1 = new_cov
                ,failcode = 1
                ,cencode = 0)
comprisk[["converged"]]=T
summary(comprisk,conf.int = 0.95)


seer_2$age.10per
library(cmprsk)
library(survival)
library(riskRegression)
library(prodlim)
fgr1<-FGR(Hist(Survival.months,status)~age.10per+Sex+M.stage+
            Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
            Size.Group+Surg.Group+
            Surg.Radio.Chemo,data =seer2_45,cause = 1)
summary(fgr1)
fgr1[["converged"]]=T

seer2_45$compete_riskstatus

seer_2$Histology[seer_2$Histologic.Type.==9220]=9220
seer_2$Histology[seer_2$Histologic.Type.==9221]=9221
seer_2$Histology[seer_2$Histologic.Type.==9231]=9231
seer_2$Histology[seer_2$Histologic.Type.==9240]=9240
seer_2$Histology[seer_2$Histologic.Type.==9242]=9242
seer_2$Histology[seer_2$Histologic.Type.==9243]=9243

summary(fgr1)
fit1 <- Score(list('model1'=fgr1),
              formula=Hist(Survival.months,status)~1,
              data = seer_2 ,se.fit=1L,times=c(12,36,60),
              plots="ROC",
              metrics ="auc")

plotROC(fit1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        legend='',
        cex=1,
        auc.in.legend = T,  
        times = 12,col = 'skyblue',brier.in.legend = T)
#auc 88.4 [82.0-88.1]
plotROC(fit1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        legend="",
        cex=1,
        auc.in.legend = T, 
        times = 36,col = 'orange',add = T)
#auc 82.1 [79.1-85.1]
plotROC(fit1, 
        xlab="1-Specificity",
        ylab="Sensitivity",
        legend="",
        cex=1,
        auc.in.legend = T, 
        times = 60,col = 'black',add = T)
#auc 79.5 [76.5-82.6]






dd <- datadist(seer_2)
options(datadist='dd')
fit <- cph(Surv(Survival.months,compete_riskstatus==1)~age+
             Radio.bio+
             Chemotherapy+
             Grade.code+
             Size.Group+
             Primary.Site.3F+
             Surg.Group+
             Histology,surv=T,x=T,y=T,data = seer_2,time.inc = 12)
coxpe <- predict(fit)
coxpe <- predict(cox2)
c_index=1-rcorr.cens(coxpe,Surv(seer_2$Survival.months,seer_2$status))
c_index








comprisk <- crr.fit(fit,cencode = 0,failcode = 1)

seer_2$preds.tenf <- tenf.crr(comprisk,time = 12,fold = 10)
with(data = seer_2,cindex(preds.tenf,ftime = Survival.months,fstatus = compete_riskstatus,type = 'crr'))['cindex']


















cox.multi <- coxph(Surv(Survival.months,status)~age+Sex+T.stage+M.stage+Grade.code+Histologic.Type.+
                     Size.Group+Surg.Group+Primary.Site.3F+
                     Surg.Radio.Chemo,data =seer_2)





seer_2$Primary.Site

library(regplot)
AFT=survreg(Surv(seer_2$Survival.months,seer_2$status) ~age+Sex+Race.code.num+T.stage+
              M.stage+Grade.code+Histologic.Type.+
              Size.Group+Surg.Group+Primary.Site.3F+Surg.bio+Radio.bio+Chemotherapy+
              Surg.Radio.Chemo,data =seer_2,dist = 'weibull')

regplot(AFT)
regplot(Cox)
AFT_predic_score=predict(AFT)
summary(AFT)


library(MASS)
stepAIC(AFT,direction = 'both')

summary(AFT)




setwd("D:/Rdata")
library(givitiR)
library(ResourceSelection)
library(rcompanion)
library(rms)
library(VGAM)

model=glm(status~age.10per+Sex+M.stage+
            Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
            Size.Group+Surg.Group+
            Surg.Radio.Chemo,
          family = binomial(link="logit"),
          data=seer_2)
seer_2$prob <- model$fitted.values
cali <- givitiCalibrationBelt(o = seer_2$status, e = seer_2$prob,
                              devel = "internal")
plot(cali, main = " GiViTI Calibration Belt",
     xlab = "predicted probability",
     ylab = "Observed probability")

library(e1071)

age.10per <- Training$age.10per
Sex <- Training$Sex
M.stage <- Training$M.stage
Grade.code <- Training$Grade.code
Histologic.Type.ICD.O.3 <- Training$Histologic.Type.
Primary.Site.3F <- Training$Primary.Site.3F
Size.Group <- Training$Size.Group
Surg.Group <- Training$Size.Group
Surg.Radio.Chemo <- Training$Surg.Radio.Chemo
status <- Training$status
ddist <- datadist(age.10per,Sex,M.stage,Grade.code,Histologic.Type.ICD.O.3,Primary.Site.3F,Size.Group,Surg.Group
                  ,Surg.Radio.Chemo)
options(datadist='ddist')

data_f <- data.frame(seer_2$age
                     ,seer_2$Sex.code.num
                     ,seer_2$M.stage.num
                     ,seer_2$Grade.code.num
                     ,seer_2$Histologic.Type_num
                     ,seer_2$Primary.Site.3F.num
                     ,seer_2$Size.Group.num
                     ,seer_2$Surg.Group.num
                     ,seer_2$Surg.Radio.Chemo_num
                     ,seer_2$Diag.Group
                     ,seer_2$status)
data_f <- data.frame(seer_2$age
                     ,seer_2$Sex
                     ,seer_2$M.stage
                     ,seer_2$Grade.code
                     ,seer_2$Histologic.Type.
                     ,seer_2$Primary.Site.3F
                     ,seer_2$Size.Group
                     ,seer_2$Surg.Group
                     ,seer_2$Surg.Radio.Chemo
                     ,seer_2$Diag.Group
                     ,seer_2$Survival.months
                     ,seer_2$status)
Training <- subset(x = data_f,subset = data_f$seer_2.Diag.Group=='Training')
Validation <- subset(x = data_f,subset = data_f$seer_2.Diag.Group=='Validation')
library(dplyr)
Training <- select(.data = Training,-c(seer_2.Diag.Group))
Validation <- select(.data = Validation,-c(seer_2.Diag.Group))
Training$seer_2.status

SVM=svm(seer_2.status~.,data = Training
        ,type='C-classification'
        ,kernel='sigmoid')

pre_ran <- predict(SVM,newdata = Validation)
obs_p_ran <- data.frame(prob=pre_ran,obs=Validation$seer_2.status)



library(pROC)
library(ROCR)
ran_roc <- roc(Validation$seer_2.status,as.numeric(pre_ran))
plot(ran_roc,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),grid.col=c('green','blue'),max.auc.polygon=T
     ,auc.polygon.col="skyblue",print.thres=T,main='Support Vector Machine')





model=glm(status~age.10per+Sex+M.stage+
            Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
            Size.Group+Surg.Group+
            Surg.Radio.Chemo,
          family = binomial(link="logit"),
          data=seer_2)
seer_2$prob <- model$fitted.values

class(obs_p_ran$obs)
obs_p_ran$obs <- as.character(obs_p_ran$obs)
obs_p_ran$obs <- as.numeric(obs_p_ran$obs)
class(obs_p_ran$prob)
obs_p_ran$prob <- as.character(obs_p_ran$prob)
obs_p_ran$prob <- as.numeric(obs_p_ran$prob)
class(obs_p_ran$prob)


cali <- givitiCalibrationBelt(o = obs_p_ran$obs, e = obs_p_ran$prob,devel = "internal")
plot(cali, main = " GiViTI Calibration Belt",
     xlab = "predicted probability",
     ylab = "Observed probability")



table(data$meta)
class(data$meta)
table(data$Derived.AJCC.M..6th.ed..2004.2015.)
table(data$T)
class(data$T)
table(data$N)
class(data$N)

data_f <- data.frame(data$Regional.nodes.examined..1988..
                     ,data$Regional.nodes.positive..1988..
                     ,data$T
                     ,data$N
                     ,data$Total.number.of.in.situ.malignant.tumors.for.patient
                     ,data$RX.Summ..Surg.Prim.Site..1998..
                     ,data$CS.tumor.size..2004.2015.
                     ,data$meta)

n=nrow(data_f)
set.seed(20221027)
trainindex <- sample(1:n,size = 0.7*n)
trainset <- data_f[trainindex,]
testset <- data_f[-trainindex,]

library(e1071)
nb.model <- naiveBayes(Training$seer_2.status~.,data = Training)
nb_predict <- predict(nb.model,newdata = Validation)
class(testset$data.meta)
table(testset$data.meta)


nb.table <- table(actual=testset$data.meta,precdict=nb_predict)
nb.table
nb_ratio <- sum(diag(nb.table))/sum(nb.table)
nb_ratio


library(ROCR)
library(pROC)
ran_roc <- roc(Validation$seer_2.status,as.numeric(nb_predict))
plot(ran_roc,print.auc=T,auc.polygon=T,grid=c(0.1,0.2),grid.col=c('green','red'),max.auc.polygon=T
     ,auc.polygon.col='skyblue',print.thres=T,main='Bayesian Model')













setwd("D:\\Rdata")
ext.validation <- read.table('external.validation.txt'
                   ,sep = '\t'
                   ,header = T)
ext.validation$M.stage <- 'M0'
ext.validation$M.stage[ext.validation$Patient.ID==19041819]='M1'
table(ext.validation$M.stage)
ext.validation$Surg.Radio.Chemo <- paste(ext.validation$Surge.method
                                         ,ext.validation$Radiation,ext.validation$Chemotherapy)
table(ext.validation$Surg.Radio.Chemo)
library(dplyr)
ext.validation$Surg.Radio.Chemo[ext.validation$Surg.Radio.Chemo=='Resection no no']='Surgery no no'
ext.validation$Surg.Radio.Chemo[ext.validation$Surg.Radio.Chemo=='surgery no no']='Surgery no no'
ext.validation$Surg.Radio.Chemo <- as.factor(ext.validation$Surg.Radio.Chemo)
ext.validation$Surg.Radio.Chemo <- relevel(x = ext.validation$Surg.Radio.Chemo,ref = 'Surgery no no')

ext.validation <- subset(x = ext.validation,subset = ext.validation$Age>46)

ext.validation[2,11]=30
ext.validation[2,12]=1

ext.validation[6,11]=60
ext.validation[6,12]=1

ext.validation[14,11]=98
ext.validation[14,12]=1

ext.validation[3,11]=95
ext.validation[3,12]=1

ext.validation_F <- ext.validation
cox.multi <- coxph(Surv(Survival.moths,Status)~Age+Sex+
                     Histologic.type+Size+Surg.Radio.Chemo,data =ext.validation_F)
summary(cox.multi)

cox.multi <- coxph(Surv(Survival.moths,Status)~Age+Sex,data =ext.validation)
summary(cox.multi)


path<-file.path("D:\\Rdata","ff.xls")
library(openxlsx)
write.xlsx(ext.validation,file = path)










library(ggplot2)



library(grid)
library(gridExtra)
p1 <- qplot(1:5,1:5)
p4 <- ggplot(mtcars,aes(x=mpg,y=wt))+geom_point()
p3 <- rectGrob(gp=gpar(fill='pink'))
p2 <- textGrob('Basic Usage')
p<- grid.arrange(p1,p2,p3,p4,ncol=2)
gs <- lapply(1:8,function(ii)
  grobTree(rectGrob(gp=gpar(fill=ii,alpha=0.5)),textGrob(ii)))



layout <- rbind(c(1,1,1,2,3),
                c(1,1,1,4,5),
                c(6,7,7,8,8))
grid.arrange(grobs=gs,layout_matrix=layout)



tiff('D:\\Rdata\\Article_Image\\123456789.tiff',width = 2800,height = 2500,res = 300)
p <- plot(call,lwd=2,lty=1,col=c('red'),xlab = '1 year predictive probabillity of survival'
          ,ylab = '1 year actual probabillity of survival')
p
dev.off()










library(survival)
table(seer_1$Sequence.number)
colnames(seer_1)
cox.multi <- coxph(Surv(Survival.months,status)~age+Sex+M.stage+
                     Grade.code+Histologic.Type.ICD.O.3+Primary.Site.3F+
                     Size.Group+Surg.Group+
                     Surg.Radio.Chemo,data =seer_1)
summary(cox.multi)

for (i in 1:nrow(seer_1)) {
  if(seer_1$age[i]<21){
    seer_1$age.10per[i]='0-20'
  }else if(seer_1$age[i]<31){
    seer_1$age.10per[i]='20-30'
  }else if(seer_1$age[i]<41){
    seer_1$age.10per[i]='30-40'
  }else if(seer_1$age[i]<51){
    seer_1$age.10per[i]='40-50'
  }else if(seer_1$age[i]<61){
    seer_1$age.10per[i]='50-60'
  }else if(seer_1$age[i]<71){
    seer_1$age.10per[i]='60-70'
  }else if(seer_1$age[i]<81){
    seer_1$age.10per[i]='70-80'
  }else{
    seer_1$age.10per[i]='80-100'
  }
}
table(seer_1$age.10per)
table(seer_1$M.stage)

seer_1$Primary.Site.3F <- 'Other'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C14.0-Pharynx, NOS']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C30.0-Nasal cavity']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C32.0-Glottis']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C32.1-Supraglottis']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C32.2-Subglottis']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C32.3-Laryngeal cartilage']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C32.9-Larynx, NOS']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C33.9-Trachea']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C34.0-Main bronchus']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C40.9-Bone of limb, NOS']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.0-Bones of skull and face and associated joints']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.3-Rib, sternum, clavicle and associated joints']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C49.0-Conn, subcutaneous, other soft tis: head, face, neck']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C49.1-Conn, subcutaneous, other soft tis: upr limb, shoulder']='hyaline cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C49.2-Conn, subcutaneous, other soft tis: lower limb, hip']='hyaline cartilage'

seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.1-Mandible']='elastic cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.8-Overlap bones, joints, and art. cartilage']='elastic cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.9-Bone, NOS']='elastic cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C49.3-Conn, subcutaneous, other soft tis: thorax']='elastic cartilage'

seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C40.0-Long bones: upper limb, scapula, and associated joints']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C40.1-Short bones of upper limb and associated joints']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C40.2-Long bones of lower limb and associated joints']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C40.3-Short bones of lower limb and associated joints']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.2-Vertebral column']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C41.4-Pelvic bones, sacrum, coccyx and associated joints']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C49.5-Conn, subcutaneous, other soft tis: pelvis']='fibrous cartilage'
seer_1$Primary.Site.3F[seer_1$Primary.Site...labeled=='C72.0-Spinal cord']='fibrous cartilage'







save.image()
savehistory()
save(seer,data,seer_1,seer_2,seer_21,seer2_45,Training,Validation,ext.validation
     ,file = 'seer2.Rdata')



