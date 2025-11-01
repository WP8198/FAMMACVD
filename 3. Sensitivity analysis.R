#Main analysis####
#1. The associations of RBC folate and MMA with CVD mortality during the two included durations (within 10 years and >10 years)
#2. Multiplicative interaction analysis was used to verify the interaction between RBC folate and MMA
#3. Excluded patients who died within 1 years to mitigate potential reverse causation bias
#4. The interaction effect of RBC folate and MMA on CVD mortality was analyzed in different gender populations
#5. spline models: interaction effect of RBC folate (as a continuous variable) and MMA on CVD mortality across different gender populations




sensitivity <- ALL_DATA

###1.KM ≤ 10years or > 10years####
####1.1.RBCfolate####
sensitivity$RBCfolate_category <- as.factor(sensitivity$RBCfolate_category)  
sensitivity$RBCfolate_category <- relevel(sensitivity$RBCfolate_category, ref ='Lower RBCfolate')

sensitivity_design <- svydesign(data = sensitivity,
                                ids = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                nest = TRUE,
                                weights = ~ WTMEC2YR,
                                survey.lonely.psu="adjust")

RBCfolate_KM_10 <- svykm(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category, design = sensitivity_design)
svyjskm(RBCfolate_KM_10)

KM_RBCfolate_10 <- svyjskm(RBCfolate_KM_10,xlabs = "Time(years)",ylabs = "Survival probability",main = "CVD mortality",
                           ystrataname = "RBC folate groups",ystratalabs=c("Lower RBC folate","Higher RBC folate"),
                           xlims=c(0,20), ylim = c(0.9, 1),timeby=4,pval=T,pval.coord=c(15,0.2),
                           legendposition=c(0.2,0.2),linecols="Set1",table=T,
                           label.nrisk="Numbers at risk",size.label.nrisk=12,cut.landmark=10)
KM_RBCfolate_10
ggsave("KM-RBCfolate-10-CVD.pdf",width = 5,height = 4)


####1.2.MMA####
sensitivity$MMA_category <- as.factor(sensitivity$MMA_category)  
sensitivity$MMA_category <- relevel(sensitivity$MMA_category, ref ='Lower MMA')

sensitivity_design <- svydesign(data = sensitivity,
                                ids = ~SDMVPSU,
                                strata = ~SDMVSTRA,
                                nest = TRUE,
                                weights = ~ WTMEC2YR,
                                survey.lonely.psu="adjust")

MMA_KM_10 <- svykm(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category, design = sensitivity_design)
svyjskm(MMA_KM_10)

KM_MMA_10 <- svyjskm(MMA_KM_10,xlabs = "Time(years)",ylabs = "Survival probability",main = "CVD mortality",
                     ystrataname = "MMA groups",ystratalabs=c("Lower MMA","Higher MMA"),
                     xlims=c(0,20), ylim = c(0.9, 1),timeby=4,pval=T,pval.coord=c(15,0.2),
                     legendposition=c(0.2,0.2),linecols="Set1",table=T,
                     label.nrisk="Numbers at risk",size.label.nrisk=12,cut.landmark=10)
KM_MMA_10
ggsave("KM-MMA-10-CVD.pdf",width = 5,height = 4)


###2.Multiplicative interaction ####
interaction_CVD_2 <- ALL_DATA

interaction_CVD_2$RBCfolate_category <- ifelse(interaction_CVD_2$RBCfolate_category == "Lower RBCfolate","1","2")

interaction_CVD_2$MMA_category <- ifelse(interaction_CVD_2$MMA_category == "Lower MMA","1","2")

interaction_CVD_2_design <- svydesign(data = interaction_CVD_2,
                                      ids = ~SDMVPSU,
                                      strata = ~SDMVSTRA,
                                      nest = TRUE,
                                      weights = ~ WTMEC2YR,
                                      survey.lonely.psu="adjust") 
summary(interaction_CVD_2_design)


concat = function(lst) {
  output = ""
  for (var in rev(lst)) {
    output = paste(var, "+", output, sep = " ")
  }
  substr(output, 1, nchar(output) - 3)
}
my_cox = function(dat, t, sts, exp, covr) {
  svycoxph(as.formula(paste("Surv(", t, ", ", sts, ")", " ~", exp, "+", concat(covr), sep = " ")), design  = dat)
}

var_exposure_factor <- c("RBCfolate_category")

var_blood <- c("MMA_category")

var_blood_temp <- c("MMA_category")

var_outcome <- c("MORTSTAT_CVD")

covr <-c("Age","Sex","Race","Education","PIR","Glycohemoglobin","Fasting_Glucose",
         "BMXBMI","Smoking_status","Drinking_status","Physical","HEI2015","BPXSY",
         "Food_fast","SEVB12","DRVB12")

Data <- interaction_CVD_2_design
data <- interaction_CVD_2

inter_all_2_CVD <- data.frame()
for (h in 1:length(var_exposure_factor)) {
  for (k in 1:length(var_outcome)) {
    for(j in 1:length(var_blood)){
      for(i in 1:length(table(data[,var_blood[j]]))){
        
        covr_here=covr[which(covr!=var_blood_temp[j])]
        
        data1 <- eval(parse(text=paste0("subset(Data,",var_blood[j],"==",i,")")))
        data2 <- eval(parse(text=paste0("subset(Data,",var_blood[j],"==",i,")")))
        #data_fw <- eval(parse(text=paste0("subset(Individual_nhanes5_dvalue_temp3,",var_blood[j],"==",i,")")))
        ## inter
        model_inter=eval(parse(text=paste0("my_cox(Data,","'PERMTH_Y',","'",var_outcome[k],"'",",","'",var_exposure_factor[h],"*",var_blood[j],"'",",covr_here)")))
        m <- summary(model_inter)
        p_inter <-sprintf("%0.3f", m$coefficients[dim(m$coefficients)[1],6]) #Extract the interaction P-value
        ## stratification results
        # cox 
        model1=eval(parse(text=paste0("my_cox(data1,","'PERMTH_Y',","'",var_outcome[k],"'",",","'factor(",var_exposure_factor[h],")'",",covr_here)")))
        
        m1 <- summary(model1)
        hr <- sprintf("%0.2f", m1$coefficients[1:(length(table(data[,var_exposure_factor[h]]))-1),2])#Take two decimal places and convert them into numeric format
        hr_l <-sprintf("%0.2f", m1$conf.int[1:(length(table(data[,var_exposure_factor[h]]))-1),3])
        hr_u <-sprintf("%0.2f", m1$conf.int[1:(length(table(data[,var_exposure_factor[h]]))-1),4])
        HR <- paste0(hr," (",hr_l,", ",hr_u,")")
        #c_value <-length(table(data1$variables[,var_exposure_factor[h]]))
        ## p trend
        model2=eval(parse(text=paste0("my_cox(data2,","'PERMTH_Y',","'",var_outcome[k],"'",",","'",var_exposure_factor[h],"'",",covr_here)")))
        m2 <- summary(model2)
        p_trend <- sprintf("%0.3f", m2$coefficients[1,6])
        ## case number
        num_participants <- dim(data2)[1]
        num_event <-table(data2$variables[,var_outcome[k]])[2]#Calculate the number of patients
        num <- paste0(num_event,"/",num_participants)
        #MSD <- paste0(round(mean(temp_vb12$blood_vb12),1),"±",round(sd(temp_vb12$blood_vb12),1))
        ## merge
        if (h==1) {
          re <- data.frame(var_exposure_factor[h],paste0(var_blood[j],"==",i),var_outcome[k],num,"1.00 (Reference)",t(HR),p_trend,p_inter)
        } else {
          re <- data.frame(var_exposure_factor[h],paste0(var_blood[j],"==",i),var_outcome[k],num,"1.00 (Reference)",t(HR),"",p_trend,p_inter)
        }
        names(re) <- c( "var_exposure_factor","var_blood", "var_outcome","Case/N","1.00 (Reference)","Q1","p_trend","p_inter")
        inter_all_2_CVD <- rbind(inter_all_2_CVD,re) #Result summary
      }
    }
  }
}

write.csv(inter_all_2_CVD,"inter_all_2_CVD.csv")

###3.Cox-Follow up on those who died within one year####
sensitivity_1death <-  ALL_DATA

#Screen the deceased within a year
sensitivity_1death$death<-ifelse(sensitivity_1death$PERMTH_Y<=1&sensitivity_1death$MORTSTAT_CVD==1,1,0)

#Exclude personnel                   
sensitivity_1death <- sensitivity_1death %>%  filter(sensitivity_1death$death < 1)

sensitivity_1death$RBCfolate <- log(sensitivity_1death$RBCfolate,base = 2)
sensitivity_1death$MMA <- log(sensitivity_1death$MMA,base = 2)

sensitivity_1death$RBCfolate_category <- as.factor(sensitivity_1death$RBCfolate_category)  
sensitivity_1death$RBCfolate_category <- relevel(sensitivity_1death$RBCfolate_category, ref ='Lower RBCfolate')

sensitivity_1death$MMA_category <- as.factor(sensitivity_1death$MMA_category)  
sensitivity_1death$MMA_category <- relevel(sensitivity_1death$MMA_category, ref ='Lower MMA')

sensitivity_1death_design <- svydesign(data = sensitivity_1death,
                                       ids = ~SDMVPSU,
                                       strata = ~SDMVSTRA,
                                       nest = TRUE,
                                       weights = ~ WTMEC2YR,
                                       survey.lonely.psu="adjust")

####3.1.RBCfolate####
# Model 1:
# Classification
all_1death_cox1 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category, data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox1.docx')

# Continuous
all_1death_cox2 <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ RBCfolate, data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox2.docx')

# Model 2:
# Classification
all_1death_cox3 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category + Age + Sex +
                              Race + Education + PIR+ BMXBMI + Smoking_status + 
                              Drinking_status + Physical,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox3.docx')

# Continuous
all_1death_cox4 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate + Age + Sex +
                              Race + Education + PIR+ BMXBMI + Smoking_status + 
                              Drinking_status + Physical,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox4.docx')

# Model 3:
# Classification
all_1death_cox5 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category+Age+Sex+Race+
                              Education+PIR+Glycohemoglobin+Fasting_Glucose+
                              BMXBMI+Smoking_status+Drinking_status+Physical+
                              HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox5.docx')

# Continuous
all_1death_cox6 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate +Age+Sex+Race+
                              Education+PIR+Glycohemoglobin+Fasting_Glucose+
                              BMXBMI+Smoking_status+Drinking_status+Physical+
                              HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox6.docx')


####3.2.MMA####
# Model 1:
# Classification
all_1death_cox1 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category, data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox1.docx')

# Continuous
all_1death_cox2 <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ MMA, data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox2.docx')

# Model 2:
# Classification
all_1death_cox3 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category + Age + Sex +
                              Race + Education + PIR+ BMXBMI + Smoking_status + 
                              Drinking_status + Physical,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox3.docx')

# Continuous
all_1death_cox4 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA + Age + Sex +
                              Race + Education + PIR+ BMXBMI + Smoking_status + 
                              Drinking_status + Physical,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox4.docx')

# Model 3:
# Classification
all_1death_cox5 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category+Age+Sex+Race+
                              Education+PIR+Glycohemoglobin+Fasting_Glucose+
                              BMXBMI+Smoking_status+Drinking_status+Physical+
                              HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox5.docx')

# Continuous
all_1death_cox6 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA +Age+Sex+Race+
                              Education+PIR+Glycohemoglobin+Fasting_Glucose+
                              BMXBMI+Smoking_status+Drinking_status+Physical+
                              HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                            data = sensitivity_1death, design = sensitivity_1death_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'all_1death_cox6.docx')


###4.Multiplicative interactive spline plot in different gender populations####
####4.1.man####
spline_CVD_SEX  <- ALL_DATA
spline_CVD_SEX$Sex <- ifelse(spline_CVD_SEX$Sex == "Male",1,
                             ifelse(spline_CVD_SEX$Sex == "Female",2,NA))

spline_man_CVD <- spline_CVD_SEX %>%  filter(spline_CVD_SEX$Sex == 1)  
dim(spline_man_CVD) #  11134

spline_man_CVD$MMA_3 <- ntile(spline_man_CVD$MMA,3)

spline_man_CVD$RBCfolate <- log(spline_man_CVD$RBCfolate,base = 2)

spline.man <- coxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate*MMA_3+ Age+Sex+Race+
                      Education+PIR+Glycohemoglobin+Fasting_Glucose+
                      BMXBMI+Smoking_status+Drinking_status+Physical+
                      HEI2015+BPXSY+Food_fast+SEVB12+DRVB12, spline_man_CVD)

plot(visreg(spline.man,xvar = "RBCfolate",by="MMA_3",plot=F),xlab="RBC folate (nmol/L)",ylab="Hazard ratio (95%CI)",overlay = T,partial = F,rug=F)

legend("topleft",c("Low MMA","Middle MMA","High MMA"), lty=c(1,1), col=c("red","green","blue"), lwd=c(1,1),bty="n")


####4.2.weman####
spline_CVD_SEX <- ALL_DATA
spline_CVD_SEX$Sex <- ifelse(spline_CVD_SEX$Sex == "Male",1,
                             ifelse(spline_CVD_SEX$Sex == "Female",2,NA))

spline_weman_CVD <- spline_CVD_SEX %>%  filter(spline_CVD_SEX$Sex == 2)  
dim(spline_weman_CVD) #  11353

spline_weman_CVD$MMA_3 <- ntile(spline_weman_CVD$MMA,3)

spline_weman_CVD$RBCfolate <- log(spline_weman_CVD$RBCfolate,base = 2)

spline.weman <- coxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate*MMA_3+ Age+Sex+Race+
                        Education+PIR+Glycohemoglobin+Fasting_Glucose+
                        BMXBMI+Smoking_status+Drinking_status+Physical+
                        HEI2015+BPXSY+Food_fast+SEVB12+DRVB12, spline_weman_CVD)

plot(visreg(spline.weman,xvar = "RBCfolate",by="MMA_3",plot=F),xlab="RBC folate (nmol/L)",ylab="Hazard ratio (95%CI)",overlay = T,partial = F,rug=F)


###5.Multiplicative interactive RBC folate (as a continuous variable)####
final <- ALL_DATA

final$MMA_3 <- ntile(final$MMA,3)

final$RBCfolate <- log(final$RBCfolate,base = 2)

spline.all <- coxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate*MMA_3+ Age+Sex+Race+
                      Education+PIR+Glycohemoglobin+Fasting_Glucose+
                      BMXBMI+Smoking_status+Drinking_status+Physical+
                      HEI2015+BPXSY+Food_fast+SEVB12+DRVB12, final)

plot(visreg(spline.all,xvar = "RBCfolate",by="MMA_3",plot=F),xlab="RBC folate (nmol/L)",ylab="Hazard ratio (95%CI)",overlay = T,partial = F,rug=F)

legend("topleft",c("Low MMA","Middle MMA","High MMA"), lty=c(1,1), col=c("red","green","blue"), lwd=c(1,1),bty="n")
