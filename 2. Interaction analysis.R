#Main analysis####
#1. Weighted Kaplan-Meier curves     
#2. survey-weighted Cox proportional hazards (CPH) models   
#3. Restricted cubic splines （RCS）  
#4. Additive interaction    




##1.KM curve####
final <- ALL_DATA

final$RBCfolate <- log(final$RBCfolate,base = 2)
final$MMA <- log(final$MMA,base = 2)

final$RBCfolate_category <- as.factor(final$RBCfolate_category)  
final$RBCfolate_category <- relevel(final$RBCfolate_category, ref ='Lower RBCfolate')

final$MMA_category <- as.factor(final$MMA_category)  
final$MMA_category <- relevel(final$MMA_category, ref ='Lower MMA')

final$RBCfolate_MMA <- as.factor(final$RBCfolate_MMA)  
final$RBCfolate_MMA <- relevel(final$RBCfolate_MMA, ref ='Lower RBCfolate & Lower MMA')

final_design <- svydesign(data = final,
                          ids = ~SDMVPSU,
                          strata = ~SDMVSTRA,
                          nest = TRUE,
                          weights = ~ WTMEC2YR,
                          survey.lonely.psu="adjust") 
summary(final_design)

###1.1.folate####
RBCfolate_KM <- svykm(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category, design = final_design)
svyjskm(RBCfolate_KM)

KM_RBCfolate <- svyjskm(RBCfolate_KM,xlabs = "Time(years)",ylabs = "Survival probability",main = "CVD mortality",
                        ystrataname = "RBCfolate groups",ystratalabs=c("Lower RBCfolate","Higher RBCfolate"),
                        xlims=c(0,18), ylim = c(0.6, 1),timeby=3,pval=T,pval.coord=c(3,0.9),
                        legendposition=c(0.9,0.97),linecols="Set1",table=T,
                        label.nrisk="Numbers at risk",size.label.nrisk=11)
KM_RBCfolate
ggsave("KM_ALL_CVD_RBCfolate.pdf",width = 6,height = 5)


###1.2.MMA####
MMA_KM <- svykm(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category, design = final_design)
svyjskm(MMA_KM)

KM_MMA <- svyjskm(MMA_KM,xlabs = "Time(years)",ylabs = "Survival probability",main = "CVD mortality",
                  ystrataname = "MMA groups",ystratalabs=c("Lower MMA","Higher MMA"),
                  xlims=c(0,18), ylim = c(0.6, 1),timeby=3,pval=T,pval.coord=c(3,0.9),
                  legendposition=c(0.9,0.97),linecols="Set1",table=T,
                  label.nrisk="Numbers at risk",size.label.nrisk=11)
KM_MMA
ggsave("KM_ALL_CVD_MMA.pdf",width = 6,height = 5)


###1.3.RBCfolate_MMA ####
RBCfolate_MMA_KM <- svykm(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_MMA, design = final_design)
svyjskm(RBCfolate_MMA_KM)

KM_RBCfolate_MMA <- svyjskm(RBCfolate_MMA_KM,xlabs = "Time(years)",ylabs = "Survival probability",main = "CVD mortality",
                            ystrataname = "RBCfolate & MMA groups",
                            ystratalabs=c("Lower RBCfolate & Lower MMA","Higher RBCfolate & Higher MMA","Higher RBCfolate & Lower MMA","Lower RBCfolate & Higher MMA"),
                            xlims=c(0,18), ylim = c(0.5, 1),timeby=3,pval=T,pval.coord=c(3,0.9),
                            legendposition=c(0.9,0.97),linecols="Set1",table=T,
                            label.nrisk="Numbers at risk",size.label.nrisk=11)
KM_RBCfolate_MMA
ggsave("KM_RBCfolate_MMA.pdf",width = 8,height = 5)


##2.COX ####
final <- ALL_DATA

final$RBCfolate <- log(final$RBCfolate,base = 2)
final$MMA <- log(final$MMA,base = 2)

final$RBCfolate_category <- as.factor(final$RBCfolate_category)  
final$RBCfolate_category <- relevel(final$RBCfolate_category, ref ='Lower RBCfolate')

final$MMA_category <- as.factor(final$MMA_category)  
final$MMA_category <- relevel(final$MMA_category, ref ='Lower MMA')

final$RBCfolate_MMA <- as.factor(final$RBCfolate_MMA)  
final$RBCfolate_MMA <- relevel(final$RBCfolate_MMA, ref ='Lower RBCfolate & Lower MMA')

final_design <- svydesign(data = final,
                          ids = ~SDMVPSU,
                          strata = ~SDMVSTRA,
                          nest = TRUE,
                          weights = ~ WTMEC2YR,
                          survey.lonely.psu="adjust") 
summary(final_design)

###2.1.RBCfolate####
# Model 1:
# Classification
CVD_RBCfolate_cox1 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category, data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox1.docx')

# Continuous
CVD_RBCfolate_cox2 <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ RBCfolate, data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox2.docx')

# Model 2:
# Classification
CVD_RBCfolate_cox3 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category + Age + Sex +
                                 Race + Education + PIR+ BMXBMI + Smoking_status + 
                                 Drinking_status + Physical,
                               data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox3.docx')

# Continuous
CVD_RBCfolate_cox4 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate + Age + Sex +
                                 Race + Education + PIR+ BMXBMI + Smoking_status + 
                                 Drinking_status + Physical,
                               data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox4.docx')

# Model 3:
# Classification
CVD_RBCfolate_cox5 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_category+Age+Sex+Race+
                                 Education+PIR+Glycohemoglobin+Fasting_Glucose+
                                 BMXBMI+Smoking_status+Drinking_status+Physical+
                                 HEI2015+BPXSY+Food_fast+SEVB12+DRVB12,
                               data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox5.docx')

# Continuous
CVD_RBCfolate_cox6 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate+Age+Sex+Race+
                                 Education+PIR+Glycohemoglobin+Fasting_Glucose+
                                 BMXBMI+Smoking_status+Drinking_status+Physical+
                                 HEI2015+BPXSY+Food_fast+SEVB12+DRVB12,
                               data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_cox6.docx')


###2.2.MMA####
# Model 1:
# Classification
CVD_MMA_cox1 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category, data = final, design = final_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox1.docx')

# Continuous
CVD_MMA_cox2 <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ MMA, data = final, design = final_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox2.docx')

# Model 2:
# Classification
CVD_MMA_cox3 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category + Age + Sex +
                           Race + Education + PIR+ BMXBMI + Smoking_status + 
                           Drinking_status + Physical,
                         data = final, design = final_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox3.docx')

# Continuous
CVD_MMA_cox4 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA + Age + Sex +
                           Race + Education + PIR+ BMXBMI + Smoking_status + 
                           Drinking_status + Physical,
                         data = final, design = final_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox4.docx')

# Model 3:
# Classification
CVD_MMA_cox5 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category+Age+Sex+Race+
                           Education+PIR+Glycohemoglobin+Fasting_Glucose+
                           BMXBMI+Smoking_status+Drinking_status+Physical+
                           HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                         data = final, design = final_design)%>%
  tbl_regression(include=c('MMA_category'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox5.docx')

# Continuous
CVD_MMA_cox6 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA+Age+Sex+Race+
                           Education+PIR+Glycohemoglobin+Fasting_Glucose+
                           BMXBMI+Smoking_status+Drinking_status+Physical+
                           HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12,
                         data = final, design = final_design)%>%
  tbl_regression(include=c('MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_MMA_cox6.docx')

###2.3.RBCfolate_MMA####
# Model 1:
CVD_RBCfolate_MMA_cox1 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_MMA, data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_MMA_cox1.docx')

# Model 2:
CVD_RBCfolate_MMA_cox3 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_MMA + Age + Sex +
                                     Race + Education + PIR+ BMXBMI + Smoking_status + 
                                     Drinking_status + Physical,
                                   data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_MMA_cox3.docx')

# Model 3：
CVD_RBCfolate_MMA_cox5 <- svycoxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ RBCfolate_MMA+Age+Sex+Race+
                                     Education+PIR+Glycohemoglobin+Fasting_Glucose+
                                     BMXBMI+Smoking_status+Drinking_status+Physical+
                                     HEI2015+BPXSY+Food_fast+DRVB12,
                                   data = final, design = final_design)%>%
  tbl_regression(include=c('RBCfolate_MMA'), exponentiate = TRUE)%>%
  as_flex_table()%>%
  flextable::save_as_docx(path = 'CVD_RBCfolate_MMA_cox5.docx')


##3.RCS##### 
final <- ALL_DATA

final$RBCfolate <- log(final$RBCfolate,base = 2)
final$MMA <- log(final$MMA,base = 2)

final_design <- svydesign(data = final,
                          ids = ~SDMVPSU,
                          strata = ~SDMVSTRA,
                          nest = TRUE,
                          weights = ~ WTMEC2YR,
                          survey.lonely.psu="adjust") 
summary(final_design)

###3.1.RBCfolate-->death####
fit_RBCfolate <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ rcs(RBCfolate,3)+Age+Sex+Race+
                            Education+PIR+Glycohemoglobin+Fasting_Glucose+
                            BMXBMI+Smoking_status+Drinking_status+Physical+
                            HEI2015+BPXSY+Food_fast+SEVB12+DRVB12, 
                          design = final_design)

data <- fit_RBCfolate$survey.design$variables 
ori.weight <- 1/(fit_RBCfolate$survey.design$prob)
mean.weight <- mean(ori.weight)
data$weights <- ori.weight/mean.weight 
dd <- rms::datadist(data)
options(datadist = "dd")

fit.rcs.RBCfolate <- rms::cph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ rcs(RBCfolate,3)+Age+Sex+Race+
                                Education+PIR+Glycohemoglobin+Fasting_Glucose+
                                BMXBMI+Smoking_status+Drinking_status+Physical+
                                HEI2015+BPXSY+Food_fast+SEVB12+DRVB12, 
                              data = data, weights = weights, normwt = TRUE) 

anova(fit.rcs.RBCfolate) 
dd$limits
AIC(fit.rcs.RBCfolate)  

pre.linear.RBCfolate <- rms::Predict(fit.rcs.RBCfolate, RBCfolate, fun=exp, type = "predictions", ref.zero=T)

ggplot(pre.linear.RBCfolate)

RCS_RBCfolate <- ggplot() +
  geom_line(data = pre.linear.RBCfolate, aes(x = RBCfolate, y = yhat), linetype = 'solid', linewidth = 1, alpha = 1, color = '#4F99C9')+
  geom_ribbon(data = pre.linear.RBCfolate, aes(x = RBCfolate, ymin = lower, ymax = upper), alpha = 0.1, fill = '#4F99C9')+
  geom_vline(xintercept=10.1, linetype=2, color="black") +
  geom_hline(yintercept=1, linetype=2, color="black") +
  scale_x_continuous("RBC folate (nmol/L)") +
  scale_y_continuous("Hazard ratio (95%CI)") +
  geom_text(aes(x=11.1, y=1.2, label=paste0("P-overall < 0.001","\nP-non-linear < 0.001")),hjust=0) + 
  theme_bw() +
  theme(
    axis.line=element_line(),
    panel.grid=element_blank(),
    panel.border=element_blank(),  
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11))+  
  ggtitle("CVD mortality")
RCS_RBCfolate
ggsave("RCS-ALL-RBCfolate-CVD.pdf",width = 5,height = 4)


###3.2.MMA-->death####
fit_MMA <- svycoxph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ rcs(MMA,3)+Age+Sex+Race+
                      Education+PIR+Glycohemoglobin+Fasting_Glucose+
                      BMXBMI+Smoking_status+Drinking_status+Physical+
                      HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12, 
                    design = final_design)

data <- fit_MMA$survey.design$variables 
ori.weight <- 1/(fit_MMA$survey.design$prob)
mean.weight <- mean(ori.weight)
data$weights <- ori.weight/mean.weight 
dd <- rms::datadist(data)
options(datadist = "dd")

fit.rcs.MMA <- rms::cph(Surv(PERMTH_Y, MORTSTAT_CVD) ~ rcs(MMA,3)+Age+Sex+Race+
                          Education+PIR+Glycohemoglobin+Fasting_Glucose+
                          BMXBMI+Smoking_status+Drinking_status+Physical+
                          HEI2015+BPXSY+Food_fast+RBCfolate+SEVB12+DRVB12, 
                        data = data, weights = weights, normwt = TRUE) 

anova(fit.rcs.MMA) 
dd$limits
AIC(fit.rcs.MMA)  

pre.linear.MMA <- rms::Predict(fit.rcs.MMA, MMA, fun=exp, type = "predictions", ref.zero=T)

ggplot(pre.linear.MMA)

RCS_MMA <- ggplot() +
  geom_line(data = pre.linear.MMA, aes(x = MMA, y = yhat), linetype = 'solid', linewidth = 1, alpha = 1, color = '#4F99C9')+
  geom_ribbon(data = pre.linear.MMA, aes(x = MMA, ymin = lower, ymax = upper), alpha = 0.1, fill = '#4F99C9')+
  geom_vline(xintercept=7.07, linetype=2, color="black") +
  geom_hline(yintercept=1, linetype=2, color="black") +
  scale_x_continuous("MMA (nmol/L)") +
  scale_y_continuous("Hazard ratio (95%CI)") +
  geom_text(aes(x=9, y=1, label=paste0("P-overall < 0.001","\nP-non-linear = 0.563")),hjust=0) + 
  theme_bw() +
  theme(
    axis.line=element_line(),
    panel.grid=element_blank(),
    panel.border=element_blank(),  
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11))+  
  ggtitle("CVD mortality")
RCS_MMA
ggsave("RCS-ALL-MMA-CVD.pdf",width = 5,height = 4)


##4.Additive interaction####
final <- ALL_DATA

final$RBCfolate_category <- ifelse(final$RBCfolate_category == "Lower RBCfolate","1","2")

final$MMA_category <- ifelse(final$MMA_category == "Lower MMA","1","2")

model.cox2.2 <- coxph(Surv(PERMTH_Y,MORTSTAT_CVD) ~ MMA_category*RBCfolate_category + Age+Sex+Race+
                        Education+PIR+Glycohemoglobin+Fasting_Glucose+
                        BMXBMI+Smoking_status+Drinking_status+Physical+
                        HEI2015+BPXSY+Food_fast+SEVB12+DRVB12,final)

table_object = interactionR(model.cox2.2, exposure_names = c("MMA_category","RBCfolate_category" ), ci.type = "mover", ci.level = 0.95, em = F, recode = F)
table_object$dframe 

interactionR_table(table_object)

# 4) Visualization
# 1> Additive interaction effect
ORRBCfolate <- 1.11
ORMMA <- 1.43
RERI <- 0.82
bar_d <- matrix(c(1, 1, 1, 1,
                  ORRBCfolate-1, 0, ORRBCfolate-1, 0,
                  ORMMA-1 ,ORMMA-1, 0, 0,
                  RERI, 0, 0, 0),
                c(4,4), byrow = T,
                dimnames = list(c('NO RBCfolate & NO MMA','RBCfolate','MMA','RBCfolate & MMA')))

plot <- barplot(bar_d, legend=rownames(bar_d),col = c("#D3DCE3","#9BBBE1","#F7E6BA","#C89D94"),ylim = c(0,3),ylab = "Hazard ratio (95%CI)")
