library("dplyr")
install.packages("ggpubr")
library("ggpubr")
setwd("~/Desktop/STAT7995- Stat Consulting  2/kidney")
demo <- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/DemogData.csv")
data<- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/EPOdosing.csv")
hgb<- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/HGBdata.csv")
iron<- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/IronData.csv")

#==================reduce average weekly ESA dosing? ==================
#data cleaning 
data$PatientID <- as.factor(data$PatientID)

data$Post.Intervention <- as.factor(data$Post.Intervention)

dose_split <- split(data, data$PatientID)

result <- data.frame(matrix(nrow = 50, ncol = 3))
colnames(result) <- c("Patient ID", "Post.Intervention", "Ave_EPO_Dose")
result$`Patient ID` <-  rep(levels(data$PatientID), each = 2)

for(id in levels(data$PatientID)){
  dose_id <- dose_split[[id]]
  dose_id_t <- split(dose_id, dose_id$Post.Intervention)$"True"
  dose_id_f <- split(dose_id, dose_id$Post.Intervention)$"False"
  dose_id_t_weeks <- split(dose_id_t, list(dose_id_t$Year, dose_id_t$Week))
  dose_id_f_weeks <- split(dose_id_f, list(dose_id_f$Year, dose_id_f$Week))
  tsum = 0
  fsum = 0
  for(week in dose_id_t_weeks){
    if(!is.nan(mean(week$EPO_Dose))){
      tsum = tsum + mean(week$EPO_Dose)
    }
  }
  for(week in dose_id_f_weeks){
    if(!is.nan(mean(week$EPO_Dose))){
      fsum = fsum + mean(week$EPO_Dose)
    }
  }
  result[as.numeric(id)*2+1, "Post.Intervention"] = "True";
  result[as.numeric(id)*2+2, "Post.Intervention"] = "False";
  result[as.numeric(id)*2+1, "Ave_EPO_Dose"] = tsum/44;
  result[as.numeric(id)*2+2, "Ave_EPO_Dose"] = fsum/44;
}
write.csv(result, "does.csv")
result_split<-split(result, result$Post.Intervention)
write.csv(result_split$False, "prior.csv")
write.csv(result_split$True, "post.csv")
dose <- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/does.csv")
dose_2 <- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/dose_2.csv")
#summary 
group_by(dose_2, group) %>%
  summarise(
    count = n(),
    median = median(dose, na.rm = TRUE),
    IQR = IQR(dose, na.rm = TRUE)
  )
group_by(dose_2, group) %>%
  summarise(
    count = n(),
    mean = mean(dose, na.rm = TRUE),
    sd = sd(dose, na.rm = TRUE)
  )

ggboxplot(dose_2, x = "group", y = "dose", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("Prior", "Post"),
          ylab = "Dose", xlab = "Groups")

#summary2
# Subset weight data before treatment
Prior_dose <- subset(dose_2,  group == "Prior", dose,
                      drop = TRUE)
# subset weight data after treatment
Post_dose <- subset(dose_2,  group == "Post", dose,
                     drop = TRUE)
# Plot paired data
library(PairedData)
plot_dose <- paired(Prior_dose, Post_dose)
plot(plot_dose, type = "profile") + theme_bw()

#check outliers 
dose_box<-boxplot(dose$difference,
                  main = "Differences of prior and post's EPO_Dose",
                  col = "orange",
                  horizontal = FALSE,
                  notch = TRUE)
dose_outlier_remove<-dose[-c(8,14),]
dose_box<-boxplot(dose_outlier_remove$difference,
                  main = "Differences of prior and post's EPO_Dose after the outliers",
                  col = "orange",
                  horizontal = FALSE,
                  notch = TRUE)
write.csv(dose_outlier_remove, "dose_2_remove.csv")
#check nomality
shapiro.test(dose_outlier_remove$difference)#0.1673 not normal 
#paired t-test - one-side 
result_dose_one<-t.test(dose ~ group, data = dose_2_remove, paired = TRUE,
                         )
result_dose_one













#2
#==================more time in range group level?===========================
hgb_split<-split(hgb,hgb$PatientID)
range <- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/range.csv")
#summary
group_by(range2, group) %>%
  summarise(
    count = n(),
    mean = mean(percentage_time_in_range, na.rm = TRUE),
    sd = sd(percentage_time_in_range, na.rm = TRUE)
  )
ggboxplot(range2, x = "group", y = "percentage_time_in_range", 
          color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("Prior", "Post"),
          ylab = "Percentage_time_in_range", xlab = "Groups")
#summary2
# Subset weight data before treatment
Prior_range <- subset(range2,  group == "Prior", percentage_time_in_range,
                 drop = TRUE)
# subset weight data after treatment
Post_range <- subset(range2,  group == "Post", percentage_time_in_range,
                drop = TRUE)
# Plot paired data
library(PairedData)
plot_range <- paired(Prior_range, Post_range)
plot(plot_range, type = "profile") + theme_bw()

#check nomality
shapiro.test(range$diff)# p-value = 0.8604--> the data are not significantly different from normal distribution 
#check outliers 
range2 <- read.csv("~/Desktop/STAT7995- Stat Consulting  2/kidney/range2.csv")
diff_box<-boxplot(range$diff,
                   main = "Differences of prior and post's percentage of time in range",
                   col = "orange",
                   horizontal = FALSE,
                   notch = TRUE)


acf(range$diff)
#paired t-test - one-side 
result_range_one<-t.test(percentage_time_in_range ~ group, data = range2, paired = TRUE,
       alternative = "less")
result_range_one
#p-value = 0.4353 cannot reject 

#paired t-test - two-side
result_range_two<-t.test(percentage_time_in_range ~ group, data = range2, paired = TRUE)
result_range_two
#still cannot reject 
plot.ts(hgb_split$`23`$HGB)
acf(hgb_split$`17`$HGB)



#==================more time in range group level? shift 2 month===========================

#paired t-test - one-side 
result_range_one_2month<-t.test(percentage ~ group, data = range_2_2, paired = TRUE,
                         alternative = "less")
result_range_one_2month
#p-value = 0.4353 cannot reject 

#paired t-test - two-side
result_range_two_2month<-t.test(percentage ~ group, data = range_2_2, paired = TRUE)
result_range_two_2month
#still cannot reject 







