data <- read.csv("EPOdosing.csv")

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


