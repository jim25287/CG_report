#身體維度資料 e.g., wc 改用 3D data ##**缺date 欄位



source("~/Lincoln/02.Work/04. R&D/02. HIIS_OPP/00.Gitbook/01.CG/CG_report/rscript/lin_function.R")


# dashboard --------------------------------------------------------------


# Pool data from cliets w/ blood data
# - weight loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - fat loss (∆kg, %) total, avg (**cal. the max, and ignore rebound)
# - improvement from BMI-classification
# - 減重成效，各組百分比 (**single treatment)




#1. med_id_pool
med_id_pool <- dplyr::intersect(clinic_inbody_data_ori %>% select(id) %>% unique() %>% pull(),
                                clinic_blood_data_ori %>% select(id) %>% unique() %>% pull() )
#2. produce dashborad dataset
clinic_inbody_data_dashboard <- clinic_inbody_data_ori %>% filter(id %in% med_id_pool)
clinic_blood_data_dashboard <- clinic_blood_data_ori %>% filter(id %in% med_id_pool)
rm(med_id_pool)



for (i in unique(clinic_inbody_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_inbody_data_dashboard$id),1)) {
    clinic_inbody_data_dashboard <- clinic_inbody_data_dashboard[with(clinic_inbody_data_dashboard, order(id, date_inbody)),] #order by date
    dashboard_table <- clinic_inbody_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) - clinic_inbody_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_inbody")) %>% head(1) %>% as.vector()
  #collect data
  dashboard_table <- rbind(dashboard_table, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_inbody_data_dashboard$id),1)) {
    colnames(dashboard_table) <- names(clinic_inbody_data_dashboard[0,c(-1,-2)])
    dashboard_table <- cbind(unique(clinic_inbody_data_dashboard$id), dashboard_table)
    colnames(dashboard_table)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

for (i in unique(clinic_blood_data_dashboard$id)) {
  #loop 1st
  if (i == head(unique(clinic_blood_data_dashboard$id),1)) {
    clinic_blood_data_dashboard <- clinic_blood_data_dashboard[with(clinic_blood_data_dashboard, order(id, date_blood)),] #order by date
    dashboard_table_blood <- clinic_blood_data_dashboard[0,c(-1,-2)] #create null table(w/t ID)
  }
  #main
  #every obs. - 1st obs.
  temp <- clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) - clinic_blood_data_dashboard %>% filter(id == i) %>% select(-c("id", "date_blood")) %>% head(1) %>% as.vector()
  
  #collect data: min
  dashboard_table_blood <- rbind(dashboard_table_blood, temp %>% apply(2, min) %>% as.vector())
  #loop last
  if (i == tail(unique(clinic_blood_data_dashboard$id),1)) {
    colnames(dashboard_table_blood) <- names(clinic_blood_data_dashboard[0,c(-1,-2)])
    dashboard_table_blood <- cbind(unique(clinic_blood_data_dashboard$id), dashboard_table_blood)
    colnames(dashboard_table_blood)[1] <- "id"
    cat("[Comopleted!]")
  }
} 

dashboard_table <- lin_mapping(dashboard_table, client_type, id, clinical_list, client_type, id)
dashboard_table_blood <- lin_mapping(dashboard_table_blood, client_type, id, clinical_list, client_type, id)

#exclude DM duration < 30 days
dashboard_table <- dashboard_table %>% filter(client_type != 1 | is.na(client_type))
dashboard_table <- dashboard_table %>% filter((bf < -2) & (weight < -1))

dashboard_table_blood <- dashboard_table_blood %>% filter(id %in% dashboard_table$id)





#Layout
  #total client
  dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  
  #total accomplishment
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) 
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE)
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE)
  ##血生化
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE)
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE)
  
  #avg
  #include all: short term: finish program, long term: weight maintenance, weight rebound
  ##inbody
  dashboard_table %>% select("weight") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  dashboard_table %>% select("bf") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##身體維度
  dashboard_table %>% select("wc") %>% sum(na.rm = TRUE) / dashboard_table %>% select(id) %>% unique() %>% pull() %>% length()
  ##血生化 不適用, 嘗試換算成∆%, baseline important
  # dashboard_table_blood %>% select("hba1c") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("glucose_ac") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("insulin") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("homa_ir") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tg") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("tc") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("hdl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()
  # dashboard_table_blood %>% select("ldl") %>% sum(na.rm = TRUE) / dashboard_table_blood %>% select(id) %>% unique() %>% pull() %>% length()

 

  

# test --------------------------------------------------------------------

  stat_table_1st_ob_lm <- stat_table_1st_ob %>% select(vars_en[vars_en != "gp"])
  names(stat_table_1st_ob_lm) <- vars_ch[vars_ch != "gp"]
  
  # Diet x Eff.
    #∆體重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    a <- k$model %>% model_equation(digits = 3, trim = TRUE)
    a
    
    #∆體脂重%
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm %>% select("∆體脂重%") %>% cbind(stat_table_1st_ob_lm %>% select(vars_ch[vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE)]))
    stat_table_1st_ob_lm_diet <- stat_table_1st_ob_lm_diet %>% select(c("∆體脂重%","年齡","飲食紀錄完成率_%","每篇上傳照片數","綠燈率","黃燈率","紅燈率","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日"))
    
    model <- lm(-`∆體脂重%` ~ ., data = stat_table_1st_ob_lm_diet)
    model %>% summary()
    k <- olsrr::ols_step_both_p(model)
    k$model
    k$model %>% summary()
    b <- k$model %>% model_equation(digits = 3, trim = TRUE)
    b

      
  

  
  #All
  # stepwise regression
  stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
    (stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE) %>% append(stat_table_1st_ob_lm %>% names() %>% grep("%", ., , value = TRUE)))
  )
  # stat_table_1st_ob_lm_diet <-  stat_table_1st_ob_lm %>% select(
  #    "∆體重%" %>% append(stat_table_1st_ob_lm %>% names() %>% grep("date|endpoint|id|type|∆", ., value = TRUE, invert = TRUE))
  # )
  
  model <- lm(-`∆體重%` ~ ., data = stat_table_1st_ob_lm_diet)
  model %>% summary()
  k <- olsrr::ols_step_both_p(model)
  k$model
  k$model %>% summary()
  c <- k$model %>% model_equation(digits = 3, trim = TRUE)
  c
  #plot(k)
  # final model
  k$model
  
  
  
  
  #[Prediction accuracy]
  ##1. create predict_table
  predict_table <- stat_table_1st_ob_lm_diet %>% select(k$predictors %>% gsub("`", "", .) %>% append("∆體重%"))
  ##2. create actual/forcast
  predict_table$actual <- -predict_table$`∆體重%`
  predict_table$forcast <- stats::predict(k$model, predict_table)
  #rm NA row
  predict_table <- predict_table %>% lin_exclude_NA_col(variables = names(.))
  ##3. calculate MAPE
  cat(paste("\n\n", "平均絕對百分比誤差(MAPE):", (MLmetrics::MAPE(predict_table$forcast, predict_table$actual) *100) %>% round(2), "%", "\n\n"))
  

  predict_table$residue <- abs((predict_table$actual - predict_table$forcast)/predict_table$actual *100) %>% round(2)
  predict_table$true <- predict_table$residue %>% cut(c(0, 10 , Inf), c(1, 0))
  predict_table$true %>% table() %>% prop.table() %>% multiply_by(100) %>% round(2)
  
  
  MLmetrics::Accuracy(predict_table$forcast, predict_table$true)
  MLmetrics::Precision(predict_table$forcast, predict_table$actual)
  
  
  
  
  
  
  
  #install.packages("Metrics")
  library(Metrics)

  
  set.seed(777)
  split <- caTools::sample.split(stat_table_1st_ob_lm_diet, SplitRatio = 0.8)
  
  train <- subset(stat_table_1st_ob_lm_diet, split == TRUE)
  test <- subset(stat_table_1st_ob_lm_diet, split == FALSE)
  
  
  
  


  
  

# verify the "ns" reason of testosterone gps 20230302 ---------------------

  # set.seed(333)
  # #sample size set to 100
  # a <- a %>% rbind(rbind(a[sample(which(a$gp_testosterone == "Low_testosterone"), (30 - sum(a$gp_testosterone == "Low_testosterone")), replace = TRUE),], 
  #                        a[sample(which(a$gp_testosterone != "Low_testosterone"), (30 - sum(a$gp_testosterone != "Low_testosterone")), replace = TRUE),]) )
  # table(a$gp_testosterone) %>% addmargins()
  #DONE
  
  
  
  "weight","bmi","bf", "pbf","bsmi",  "bm", "pbm", "vfa","wc", "ffm","bmr","hba1c", "glucose_ac", "insulin","homa_ir", "homa_beta", "tg", "tc", "hdl", "ldl","lipase"
  
  db = 'postgres'
  host = '35.201.248.55'
  pw = 'zCxHjp0Byy11Jm2D'
  user = 'postgres'
  
  
  
  
  
  
  
  
  
  