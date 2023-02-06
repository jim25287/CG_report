
# ## Analysis - 1: Baseline 分布 (Pie-chart) --------------------------------

#Add doctor, 
#[Assumption:] NA are clients of Dr. 宋 from HCP other than Genesis
stat_table_1st$doctor <- "宋醫師"
stat_table_1st <- lin_mapping(stat_table_1st, doctor, id, clinical_list, doctor, id, overwrite = TRUE)


#[Pie chart] 1. cut (cluster) 2. call pie chart
#Age
stat_table_1st$age_gp <- cut(stat_table_1st$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
pie_01 <- 
stat_table_1st %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                           legend = "{position:'right'}",
                                                                                           pieHole = 0.5,
                                                                                           #slices = "{1:{offset:0.1}}",
                                                                                           backgroundColor = "#edeff2",
                                                                                           width = "600",
                                                                                           height = "400"))

#Gender
pie_02 <- 
stat_table_1st %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                           legend = "{position:'right'}",
                                                                                           pieHole = 0.5,
                                                                                           #slices = "{1:{offset:0.1}}",
                                                                                           backgroundColor = "#edeff2",
                                                                                           width = "600",
                                                                                           height = "400"))



#BMI x Obesity

stat_table_1st$bmi_gp <- cut(stat_table_1st$bmi_baseline, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

pie_03 <- 
stat_table_1st %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                        legend = "{position:'right'}",
                                                                                                                        pieHole = 0.5,
                                                                                                                        #slices = "{2:{offset:0.1}}",
                                                                                                                        backgroundColor = "#edeff2",
                                                                                                                        width = "600",
                                                                                                                        height = "400"))

pie_04 <- 
stat_table_1st %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                          legend = "{position:'right'}",
                                                                                                                          pieHole = 0.5,
                                                                                                                          #slices = "{1:{offset:0.1}}",
                                                                                                                          backgroundColor = "#edeff2",
                                                                                                                          width = "600",
                                                                                                                          height = "400"))




