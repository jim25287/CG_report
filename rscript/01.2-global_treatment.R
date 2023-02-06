
# ## Analysis - 1: Baseline 分布 (Pie-chart) --------------------------------



#[Pie chart] 1. cut (cluster) 2. call pie chart
#Age
stat_table_1st_ob$age_gp <- cut(stat_table_1st_ob$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
pie_01 <- 
stat_table_1st_ob %>% group_by(age_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Age',
                                                                                           legend = "{position:'right'}",
                                                                                           pieHole = 0.5,
                                                                                           #slices = "{1:{offset:0.1}}",
                                                                                           backgroundColor = "#edeff2",
                                                                                           width = "600",
                                                                                           height = "400"))

#Gender
pie_02 <- 
stat_table_1st_ob %>% group_by(gender) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Gender',
                                                                                           legend = "{position:'right'}",
                                                                                           pieHole = 0.5,
                                                                                           slices = "{0:{offset:0.1}}",
                                                                                           backgroundColor = "#edeff2",
                                                                                           colors = "['#DC3912', '#3366CC']",
                                                                                           width = "600",
                                                                                           height = "400"))



#BMI x Obesity

stat_table_1st_ob$bmi_gp <- cut(stat_table_1st_ob$bmi_baseline, c(0,18.5,24,27,100), c("underweight", "normal", "overweight", "obesity"))

pie_03 <- 
stat_table_1st_ob %>% filter(gender == "male") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Male',
                                                                                                                        legend = "{position:'right'}",
                                                                                                                        pieHole = 0.5,
                                                                                                                        #slices = "{2:{offset:0.1}}",
                                                                                                                        backgroundColor = "#edeff2",
                                                                                                                        width = "600",
                                                                                                                        height = "400"))

pie_04 <- 
stat_table_1st_ob %>% filter(gender == "female") %>% group_by(bmi_gp) %>% summarise(n = n()) %>% gvisPieChart(options = list(title = 'Female',
                                                                                                                          legend = "{position:'right'}",
                                                                                                                          pieHole = 0.5,
                                                                                                                          #slices = "{1:{offset:0.1}}",
                                                                                                                          backgroundColor = "#edeff2",
                                                                                                                          width = "600",
                                                                                                                          height = "400"))





# Efficacy_Inbody (weight,  fat,  muscle) ----------------------------------

####控糖減重成效-身體組成


stat_table_1st_ob$`∆weight_gp` <- cut(stat_table_1st_ob$`∆weight%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆fat_gp` <- cut(stat_table_1st_ob$`∆bf%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
stat_table_1st_ob$`∆muscle_mass_gp` <- cut(stat_table_1st_ob$`∆bm%`, c(-100, -15, -8, -4, 0, 5, 15, 100), c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))

data <- stat_table_1st_ob %>% select(c("gender","∆weight%","∆bf%","∆bm%","∆weight_gp","∆fat_gp","∆muscle_mass_gp"))
data$gender <-  factor(data$gender, levels = c("female", "male"))
#table
#weight
a <- 
  data %>% 
  dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆weight%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆weight_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
a <- a[with(a, order(gender)),]
a <- a %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(a)[c(1,3)] <- c("Group", "Value")
a$variable <-  "Weight"

#fat
b <- 
  data %>% 
  dplyr::group_by(`∆fat_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆bf%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆fat_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
b <- b[with(b, order(gender)),]
b <- b %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(b)[c(1,3)] <- c("Group", "Value")
b$variable <-  "Fat"
a <- rbind(a,b)
#muscle
b <- 
  data %>% 
  dplyr::group_by(`∆muscle_mass_gp`, `gender`, .drop = FALSE) %>%
  summarise_at(
    vars(`∆bm%`),
    funs(round(mean(., na.rm = TRUE), 2))
  )  %>% 
  inner_join(
    data %>% 
      dplyr::group_by(`∆muscle_mass_gp`, `gender`, .drop = FALSE) %>%
      summarise(
        N = n()
      ))
b <- b[with(b, order(gender)),]
b <- b %>% dplyr::group_by(gender) %>% mutate(percentage = round(N / sum(N) *100,2)) 
names(b)[c(1,3)] <- c("Group", "Value")
b$variable <-  "Muscle"
a <- rbind(a,b)



#output thr .kable
# b <- a
# b <- b %>% dplyr::rename(., "Mean ± SE" = "Value") 
#   




#output plot
a$Group <-  factor(a$Group, levels = c("<-15%","-8~15%","-4~8%", "-4~0%","0~5%","5~15%",">15%"))
a$variable <-  factor(a$variable, levels = c("Weight","Fat","Muscle"))

# Stacked + percent
library(ggplot2)
# plot_stack_col <- 
# ggplot(a) +
#   aes(x = variable, y = percentage, fill = Group) +
#   geom_col() +
#   scale_fill_brewer(palette = "RdYlBu", 
#                     direction = 1) +
#   labs(x = " ", y = "Percentage(%)", title = "控糖減重成效-身體組成(OB)", fill = "Interval") +
#   geom_text(data = subset(a, percentage > 5), aes(label = paste0(percentage,"%")), size = 5, position = position_stack(vjust = 0.8)) +
#   theme_minimal() +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), 
#         axis.text.x = element_text(size = 17,face = "bold"),
#         axis.text.y = element_text(size = 12),
#         axis.title.y = element_text(size = 17, face = "bold"),
#         legend.text = element_text(size = 15)) +
#   facet_wrap(vars(gender))


#ggviz_bar_stack
  #create df
  b <- a %>% filter((gender == "female") & (variable == levels(a$variable)[1])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select(-gender) %>% t()
  colnames(b) <- b["Group",]
  b <- b %>%  as.data.frame()
  b <- b["N",]
  #transform ori table
  b <- 
    Reduce(rbind,list(b,
                      a %>% filter((gender == "female") & (variable == levels(a$variable)[2])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector(),
                      a %>% filter((gender == "female") & (variable == levels(a$variable)[3])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector()
    ))
  #adjust df
  b <- b %>% lapply(as.numeric) %>% as.tibble()
  b$var <- levels(a$variable)
  
  
  
  
  #plot
  col_color <- 
    paste0("[", RColorBrewer::brewer.pal(11, 'RdYlBu')[c(2,3,4,5,7,9,10)] %>% rev() %>% 
             stringr::str_c('\'', ., '\'') %>% 
             stringr::str_c(collapse = ","), "]")
  
  plot_stack_col <-
    gvisColumnChart(b , xvar = "var", yvar = b %>% select(-var) %>% names() %>% rev(),
                    options = list(isStacked = 'percent',
                                   title = '控糖減重成效-身體組成(OB)',
                                   legend = "{position:'right'}",
                                   colors = col_color,
                                   backgroundColor = "#edeff2",
                                   width = "600",
                                   height = "600"))



rm(list = c("a", "b", "data"))



