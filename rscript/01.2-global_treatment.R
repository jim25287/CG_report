
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
                                                                                           #slices = "{0:{offset:0.1}}",
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


#*** for Index.Rmd highlight
success_df <- a %>% filter(variable == "Weight") %>% filter(!is.nan(Value)) %>% dplyr::mutate(., success = cut(Value, c(-Inf, 0, Inf), c(1, 0)))
success_df_freq <- data.frame(gender = c("female", "male"),
                              success = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 1) %>% pull(),
                              failure = success_df %>% group_by(gender, success, .drop = FALSE) %>% summarise(N = sum(N)) %>% filter(success == 0) %>% pull()) %>% janitor::adorn_totals("row") %>% janitor::adorn_totals("col")
success_df_pct <- success_df_freq %>% janitor::adorn_percentages() %>% janitor::adorn_pct_formatting()
rm(success_df)

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
  global_eff_bar_df <- function(data, gender){
    sex <- deparse(substitute(gender))
    data <- data %>% as.tibble()
    #create df
    b <- data %>% filter((gender == sex) & (variable == levels(data[[deparse(substitute(variable))]])[1])) %>% select(c("Group", "N")) %>% as.data.frame() %>% t()
    colnames(b) <- b["Group",]
    b <- b %>%  as.data.frame()
    b <- b["N",]
    #transform ori table
    b <-
      Reduce(rbind,list(b,
                        data %>% filter((gender == sex) & (variable == levels(data$variable)[2])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector(),
                        data %>% filter((gender == sex) & (variable == levels(data$variable)[3])) %>% select(c("Group", "N")) %>% as.data.frame() %>% select("N") %>% t() %>%  as.vector()
      ))
    #adjust df
    b <- b %>% lapply(as.numeric) %>% as.tibble()
    b$var <- levels(a$variable)
    return(b)
  }
  
  b <- global_eff_bar_df(data = a, female)
  
  
  #plot
  col_color <- 
    paste0("[", RColorBrewer::brewer.pal(11, 'RdYlBu')[c(2,3,4,5,7,9,10)] %>% rev() %>% 
             stringr::str_c('\'', ., '\'') %>% 
             stringr::str_c(collapse = ","), "]")
  
  plot_stack_col_female <-
    gvisColumnChart(b , xvar = "var", yvar = b %>% select(-var) %>% names() %>% rev(),
                    options = list(isStacked = 'percent',
                                   bar="{groupWidth:'50%'}",
                                   title = '控糖減重成效-身體組成(Female)',
                                   legend = "{position:'right'}",
                                   colors = col_color,
                                   backgroundColor = "#edeff2",
                                   width = "600",
                                   height = "600"))
  
  b <- global_eff_bar_df(data = a, male)
  
  col_color <- 
    paste0("[", RColorBrewer::brewer.pal(11, 'RdYlBu')[c(2,3,4,5,7,9,10)] %>% rev() %>% 
             stringr::str_c('\'', ., '\'') %>% 
             stringr::str_c(collapse = ","), "]")
  plot_stack_col_male <-
    gvisColumnChart(b , xvar = "var", yvar = b %>% select(-var) %>% names() %>% rev(),
                    options = list(isStacked = 'percent',
                                   bar="{groupWidth:'50%'}",
                                   title = '控糖減重成效-身體組成(Male)',
                                   legend = "{position:'right'}",
                                   colors = col_color,
                                   backgroundColor = "#edeff2",
                                   width = "600",
                                   height = "600"))
  

rm(list = c("a", "b", "data"))

# Line plot ---------------------------------------------------------------
#Establish summary table
a <- stat_table_1st_ob %>% select(grep("baseline$", stat_table_1st_ob %>% names())) %>%
  select(-c("date_baseline", "extracellular_water_ratio_baseline", "wepa50_baseline", "e2_baseline", "testosterone_baseline"))
b <- stat_table_1st_ob %>% select(grep("endpoint$", stat_table_1st_ob %>% names())) %>%
  select(-c("date_endpoint", "extracellular_water_ratio_endpoint", "wepa50_endpoint", "e2_endpoint", "testosterone_endpoint"))

#[D0] mean
stat_table_1st_ob_temp <- 
  a %>%
  apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))

#[D0] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        a %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE ), 2))
  )

#[D60] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        b %>%
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[D60] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        b %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE ), 2))
  )

stat_table_1st_ob_temp <- stat_table_1st_ob_temp %>% as.data.frame()
names(stat_table_1st_ob_temp) <-  c("D0_mean","D0_SEM","D60_mean","D60_SEM")

#[∆] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        ((b) - (a)) %>% 
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[∆] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        ((b) - (a)) %>%
          apply(2, function(x) round(sd(x, na.rm = TRUE )/sqrt(length(x)), 2))
  )

#[∆%] mean
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        (
          ((b) - (a)) / a *100) %>% 
          apply(2, function(x) round(mean(x, na.rm = TRUE ), 2))
  )

#[∆%] SEM
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        (
          ((b) - (a)) / a *100) %>% 
          apply(2, function(x) round(sd(x, na.rm = TRUE )/sqrt(length(x)), 2))
  )

for (i in c(1:((stat_table_1st_ob_temp %>% nrow())))) {
  if (!("p-value" %in% names(stat_table_1st_ob_temp))) {
    stat_table_1st_ob_temp$`p-value` <- NA
  }
  stat_table_1st_ob_temp$`p-value`[i] <- t.test(a[i] %>% pull(),
                                                b[i] %>% pull(),
                                                paired = TRUE,
                                                na.rm = TRUE,
                                                alternative = c("two.sided", "less", "greater"))$p.value
  if ( i == ((stat_table_1st_ob_temp %>% nrow()))) {
    print("[completed!]")
  }
}
#n()
stat_table_1st_ob_temp <- 
  cbind(stat_table_1st_ob_temp, 
        a %>% apply(2, function(x) length(x))
  )


#df cleaning
names(stat_table_1st_ob_temp) <-  c("D0_mean","D0_SEM","D60_mean","D60_SEM", "∆_mean", "∆_SEM", "∆%_mean", "∆%_SEM", "p-value", "N")
stat_table_1st_ob_temp$variable <- gsub("_baseline$","", row.names(stat_table_1st_ob_temp))

row.names(stat_table_1st_ob_temp) <- seq(1,nrow(stat_table_1st_ob_temp))
library(dplyr)
stat_table_1st_ob_temp <- stat_table_1st_ob_temp %>% relocate(variable)

#before/after bar chart 
library(reshape)
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp %>% select("variable","∆%_mean", "∆%_SEM")
stat_table_1st_ob_temp_barchart <- melt(stat_table_1st_ob_temp_barchart, c("variable"))
colnames(stat_table_1st_ob_temp_barchart) <-  c("variable", "pre_post", "value")
stat_table_1st_ob_temp_barchart <- cbind(stat_table_1st_ob_temp_barchart[grep("mean$", stat_table_1st_ob_temp_barchart$pre_post),],
                                         stat_table_1st_ob_temp_barchart[grep("SEM$", stat_table_1st_ob_temp_barchart$pre_post),]["value"]
)
colnames(stat_table_1st_ob_temp_barchart) <-c("variable", "pre_post", "mean", "sem")

#create baseline 0,  w/ rbind
a <- stat_table_1st_ob_temp_barchart
a$mean <- a$sem <- 0
a$pre_post <- "Before"
stat_table_1st_ob_temp_barchart <- rbind(a, stat_table_1st_ob_temp_barchart)
rm(a)
stat_table_1st_ob_temp_barchart$pre_post[grep("^∆%", stat_table_1st_ob_temp_barchart$pre_post)] <- "After"
stat_table_1st_ob_temp_barchart$pre_post <-  stat_table_1st_ob_temp_barchart$pre_post %>% factor(levels = c("Before", "After"))
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp_barchart[with(stat_table_1st_ob_temp_barchart, order(variable, pre_post)),]
row.names(stat_table_1st_ob_temp_barchart) <- seq(1, nrow(stat_table_1st_ob_temp_barchart))
stat_table_1st_ob_temp_barchart <- stat_table_1st_ob_temp_barchart %>% mutate(sd = round(sem * sqrt(stat_table_1st_ob %>% nrow()), 2)) 


#line chart
line_plot_df <- stat_table_1st_ob_temp_barchart[!is.element(stat_table_1st_ob_temp_barchart$variable, c("glucose_pc_1hr", "glucose_pc_2hr", "insulin_pc_1hr", "insulin_pc_2hr")),]
var_ch <- c("體重", "BMI", "體脂重", "體脂率", "骨骼肌指數", "肌肉重", "內臟脂肪", "腰圍", "除脂體重", "基礎代謝率", "糖化血色素",
            "空腹血糖", "空腹胰島素", "HOMA-IR", "HOMA-Beta", "三酸甘油脂", "總膽固醇", "HDL", "LDL", "解脂酶")
line_plot_df$variable <- var_ch %>% rep(each = 2)
line_plot_df$variable <- line_plot_df$variable %>% factor(levels = c("體重", "BMI", "體脂重", "體脂率", "骨骼肌指數", "肌肉重", "內臟脂肪", "腰圍", "除脂體重", "基礎代謝率", "糖化血色素",
                                                               "空腹血糖", "空腹胰島素", "HOMA-IR", "HOMA-Beta", "三酸甘油脂", "總膽固醇", "HDL", "LDL", "解脂酶"))


line_plot <- 
  line_plot_df %>% 
  ggplot( aes(x = pre_post, group = 1)) + 
  geom_point(aes(y = mean), size = 1.5, color = "red3",) +
  geom_line(aes(y = mean), size = 0.3, color = "red3") +
  geom_text(data = . %>% filter(pre_post == "After"),
            aes(y = mean, label = paste0(round(mean, 1),"%")), 
            nudge_x = -0.7, size = 3,
            show.legend = FALSE) +
  labs(x = "", y = "成效(%)", title = "")+
  xlim("Before", "After") +
  scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) +
  facet_wrap(vars(variable), scales = "free") +
  theme_linedraw() +
  theme(
    plot.title = element_text(face = "bold",
                              hjust = 0.5),
    axis.title.y = element_text(face = "bold", size = 15)
  ) 


#google viz
a <- stat_table_1st_ob_temp_barchart[!is.element(stat_table_1st_ob_temp_barchart$variable, c("glucose_pc_1hr", "glucose_pc_2hr", "insulin_pc_1hr", "insulin_pc_2hr")),] %>% filter(pre_post == "After") %>% select(c("variable", "mean"))
a <- a %>% add_column(before = 0)
names(a) <- c("var", "post", "pre")
a <- a %>% select(c("var", "pre", "post")) %>% t()
colnames(a) <- a["var",]
a <- a[2:3,] %>% as.tibble()  %>% lapply(as.numeric) %>% as.tibble()
a$pre_post <- c("pre", "post")

#line_plot <- 
gvisLineChart(a, xvar = "pre_post", yvar = c("weight", "bf")) %>% plot()




# cor_plot ----------------------------------------------------------------





#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- stat_table_1st_ob %>% 
  select(c("∆weight%","∆bf%","∆bm%","∆vfa","∆wc","∆bmr","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase"))
names(profile_efficacy) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
                             "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "∆解脂酶") #10

profile_baseline <- stat_table_1st_ob %>% 
  select(c("age", "bmi_baseline","pbf_baseline","vfa_baseline","bsmi_baseline","bm_baseline","wc_baseline","bmr_baseline",
           "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline"))

names(profile_baseline) <- c("年齡", "BMI(T0)", "體脂率(T0)", "內臟脂肪(T0)", "BSMI(T0)", "肌肉重(T0)", "腰圍(T0)", "BMR(T0)",
                             "糖化血色素(T0)", "空服血糖(T0)", "空腹胰島素(T0)", "HOMA_IR(T0)", "HOMA_ß(T0)", "三酸甘油脂(T0)", "總膽固醇(T0)", #17
                             "HDL(T0)", "LDL(T0)", "解脂酶(T0)")

profile_diet <- stat_table_1st_ob %>% 
  select(c("upload_day_%", "pic_count","calorie","carb_E%","protein_E%","fat_E%","fruits","vegetables","grains","meat_bean","milk", "oil","light_G_%","light_Y_%","light_R_%"))
names(profile_diet) <- c("上傳天數%","上傳照片數", "總攝取卡路里","總碳水比_E%","總蛋白比_E%","總脂肪比_E%",
                         "水果(日)","蔬菜(日)","全穀雜糧(日)","蛋豆魚肉(日)","乳品(日)","油脂(日)",
                         "綠燈比_%","黃燈比_%","紅燈比_%")


##[Method 2] corrplot

library(corrplot)
#[Correlation r] Efficacy x Diet
M1 <- cor(cbind(-profile_efficacy, profile_diet), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test1 <- cor.mtest(cbind(-profile_efficacy, profile_diet) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# colnames(M1) <- row.names(M1) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
#                                    "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "∆解脂酶", #10
#                                    "上傳天數%","上傳照片數", "總攝取卡路里","總碳水比_E%","總蛋白比_E%","總脂肪比_E%",
#                                    "水果(日)","蔬菜(日)","全穀雜糧(日)","蛋豆魚肉(日)","乳品(日)","油脂(日)",
#                                    "綠燈比_%","黃燈比_%","紅燈比_%") #15

#run corrplot plot
# corrplot(M1, 
#          p.mat = M_test1$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Diet", 
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0))


for (i in c(1:length(colnames(M1)))) {
  if (i == 1) {
    M1_value <- M1 %>% round(2) 
    M1_sign <- M_test1$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M1_df <- M1_value
    M1_df[,] <- NA
  }
  
  M1_df[,i] <- paste0(M1_value[,i], " (", M1_sign[,i], ")")
  
  if (i ==  length(colnames(M1))) {
    rm(list = c("M1_value", "M1_sign"))
    M1_df <- M1_df %>% as.data.frame()
    M1_df <- M1_df %>% add_column(vars = rownames(M1_df), .before = names(M1_df)[1])
  }
  
} 

cor_table_01 <- M1_df %>% gvisTable(options=list(height=300))




#[Correlation r] Efficacy x Baseline
M2 <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_test2 <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

# colnames(M2) <- row.names(M2) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
#                                    "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "解脂酶", #10
#                                    "BMI(T0)", "體脂率(T0)", "內臟脂肪(T0)", "BSMI(T0)", "肌肉重(T0)", "腰圍(T0)", "BMR(T0)",
#                                    "糖化血色素(T0)", "空服血糖(T0)", "空腹胰島素(T0)", "HOMA_IR(T0)", "HOMA_ß(T0)", "三酸甘油脂(T0)", "總膽固醇(T0)", #17
#                                    "HDL(T0)", "LDL(T0)", "解脂酶(T0)") #

#run corrplot plot
# corrplot(M2, 
#          p.mat = M_test2$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Baseline",
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0)) 

for (i in c(1:length(colnames(M2)))) {
  if (i == 1) {
    M2_value <- M2 %>% round(2) 
    M2_sign <- M_test2$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M2_df <- M2_value
    M2_df[,] <- NA
  }
  
  M2_df[,i] <- paste0(M2_value[,i], " (", M2_sign[,i], ")")
  
  if (i ==  length(colnames(M2))) {
    rm(list = c("M2_value", "M2_sign"))
    M2_df <- M2_df %>% as.data.frame()
    M2_df <- M2_df %>% add_column(vars = rownames(M2_df), .before = names(M2_df)[1])
  }
  
} 
cor_table_02 <- M2_df %>% gvisTable(options=list(height=300))


# Effi.  stratification ---------------------------------------------------

#Divide into 3 group based on ∆weight
QQ1_stat_table_1st_bad <- stat_table_1st %>% filter(client_type != 1) %>% filter(`∆weight%` > -3)
QQ1_stat_table_1st_bad$`∆weight%` %>%summary()
QQ1_stat_table_1st_bad$id %>% unique() %>% length()
QQ1_stat_table_1st_bad %>% summary()
QQ1_stat_table_1st_bad$gp <- "Poor"

QQ1_stat_table_1st_medium <- stat_table_1st %>% filter(client_type != 1) %>% filter((`∆weight%` > -10) & (`∆weight%` < -5) )
QQ1_stat_table_1st_medium$`∆weight%` %>%summary()
QQ1_stat_table_1st_medium$id %>% unique() %>% length()
QQ1_stat_table_1st_medium %>% summary()
QQ1_stat_table_1st_medium$gp <- "Medium"

QQ1_stat_table_1st_good <- stat_table_1st %>% filter(client_type != 1) %>% filter(`∆weight%` < -10)
QQ1_stat_table_1st_good$`∆weight%` %>%summary()
QQ1_stat_table_1st_good$id %>% unique() %>% length()
QQ1_stat_table_1st_good %>% summary()
QQ1_stat_table_1st_good$gp <- "Good"

QQ1_stat_table_1st <- rbind(QQ1_stat_table_1st_bad, QQ1_stat_table_1st_good)
QQ1_stat_table_1st <- rbind(QQ1_stat_table_1st, QQ1_stat_table_1st_medium)
QQ1_stat_table_1st$gp %<>% factor(levels = c("Poor", "Medium", "Good"))



#turn ∆ into positve(reverse)
QQ1_stat_table_1st_a <- QQ1_stat_table_1st %>% select(-grep("∆", names(QQ1_stat_table_1st))) 
#rm NA vars
QQ1_stat_table_1st_a %<>% select(-grep("extracellular", names(QQ1_stat_table_1st_a)))
QQ1_stat_table_1st_a %<>% select(-grep("wepa50", names(QQ1_stat_table_1st_a)))
QQ1_stat_table_1st_a %<>% select(-grep("e2", names(QQ1_stat_table_1st_a)))
QQ1_stat_table_1st_a %<>% select(-grep("testosterone", names(QQ1_stat_table_1st_a)))
QQ1_stat_table_1st_a %<>% select(-grep("hr", names(QQ1_stat_table_1st_a)))


#Setting improvement direction
##Improvement: Uncertain, default setting
QQ1_stat_table_1st_b <- QQ1_stat_table_1st %>% 
  select(c("∆bmr","∆bmr%", "∆lipase","∆lipase%"))
#觀察值不夠: select(c("∆extracellular_water_ratio","∆wepa50","∆bmr","∆extracellular_water_ratio%","∆wepa50%","∆bmr%","∆e2","∆testosterone","∆e2%","∆testosterone%"))

##Improvement: negative
QQ1_stat_table_1st_c <- QQ1_stat_table_1st %>% select(c("∆weight","∆bmi","∆bf","∆pbf","∆vfa","∆wc","∆ffm","∆weight%","∆bmi%","∆bf%","∆pbf%","∆vfa%","∆wc%","∆ffm%","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆tg","∆tc","∆ldl","∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆tg%","∆tc%","∆ldl%")) %>% multiply_by(-1)
##Improvement: positive
QQ1_stat_table_1st_d <- QQ1_stat_table_1st %>% select(c("∆bsmi","∆bm","∆bsmi%","∆bm%","∆homa_beta","∆hdl","∆homa_beta%","∆hdl%"))

QQ1_stat_table_1st <- Reduce(cbind,list(QQ1_stat_table_1st_a, QQ1_stat_table_1st_b, QQ1_stat_table_1st_c, QQ1_stat_table_1st_d), accumulate =FALSE) 



#Sort var order: baseline, endpoint, diet, ∆, ∆%
  vars_en <- c("id","client_type","age","gender","date_baseline","date_endpoint",
            "weight_baseline","bmi_baseline","bf_baseline","pbf_baseline","bsmi_baseline","bm_baseline","vfa_baseline","wc_baseline","ffm_baseline","bmr_baseline",
            "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
            "weight_endpoint","bmi_endpoint","bf_endpoint","pbf_endpoint","bsmi_endpoint","bm_endpoint","vfa_endpoint","wc_endpoint","ffm_endpoint","bmr_endpoint",
            "hba1c_endpoint","glucose_ac_endpoint","insulin_endpoint","homa_ir_endpoint","homa_beta_endpoint","tg_endpoint","tc_endpoint","hdl_endpoint","ldl_endpoint","lipase_endpoint",
            "day_count","upload_day_%","note_count","light_G","light_Y","light_R","pic_count","carb_E%","protein_E%","fat_E%","calorie","pic_per_note","light_G_%","light_Y_%","light_R_%","fruits","vegetables","grains","meat_bean","milk","oil",
            "gp",
            "∆weight","∆bmi","∆bf","∆pbf","∆bsmi","∆bm","∆vfa","∆wc","∆ffm","∆bmr",
            "∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase",
            "∆weight%","∆bmi%","∆bf%","∆pbf%","∆bsmi%","∆bm%","∆vfa%","∆wc%","∆ffm%","∆bmr%",
            "∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆homa_beta%","∆tg%","∆tc%","∆hdl%","∆ldl%","∆lipase%"
  )

QQ1_stat_table_1st %<>% select(vars_en)


vars_ch <- c("id","client_type","年齡","gender","date_baseline","date_endpoint",
          "體重_baseline","BMI_baseline","體脂重_baseline","體脂率_baseline","骨骼肌指數_baseline","肌肉重_baseline","內臟脂肪_baseline","腰圍_baseline","除脂體重_baseline","基礎代謝率_baseline",
          "糖化血色素_baseline","空腹血糖_baseline","空腹胰島素_baseline","HOMA_IR_baseline","HOMA_Beta_baseline","三酸甘油脂_baseline","總膽固醇_baseline","HDL_baseline","LDL_baseline","解脂酶_baseline",
          "體重_endpoint","BMI_endpoint","體脂重_endpoint","體脂率_endpoint","骨骼肌指數_endpoint","肌肉重_endpoint","內臟脂肪_endpoint","腰圍_endpoint","除脂體重_endpoint","基礎代謝率_endpoint",
          "糖化血色素_endpoint","空腹血糖_endpoint","空腹胰島素_endpoint","HOMA_IR_endpoint","HOMA_Beta_endpoint","三酸甘油脂_endpoint","總膽固醇_endpoint","HDL_endpoint","LDL_endpoint","解脂酶_endpoint",
          "飲食紀錄日數","飲食紀錄完成率_%","飲食紀錄篇數","綠燈數","黃燈數","紅燈數","上傳照片數","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","每篇上傳照片數","綠燈率","黃燈率","紅燈率","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日",
          "gp",
          "∆體重","∆BMI","∆體脂重","∆體脂率","∆骨骼肌指數","∆肌肉重","∆內臟脂肪","∆腰圍","∆除脂體重","∆基礎代謝率",
          "∆糖化血色素","∆空腹血糖","∆空腹胰島素","∆HOMA_IR","∆HOMA_Beta","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL","∆解脂酶",
          "∆體重%","∆BMI%","∆體脂重%","∆體脂率%","∆骨骼肌指數%","∆肌肉重%","∆內臟脂肪%","∆腰圍%","∆除脂體重%","∆基礎代謝率%",
          "∆糖化血色素%","∆空腹血糖%","∆空腹胰島素%","∆HOMA_IR%","∆HOMA_Beta%","∆三酸甘油脂%","∆總膽固醇%","∆HDL%","∆LDL%","∆解脂酶%"
)

names(QQ1_stat_table_1st) <- vars_ch



#change colname to run plot
QQ1_stat_table_1st_for_plot <- QQ1_stat_table_1st
names(QQ1_stat_table_1st_for_plot) <- gsub("∆", "delta_", names(QQ1_stat_table_1st_for_plot))
names(QQ1_stat_table_1st_for_plot) <- gsub("%", "_percent", names(QQ1_stat_table_1st_for_plot))


#var_vector <- which(!(vars %in% c("id","client_type","gender","date_baseline","date_endpoint", "gp"))) #old method
  # #new
  # var_vector <- c(setdiff(vars %>% grep("baseline$", .), vars %>% grep("date", .)),
  #                 setdiff(vars %>% grep("endpoint$", .), vars %>% grep("date", .)),
  #                 vars %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE),
  #                 setdiff(vars %>% grep("[∆]", .), vars %>% grep("[%]", .)),
  #                 vars %>% grep("[%]", .)
  # )
#new
var_vector <- c(setdiff(vars_ch %>% grep("baseline$", .), vars_ch %>% grep("date", .)),
                setdiff(vars_ch %>% grep("endpoint$", .), vars_ch %>% grep("date", .)),
                vars_ch %>% grep("baseline$|endpoint$|[∆]|id|client|gender|gp", ., invert = TRUE),
                setdiff(vars_ch %>% grep("[∆]", .), vars_ch %>% grep("[%]", .)),
                intersect(vars_ch %>% grep("[∆]", .), vars_ch %>% grep("[%]", .))
)



myplots <- vector('list', length(var_vector))

for (i in c(var_vector)) {
  j <- match(i, var_vector)
  if (j == 1) {
    vector_pvalue <- c()
  }
  
  a <- QQ1_stat_table_1st_for_plot %>% colnames() %>% head(i) %>% tail(1)
  a_title <- a %>% gsub("delta_", "∆", .) %>% gsub("percent", "(%)", .)
  
  #p.sign?
  stat.test <- 
    QQ1_stat_table_1st_for_plot %>%
    group_by(gender) %>%
    rstatix::t_test(as.formula(paste(a, "gp", sep = " ~ ")), ref.group = "Poor") 
  stat.test <- stat.test %>% rstatix::add_xy_position(x = "gender", fun = "mean_se", dodge = 0.8)
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot <- 
    QQ1_stat_table_1st_for_plot %>% 
    ggbarplot(x = "gender", y = a, fill = "gp", alpha = 0.5,
              add = "mean_se", add.params = list(group = "gp"),
              position = position_dodge(0.8), legend = "right", legend.title = "") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    labs(x = "", y = "Mean ± SE", title = a_title) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15)
    ) +
    stat_pvalue_manual(
      stat.test, label = "p.adj.signif", tip.length = 0.0,
      bracket.nudge.y = 1, step.increase = 0.01, hide.ns = FALSE 
    )
  
  myplots[[j]] <- plot
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}



#(2.)Gender x Group table
table_01 <- 
  table(QQ1_stat_table_1st$gender, QQ1_stat_table_1st$gp) %>% addmargins() %>% 
  kable(format = "html", caption = "<b>Table: Stuty Group</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  footnote(general_title = c("Categorization:  ∆weight(%)"), general = c(rbind("\n", c("- Poor: Less than 3%", "- Medium: Between 5~10%", "- Good: More than 10%"))),
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .)


#(3.)output statistics table
#for customed summary table [summary table]
summary_table <- 
  QQ1_stat_table_1st %>% 
  group_by(gender, gp) %>% 
  summarize_at(vars_ch[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
summary_table <- cbind(summary_table %>% as.data.frame() %>% select(-c("gender", "gp")) %>% t(), as.data.frame(vector_pvalue)) 

names(summary_table) <- c(rep(c("Poor", "Medium", "Good"), 2), "顯著差異")

table_02 <- 
  summary_table %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1, "Female" = 3, "Male" = 3, " " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Good vs. Poor in female population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")



