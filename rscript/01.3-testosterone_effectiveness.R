# 20230214 Dr. Lee analysis request:  Testosterone x Effectiveness

# 主要想看 cutoff< 350 ng/dl 這幾個人(Male) 肥胖 胰島素阻抗 腰圍 體脂肪
# group by cutoff, gender: N, pie_chart, univariate correlation, baseline, effectiveness w/ sign.symbol

# #pie
#   googleVis::gvisMerge(pie_testosterone_01, pie_testosterone_02) %>% plot()
# #[Correlation r] Testosterone x Baseline
#   corrplot(M_testosterone,
#            p.mat = M_testosterone_test$p,
#            type = "lower",
#            insig = "label_sig",
#            sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#            tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#            cl.ratio = 0.1,
#            col = M_col(200),
#            title = "[Correlation] Efficacy x Baseline",
#            #c(bottom, left, top, right)
#            mar = c(0,0,1,0))
# #[Correlation r] table
#   cor_table_testosterone
# #>>>> Low T, High Weigt/Fat/Blood/Insulin/IR, IR improvement越差
# #[boxplot]
#   plot_grid(plotlist = myplots_plot_testosterone[84:87], ncol = 4, labels = paste0(LETTERS[6], seq(1,10)))
#   table_02_testosterone


# 0. Global setting / exploration -------------------------------------------------


cutoff_testosterone = 3.5 #ng/ml

table((stat_table_1st_ob$testosterone_baseline > cutoff_testosterone), stat_table_1st_ob$gender) %>% addmargins()
table((stat_table_1st_ob$testosterone_baseline > cutoff_testosterone), stat_table_1st_ob$gender) %>% prop.table() %>% addmargins() %>% round(2)





# 1. pie ------------------------------------------------------------------
a <- stat_table_1st_ob %>% filter(gender == "male")

a$age_gp <- cut(a$age, c(0,25,29.5,34.5,39.5,44.5,49.5,54.5,59.5,64.5,69.5,100), c("<25", "25-29", "30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69",">70"))
a$gp_testosterone <- a$testosterone_baseline %>% cut(c(-Inf, cutoff_testosterone, Inf), c("Low_testosterone", "Normal"))

a <- a[complete.cases(a$testosterone_baseline),]


pie_testosterone_01 <- 
  a %>% group_by(gp_testosterone, age_gp) %>% summarise(n = n()) %>% as_tibble() %>% filter(gp_testosterone != "Normal") %>% select(-gp_testosterone) %>% gvisPieChart(options = list(title = 'Low Testosterone',
                                                                                                                                                                                      legend = "{position:'right'}",
                                                                                                                                                                                      pieHole = 0.5,
                                                                                                                                                                                      #slices = "{0:{offset:0.1}}",
                                                                                                                                                                                      backgroundColor = "#edeff2",
                                                                                                                                                                                      #colors = "['#DC3912', '#3366CC']",
                                                                                                                                                                                      width = "600",
                                                                                                                                                                                      height = "400"))
pie_testosterone_02 <- 
  a %>% group_by(gp_testosterone, age_gp) %>% summarise(n = n()) %>% as_tibble() %>% filter(gp_testosterone == "Normal") %>% select(-gp_testosterone) %>% gvisPieChart(options = list(title = 'Normal',
                                                                                                                                                                                      legend = "{position:'right'}",
                                                                                                                                                                                      pieHole = 0.5,
                                                                                                                                                                                      #slices = "{0:{offset:0.1}}",
                                                                                                                                                                                      backgroundColor = "#edeff2",
                                                                                                                                                                                      #colors = "['#DC3912', '#3366CC']",
                                                                                                                                                                                      width = "600",
                                                                                                                                                                                      height = "400"))
#googleVis::gvisMerge(pie_testosterone_01, pie_testosterone_02) %>% plot()



# 2. Correlation ----------------------------------------------------------
  #male


#[Create profile]  Efficacy, Baseline, Diet table
profile_efficacy <- stat_table_1st_ob %>% filter(gender == "male") %>% 
  select(c("∆weight%","∆bf%","∆bm%","∆vfa","∆wc","∆bmr","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase"))
names(profile_efficacy) <- c("∆體重(%)", "∆體脂(%)","∆肌肉(%)","∆內臟脂肪","∆腰圍", "∆BMR", #6
                             "∆糖化血色素","∆空服血糖","∆空腹胰島素","∆Homa_IR","∆Homa_ß","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL", "∆解脂酶") #10
##add testosterone_baseline
profile_baseline <- stat_table_1st_ob %>% filter(gender == "male") %>%  
  select(c("age", "bmi_baseline","pbf_baseline","vfa_baseline","bsmi_baseline","bm_baseline","wc_baseline","bmr_baseline",
           "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline",
           "testosterone_baseline"))

names(profile_baseline) <- c("年齡", "BMI(T0)", "體脂率(T0)", "內臟脂肪(T0)", "BSMI(T0)", "肌肉重(T0)", "腰圍(T0)", "BMR(T0)",
                             "糖化血色素(T0)", "空服血糖(T0)", "空腹胰島素(T0)", "HOMA_IR(T0)", "HOMA_ß(T0)", "三酸甘油脂(T0)", "總膽固醇(T0)", #17
                             "HDL(T0)", "LDL(T0)", "解脂酶(T0)", "睪固酮(T0)")

profile_diet <- stat_table_1st_ob %>% filter(gender == "male") %>%  
  select(c("upload_day_%", "pic_count","calorie","carb_E%","protein_E%","fat_E%","fruits","vegetables","grains","meat_bean","milk", "oil","light_G_%","light_Y_%","light_R_%"))
names(profile_diet) <- c("上傳天數%","上傳照片數", "總攝取卡路里","總碳水比_E%","總蛋白比_E%","總脂肪比_E%",
                         "水果(日)","蔬菜(日)","全穀雜糧(日)","蛋豆魚肉(日)","乳品(日)","油脂(日)",
                         "綠燈比_%","黃燈比_%","紅燈比_%")



a <-  cbind(-profile_efficacy, testosterone_baseline = stat_table_1st_ob[stat_table_1st_ob$gender == "male", "testosterone_baseline"] %>% as.vector())

#rm NA obs.
a <- a[complete.cases(a$testosterone_baseline),]


aa1 <- 
  a %>% dplyr::rename("weight_p" = "∆體重(%)") %>% 
  ggscatter(x = "testosterone_baseline", y = "weight_p",
            color = "black",
            fill = "red",
            shape = 21,
            size = 1,
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            xlab = "testosterone_baseline(ng/ml)",
            ylab = "∆Weight(%)",
            xlim = c(0, 7),
            ylim = c(0, 30),
  ) +
  geom_vline(xintercept = c(3.5),linetype ="dashed", ) +
  annotate("text", x=3.4, y=25, label="Cutoff = 350 ng/dl", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 4, label.y = 25) # Add correlation coefficient



aa2 <- 
  a %>% dplyr::rename("fat_p" = "∆體脂(%)") %>%
  ggscatter(x = "testosterone_baseline", y = "fat_p",
            color = "black",
            fill = "red",
            shape = 21,
            size = 1,
            add = "reg.line",  # Add regressin line
            add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
            conf.int = TRUE, # Add confidence interval
            xlab = "testosterone_baseline(ng/ml)",
            ylab = "∆Fat(%)",
            xlim = c(0, 7),
            ylim = c(0, 30),
  ) +
  geom_vline(xintercept = c(3.5),linetype ="dashed", ) +
  annotate("text", x=3.4, y=25, label="Cutoff = 350 ng/dl", angle=90) +
  stat_cor(method = "pearson", size = 5, label.x = 4, label.y = 25) # Add correlation coefficient

#plot_grid(aa1, aa2, labels = LETTERS)




#[Correlation r] Efficacy x Baseline
library(corrplot)
M_testosterone <- cor(cbind(-profile_efficacy, profile_baseline), use = "pairwise.complete.obs")
#[2Do]change row,col names into chinese
M_testosterone_test <- cor.mtest(cbind(-profile_efficacy, profile_baseline) , conf.level = .95)
M_col <- colorRampPalette(c("#4477AA", "#77AADD", "#FFFFFF", "#EE9988", "#BB4444"))

#run corrplot plot
# corrplot(M_testosterone,
#          p.mat = M_testosterone_test$p,
#          type = "lower",
#          insig = "label_sig",
#          sig.level = c(.001, .01, .05), pch.cex = .8, pch.col = "black",
#          tl.col = "black", tl.srt = 35, tl.cex = 1.0,
#          cl.ratio = 0.1,
#          col = M_col(200),
#          title = "[Correlation] Efficacy x Baseline",
#          #c(bottom, left, top, right)
#          mar = c(0,0,1,0))



for (i in c(1:length(colnames(M_testosterone)))) {
  if (i == 1) {
    M2_value <- M_testosterone %>% round(2) 
    M2_sign <- M_testosterone_test$p %>% stats::symnum(corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", "ns")) %>% as.data.frame.matrix()
    M_testosterone_test_df <- M2_value
    M_testosterone_test_df[,] <- NA
  }
  
  M_testosterone_test_df[,i] <- paste0(M2_value[,i], " (", M2_sign[,i], ")")
  
  if (i ==  length(colnames(M_testosterone))) {
    rm(list = c("M2_value", "M2_sign"))
    M_testosterone_test_df <- M_testosterone_test_df %>% as.data.frame()
    M_testosterone_test_df <- M_testosterone_test_df %>% add_column(vars = rownames(M_testosterone_test_df), .before = names(M_testosterone_test_df)[1])
  }
  
} 
cor_table_testosterone <- M_testosterone_test_df %>% gvisTable(options=list(showRowNumber = TRUE, height=300))


#>>>> Low T, High Weigt/Fat/Blood/Insulin/IR, IR improvement越差

# Effectiveness --------------------------------------------------------------------

#divide gp_testosterone
a <- stat_table_1st_ob %>% filter(gender == "male")

a$gp_testosterone <- a$testosterone_baseline %>% cut(c(-Inf, cutoff_testosterone, Inf), c("Low_testosterone", "Normal"))

table(a$gp_testosterone) %>% addmargins()

a <- a[complete.cases(a$gp_testosterone),]



#turn ∆ into positve(reverse)
tmp_1 <- a %>% select(-grep("∆", names(a))) 
#rm NA vars
tmp_1 %<>% select(-grep("extracellular", names(tmp_1)))
tmp_1 %<>% select(-grep("wepa50", names(tmp_1)))
tmp_1 %<>% select(-grep("e2", names(tmp_1)))
tmp_1 %<>% select(-grep("hr", names(tmp_1)))


#Setting improvement direction
##Improvement: Uncertain, default setting
tmp_2 <- a %>% 
  select(c("∆bmr","∆bmr%", "∆lipase","∆lipase%"))
#觀察值不夠: select(c("∆extracellular_water_ratio","∆wepa50","∆bmr","∆extracellular_water_ratio%","∆wepa50%","∆bmr%","∆e2","∆testosterone","∆e2%","∆testosterone%"))

##Improvement: negative
tmp_3 <- a %>% select(c("∆weight","∆bmi","∆bf","∆pbf","∆vfa","∆wc","∆ffm","∆weight%","∆bmi%","∆bf%","∆pbf%","∆vfa%","∆wc%","∆ffm%","∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆tg","∆tc","∆ldl","∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆tg%","∆tc%","∆ldl%")) %>% multiply_by(-1)
##Improvement: positive
tmp_4 <- a %>% select(c("∆bsmi","∆bm","∆bsmi%","∆bm%","∆homa_beta","∆hdl","∆homa_beta%","∆hdl%"))

a <- Reduce(cbind,list(tmp_1, tmp_2, tmp_3, tmp_4), accumulate =FALSE) 

rm(list = c("tmp_1","tmp_2","tmp_3","tmp_4"))

#Sort var order: baseline, endpoint, diet, ∆, ∆%
vars_en <- c("id","client_type","age","gender","date_baseline","date_endpoint",
             "weight_baseline","bmi_baseline","bf_baseline","pbf_baseline","bsmi_baseline","bm_baseline","vfa_baseline","wc_baseline","ffm_baseline","bmr_baseline",
             "hba1c_baseline","glucose_ac_baseline","insulin_baseline","homa_ir_baseline","homa_beta_baseline","tg_baseline","tc_baseline","hdl_baseline","ldl_baseline","lipase_baseline", "testosterone_baseline",
             "weight_endpoint","bmi_endpoint","bf_endpoint","pbf_endpoint","bsmi_endpoint","bm_endpoint","vfa_endpoint","wc_endpoint","ffm_endpoint","bmr_endpoint",
             "hba1c_endpoint","glucose_ac_endpoint","insulin_endpoint","homa_ir_endpoint","homa_beta_endpoint","tg_endpoint","tc_endpoint","hdl_endpoint","ldl_endpoint","lipase_endpoint",
             "day_count","upload_day_%","note_count","light_G","light_Y","light_R","pic_count","carb_E%","protein_E%","fat_E%","calorie","pic_per_note","light_G_%","light_Y_%","light_R_%","fruits","vegetables","grains","meat_bean","milk","oil",
             "gp_testosterone",
             "∆weight","∆bmi","∆bf","∆pbf","∆bsmi","∆bm","∆vfa","∆wc","∆ffm","∆bmr",
             "∆hba1c","∆glucose_ac","∆insulin","∆homa_ir","∆homa_beta","∆tg","∆tc","∆hdl","∆ldl","∆lipase",
             "∆weight%","∆bmi%","∆bf%","∆pbf%","∆bsmi%","∆bm%","∆vfa%","∆wc%","∆ffm%","∆bmr%",
             "∆hba1c%","∆glucose_ac%","∆insulin%","∆homa_ir%","∆homa_beta%","∆tg%","∆tc%","∆hdl%","∆ldl%","∆lipase%"
)

a %<>% select(vars_en)

vars_ch <- c("id","client_type","年齡","gender","date_baseline","date_endpoint",
             "體重_baseline","BMI_baseline","體脂重_baseline","體脂率_baseline","骨骼肌指數_baseline","肌肉重_baseline","內臟脂肪_baseline","腰圍_baseline","除脂體重_baseline","基礎代謝率_baseline",
             "糖化血色素_baseline","空腹血糖_baseline","空腹胰島素_baseline","HOMA_IR_baseline","HOMA_Beta_baseline","三酸甘油脂_baseline","總膽固醇_baseline","HDL_baseline","LDL_baseline","解脂酶_baseline", "睪固酮_baseline",
             "體重_endpoint","BMI_endpoint","體脂重_endpoint","體脂率_endpoint","骨骼肌指數_endpoint","肌肉重_endpoint","內臟脂肪_endpoint","腰圍_endpoint","除脂體重_endpoint","基礎代謝率_endpoint",
             "糖化血色素_endpoint","空腹血糖_endpoint","空腹胰島素_endpoint","HOMA_IR_endpoint","HOMA_Beta_endpoint","三酸甘油脂_endpoint","總膽固醇_endpoint","HDL_endpoint","LDL_endpoint","解脂酶_endpoint",
             "飲食紀錄日數","飲食紀錄完成率_%","飲食紀錄篇數","綠燈數","黃燈數","紅燈數","上傳照片數","碳水化合物_E%","蛋白質_E%","脂肪_E%","攝取熱量","每篇上傳照片數","綠燈率","黃燈率","紅燈率","水果攝取量_日","蔬菜攝取量_日","全穀雜糧攝取量_日","蛋豆魚肉攝取量_日","乳品攝取量_日","油脂攝取量_日",
             "gp",
             "∆體重","∆BMI","∆體脂重","∆體脂率","∆骨骼肌指數","∆肌肉重","∆內臟脂肪","∆腰圍","∆除脂體重","∆基礎代謝率",
             "∆糖化血色素","∆空腹血糖","∆空腹胰島素","∆HOMA_IR","∆HOMA_Beta","∆三酸甘油脂","∆總膽固醇","∆HDL","∆LDL","∆解脂酶",
             "∆體重%","∆BMI%","∆體脂重%","∆體脂率%","∆骨骼肌指數%","∆肌肉重%","∆內臟脂肪%","∆腰圍%","∆除脂體重%","∆基礎代謝率%",
             "∆糖化血色素%","∆空腹血糖%","∆空腹胰島素%","∆HOMA_IR%","∆HOMA_Beta%","∆三酸甘油脂%","∆總膽固醇%","∆HDL%","∆LDL%","∆解脂酶%"
)

names(a) <- vars_ch

b <- a
#change colname to run plot
QQ1_stat_table_1st_for_plot <- a
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




myplots_plot_testosterone <- vector('list', length(var_vector))

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
    rstatix::dunn_test(as.formula(paste(a, "gp", sep = " ~ ")),p.adjust.method = "bonferroni")
  stat.test <- stat.test %>% rstatix::add_y_position()
  
  
  #for customed summary table - part 1/4 [p value]
  vector_pvalue <- append(vector_pvalue, 
                          stat.test %>% select(p.adj.signif) %>% pull() %>% head(2) %>% tail(1)
  )
  
  #plot
  plot_testosterone <- 
    QQ1_stat_table_1st_for_plot %>% 
    ggboxplot(x = "gp", y = a, fill = "gp", alpha = 0.5, width = 0.5, 
              title = a_title,
              legend = "none", xlab = FALSE, ylab = FALSE) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      axis.text.x = element_text(angle = -30, vjust = 0.9, hjust = 0.05)
    ) +
    stat_pvalue_manual(stat.test, label = "p.adj.signif")
  
  myplots_plot_testosterone[[j]] <- plot_testosterone
  
  progress(j, max = length(var_vector))
  if (j == length(var_vector)) {
    cat("-----[Completed!!]-----", rep("\n", 3))
  }
}









#(3.)output statistics table
#for customed summary table [summary table]
summary_table_testosterone <- 
  b %>% select(var_vector, "gp") %>% 
  group_by(gp) %>% 
  summarize_at(vars_ch[var_vector],
               function(x) paste(mean(x, na.rm = TRUE) %>% round(2), (sd(x, na.rm = TRUE)/sqrt(n())) %>% round(2), sep = " ± ")
  )



#rbind: summary_table, p.adj.sign, dif, improvement
summary_table_testosterone <- cbind(summary_table_testosterone %>% as.data.frame() %>% select(-c("gp")) %>% t(), as.data.frame(vector_pvalue)) 

names(summary_table_testosterone) <- c(rep(c("Low", "Normal")), "顯著差異")

summary_table_testosterone <- 
rbind("人數" = table(b$gp) %>% as.numeric() %>% append(""), summary_table_testosterone)

table_02_testosterone <- 
  summary_table_testosterone %>% 
  kbl(format = "html", caption = "<b>Statistics:</b>", align = "c") %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            full_width = FALSE, font_size = 15) %>% 
  add_header_above(c(" " = 1,"Male" = 2, " " = 1)) %>% 
  footnote(general_title = c("Significance:"), general = "\n Comparison: Testosterone in male population.",
           footnote_as_chunk = T, title_format = c("italic", "underline", "bold")
  )%>% 
  gsub("font-size: initial !important;", 
       "font-size: 15pt !important;", 
       .) %>% 
  scroll_box(height = "500px", width = "100%")










