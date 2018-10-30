library(MASS)
library(ggplot2)
library(ggthemes)
library(GGally)
library(magrittr)
library(imputeTS)
library(corrplot)
library(dplyr)
# 
# library(neuralnet)
# library(randomForest)

m = read.csv('massachusetts-public-schools-data/MA_Public_Schools_2017.csv')
dict = read.csv('massachusetts-public-schools-data/MA_Public_Schools_datadict.csv')
View(head(m))



# Explore lists features . . . . . . . . . . . . . . . . . . . . .. . . . . . .|||
m_names = data.frame("names" = colnames(m))
m_names$grep_test = grepl('X._Enrollment',m_names$names)
name_list = as.character(m_names$names)
q = paste0(name_list,collapse = ',')
# . . . . . . . . .. . . . . . . . . . . . . . . . . . . . . . . .. . . . . . .|||
#grad_look = m[,c(29,31,64,70,53)]

# Highschool Only ***************************************************************



# ********************************************************************************
#  --  Function: Analyze NAs  - -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# ********************************************************************************
na_analysis = function(df){
    na_count <- as.data.frame(sapply(df, function(y) sum(is.na(y))))
    names(na_count) = 'num_na'
    na_count$pct_nn = (nrow(df) - na_count$num_na)/(nrow(df))
    na_count$pct_na = 1 - na_count$pct_nn
    return(na_count)
}
# ********************************************************************************
# ^^^ -- END Function: Analyze NAs  ----------  -- -- -- -- -- ---- -- -- -- #
# ********************************************************************************


# ********************************************************************************
# -  -  - Prepare Correlation Data Function -------------------------------------- #
# ********************************************************************************
prep_corr_data = function(x){
    #hs2 = x[x$X12_Enrollment > 0,]
    hs2 = x[x$X12_Enrollment > 0, -grep("MCAS",colnames(x))]
    nums <- sapply(hs2, is.numeric)
    to_merge <- hs2[,colnames(hs2) %in% c('School.Name','School.Code')]
    hs_corr_data <- hs2[,nums]    
    corrdat = merge(to_merge,hs_corr_data)
    cornames = as.data.frame(colnames(corrdat))
    colnames(cornames) = 'names'
    cornames$new = tolower(gsub('X\\..','x_',cornames$names))
    cornames$new2 = gsub('\\.','_',cornames$new)
    cornames$new3 = gsub('progress_and_performance_index__ppi____',
                         'ppi_',cornames$new2)
    cornames$new4 = gsub('____','',cornames$new3)
    newnames = as.list(cornames$new4)
    colnames(corrdat) = newnames
    rownames(corrdat) = corrdat$school_name
    corrdat = corrdat[,!names(corrdat) %in% c('school_name','school_code',
                                              'district_code','zip','x1_enrollment',
                                              'x2_enrollment','x3_enrollment',
                                              'x4_enrollment','x5_enrollment',
                                              'x6_enrollment','x7_enrollment',
                                              'x8_enrollment','total___of_classes',
                                              'ap_five_or_more_tests',
                                              'ap_four_tests','ap_three_tests',
                                              'ap_two_tests','ap_one_test')]
    
    # !!! This is the step where all records are eliminated if not drop MCAS cols .... <<<
    corrdat = na.omit(corrdat)
    return(corrdat)
}
# ********************************************************************************
# -  -  - END - Prepare Correlation Data Function --------------------------------- #
# ********************************************************************************



# ********************************************************************************
# -  -  -  - Expand Data Function --------------------------------- #
# ********************************************************************************

expand_dataset = function(data,ex_df){
    mcas_cols = ex_df[ex_df$X12_Enrollment > 0, grep("MCAS|School.Name",colnames(ex_df))]
    n = sapply(mcas_cols,is.numeric)
    mcas = mcas_cols[,n]
    mcas_narm = na.replace(mcas,0)
    mcas_narm = mcas_narm[,grep("10th",colnames(mcas_narm))]
    mcas_narm = mcas_narm[,grep("X\\.",colnames(mcas_narm))]
    u = as.data.frame(mcas_cols[,colnames(mcas_cols) %in% 'School.Name'])
    mcas_full = cbind(u,mcas_narm)
    colnames(mcas_full)[1] = 'School.Name'
    data$School.Name = rownames(data)
    full = merge(data, mcas_full, by="School.Name")
    rownames(full) = full$School.Name
    full = full[,-1]
}
# ********************************************************************************
# -  -  - END - Expand Data Function --------------------------------- #
# ********************************************************************************




#  ::::::::::::::::::::::::::::: General Calls:::::::::::::::::::::::::::::|||


na_counts=na_analysis(m)
#m_new = m[,-c(16:25,32:50,63,65:69,71:79,94,299:302)]
m_new = m[,-c(16:28,30:63,65:79,92:94,294:302)]
xy = prep_data(m_new)
full = expand_dataset(xy,m_new)

clean = clean_data(m)

# 
# # / # / # / - - -- - -- - -     Everfi Impact   - - -- - -- - - \ * \ * \ 
# # / # / # / - - -- - -- - -     Everfi Impact   - - -- - -- - - \ * \ * \ 
# # / # / # / - - -- - -- - -     Everfi Impact   - - -- - -- - - \ * \ * \ 
# 
# full = full[order(full$average_sat_math),]
# full$math_inc = (.02 * full$average_sat_math) + full$average_sat_math
# full$math_inc[30:295] = full$average_sat_math[30:295]
# 
# full$writ_inc = (.02 * full$average_sat_writing) + full$average_sat_writing
# full$writ_inc[30:295] = full$average_sat_writing[30:295]
# 
# full$read_inc = (.02 * full$average_sat_reading) + full$average_sat_reading
# full$read_inc[30:295] = full$average_sat_reading[30:295]
# 
# impact = data.frame('average_math' = mean(full$average_sat_math),
#                'average_reading' = mean(full$average_sat_reading),
#                'average_writing' = mean(full$average_sat_writing),
#                'average_math_inc' = mean(full$math_inc),
#                'average_writing_inc' = mean(full$writ_inc),
#                'average_reading_inc' = mean(full$read_inc))
# # ^^^ --  -----------------  -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# 
# 



# . . . random views . . . . . .. . .. . .. . .. . .. . .. . .. . .. . .#



# ^^^ --  -----------------  -- -- -- -- -- -- -- -- -- -- -- -- -- -- #


# --------------------------------------------------------------------------------------------- #
# *********************************** CORRELATION ANALYSIS ********************************** #
# ***************************************************************************************** #


# - - - - - Correlations for full dataframe including MCAS
nf = full[,!colnames(full) %in% 'School.Name']
corr = cor(nf[,!colnames(nf) %in% c('x_graduated','x12_enrollment')], y = nf$x_graduated)
c = as.data.frame(corr)
colnames(c) = 'cor'
rownames(c) = rownames(corr)
c$cor = round(c$cor,3)
v = c

#eliminate low cor vars
#c = subset(v, cor > 0)



# = = = = = Correlations for non-MCAS 
nf = xy[,!colnames(xy) %in% 'School.Name']
corr = cor(nf[,!colnames(nf) %in% c('x_graduated','x12_enrollment')], y = nf$x_graduated)
c = as.data.frame(corr)
colnames(c) = 'cor'
rownames(c) = rownames(corr)
c$cor = round(c$cor,3)
v = c



#Correlation to Outcome Graph

ggplot(c, aes(x=reorder(rownames(c),cor), y=cor, label=cor, fill=cor)) + 
    geom_bar(stat = 'identity', alpha = 0.78)+
    scale_fill_gradient(low = "salmon", high = "seagreen")+
    labs(x = "Variable", y = "Correlation to % of Graduating Students") +
    ggtitle("How Strongly Correlated are Students' Performance Scores to Graduation Rates?",
            subtitle = "Across 296 Massachussetts Highschools") +
    geom_label(aes(fill = c$cor), colour = "white", fontface = "bold")+
    coord_flip()+
    theme_hc()+
    guides(fill = FALSE)+
    theme(axis.line = element_blank(),
          axis.text.y = element_text(size=14),
          panel.grid.major.y = element_line(colour = 'lightblue', linetype = 2),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.background = element_rect(fill = "white")
          )+
    annotate("text", label = "SAT Scores show the strongest\n postive correlation to Graduation Rate (%).", 
              x = 7.3, y = -0.19, size = 4.2, colour = "black")#+
    # annotate("text", label = "More variables must be accounted for.", 
    #          x = 3.2, y = -0.3, size = 4, colour = "black")


# ************ ^ ^ ^ ^ **************************************************************** ^ ^ ^ ^ ********** #
# ************************************************************************************************************ #


# ************************************************************************************************************ #
#  --  Machine Learning -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# ************************************************************************************************************ #
library(caret)
library(neuralnet)
library(kernlab)
library(randomForest)
#library(imputeTS)
#xdat = na.replace()
model = lm(x_graduated ~. - 1,
           as.data.frame(xdat))
modelSelection <- stepAIC(model, direction=c('backward'))
summary(modelSelection)
plot(model)
summary(model)

#Scaled Dataframe
xdat = as.matrix(scale(full))

inTrain <- createDataPartition(y = full$x_graduated, p = 0.75, list = FALSE)
training <- xdat[inTrain, ]
testing <- xdat[-inTrain, ]


#Using Full Dataframe
inTrain <- createDataPartition(y = full$x_graduated, p = 0.75, list = FALSE)
training <- full[inTrain, ]
testing <- full[-inTrain, ]

# ------ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# ------ -- Random Forest Regression   -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #
# ------ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #


modelFit = randomForest(x_graduated ~., data = training)

round(importance(modelFit),2)
# ------ -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- #

# *

# *

# *

