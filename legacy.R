
m=m_new

# - - - - - - - - - - Below non-Functionalized for reference >> - - - -- - - - - - - - - -  - - - - - - - - - - #
hs2 = m[m$X12_Enrollment > 0, -grep("MCAS",colnames(m))]
modenums <- sapply(hs2, is.numeric)
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
                                          'x8_enrollment','total___of_classes')]

# !!! This is the step where all records are eliminated if not drop MCAS cols .... <<<
corrdat = na.omit(corrdat)


# - - - - - - - - - - non-Functionalized >> - - - - - - - - - - - - - - #
# - - - - - - - - - - non-Functionalized >> - - - - - - - - - - - - - - #
hs2 = m[m$X12_Enrollment > 0, -grep("MCAS",colnames(m))]
hs2 = m[m$X12_Enrollment > 0,]
nums <- sapply(hs2, is.numeric)
to_merge <- hs2[,colnames(hs2) %in% c('School.Name','School.Code')]
hs_corr_data <- hs2[,nums]    
corrdat = merge(to_merge,hs_corr_data)
cornames = as.data.frame(colnames(corrdat))
colnames(cornames) = 'names'
cornames$new = tolower(gsub('X\\..','x_',cornames$names))
cornames$new2 = gsub('\\.','_',cornames$new)
cornames$new3 = gsub('progress_and_performance_index__ppi____','ppi_',cornames$new2)
newnames = as.list(cornames$new2)
colnames(corrdat) = newnames
rownames(corrdat) = corrdat$school_name
corrdat = corrdat[,!names(corrdat) %in% c('school_name','school_code',
                                          'district_code','zip','x1_enrollment',
                                          'x2_enrollment','x3_enrollment',
                                          'x4_enrollment','x5_enrollment',
                                          'x6_enrollment','x7_enrollment',
                                          'x8_enrollment')]
corrdat = na.omit(corrdat)
#hs_corr_data <- cbind(hs2$School.Name,hs_corr_data)   
#hs_corr_data <- merge(hs[,]$School.Name,hs_corr_data, by = 'School.Code')

# -)-( GET MCAS COLUMNS )-(-)-(-)-NON FUNCTIONALIZED <><><> )-(-)-(-)-(-)-(-)-(-
#)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)- ***
mcas_cols = m[m$X12_Enrollment > 0, grep("MCAS|School.Name",colnames(m))]
### Step 1: Get numeric mca columns only
n = sapply(mcas_cols,is.numeric)
mcas = mcas_cols[,n]
mcas_narm = na.replace(mcas,0)
u = as.data.frame(mcas_cols[,colnames(mcas_cols) %in% 'School.Name'])
mcas_full = cbind(u,mcas_narm)
colnames(mcas_full)[1] = 'School.Name'
### Step 2: Append only relevant columns to dataset
mcas_narm = mcas_narm[,grep("10th",colnames(mcas_narm))]
full = merge(xy, mcas_full, by="School.Name")
#check = cbind(u,mcas_cols$School.Name)
# mca_nulls = na_analysis(mcas)
#)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)- ***
#)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-(-)-


# -^-  -^-  -^-  -^- -^- END non-Functionalized >> - - - -  - -  - -  - -  - -  - -  - -  - - - - - - - - #
# -^-  -^-  -^-  -^- -^- END non-Functionalized >> - - - -  - -  - -  - -  - -  - -  - -  - - - - - - - - #
# -^-  -^-  -^-  -^- -^- END non-Functionalized >> - - - -  - -  - -  - -  - -  - -  - -  - - - - - - - - #





# .

# .

# .