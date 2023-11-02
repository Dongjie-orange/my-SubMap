rm(list = ls()); gc()
library(readxl) 
library(tibble)
library(tidyverse)
library(cmapR)
#submap data doi: 10.1126/scitranslmed.aah3560
#Genepattern 

df <- read_xls('NIHMS927815-supplement-Tables_S1-S18.xls',sheet = 9)
df[df=='NA'] <- NA
df <- df %>% column_to_rownames('sample')

marker_gene <- colnames(df)[5:ncol(df)] # 251

df_c <- df %>% drop_na('aCTLA4_response') #53
exp_c <- cbind(Gene=df_c[,1],df_c[,5:ncol(df_c)]) 
pd_c <- df_c$aCTLA4_response 

df_p <- df %>% drop_na('aPD1_response') #46
exp_p <- cbind(Gene=df_p[,1],df_p[,5:ncol(df_p)]) 
pd_p <- df_p$aPD1_response 

load('/Users/djchen/Desktop/R data/A01_Data_Processing/PAAD/all_15_set.rds')
load('/Users/djchen/Desktop/R data/Case21 SE_TN/Result/04.os_validation/clin_for_validation.rds')
# traning_set=all_15_set$RJ
# traning_set=traning_set %>% column_to_rownames('ID')
# samegene=intersect(colnames(traning_set),marker_gene) #232
# 
# exp_rj=as.data.frame(t(traning_set[,samegene]))
# pd_rj=temp$RJ
# 
traning_set2=all_15_set$TCGA
traning_set2=traning_set2 %>% column_to_rownames('ID')
samegene=intersect(colnames(traning_set2),marker_gene) #232
exp_tcga=as.data.frame(t(traning_set2[,samegene]))
pd_tcga=temp$TCGA

exp_c=as.data.frame(t(exp_c[,samegene]))
exp_p=as.data.frame(t(exp_p[,samegene]))

# load('../../../Case18 pro_NAT/A02_Result_v2/8-Sub_clinical/Trans_info_new.Rdata')
# 
# samegene=intersect(rownames(trans_exp),marker_gene) #231
# trans_pd <- trans_pd %>% filter(SampleType=='PDAC')
# trans_pd <- trans_pd[order(trans_pd$group,decreasing = F),]
# trans_exp <- trans_exp[samegene,trans_pd$Trans_ID]
# 
# exp_c=as.data.frame(t(exp_c[,samegene]))
# exp_p=as.data.frame(t(exp_p[,samegene]))

# #Expression dataset---------------------------------------------------
# rj <- data.frame(NAME=row.names(trans_exp),Description=row.names(trans_exp))
# trans_exp <- cbind(rj,trans_exp)
# row.names(trans_exp) <- NULL
# write.table(trans_exp,file = "trans_exp.txt",sep = "\t",row.names = F,col.names = T,quote = F)
# # 用excel打开 
# # 第一行 #1.2
# # 第二行 基因数 样本数
# # 保存为tab分隔的txt文件 改后缀为gct
# #Phenotype labels---------------------------------------------------
# trans_pd$group=ifelse(trans_pd$group=='PDAC_1',1,2)
# group <- trans_pd$group
# group <- paste(group,collapse = " ")
# group <- c(paste(c(117,2,1),collapse = " "),"# 1 2",group)     #"2"代表两组 1为固定值
# write.table(group,file = "group.cls",col.names = F,row.names = F,quote = F,sep = '\t')



# #Expression dataset---------------------------------------------------
# rj <- data.frame(NAME=row.names(exp_rj),Description=row.names(exp_rj))
# exp_rj <- cbind(rj,exp_rj)
# row.names(exp_rj) <- NULL
# write.table(exp_rj,file = "exp_rj.txt",sep = "\t",row.names = F,col.names = T,quote = F)
# # 用excel打开 
# # 第一行 #1.2
# # 第二行 基因数 样本数
# # 保存为tab分隔的txt文件 改后缀为gct
# #Phenotype labels---------------------------------------------------
# pd_rj$group=ifelse(pd_rj$group=='High_SERS',1,2)
# group <- pd_rj$group
# group <- paste(group,collapse = " ")
# group <- c(paste(c(116,2,1),collapse = " "),"# 1 2",group)     #"2"代表两组 1为固定值
# write.table(group,file = "group_rj.cls",col.names = F,row.names = F,quote = F,sep = '\t')
# 
# 
#Expression dataset---------------------------------------------------
tcga <- data.frame(NAME=row.names(exp_tcga),Description=row.names(exp_tcga))
exp_tcga <- cbind(tcga,exp_tcga)  #232
row.names(exp_tcga) <- NULL
write.table(exp_tcga,file = "exp_tcga.txt",sep = "\t",row.names = F,col.names = T,quote = F)
#Phenotype labels---------------------------------------------------
pd_tcga$group=ifelse(pd_tcga$group=='High_ERS',1,2)
group <- pd_tcga$group
group <- paste(group,collapse = " ")
group <- c(paste(c(176,2,1),collapse = " "),"# 1 2",group)     #"2"代表两组 1为固定值
write.table(group,file = "group_tcga.cls",col.names = F,row.names = F,quote = F,sep = '\t')


#Expression dataset---------------------------------------------------
c <- data.frame(NAME=row.names(exp_c),Description=row.names(exp_c))  # n=53
exp_c <- cbind(c,exp_c)
row.names(exp_c) <- NULL
write.table(exp_c,file = "exp_c.txt",sep = "\t",row.names = F,col.names = T,quote = F)
#Phenotype labels---------------------------------------------------
pd_c=ifelse(pd_c=='response',1,2)
group <- pd_c
group <- paste(group,collapse = " ")
group <- c(paste(c(53,2,1),collapse = " "),"# 1 2",group)     #"2"代表两组 1为固定值
write.table(group,file = "group_c.cls",col.names = F,row.names = F,quote = F,sep = '\t')


#Expression dataset---------------------------------------------------
p <- data.frame(NAME=row.names(exp_p),Description=row.names(exp_p)) # n = 46
exp_p <- cbind(p,exp_p)
row.names(exp_p) <- NULL
write.table(exp_p,file = "exp_p.txt",sep = "\t",row.names = F,col.names = T,quote = F)
#Phenotype labels---------------------------------------------------
pd_p=ifelse(pd_p=='response',1,2)
group <- pd_p
group <- paste(group,collapse = " ")
group <- c(paste(c(46,2,1),collapse = " "),"# 1 2",group)     #"2"代表两组 1为固定值
write.table(group,file = "group_p.cls",col.names = F,row.names = F,quote = F,sep = '\t')



