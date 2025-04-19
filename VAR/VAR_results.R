library(tseries)
library(vars)
library(vegan)
library(xtable)
library(stargazer)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)


#
cbbPalette <- c("#000000", "red", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Data Processing

source("IRF_flexible_shock.R")

load("dataset/topic_mapping.RData")


features <- c("topic1sent1",
              "topic6sent1",
              "topic8sent1",
              "topic2sent1",
              "topic3sent3",
              "topic5sent3")
labels <- c("Election Adminstrative-Support", 
            "Mail/Registration Vote Guidance",
            "Election Adminstrative-Early Vote Help",
            "GOP Lack of Faith in GOP LEOs",
            "Trump Won",
            "Decertify the Vote")



labelled_data <- read.csv("../dataset/hand_labeled.csv", colClasses=c("handle"="character"))
labelled_data <- subset(labelled_data, select = c(handle, classification))

jst_data      <- read.csv("../dataset/jst_results_new.csv",  colClasses=c("handle"="character"))

var_data <- merge(x = jst_data, y = labelled_data, by = "handle")
var_data <- subset(var_data, select = -c(X, docID))
var_data <- filter(var_data, classification != -1)
var_data <- subset(var_data, select=c("handle",
                                      "classification",
                                      features))


time_df     <- read.csv("../dataset/alltweets.csv", colClasses=c("docID"="character"))
time_df <- time_df %>% rename( handle = author_id)


var_data_time <- left_join(time_df, var_data, by='handle', multiple='any', unmatched='drop')
var_data_time <- var_data_time %>% drop_na()
var_data <- var_data_time %>%mutate(date = as.Date(created_at))




## VAR

# Create group-wide time series of daily topic probabilities for LEOs and Non LEOs
df <- var_data
df <- df %>% rename( LEO = classification)
LEO_vec <- df  %>% 
  group_by(date,LEO)        %>% 
  filter(LEO==1,year(date)>=2020)        %>%
  summarise(across(topic1sent1:topic5sent3,mean)) %>%
  mutate(handle="LEO")      %>%
  dplyr::select(date, handle,topic1sent1:topic5sent3)

NON_vec <-  df %>% 
  group_by(date,LEO)        %>% 
  filter(LEO ==0,year(date)>=2020)        %>%
  summarise(dplyr::across(topic1sent1:topic5sent3,mean)) %>%
  mutate(handle="Non-LEO")      %>%
  dplyr::select(date, handle,topic1sent1:topic5sent3)




Org_vec_all <- rbind(LEO_vec,NON_vec)
Org_vec_wide <- Org_vec_all%>% ungroup()        %>% 
  dplyr::select(date, handle,topic1sent1:topic5sent3) %>%
  tidyr::pivot_wider(names_from=handle,
                     values_from=c(topic1sent1:topic5sent3)) 

Org_vec_wide <- Org_vec_wide %>% 
  na.omit()




################################
irf_mat <- list()
temp.irf <-list()
idx      <- 1
idx_var  <- 1
var_irf_list <-list()

adftests <-list()
kpsstests <-list()




temp <- Org_vec_wide %>% dplyr::select(starts_with("topic"))

ts.mat         <- ts(temp)
# do all columns instead of 1:2
ts.mat   <- log(ts.mat /(1-ts.mat))





VAR.temp <- VAR(ts.mat, 1, type = "const")



var_irf_list[[idx_var]] <- irf_corrected(VAR.temp,
                                         n.ahead= 15,
                                         runs   = 500,
                                         cumulative = T,
                                         ortho = F,
                                         boot = T)

irf_mat[[idx]]  <-list()
temp.irf[[idx]] <-list()

# for all i in var_irf_list[[idx_var]]$irf[[i]]
for(i in 1:12)
{
  IRF<-DeltaLOtoDeltaProb( x= var_irf_list[[idx_var]]$irf[[i]][15,-i],
                           impulse=(ts.mat[,i]),
                           response=(ts.mat[,-i]),
                           logodds=T)
  Lower<-DeltaLOtoDeltaProb(  var_irf_list[[idx_var]]$Lower[[i]][15,-i],
                              impulse=(ts.mat[,i]),
                              response=(ts.mat[,-i]),
                              
                              logodds=T)
  Upper<-DeltaLOtoDeltaProb(  var_irf_list[[idx_var]]$Upper[[i]][15,-i],
                              impulse=(ts.mat[,i]),
                              response=(ts.mat[,-i]),
                              
                              logodds=T)
  
  
  
  
  irf_mat[[idx]]            <- data.frame(t(IRF))[,]  
  temp.irf[[idx]]           <- data.frame(IRF = (IRF),
                                          Lower = (Lower),
                                          Upper = (Upper),
                                          impulse = colnames(ts.mat)[i],
                                          response = colnames(ts.mat)[-i])
  
  names(irf_mat[[idx]])     <- colnames(ts.mat)[-i]
  
  
  
  
  adftests[[idx]] <- sapply(ts.mat,tseries::adf.test,k=trunc((nrow(ts.mat)-1)^(1/3)))
  kpsstests[[idx]] <- lapply(ts.mat,tseries::kpss.test,null="Trend")
  adftests[[idx]] <- sapply(ts.mat,FUN=function(x) {temp <- tseries::adf.test(x,k=trunc((nrow(ts.mat)-1)^(1/3)))
  return(temp$statistic) })
  
  kpsstests[[idx]] <- sapply(ts.mat,FUN=function(x) {temp <- tseries::kpss.test(x,null="Trend")
  return(temp$statistic) })
  
  
  idx <-idx+1
}



baserates <- data.frame(BaseRates=colMeans(as.matrix(Org_vec_wide[,2:13])),
                        influenced_topic=names(colMeans(as.matrix(Org_vec_wide[,2:13]))))

irf_df <- data.table::rbindlist(temp.irf)


# run as is
irf_df <- irf_df %>% mutate(topic = sub("_.*","",impulse),
                            topic_check =sub("_.*","",response),
                            influenced = sub(".*_","",response),
                            influencer = sub(".*_","",impulse))

irf_df <- irf_df %>% mutate(influenced_topic = sub("ts.mat.","",response))

irf_df <- irf_df %>% left_join(baserates, by="influenced_topic")
irf_df <- irf_df %>% mutate(influenced_topic_name = sub("_LEO", "", influenced_topic),
                            influenced_topic_name = sub("_Non-LEO", "", influenced_topic_name))

irf_df <- irf_df %>% left_join(topic_mapping, by = c("influenced_topic_name" = "topic_titles"))



# dividing plots by impulse applied
# we want to see how impulses in LEO topics by LEOs affect the public
# and how impulses in public topics by the public affect LEOs


# LEO topics influencing the public

irf_df_t1s1 <- irf_df %>% filter(impulse=="ts.mat.topic1sent1_LEO") %>% filter(influenced=="Non-LEO")
irf_df_t1s1$delta <- DeltaLOtoDeltaProb_value(irf_df_t1s1$IRF,irf_df_t1s1$BaseRates)
irf_df_t1s1 <- irf_df_t1s1[order(delta),]
irf_df_t1s1$topic_labels <- factor(irf_df_t1s1$topic_labels, levels = irf_df_t1s1$topic_labels)

irf_df_t6s1 <- irf_df %>% filter(impulse=="ts.mat.topic6sent1_LEO") %>% filter(influenced=="Non-LEO")
irf_df_t6s1$delta <- DeltaLOtoDeltaProb_value(irf_df_t6s1$IRF,irf_df_t6s1$BaseRates)
irf_df_t6s1 <- irf_df_t6s1[order(delta),]
irf_df_t6s1$topic_labels <- factor(irf_df_t6s1$topic_labels, levels = irf_df_t6s1$topic_labels)

irf_df_t8s1 <- irf_df %>% filter(impulse=="ts.mat.topic8sent1_LEO") %>% filter(influenced=="Non-LEO")
irf_df_t8s1$delta <- DeltaLOtoDeltaProb_value(irf_df_t8s1$IRF,irf_df_t8s1$BaseRates)
irf_df_t8s1 <- irf_df_t8s1[order(delta),]
irf_df_t8s1$topic_labels <- factor(irf_df_t8s1$topic_labels, levels = irf_df_t8s1$topic_labels)




# nonLEO topics influencing LEOs
irf_df_t2s1 <- irf_df %>% filter(impulse=="ts.mat.topic2sent1_Non-LEO") %>% filter(influenced=="LEO")
irf_df_t2s1$delta <- DeltaLOtoDeltaProb_value(irf_df_t2s1$IRF,irf_df_t2s1$BaseRates)
irf_df_t2s1 <- irf_df_t2s1[order(delta),]
irf_df_t2s1$topic_labels <- factor(irf_df_t2s1$topic_labels, levels = irf_df_t2s1$topic_labels)

irf_df_t3s3 <- irf_df %>% filter(impulse=="ts.mat.topic3sent3_Non-LEO") %>% filter(influenced=="LEO")
irf_df_t3s3$delta <- DeltaLOtoDeltaProb_value(irf_df_t3s3$IRF,irf_df_t3s3$BaseRates)
irf_df_t3s3 <- irf_df_t3s3[order(delta),]
irf_df_t3s3$topic_labels <- factor(irf_df_t3s3$topic_labels, levels = irf_df_t3s3$topic_labels)

irf_df_t5s3 <- irf_df %>% filter(impulse=="ts.mat.topic5sent3_Non-LEO") %>% filter(influenced=="LEO")
irf_df_t5s3$delta <- DeltaLOtoDeltaProb_value(irf_df_t5s3$IRF,irf_df_t5s3$BaseRates)
irf_df_t5s3 <- irf_df_t5s3[order(delta),]
irf_df_t5s3$topic_labels <- factor(irf_df_t5s3$topic_labels, levels = irf_df_t5s3$topic_labels)



pdf("../plots/VAR_t1s1_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t1s1$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t1s1) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) + 
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette)+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="A: Effect of 'Election Adminstrative-Support' on Public") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=23, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 1.0),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()


pdf("../plots/VAR_t6s1_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t6s1$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t6s1) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) +
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette)+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="E: Effect of 'Mail/Registration Vote Guidance' on Public") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=23, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 1.0),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()

pdf("../plots/VAR_t8s1_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t8s1$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t8s1) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) + 
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette)+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="C: Effect of 'Election Admin.-Early Vote Help' on Public") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=22, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 1.1),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()




pdf("../plots/VAR_t2s1_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t2s1$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t2s1) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) +
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette[2])+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="D: Effect of 'GOP Lack of Faith in GOP LEOs' on LEOs") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=23, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 1.0),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()

pdf("../plots/VAR_t3s3_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t3s3$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t3s3) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) + 
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette[2])+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="B: Effect of 'Trump Won' on LEOs") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=23, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 2.3),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()

pdf("../plots/VAR_t5s3_lag_days_1.pdf",height=6,width=9)
g<- ggplot(aes( irf_df_t5s3$delta,
                topic_labels,
                colour= influenced),
           data=irf_df_t5s3) + 
  geom_pointrange(aes(xmin =  DeltaLOtoDeltaProb_value(Lower,BaseRates), 
                      xmax =  DeltaLOtoDeltaProb_value(Upper,BaseRates)),
                  linetype="solid",
                  position=position_dodge(0.4)) + 
  xlim(-10, 7) +
  scale_color_manual(values=cbbPalette[2])+
  geom_vline(aes(xintercept=0), linetype="solid")+
  ylab("Sentiment-Topic") +
  xlab("Influence") +
  labs(title="F: Effect of 'Decertify the Vote' on LEOs") +
  theme(axis.text = element_text(size = 1))+
  theme_bw() +
  theme(text = element_text(size=23, family='serif'),
        legend.position = "none",
        plot.title = element_text(hjust = 1.5),
        axis.text.y = element_text(size=20))
plot(g)
dev.off()


