library(ggfortify)

## Joint Topic-Sentiment Modeling 


# Load  NLP Libraries 
library(tidytext)
library(quanteda)
library(rJST)
library(stm)
library(text2vec)
library(topicmodels)
# Utilities 
library(dplyr, warn.conflicts=FALSE)
library(parallel)
library(ggplot2)
library(lubridate)
library(readr)
library(wesanderson)
library(zoo)
#cluster analysis 
library(factoextra)
library(fpc)
library(tidyr)
library(magrittr)
library(progress)


#### Load Cleaned Data 

load("../dataset/results10.RData")
labels  <- readr::read_csv("../dataset/hand_labeled.csv")
time_df <- readr::read_csv("../datasets/alltweets.csv")



df      <- get_parameter(result.10,"theta")
df_sent <- get_parameter(result.10,"pi")
df_sent <- df_sent %>%mutate(docID=rownames(df_sent))
df <-  df %>%left_join(df_sent,by=c("docID","handle"))

df_label     <- df[paste(df$handle) %in% paste(labels$id[labels$election_official!=-1]),]
df_label     <- df_label %>% left_join(labels%>%mutate(handle = (id)))

df_unlabelled <- df[paste(df$handle) %in% paste(labels$id[labels$election_official==-1]),]
df_unlabelled <- df_unlabelled %>% left_join(labels%>%mutate(handle = (id))) %>%select(-election_official)



theta <- get_parameter(result.10,"theta")

th <- theta
# find the max sentopic number
th <- th %>% mutate(max = do.call(pmax, subset(., select = topic1sent1:topic10sent3)))

# find the name of the max sentopic
th$max_sentopic <- topic_titles[max.col(th[3:32])]

# subset to the relevant columns
th <- th %>% select(docID, handle, max, max_sentopic)

# find the max of each sentopic
topic_maxes <- th %>% group_by(max_sentopic) %>% summarise(max = max(max, na.rm=TRUE))
topic_maxes <- topic_maxes %>% pull(max, max_sentopic)

# keep documents within 0.01 of the max sentopic score
th_summary <-  th %>% filter(topic_maxes[max_sentopic] - max <= 0.001)

th_tweets <- th_summary %>% 
  left_join(time_df%>%mutate(docID=paste(id) ,by=c("docID")) %>%
              dplyr::select(text,docID),
            by=c("docID"))

th_tweets <- th_tweets %>% distinct(text, .keep_all=TRUE) %>% arrange(max_sentopic)


# join with date
time_df<-time_df%>%mutate(docID=paste(id))
df <- df %>% left_join(time_df,by=c("docID","handle"="author_id")) %>%
  select(-tweet) %>%mutate(date = as.Date(created_at))

topic_freq <- df %>% ungroup()%>%
  mutate(name = names(df %>% select(topic1sent1:topic10sent3))[max.col(df %>% select(topic1sent1:topic10sent3))])%>%group_by(date) %>%mutate(total=n()) %>%group_by(date,name) %>%summarise(freq=n(),
                                                                                                                                                                                            total=mean(total)) 

time_series <- df %>%group_by(date)%>% summarise(across(-docID,mean))


topic_labels <- c("Election Adminstrative-Support","BLM Protests-Law Enforcement","Attacking Joeclyn Benson", #1
                  "GOP Lack of Faith in GOP LEOs","Biden/Trump News","Partisans Attacking Each Other", #2
                  "Election Adminstrative-Help Voters","Anti-Trump (Vulgar)","Trump Won", #3
                  "Record Turnout-CO","Sports","Griswold Election Policies in CO", #4
                  "Threats to Katie Hobbs","Pro-Mask-Pro-Vaccine","Decertify the Vote", #5
                  "Mail/Registration Vote Guidance","Supreme Court Rulings","Michigan Support Poll Workers", #6
                  "Support Trump Fraud Claims: anti-GOP","Press Conferences","Support Raffsenberger-Dems", #7
                  "Election Adminstrative-Early Vote Help","School Pandemic Policy","Legislation and Policy Moves", #8
                  "Dems Election Adminstrative-Support","Covid","Colorado USPS Court Case", #9
                  "Traffic News","Fight the Fraud","NFL-Sports") #10)


dem_supportGOP    <- c("topic7sent3","topic9sent1")
troll             <- c("topic5sent1","topic1sent3","topic3sent2","topic2sent3")
pro_leos_neutral  <- c("topic1sent1","topic4sent1","topic8sent1","topic3sent1","topic4sent3","topic6sent3","topic6sent1","topic9sent3")
anti_leos_right   <- c("topic7sent1","topic2sent1","topic5sent3","topic10sent2","topic3sent3")
domestic_politics <- c("topic1sent2","topic7sent2","topic2sent2","topic5sent2","topic8sent2","topic9sent2","topic8sent3","topic6sent2")




topic_titles <-c(sort(names(df)[3:32])[4:30],sort(names(df)[3:32])[1:3])
topic_mapping <- data.frame(topic_labels,topic_titles)
save(topic_mapping, file = "../dataset/topic_mapping.RData")

topic_freq<-topic_freq %>%
  left_join(topic_mapping,by=c("name"="topic_titles")) 


topic_freq <- data.frame(topic_freq) %>% 
  mutate(TopicType = case_when(name%in% dem_supportGOP ~"Dem-Inflected LEOs Support",
                               name%in% pro_leos_neutral ~"Election Administration Guidance",
                               name%in% anti_leos_right ~"GOP Hostility",
                               name%in% domestic_politics ~"Domestic Politics",
                               name%in% troll ~"Trolling",
                               TRUE ~"Other"))%>%
  group_by(TopicType,date) %>%summarise(freq = sum(freq),
                                        total=mean(total))


topic_titles = c('topic1sent1','topic2sent1','topic3sent1','topic4sent1','topic5sent1',
                 'topic6sent1','topic7sent1','topic8sent1','topic9sent1','topic10sent1',
                 'topic1sent2','topic2sent2','topic3sent2','topic4sent2','topic5sent2',
                 'topic6sent2','topic7sent2','topic8sent2','topic9sent2','topic10sent2',
                 'topic1sent3','topic2sent3','topic3sent3','topic4sent3','topic5sent3',
                 'topic6sent3','topic7sent3','topic8sent3','topic9sent3','topic10sent3')


df$max_sentopic <- topic_titles[max.col(df[3:32])]


df <- df %>%
  mutate(topic_label = case_when(max_sentopic %in% dem_supportGOP ~"Dem-Inflected LEOs Support",
                                 max_sentopic %in% pro_leos_neutral ~"Election Administration Guidance",
                                 max_sentopic %in% anti_leos_right ~"GOP Hostility",
                                 max_sentopic %in% domestic_politics ~"Domestic Politics",
                                 max_sentopic %in% troll ~"Trolling",
                                 TRUE ~"Other"))



df$topic_label <- as.factor(df$topic_label)
print(summary(df$topic_label))


write.csv(df, "../dataset/jst_results_new.csv")




# Visualizations

pdf(paste0("../plots/composition.pdf"))
ggplot(data=topic_freq, aes(x=date, y=rollapply(freq/total*100, width=14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"), fill=TopicType)) +
  geom_area(alpha=0.6 , size=0.5, colour="black")+
  scale_fill_manual(values=c("#046C9A","#ABDDDE","#D69C4E", "red","#ECCBAE", "#000000" ))+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="black",size=1)+
  theme_classic() +
  xlim(min((topic_freq$date+14),na.rm=T),max((topic_freq$date-14),na.rm=T))+
  ylim(0,100)+
  ylab("Share of Discussion (%)")+
  xlab("")  +
  scale_color_manual(guide=guide_legend(ncol=1)) +
  theme(legend.title    = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        text = element_text(family="serif",face="plain",size=23)) +
  guides(fill=guide_legend(ncol=2))
dev.off()



time_series<-time_series %>% select(date,topic1sent1:topic10sent3)%>%
  pivot_longer(cols = topic1sent1:topic10sent3) %>%
  left_join(topic_mapping,by=c("name"="topic_titles"))

pro_leos_neutral_short  <- c("topic1sent1","topic4sent1","topic8sent1","topic3sent1")

pdf(paste0("../plots/proLEOs-nonpartisan.pdf"))
ggplot(data=time_series %>% filter(name %in%pro_leos_neutral_short ),
       aes(x=date,y=value*100))+
  geom_line() +
  ylab("Share of Daily Discussion (%)")+
  xlab("")+
  ylim(c(0,80))+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="red",lty=2)+
  facet_wrap(~as.factor(topic_labels),nrow=4,scale="fixed",strip.position = "top")+
  theme(text = element_text(family="serif",
                            face="plain",size=23),
                            panel.background=element_blank(),
                            axis.line = element_line(colour = "black"),
                            panel.grid.major = element_line(colour = 'lightgrey'),
                            strip.background=element_blank(),
                            panel.border = element_rect(colour = "black", fill = NA),
                            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()


anti_leos_right <- c("topic7sent1","topic2sent1","topic5sent3")

pdf(paste0("../plots/attackLEOs-right.pdf"))
ggplot(data=time_series %>%filter(name %in%anti_leos_right ),
       aes(x=date,y=value*100))+
  geom_line(colour="red") +
  ylab("Share of Daily Discussion (%)")+
  xlab("")+
  ylim(c(0,80))+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="black",lty=2)+
  facet_wrap(~as.factor(topic_labels),nrow=4,scale="fixed",strip.position = "top")+
  theme(text = element_text(family="serif",face="plain",size=23),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        strip.background=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()


troll <- c("topic5sent1","topic1sent3","topic3sent2","topic2sent3")
pdf(paste0("../plots/troll.pdf"))
ggplot(data=time_series %>%filter(name %in%troll ),
       aes(x=date,y=value*100))+
  geom_line(colour="red") +
  ylab("Share of Daily Discussion (%)")+
  xlab("")+
  ylim(c(0,80))+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="black",lty=2)+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")),colour="black",lty=2)+

  facet_wrap(~as.factor(topic_labels),nrow=4,scale="fixed",strip.position = "top")+
  theme(text = element_text(family="serif",face="plain",size=23),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        strip.background=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()


dem_supportGOP <- c("topic7sent3","topic9sent1")
pdf(paste0("../plots/demssupportGOP.pdf"))
ggplot(data=time_series %>%filter(name %in%dem_supportGOP ),
       aes(x=date,y=value*100))+
  geom_line(colour="blue") +
  ylab("Share of Daily Discussion (%)")+
  xlab("")+
  ylim(c(0,80))+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="black",lty=2)+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")),colour="black",lty=2)+

  facet_wrap(~as.factor(topic_labels),nrow=4,scale="fixed",strip.position = "top")+
  theme(text = element_text(family="serif",face="plain",size=23),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        strip.background=element_blank(),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()


# volume
time_series <- df%>% group_by(date) %>% summarize(total_count = n())

pdf(paste0("../plots/tweet_volume.pdf"))
ggplot(data=time_series,
       aes(x=date,y=total_count/1000))+
  geom_line(aes(y=rollmean(total_count/1000, 7, na.pad=TRUE)),colour="black",face="bold") +
  ylab("Number of Tweets (Thousands)")+
  xlab("")+
  geom_vline(xintercept = as.numeric(as.Date("2020-11-03")),colour="red",lty=2)+
  geom_vline(xintercept = as.numeric(as.Date("2021-01-06")),colour="red",lty=2)+
  theme(text = element_text(family="serif",face="plain",size=23),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()




# ###########
# 
# 
### Topic Visualization

pca_mat <- as.matrix(t(result.10@phi.termScores))
pca_mat <- (pca_mat)


fit  <-prcomp(pca_mat)
coords   <- fit$x[,1:2]
coords <- data.frame(coords) %>% mutate(TopicType = case_when(rownames(.)%in% dem_supportGOP ~"Dem-Inflected LEOs Support",
                                                              rownames(.)%in% pro_leos_neutral ~"Election Administration Guidance",
                                                              rownames(.)%in% anti_leos_right ~"GOP Hostility",
                                                              rownames(.)%in% domestic_politics ~"Domestic Politics",
                                                              rownames(.)%in% troll ~"Trolling",
                                                              TRUE ~"Other"))

# ## Full Pic
pdf("../plots/topic_position_total.pdf")
ggplot(data=data.frame(coords),aes(x=PC1,y=PC2))+
  geom_point(aes(colour=as.factor(TopicType)),size=2.5,)+
  scale_color_manual(values=c(wes_palette(n=5, name="Darjeeling2"),"red"))+
  ylab("Dimension 2") +
  xlab("Dimension 1") +
  theme_classic() +
  theme(text=element_text(family="serif", size=23),
        legend.title    = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  guides(color=guide_legend(ncol=2))
dev.off()



## Zoom in
pdf("../plots/topic_position_zoom.pdf")
ggplot(data=data.frame(coords)%>%filter(PC2<1),aes(x=PC1,y=PC2))+
  geom_point(aes(colour=as.factor(TopicType)),size=2.5,)+
  scale_color_manual(values=c(wes_palette(n=5, name="Darjeeling2"),"red"))+
  ylab("Dimension 2") +
  xlab("Dimension 1")+
  theme_classic()+
  theme(text = element_text(family="serif",face="plain",size=23),
        legend.title    = element_blank(),
        legend.position = "bottom",
        legend.justification = "left",
        panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        legend.text = element_text(size = 17)) +
  guides(color=guide_legend(ncol=2))
dev.off()




# Vulgarity Plot
                                                
curse_words = c('fuck', 'shit', 'damn', 'hell', 'cunt', 'bitch', 'whore',
                'dick', 'ass', 'bastard', 'cock', 'pussy', 'retard', 'skank')

phi <- result.10@phi
phi <- tibble::rownames_to_column(phi, var = "word")

vulgarity_df <- phi %>% filter(word %in% curse_words)

vulgarity_comp <- subset(vulgarity_df, select = -c(word))
vulgarity_comp <- vulgarity_comp %>% summarize_all(sum)
vulgarity_comp <- pivot_longer(vulgarity_comp, everything(), names_to = "topic_titles", values_to = "vulgarity")


vulgarity_comp <- merge(x=vulgarity_comp, y=topic_mapping, by="topic_titles", all.x=TRUE)
vulgarity_comp$topic_titles <- NULL

vulgarity_comp <- arrange(vulgarity_comp, vulgarity)
vulgarity_comp$topic_labels <- factor(vulgarity_comp$topic_labels, levels=vulgarity_comp$topic_labels)

pdf("../plots/vulgarity_comp.pdf")
ggplot(data=filter(vulgarity_comp, vulgarity > 0.000001), aes(x=topic_labels, y=vulgarity)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ylab("Vulgarity") +
  xlab("Topic Labels") +
  theme(text = element_text(family="serif",face="plain",size=23),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
dev.off()

