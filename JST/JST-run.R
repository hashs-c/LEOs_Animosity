#####
# LEO Estimation
############################

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
library(dplyr)
library(parallel)
library(ggplot2)
library(lubridate)

#cluster analysis 
library(factoextra)
library(fpc)





#### Load Cleaned Data 

textData <- readr::read_csv("../dataset/clean_dataset.csv")




## prepare JST model
attach(textData)
llDisplay <- data.frame(author_id = paste(author_id),leo=leo,text = paste(tweet),handle=handle,doc_id=`...1`,stringsAsFactors = F)
detach(textData)

stop_words_custom = c("thread","say","will","has","by","for","hi","hey","hah",
                      "said","talk","congrats","congratulations","are","as","i", 
                      "me", "my", "myself", "we", "our", "ours", "ourselves", 
                      "you", "your", "yours","he","her","him","she","hers","that",
                      "be","with","their","they're","is","was","been","not","they","theyre",
                      "it","have",  "one","think",   "thing"    ,"bring","put","well",
                      "call", "wouldnt","wow", "learned","hi", "things" ,"things","can't","can",
                      "cant","will","go","going","let","would","could","him","his","think","thi","ha",
                      "lets","let's","say","says","know","talk","talked","talks","dont","think",
                      "said","something","this","was","has","had","abc","rt","ha","haha","hat",
                      "something","wont","people","make","want","went","goes","people","had",
                      "person","like","come","from","yet","able","abl","wa","yah","yeh","yeah","youv",
                      "need","us", "men", "women", "get", "woman", "man", "amp","amp&","yr","yrs",
                      paste(textData$firstname),paste(tidytext::stop_words$word),stopwords())

# Create tokens for the words you keep
print(Sys.time())
toks1   <- tokens(corpus(llDisplay), remove_punct = TRUE)
toks2   <- tokens_remove(toks1, stop_words_custom)
toks2.5 <- tokens_select(x=toks2,selection = "keep",min_nchar=3)
toks3   <- tokens_ngrams(toks2.5, 1:2)


N = nrow(llDisplay)

# create a document feature matrix (quanteda style) to run JST
dfm_speeches <- dfm(toks3 , 
                    remove_punct = TRUE ,remove_numbers = TRUE,stem = F) %>% 
  dfm_trim(min_termfreq = 25,min_docfreq =0.001*N,max_docfreq = 0.5*N)

dfm_speeches <- dfm_subset(dfm_speeches,ntoken(dfm_speeches) >= 3)

print(Sys.time())

## Create Paradigm list
df_p       = read.csv("paradigm.csv")



## top 100 sentiments
top_sents =df_p[ df_p$Token %in% names(topfeatures(dfm_speeches,n=10000)), ]

neutral_features = names(topfeatures(dfm_speeches,n=10000))[!(dfm_speeches@Dimnames$features
                                                              %in%top_sents$Token)]

paradigm_dict =dictionary(list(positive = top_sents$Token[top_sents$Positive==0.9],
                               negative = top_sents$Token[top_sents$Negative==0.9]
))






#optimize for topic number
kk = c(5,10,15,20,25,30,40,50,60)


results <- mclapply(kk,
                    FUN = function(kk) {
                      fit<-jst(dfm_speeches,
                          paradigm_dict,
                          numTopics = kk,
                          numSentiLabs = 3,
                          numIters = 2000)
                    save(fit,file=paste0("../dataset/results",kk,".RData"))
                    return(fit) },
                    mc.cores = getOption("mc.cores", 
                                         5L))  

save(results,file="../dataset/results.RData")





### Coherence scores

load("../dataset/results5.RData")
load("../dataset/results10.RData")
load("../dataset/results15.RData")
load("../dataset/results20.RData")
load("../dataset/results25.RData")
load("../dataset/results30.RData")
load("../dataset/results40.RData")
load("../dataset/results50.RData")
load("../dataset/results60.RData")


dtm = dfm(dfm_speeches, verbose = F)
tcm = Matrix::crossprod(sign(dtm))

result_co <-sapply(kk,
                   FUN=function(x){
                     colMeans(coherence(as.matrix(top20words(fit)), 
                                        tcm, 
                                        metrics = c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                                        smooth = 1e-12,
                                        n_doc_tcm = N),
                              na.rm=T)
                     })

# extract parameters 
k_opt <- as.numeric(which(result_co[5,]==max(result_co[5,])))


# NOTE: this is done individually because result.10 had a
#       different object name. But this should be done with
#       the sapply snippet above.
c60 <- colMeans(coherence(as.matrix(top20words(fit)), 
                   tcm, 
                   metrics = c("mean_logratio", "mean_pmi", "mean_npmi", "mean_difference", "mean_npmi_cosim", "mean_npmi_cosim2"),
                   smooth = 1e-12,
                   n_doc_tcm = N),
         na.rm=T)
rm(fit)

scores <- data.frame(c5)
scores$c10 <- c10
scores$c15 <- c15
scores$c20 <- c20
scores$c25 <- c25
scores$c30 <- c30
scores$c40 <- c40
scores$c50 <- c50
scores$c60 <- c60





scores_T <- scores %>% t() %>% as.data.frame()


pdf(paste0("../plots/coherence_mean_npmi_cosim2.pdf"))
ggplot(data=scores_T,
       aes(x=kk, y=mean_npmi_cosim2))+
  geom_line() +
  geom_vline(xintercept=10, colour="red") +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  ylab("Coherence")+
  xlab("Number of Topics")+
  theme(text = element_text(family="serif",face="plain",size=28),
        panel.background=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(colour = 'lightgrey'),
        panel.border = element_rect(colour = "black", fill = NA))
dev.off()
    
   
