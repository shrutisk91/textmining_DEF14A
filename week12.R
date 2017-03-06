install.packages("assertthat")
install.packages("BH")
install.packages("bitops")
install.packages("curl")
install.packages("DBI")
install.packages("dplyr")
install.packages("htmltab")
install.packages("httr")
install.packages("jsonlite")
install.packages("lazyeval")
install.packages("magrittr")
install.packages("mime")
install.packages("NLP")
install.packages("openssl")
install.packages("plyr")
install.packages("R6")
install.packages("Rcpp")
install.packages("RCurl")
install.packages("slam")
install.packages("stringi")
install.packages("stringr")
install.packages("tidyr")
install.packages("tm")
install.packages("XML")
install.packages("boot")
install.packages("class")
install.packages("cluster")
install.packages("codetools")
install.packages("compiler")
install.packages("datasets")
install.packages("foreign")
install.packages("graphics")
install.packages("grDevices")
install.packages("grid")
install.packages("KernSmooth")
install.packages("lattice")
install.packages("MASS")
install.packages("Matrix")
install.packages("qdap")
install.packages("ggplot2")
install.packages("wordcloud")


cleaning <- function(htmlString) {
  return(gsub("<.*?>", " ", htmlString))
}

##################################################################################################

gs.2016.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312516534905/d147599ddef14a.htm"
gs.data.2016 = readLines(gs.2016.url)
grep("What are some common terms and acronyms used in this Proxy Statement",gs.data.2016)
grep("Annex A: Calculation of Non-GAAP Figures",gs.data.2016)
gs.qa.section.2016 <- gs.data.2016[13908:14649]
gs.qa.section.2016 <- gsub("&nbsp;*", "", gs.qa.section.2016)
gs.qa.section.2016 <- cleaning(gs.qa.section.2016)
cat(gs.qa.section.2016, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs.txt", sep=" ", append = TRUE)
cname2016 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2016 <- Corpus(DirSource(cname2016))
inspect(gs.qa.sections.corpus.2016)
dir(cname2016)
summary(gs.qa.sections.corpus.2016)
library(magrittr)
viewDocs2016 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2016(gs.qa.sections.corpus.2016,1)
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, content_transformer(tolower))
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, removeWords, stopwords("english"))
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, removeNumbers)
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, removePunctuation)
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, stripWhitespace)
gs.qa.sections.corpus.2016 <- tm_map(gs.qa.sections.corpus.2016, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "proxy", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must", "broker", "beneficial", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form", "name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","instructions", "received", "person", "annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2016<-DocumentTermMatrix(gs.qa.sections.corpus.2016)
gs2016
frequency2016 <- colSums(as.matrix(gs2016))
freq2016 <- findFreqTerms(gs2016, lowfreq = 10)
freq2016 <- sort(colSums(as.matrix(gs2016)), decreasing = TRUE)
head(freq2016, 50)
wordfreq2016 <- data.frame(Words = names(freq2016), Frequency=freq2016)
head(wordfreq2016)
library(ggplot2)
subset(wordfreq2016, freq2016 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2016), freq2016, min.freq = 10, colors=dark2)

library(qdap)
words2016 <- gs2016 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2016 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))


#####################################################################################################

gs.2014.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312514131134/d627791ddef14a.htm"
gs.data.2014 = readLines(gs.2014.url)
grep("Frequently Asked Questions About our Annual Meeting",gs.data.2014)
grep("annex a",gs.data.2014)
gs.qa.section.2014 <- gs.data.2014[12717:13282]
gs.qa.section.2014 <- gsub("&nbsp;*", "", gs.qa.section.2014)
gs.qa.section.2014 <- cleaning(gs.qa.section.2014)
cat(gs.qa.section.2014, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs14.txt", sep=" ", append = TRUE)
cname2014 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2014 <- Corpus(DirSource(cname2014))
inspect(gs.qa.sections.corpus.2014)
dir(cname2014)
summary(gs.qa.sections.corpus.2014)
library(magrittr)
viewDocs2014 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2014(gs.qa.sections.corpus.2014,1)
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, content_transformer(tolower))
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, removeWords, stopwords("english"))
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, removeNumbers)
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, removePunctuation)
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, stripWhitespace)
gs.qa.sections.corpus.2014 <- tm_map(gs.qa.sections.corpus.2014, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders", "stylemargintoppxmarginbottompxfont","stylemargintoppxmarginbottompx", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted","valignbottom", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "received", "proxy","york", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must","sizeb", "narrow","bcfont","including","contents", "broker", "widthtd", "beneficial", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form","sizefonttd","valignbottomfont","tylemargintoppxmarginbottompx", "name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes","styleborderbottompx", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","valigntop","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","instructions","solid","roman", "includin","compensation","neos","annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2014<-DocumentTermMatrix(gs.qa.sections.corpus.2014)
gs2014
frequency2014 <- colSums(as.matrix(gs2014))
freq2014 <- findFreqTerms(gs2014, lowfreq = 10)
freq2014 <- sort(colSums(as.matrix(gs2014)), decreasing = TRUE)
freq2014 <- head(freq2014, 25)
wordfreq2014 <- data.frame(Words = names(freq2014), Frequency=freq2014)
head(wordfreq2014)
library(ggplot2)
subset(wordfreq2014, freq2014 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2014), freq2014, min.freq = 10, colors=dark2)

words2014 <- gs2014 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2014 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))

########################################################################################################

gs.2013.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312513152411/d447333ddef14a.htm"
gs.data.2013 = readLines(gs.2013.url)
grep("Frequently Asked Questions About our Annual Meeting",gs.data.2013)
grep("ANNEX A: Additional Details on Director Independence",gs.data.2013)
gs.qa.section.2013 <- gs.data.2013[13613:14210]
gs.qa.section.2013 <- gsub("&nbsp;*", "", gs.qa.section.2013)
gs.qa.section.2013 <- cleaning(gs.qa.section.2013)
cat(gs.qa.section.2013, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs13.txt", sep=" ", append = TRUE)
cname2013 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2013 <- Corpus(DirSource(cname2013))
inspect(gs.qa.sections.corpus.2013)
dir(cname2013)
summary(gs.qa.sections.corpus.2013)
library(magrittr)
viewDocs2013 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2013(gs.qa.sections.corpus.2013,1)
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, content_transformer(tolower))
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, removeWords, stopwords("english"))
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, removeNumbers)
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, removePunctuation)
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, stripWhitespace)
gs.qa.sections.corpus.2013 <- tm_map(gs.qa.sections.corpus.2013, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "proxy", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must", "broker", "beneficial", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form", "name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","bfontp","alignleft","cellspacing","fontp","alighleftfont","narrow","receive", "neos","stylefontsizepxmargintoppxmarginbottompxp","stylemargintoppxmarginbottompxfont","valignbottom","valigntop","width","york","instructions", "annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2013<-DocumentTermMatrix(gs.qa.sections.corpus.2013)
gs2013
frequency2013 <- colSums(as.matrix(gs2013))
freq2013 <- findFreqTerms(gs2013, lowfreq = 10)
freq2013 <- sort(colSums(as.matrix(gs2013)), decreasing = TRUE)
freq2013 <- head(freq2013, 50)
wordfreq2013 <- data.frame(Words = names(freq2013), Frequency=freq2013)
freq2013<-head(wordfreq2013)
library(ggplot2)
subset(wordfreq2013, freq2013 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2013), freq2013, min.freq = 10, colors=dark2)

words2013 <- gs2013 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2013 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))

##############################################################################################################

gs.2011.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312511086865/ddef14a.htm"
gs.data.2011 = readLines(gs.2011.url)
grep("Our Compensation Committee, which is comprised of all of our independent directors,",gs.data.2011)
grep("ANNEX A: Key Corporate Governance Facts",gs.data.2011)
gs.qa.section.2011 <- gs.data.2011[10864:11443]
gs.qa.section.2011 <- gsub("&nbsp;*", "", gs.qa.section.2011)
gs.qa.section.2011 <- cleaning(gs.qa.section.2011)
cat(gs.qa.section.2011, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs11.txt", sep=" ", append = TRUE)
cname2011 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2011 <- Corpus(DirSource(cname2011))
inspect(gs.qa.sections.corpus.2011)
dir(cname2011)
summary(gs.qa.sections.corpus.2011)
library(magrittr)
viewDocs2011 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2011(gs.qa.sections.corpus.2011,1)
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, content_transformer(tolower))
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, removeWords, stopwords("english"))
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, removeNumbers)
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, removePunctuation)
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, stripWhitespace)
gs.qa.sections.corpus.2011 <- tm_map(gs.qa.sections.corpus.2011, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders","compensation", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "proxy", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must", "broker", "beneficial", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form", "board","committee","name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","unless","received","instructions", "annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2011<-DocumentTermMatrix(gs.qa.sections.corpus.2011)
gs2011
frequency2011 <- colSums(as.matrix(gs2011))
freq2011 <- findFreqTerms(gs2011, lowfreq = 10)
freq2011 <- sort(colSums(as.matrix(gs2011)), decreasing = TRUE)
head(freq2011, 50)
wordfreq2011 <- data.frame(Words = names(freq2011), Frequency=freq2011)
head(wordfreq2011)
library(ggplot2)
subset(wordfreq2011, freq2011 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2011), freq2011, min.freq = 10, colors=dark2)

words2011 <- gs2011 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2011 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))

##################################################################################################################

gs.2010.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312510078005/ddef14a.htm"
gs.data.2010 = readLines(gs.2010.url)
grep("Compensation Discussion and Analysis",gs.data.2010)
grep("Election of Directors",gs.data.2009)
gs.qa.section.2010 <- gs.data.2010[1747:2185]
gs.qa.section.2010 <- gsub("&nbsp;*", "", gs.qa.section.2010)
gs.qa.section.2010 <- cleaning(gs.qa.section.2010)
cat(gs.qa.section.2010, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs10.txt", sep=" ", append = TRUE)
cname2010 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2010 <- Corpus(DirSource(cname2010))
inspect(gs.qa.sections.corpus.2010)
dir(cname2010)
summary(gs.qa.sections.corpus.2010)
library(magrittr)
viewDocs2010 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2010(gs.qa.sections.corpus.2010,1)
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, content_transformer(tolower))
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, removeWords, stopwords("english"))
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, removeNumbers)
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, removePunctuation)
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, stripWhitespace)
gs.qa.sections.corpus.2010 <- tm_map(gs.qa.sections.corpus.2010, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders","compensation", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "proxy", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must", "broker", "beneficial", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form", "board","committee","name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","unless","received","instructions", "annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2010<-DocumentTermMatrix(gs.qa.sections.corpus.2010)
gs2010
frequency2010 <- colSums(as.matrix(gs2010))
freq2010 <- findFreqTerms(gs2010, lowfreq = 10)
freq2010 <- sort(colSums(as.matrix(gs2010)), decreasing = TRUE)
freq2010<-head(freq2010, 50)
wordfreq2010 <- data.frame(Words = names(freq2010), Frequency=freq2010)
head(wordfreq2010)
library(ggplot2)
subset(wordfreq2010, freq2010 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2010), freq2010, min.freq = 10, colors=dark2)

words2010 <- gs2010 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2010 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))
##################################################################################################################

gs.2009.url <- "https://www.sec.gov/Archives/edgar/data/886982/000119312509073816/ddef14a"
gs.data.2009 = readLines(gs.2009.url)
grep("Compensation Discussion and Analysis",gs.data.2009)
grep("Election of Directors",gs.data.2009)
gs.qa.section.2009 <- gs.data.2009[1520:1674]
gs.qa.section.2009 <- gsub("&nbsp;*", "", gs.qa.section.2009)
gs.qa.section.2009 <- cleaning(gs.qa.section.2009)
cat(gs.qa.section.2009, file="C:/Users/shrut/GoldmanSachs/GoldmanSachs09.txt", sep=" ", append = TRUE)
cname2009 <- "C:/Users/shrut/GoldmanSachs"
library(tm)

gs.qa.sections.corpus.2009 <- Corpus(DirSource(cname2009))
inspect(gs.qa.sections.corpus.2009)
dir(cname2009)
summary(gs.qa.sections.corpus.2009)
library(magrittr)
viewDocs2009 <- function(d,n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs2009(gs.qa.sections.corpus.2009,1)
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, PlainTextDocument)
library(tm)
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, content_transformer(tolower))
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, removeWords, stopwords("english"))
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, removeNumbers)
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, removePunctuation)
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, stripWhitespace)
gs.qa.sections.corpus.2009 <- tm_map(gs.qa.sections.corpus.2009, removeWords, 
                                     c("proxystatementforannualmeetingofshareholders","compensation", "wwwcomputersharecom", "wwwgscomelectronicdelivery", "wwwgscomshareholders",
                                       "stylewhitespacenowrapnoeffectnotcounted", "york", "shareholderproposalsgscom","sarbanesoxleyclawback", "proxystatement", "pricewaterhousecoopers", "performancebased", "ourshareholders","years","meeting",
                                       "ofshareholders", "proxy", "mortgagebacked", "vote", "will", "shareholders", "may", "ifyouareashareholder", "proposals", "voting", "record", "shareholder", 
                                       "must", "broker", "beneficial", "stylemargintoppxmarginbottompx", "companys", "business", "can", "director", "proposal", "date", "directors","new","materials","street","common","held","votes","recieved",
                                       "form", "board","committee","name", "what", "entitled", "statement", "following", "you", "the ", "each", "due", "set", "nominatinggovernance","inc","font","similar","recieve","west","also","matters",
                                       "stylefontfamilytimes", "neos", "financeyahoocomtopicsyahooshareholdersmeeting", "yahoonetdocumentscfm", "reportelectronic","governmentissued","goldmansachsourfirm","access","copy","goldman",
                                       "effectofabstentions","day","proxystatementfortheannualmeetingofshareholders","stylefontfamilyarial","brokerdealer","annualmeeting","colorffffff","gsinvestorrelationsgscom","sachs",
                                       "wwwgscomproxymaterials","unless","received","instructions", "annual","shares","brokerdiscretionary","wwwproxyvotecom","againstorabstain","againsttheproposal","beverlyotoolegscom","compensationrelated"))


gs2009<-DocumentTermMatrix(gs.qa.sections.corpus.2009)
gs2009
frequency2009 <- colSums(as.matrix(gs2009))
freq2009 <- findFreqTerms(gs2009, lowfreq = 10)
freq2009 <- sort(colSums(as.matrix(gs2009)), decreasing = TRUE)
freq2009<-head(freq2009, 50)
wordfreq2009 <- data.frame(Words = names(freq2009), Frequency=freq2009)
head(wordfreq2009)
library(ggplot2)
subset(wordfreq2009, freq2009 > 25) %>% ggplot(aes(Words, Frequency)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
library(wordcloud)

set.seed(123)
dark2 <- brewer.pal(5, "Dark2")
wordcloud(names(freq2009), freq2009, min.freq = 10, colors=dark2)

words2009 <- gs2009 %>%
  as.matrix %>%
  colnames %>%
  (function(x) x[nchar(x) < 20])

words2009 %>%
  lapply(function(x) sapply(letters, gregexpr, x, fixed=TRUE)) %>%
  unlist %>%
  (function(x) x[x!=-1]) %>%
  (function(x) setNames(x, gsub("\\d", "", names(x)))) %>%
  (function(x) apply(table(data.frame(letter=toupper(names(x)),
                                      position=unname(x))),
                     1, function(y) y/length(x))) %>%
  qheat(high="green", low="yellow", by.column=NULL,
        values=TRUE, digits=3, plot=FALSE) +
  labs(y="Letter", x="Position") +
  theme(axis.text.x=element_text(angle=0)) +
  guides(fill=guide_legend(title="Proportion"))