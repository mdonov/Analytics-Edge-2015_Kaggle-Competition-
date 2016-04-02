# Analytics-Edge-2015_Kaggle-Competition
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
library(caret)
library(tm)
library(caret)
library(ROCR)
install.packages("pROC")  
library(pROC)

# make train and test
train = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
test = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

#bind them and start making vars
bind=rbind(train[, !(colnames(train) %in% "Popular")], test)

#make bond test and train
bindTrain = head(bind, nrow(train))
bindTest = tail(bind, nrow(test))


#make variables/ format variables 
bind$PubDate = strptime(bind$PubDate, "%Y-%m-%d %H:%M:%S")
bind$Weekday = bind$PubDate$wday
bind$Weekday=as.factor(bind$Weekday)

bind$Hour = bind$PubDate$hour
bind$Hour= as.factor(bind$Hour)

bind$NewsDesk = as.factor(bind$NewsDesk)
bind$SectionName = as.factor(bind$SectionName)
bind$SubsectionName = as.factor(bind$SubsectionName)

#make log of wordcount
bind$logWordCount=log(as.numeric(bind$WordCount +1))

#Add the factor variable that indicates if the headline has a question mark in it.
bind$IsQuestion =as.factor( as.numeric(ifelse(grepl("?",bind$Headline,ignore.case=TRUE, fixed=TRUE), 1, 0)))

#Headline has 'Recap'
bind$Recap = as.factor(as.numeric(ifelse(grepl("Recap",bind$Headline,ignore.case=TRUE), 1, 0)))

#Headline has 'Ask Well'
bind$AskWell = as.factor(as.numeric(ifelse(grepl("Ask Well",bind$Headline,ignore.case=TRUE), 1, 0)))

#Headline has Readers Respond
bind$ReadersRespond = as.factor(as.numeric(ifelse(grepl("Readers Respond",bind$Headline,ignore.case=TRUE), 1, 0)))

#Headline has 'No Comment Necessary'
bind$NoComment = as.factor(as.numeric(ifelse(grepl("No Comment Necessary",bind$Headline,ignore.case=TRUE), 1, 0)))

#Headline has 'Open For Comments'
bind$OpenComment = as.factor(as.numeric(ifelse(grepl("Open for Comments",bind$Headline,ignore.case=TRUE), 1, 0)))

#Headline has has words related to questions.
bind$HQWords = as.factor(as.numeric(ifelse(grepl("what|when|why|who|how|should|can",bind$Headline,ignore.case=TRUE),1,0)))

#Headline has 'stress' in Headline
bind$HStress = as.factor(as.numeric(ifelse(grepl("stress",bind$Headline,ignore.case=TRUE), 1,0)))

#Headline has 'doctor' 
bind$Hdoctor = as.factor(as.numeric(ifelse(grepl("doctor",bind$Headline,ignore.case=TRUE), 1,0)))

#Headline has 'depression'
bind$Hdepression = as.factor(as.numeric(ifelse(grepl("depression",bind$Headline,ignore.case=TRUE), 1,0)))

#'Cancer' is in Headline
bind$HCancer = as.factor(as.numeric(ifelse(grepl("cancer",bind$Headline,ignore.case=TRUE), 1,0)))

#'Stop' is in Abstract
bind$AStop = as.factor(as.numeric(ifelse(grepl("stop",bind$Abstract,ignore.case=TRUE), 1,0)))

#Headlines with no article category
bind$HNoCategories = as.factor(as.numeric(ifelse(grepl("Daily Clip Report|Today in Politics|Verbatim|What We|From the Upshot|First Draft Focus",bind

$Headline,ignore.case=TRUE), 1,0)))

#Lenght of headline and abstarct len. delete old
bind$HLenChar=nchar(bind$Headline)
bind$ALenChar=nchar(bind$Abstract)

#Health related words in headline - doctor, depression, cancer in Headline
bind$HhealthRelat = as.factor(as.numeric(ifelse(grepl("doctor|cancer|depression|stress",bind$Headline,ignore.case=TRUE), 1,0)))


#Headlines - cleared from 2014, 2015, brand name, other frequent but not important words
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, c("2014", "2015","times","new york","nytimes","new york times","new","york","vebrabtim","upshot","today","fashion","week","playlist","report","archives","spring","summer", "morning",  stopwords("english")))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)

dtmHL = DocumentTermMatrix(CorpusHeadline)
sparseHL = removeSparseTerms(dtmHL, 0.99)
findFreqTerms(sparseHL, lowfreq=10)

HeadlineWords = as.data.frame(as.matrix(sparseHL))
colnames(HeadlineWords) = paste("H", colnames(HeadlineWords))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))

#Abstract - cleared from brand name, some frequent not important words
CorpusAbstract = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusAbstract = tm_map(CorpusAbstract, tolower)
CorpusAbstract = tm_map(CorpusAbstract, PlainTextDocument)
CorpusAbstract = tm_map(CorpusAbstract, removePunctuation)
CorpusAbstract = tm_map(CorpusAbstract, removeWords, c("new york","york","new york times","nytimes","ny times","the 

times","times","new","slideshow","daily","clip","report","herald", "tribune", 

"archives","paragraphs","highlights","Metropolitan","photos","scenes","fashion","week","plucks","photographer", stopwords("english")))
CorpusAbstract = tm_map(CorpusAbstract, stemDocument)

dtmAbstract = DocumentTermMatrix(CorpusAbstract)
sparseAbstract = removeSparseTerms(dtmAbstract, 0.98)
findFreqTerms(sparseAbstract, lowfreq=10)

AbstractWords = as.data.frame(as.matrix(sparseAbstract))
colnames(AbstractWords) = paste ("A", colnames(AbstractWords))
colnames(AbstractWords) = make.names(colnames(AbstractWords))

#create frame from words extracted from both the Headline and Abstract
NewsAllWords = cbind(HeadlineWords, AbstractWords,row.names=NULL) 


#Add all variables to new data frame that has the mined words from Headlines and Abstract of articles
NewsAllWords$logWordCount = bind$logWordCount
NewsAllWords$SectionName = bind$SectionName
NewsAllWords$NewsDesk = bind$NewsDesk
NewsAllWords$SubsectionName = bind$SubsectionName
NewsAllWords$IsQuestion = bind$IsQuestion 
NewsAllWords$Weekday = bind$Weekday 
NewsAllWords$Hour = bind$Hour 
NewsAllWords$Recap=bind$Recap
NewsAllWords$AskWell=bind$AskWell
NewsAllWords$ReaderRespond=bind$ReadersRespond
NewsAllWords$NoComment=bind$NoComment
NewsAllWords$OpenComment=bind$OpenComment
NewsAllWords$HQWords=bind$HQWords
NewsAllWords$ALenChar=bind$ALenChar
NewsAllWords$HLenChar=bind$HLenChar
NewsAllWords$IsQuestion=bind$IsQuestion
NewsAllWords$Hdepression=bind$Hdepression
NewsAllWords$Hdoctor=bind$Hdoctor
NewsAllWords$HStress=bind$HStress
NewsAllWords$HCancer=bind$HCancer
NewsAllWords$AStop=bind$AStop
NewsAllWords$HNoCategories=bind$HNoCategories


#seprate the final bind into train and test
NewsAllWordsTrain = head(NewsAllWords, nrow(train))
NewsAllWordsTest = tail(NewsAllWords, nrow(test))

#add dependent variable to train set
NewsAllWordsTrain$Popular = as.factor(train$Popular)

#Add the cancer/doctor/depression/stress var
NewsAllWordsTrain$HhealthRelat = head(bind$HhealthRelat, nrow(train))
NewsAllWordsTest$HhealthRelat = tail(bind$HhealthRelat, nrow(test))

#Run before doing random forest
levels(NewsAllWordsTrain$SectionName) = levels(NewsAllWordsTest$SectionName)
levels(NewsAllWordsTrain$SubsectionName) = levels(NewsAllWordsTest$SubsectionName)
levels(NewsAllWordsTrain$Weekday) = levels(NewsAllWordsTest$Weekday)
levels(NewsAllWordsTrain$Hour) = levels(NewsAllWordsTest$Hour)
levels(NewsAllWordsTrain$IsQuestion) = levels(NewsAllWordsTest$IsQuestion)
levels(NewsAllWordsTrain$ObamaHL) = levels(NewsAllWordsTest$ObamaHL)
levels(NewsAllWordsTrain$Recap) = levels(NewsAllWordsTest$Recap)
levels(NewsAllWordsTrain$AskWell) = levels(NewsAllWordsTest$AskWell)
levels(NewsAllWordsTrain$ReaderRespond) = levels(NewsAllWordsTest$ReaderRespond)
levels(NewsAllWordsTrain$NoComment) = levels(NewsAllWordsTest$NoComment)
levels(NewsAllWordsTrain$OpenComment) = levels(NewsAllWordsTest$OpenComment)


#remove variables that the model indicated as non significant
NewsAllWordsTrain$Hdepression = NULL
NewsAllWordsTest$Hdepression = NULL

NewsAllWordsTrain$HCancer = NULL
NewsAllWordsTest$HCancer = NULL

NewsAllWordsTrain$HStress = NULL
NewsAllWordsTest$HStress = NULL

NewsAllWordsTrain$Hdoctor = NULL
NewsAllWordsTest$Hdoctor = NULL

#add plain WordCount and remove the log one for the random forest
NewsAllWordsTrain$WordCount = head(bind$WordCount, nrow(train))
NewsAllWordsTest$WordCount = tail(bind$WordCount, nrow(test))

NewsAllWordsTrain$logWordCount = NULL
NewsAllWordsTest$logWordCount = NULL

#Best Model AUC=0.92854
set.seed=111
RM48= randomForest(Popular~., data=NewsAllWordsTrain,nodezize=25,ntree=200)
RM48Pred = predict(RM48, newdata=NewsAllWordsTest, type="prob")[,2]

MySubmission = data.frame(UniqueID = bindTest$UniqueID, Probability1 = RM48Pred)
write.csv(MySubmission, "RM_48.csv", row.names=FALSE)

