#Loading Libraries
library(tokenizers)
library(readxl)
library(readr)
library(stringr)
library(stringdist)
library(tm)
library(bpa)
library(openxlsx)
library(dplyr)
#Sentiment Analysis
#Import Words indicating catastrophes
Lexicon <- read_excel("C:/Users/nickr/OneDrive/Υπολογιστής/Repositories/Natural-Disaster-Crisis-Detection-in-Social-Networks/Vocabulary/Lexicon.xlsx")
#View(Lexicon)
lexicon<-Lexicon[,2]
notepad<-c()
w<-0
#Cleaning Vowels Lexicon
grclean1<-c()
grclean2<-c()
grclean3<-c()
grclean4<-c()
grclean5<-c()
grclean6<-c()
grclean7<-c()
grclean8<-c()
grclean9<-c()
grclean10<-c()
for (k in 1:length(lexicon[[1]])) {
  grclean1[[k]]<-gsub("ά","α",lexicon[[1]][k])
  grclean2[[k]]<-gsub("έ","ε",grclean1[[k]])
  grclean3[[k]]<-gsub("ή","η",grclean2[[k]])
  grclean4[[k]]<-gsub("ί","ι",grclean3[[k]])
  grclean5[[k]]<-gsub("ό","ο",grclean4[[k]])
  grclean6[[k]]<-gsub("ύ","υ",grclean5[[k]])
  grclean7[[k]]<-gsub("ώ","ω",grclean6[[k]])
  grclean8[[k]]<-gsub("ϋ","υ",grclean7[[k]])
  grclean9[[k]]<-gsub("ϊ","ι",grclean8[[k]])
  grclean10[[k]]<-gsub("ΐ","ι",grclean9[[k]])
}
clean_lexicon<-grclean10
#View(Greek_Lexicon)
#Loading Data 
#txt or csv
#data_csv<-read.csv(file.choose(),sep=",",skipNul = TRUE)
fileslist <- choose.files()
fileslist <-sort(fileslist)
mood<-matrix(nrow = length(fileslist), ncol = 7)
n_iter <- length(fileslist)
pb <- winProgressBar(title = "Windows progress bar", # Window title
                     label = "Percentage completed", # Window label
                     min = 0,      # Minimum value of the bar
                     max = n_iter, # Maximum value of the bar
                     initial = 0,  # Initial value of the bar
                     width = 300L) # Width of the window 
for(e in 1:length(fileslist)){
  data<-read_lines(fileslist[e], skip_empty_rows = TRUE, progress = TRUE)
  #Checking encoding of the document
  encoding_check<-stringi::stri_enc_detect(data)
  if(encoding_check[[1]][1,1]!="UTF-8" || encoding_check[[2]][1,1]!="UTF-8" || encoding_check[[3]][1,1]!="UTF-8"|| encoding_check[[4]][1,1]!="UTF-8" || encoding_check[[5]][1,1]!="UTF-8"){
    #Changing encoding from ISO-8859-7 to UTF-8
    c<-encoding_check[[1]][1,1]
    #Renaming the file
    l<-substr(fileslist[e],nchar(fileslist[e])-19,nchar(fileslist[e]))
    b<-paste("fixed_encoding_UTF_8_",l,sep = "")
    file.create(b)
    #Fixing the encoding and writing a new file 
    writeLines(iconv(readLines(fileslist[e]), from ="ISO-8859-7" , to = "UTF8"),file(b, encoding="UTF-8"))
    data<-read_lines(b, skip_empty_rows = TRUE, progress = TRUE)
    writeLines(data,file(b))
  } else if (encoding_check == ""){
    next}
  #Setting Stop Words and Text cleaning
  greek_stop_words_initial<-read_excel("C:/Users/nickr/OneDrive/Υπολογιστής/Repositories/Natural-Disaster-Crisis-Detection-in-Social-Networks/Vocabulary/greek_stop_words.xlsx")
  greek_stop_words<-greek_stop_words_initial[,2]
  stop_words <- append(greek_stop_words , stopwords::stopwords(language = "en"))
  #removing urls
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  data<-gsub(pattern = url_pattern,"",data)
  #removing english strings
  data<-gsub("[a-z]|[A-Z]","",data)
  #removing emojis
  data<-gsub("<U|[$-_@.&+]|[0-9]|F|[0-9]>","",data)
  #Pops up a window to choose the txt file we want
  #Tokenization techniques
  tokens <-tokenize_words(data , lowercase = TRUE, stopwords = stop_words , strip_punct = TRUE , strip_numeric = TRUE)
  #Cleaning vowels tokens 
  clean1<-c()
  clean2<-c()
  clean3<-c()
  clean4<-c()
  clean5<-c()
  clean6<-c()
  clean7<-c()
  clean8<-c()
  clean9<-c()
  clean10<-c()
  clean1<-gsub("ά","α",unlist(tokens))# We unlist the tokens to "clean"
  clean2<-gsub("έ","ε",clean1)
  clean3<-gsub("ή","η",clean2)
  clean4<-gsub("ί","ι",clean3)
  clean5<-gsub("ό","ο",clean4)
  clean6<-gsub("ύ","υ",clean5)
  clean7<-gsub("ώ","ω",clean6)
  clean8<-gsub("ϋ","υ",clean7)
  clean9<-gsub("ϊ","ι",clean8)
  clean10<-gsub("ΐ","ι",clean9)
  count<- 0 
  clean_tokens<-tokens
  for(f in 1:length(clean_tokens)){
    for(g in 1:length(clean_tokens[[f]])){
      count<-count+1
      clean_tokens[[f]][g]<-clean10[count]
    }
  }
  #General
  #Setting Up Variables for Outputs of the code
  all_sentiments<-matrix(nrow = length(clean_tokens), ncol = 7)
  o<-c()
  s<-0
  z<-c()
  a<-0
  #Select the String Matching Technique
  str_match_meth<-"jw"
  #Starting to string match for each tweet's tokens and assigning sentiment values according to Sentiment Lexi
  for (i in 1:length((clean_tokens)))   {
    o<-c()
    s<-0
    z<-c()
    a<-0
    #Pick a tweet
    y<-i
    for (p in 1:length(clean_tokens[[y]])) {
      a<-nchar(clean_tokens[[y]][p])
      if(is.na(a)!=TRUE){
        #String Matching Function amatch() returns the position of word in clean_lexicon which is the First Column of Greek Sentiment Lexi
        o<-amatch(clean_tokens[[y]][p], clean_lexicon, method = str_match_meth, nomatch = 0, nthread = getOption("sd_num_thread")
)
      }
      if(o>0){
        notepad[w]<-data[f]
      }
    }
  pctg <- paste(round(e/n_iter *100, 0), "% completed")
  setWinProgressBar(pb, e, label = pctg) # The label will override the label set on the
  # winProgressBar function
  }
}
title<-paste("data_",str_match_meth,"revised.csv",sep = "")
write.csv2(mood, file = title)
close(pb) 
#saveWorkbook(wb = hi,file = excelName)
#excelName<-paste(Sys.Date(),"moodTwitter.xlsx",sep = "_")
#hi<-createWorkbook()
#addWorksheet(hi, l)
#addWorksheet(hi, paste("data",l,sep = "_"))
#writeData(hi, sheet = l , x =all_sentiments)
#writeData(hi, sheet = paste("data",l,sep = "_") , x =as.data.frame(data))