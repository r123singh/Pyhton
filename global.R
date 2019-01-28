
library(shiny)
library(wordcloud)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)

ebay_df= read.csv("ebay-prediction.csv",stringsAsFactors = FALSE)
Encoding(ebay_df$seller_code)="latin1"
Encoding(ebay_df$seller_name)="latin1"
Encoding(ebay_df$seller_store)="latin1"
Encoding(ebay_df$pos_feedback)="latin1"
Encoding(ebay_df$neg_feedback)="latin1"
iconv(ebay_df$seller_code, "latin1", "ASCII", sub="")
iconv(ebay_df$seller_name, "latin1", "ASCII", sub="")
iconv(ebay_df$seller_store, "latin1", "ASCII", sub="")
iconv(ebay_df$pos_feedback, "latin1", "ASCII", sub="")
iconv(ebay_df$neg_feedback, "latin1", "ASCII", sub="")
ebay_df$channel="Ebay"
ebay_df=ebay_df[!ebay_df$predicted_rating %in% NA,]
ebay_df=ebay_df[order(ebay_df$predicted_rating,decreasing = TRUE),]
ebay_df$seller_code= tolower(trimws(ebay_df$seller_code))

source("Category-Rating.R")
source("Rating_wc.R")


#Global cloud
stop_words = unlist(strsplit("nan,product,seller,a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your,id,item,it\'s,don\'t",","))
posReviews= ebay_df[,"pos_feedback"]
negReviews=ebay_df[,"neg_feedback"]
allReviews=paste(posReviews,negReviews)
reviewtxt=trimws(allReviews)
docs=Corpus(VectorSource(reviewtxt))
#inspect(docs)
toSpace= content_transformer(function(x,pattern)gsub(pattern," ",x))
docs=tm_map(docs,toSpace,"/")
docs=tm_map(docs,toSpace,"@")
docs=tm_map(docs,toSpace,"\\|")
docs=tm_map(docs,content_transformer(tolower))
docs=tm_map(docs,removeNumbers)
docs=tm_map(docs,removeWords,stopwords("english"))
docs=tm_map(docs,removeWords,c(stop_words))
docs=tm_map(docs,removePunctuation)
docs=tm_map(docs,stripWhitespace)
dtrm=TermDocumentMatrix(docs)
dmatrix= as.matrix(dtrm)
dvec=sort(rowSums(dmatrix),decreasing = TRUE)
global_frame=data.frame(word=names(dvec),freq=dvec)
head(global_frame,10)
set.seed(1235)

