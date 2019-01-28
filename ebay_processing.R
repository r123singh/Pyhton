library("wordcloud")
library("tm")
library("SnowballC")
library("RColorBrewer")
library(dplyr)
stop_words = unlist(strsplit("nan,a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your,id,item,it\'s,don\'t",","))
seller_dataframe=read.csv("ebay.csv",stringsAsFactors = FALSE);

#Function for word frequency generation
dmatrix_frame= function(feedback){
  docs=Corpus(VectorSource(feedback))
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
  dframe=data.frame(word=names(dvec),freq=dvec)
  return(dframe)
}
seller_dataframe= seller_dataframe%>%group_by(seller_code,seller_store,seller_name,seller_rating,seller_followers,shipping_rating,handling_rating,comm_rating,desc_rating,pos_ratings,neg_ratings,neu_ratings,product_cnt,pos_feedback,neg_feedback)%>%summarise(category_cnt=n())%>%ungroup();
seller_dataframe= seller_dataframe[!seller_dataframe$seller_code %in% NaN,];
seller_dataframe$totalrating_given= seller_dataframe$pos_ratings+seller_dataframe$neg_ratings+seller_dataframe$neu_ratings;
seller_dataframe$posrating_percent=(seller_dataframe$pos_ratings+seller_dataframe$neu_ratings)/seller_dataframe$totalrating_given;
seller_dataframe$negrating_percent=seller_dataframe$neg_ratings/seller_dataframe$totalrating_given;

ratingnan_df= seller_dataframe[seller_dataframe$posrating_percent %in% NaN,];
ratingnan_df$posrating_percent=0;
ratingnan_df$negrating_percent=0;
ratngs_df=seller_dataframe[!seller_dataframe$posrating_percent %in% NaN,]
seller_dataframe=rbind(ratngs_df,ratingnan_df);
nrow(seller_dataframe[seller_dataframe$posrating_percent %in% NaN,])
positive_feedback_vec= seller_dataframe$pos_feedback;
negative_feedback_vec= seller_dataframe$neg_feedback;
pos_words=NULL;
neg_words=NULL;
for (positive_feedback in positive_feedback_vec) {
    word_frame=dmatrix_frame(positive_feedback);
    num_words= nrow(word_frame)
    pos_words=rbind(pos_words,num_words);
}
for (negative_feedback in negative_feedback_vec) {
  word_frame=dmatrix_frame(negative_feedback);
  num_words= nrow(word_frame)
  neg_words=rbind(neg_words,num_words);
}
seller_dataframe$pos_words=pos_words;
seller_dataframe$neg_words=neg_words;
seller_dataframe$total_words= seller_dataframe$pos_words+seller_dataframe$neg_words;
seller_dataframe$posword_percent=seller_dataframe$pos_words/seller_dataframe$total_words
seller_dataframe$negword_percent=seller_dataframe$neg_words/seller_dataframe$total_words

# Preprocessing data for lm model
#Model-1
dat<-seller_dataframe[c(-10:-12,-14:-15)]
dat$seller_rating=(dat$shipping_rating+dat$handling_rating+dat$comm_rating+dat$desc_rating)/4;
dat[,c(4:11,13:16,18:19)]<-as.data.frame(scale(dat[,c(4:11,13:16,18:19)]))
set.seed(2018)
sample<-sample(1:nrow(dat),0.7*nrow(dat),replace = FALSE)
dt<-dat[sample,]
dv<-dat[-sample,]
model<-lm(seller_rating ~category_cnt+product_cnt+ seller_followers+pos_words,data=dt)
summary(model)
anova(model)
plot(model)
#Model Prediction
PredictedRating<-predict(model,newdata=dv) 
resi<-dv$seller_rating-PredictedRating
#X-axis observed value, y is the diff
plot(resi~dv$seller_rating)
mape<-sum(abs(resi/dv$seller_rating))/nrow(dv)*100
mape

#Model-2
dat<-seller_dataframe[c(-10:-12,-14:-15)]
dat$seller_rating=(dat$shipping_rating+dat$handling_rating+dat$comm_rating+dat$desc_rating)/4;
dat[,c(4:11,13:16,18:19)]<-as.data.frame(scale(dat[,c(4:11,13:16,18:19)]))
set.seed(2018)
sample<-sample(1:nrow(dat),0.7*nrow(dat),replace = FALSE)
dt<-dat[sample,]
dv<-dat[-sample,]
model<-lm(seller_rating ~category_cnt+product_cnt+ seller_followers+pos_words+posrating_percent,data=dt)
summary(model)
anova(model)
plot(model)
#Model Prediction
PredictedRating<-predict(model,newdata=dv) 
resi<-dv$seller_rating-PredictedRating
#X-axis observed value, y is the diff
plot(resi~dv$seller_rating)
mape<-sum(abs(resi/dv$seller_rating))/nrow(dv)*100
mape

#Model-3
dat<-seller_dataframe[c(-10:-12,-14:-15)]
dat$seller_rating=(dat$shipping_rating+dat$handling_rating+dat$comm_rating+dat$desc_rating)/4;
rating_mean= mean(dat$seller_rating);
rating_sd=sd(dat$seller_rating);
dat[,c(4:11,13:16,18:19)]<-as.data.frame(scale(dat[,c(4:11,13:16,18:19)]))
set.seed(2018)
sample<-sample(1:nrow(dat),0.7*nrow(dat),replace = FALSE)
dt<-dat[sample,]
dv<-dat[-sample,]
model<-lm(seller_rating ~product_cnt+pos_words+posrating_percent,data=dt)
summary(model)
anova(model)
plot(model)
#Model Prediction
PredictedRating<-predict(model,newdata=dv) 
resi<-dv$seller_rating-PredictedRating
#X-axis observed value, y is the diff
plot(resi~dv$seller_rating)
mape<-sum(abs(resi/dv$seller_rating))/nrow(dv)*100
mape

predicted_rating=predict(model,newdata = dat)
predicted_rating= predicted_rating*rating_sd+rating_mean

seller_dataframe$predicted_rating=predicted_rating
write.csv(seller_dataframe,"ebay-prediction.csv",row.names = FALSE)
