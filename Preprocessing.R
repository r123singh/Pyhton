#Preprocess seller
library(grid)
library(gridExtra)
todayDate=format(Sys.Date(),format="%d-%m-%Y")
file= read.csv(file = "20-02-2018_Snapdeal.csv",stringsAsFactors = FALSE)
sellerdf= data.frame(Code=file$SellerCode,Channel=file$Channel,Name=file$SellerName,StarRating=file$SellerRating,Category=file$Category,Orders=file$OrdersProcessed,Review=file$SellerNegativeReview,stringsAsFactors = FALSE);
nrow(sellerdf)
sellerdf_grouped= sellerdf%>%group_by(Code,Name,Channel)%>%summarise(TotalOrders= sum(Orders),StarRate=mean(StarRating))
sellerdf_grouped$Categories=""
sellerdf_grouped$Catcount=0
sellerdf_grouped$NegativeReview="";
sellerVec= sellerdf_grouped$Code
k=1;
for (scode in sellerVec) {
  sellerData= filter(sellerdf,Code==scode);
  categoryV=sellerData$Category
  reviewV= sellerData$Review
  categorytxt=""
  reviewtxt=""
  for (category in categoryV) {
    categorytxt=paste(categorytxt,category,sep = " ")
  }
  for (review in reviewV) {
    reviewtxt=paste(reviewtxt,review,sep = " ")
  }
 sellerdf_grouped[k,"Categories"]=trimws(categorytxt);
 sellerdf_grouped[k,"NegativeReview"]=trimws(reviewtxt);
 sellerdf_grouped[k,"Catcount"]=length(categoryV)
 k=k+1
}
fileName=paste("Snapdeal-cleaned-",todayDate,".csv",sep = "")
write.csv(sellerdf_grouped,file = fileName,row.names = FALSE)

