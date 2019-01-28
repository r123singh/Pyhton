library(rvest)
library(readr)

page= read_html("http://www.flipkart.com/seller/retailnet/d591418b408940a0?start=0")
reviewDiv= page%>%html_nodes("div.seller-review-min");
nexturls= page%>%html_nodes("a.nav_bar_next_prev");
allReviewText="";
if (length(nexturls)==0) {
  for (reviewel in reviewDiv) {
    review= reviewel%>%html_node("div.bmargin10");
    reviewText= html_text(review,trim = TRUE)
    allReviewText=paste(allReviewText,reviewText);
  }
}else {
  while (length(nexturls)!=0) {
    if (length(reviewDiv) == 0) {
      break();
    }else {
      for (reviewel in reviewDiv) {
        review= reviewel%>%html_node("div.bmargin10");
        reviewText= html_text(review,trim= TRUE)
        allReviewText=paste(allReviewText,reviewText);
      }
      nextpagelink=NaN;
      for (nexturl in nexturls) {
        nextpagetext=tolower(html_text(nexturl,trim = TRUE))
        if (grepl(nextpagetext,pattern = "next")==TRUE) {
          nextpagelink= html_attr(nexturl,"href");
        }
      }
      if (is.nan(nextpagelink) ==TRUE) {
        break();
      }else {
        nextpagelink=paste("http://www.flipkart.com",nextpagelink,sep = "")
        print(paste("next page:", nextpagelink))
        page= read_html(nextpagelink);
        reviewDiv=page%>%html_nodes("div.seller-review-min")
        nexturls=page%>%html_nodes("a.nav_bar_next_prev");
      }
     }
  }
}
sellerdf= data.frame(code=c("ssddd"),reviews=c(allReviewText),row.names = NULL,stringsAsFactors = FALSE)
write.csv(sellerdf,file ="seller_2.csv",row.names = FALSE)

??