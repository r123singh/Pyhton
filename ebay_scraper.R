#Ramandeep Singh

library(rvest)
library(gsubfn)

ebay_url="http://stores.shop.ebay.in";
ebay_store=read_html("http://stores.shop.ebay.in/_sl.html?_fe=1")
seller_list= ebay_store%>%html_nodes("div.s_item");
regex= ".*[ID]";
seller_df=read.csv("ebay.csv",stringsAsFactors = FALSE)

while (length(seller_list) >0 ) {
  
  for(seller in seller_list){
    #Seller Variables
    seller_code=NaN
    seller_store=NaN
    seller_followers=0
    seller_rating=0
    seller_category=NaN
    shipping_rating=0
    handling_rating=0
    desc_rating=0
    comm_rating=0
    pos_ratings=0
    neg_ratings=0
    neu_ratings=0
    pos_feedback=NaN
    neg_feedback=NaN
    product_cnt=0
    
    seller_name= seller%>%html_node("td.tl")%>%html_node("a");
    seller_name= html_text(seller_name,trim = TRUE);
    isAdded= seller_df[seller_df$seller_name == seller_name,]
    if (nrow(isAdded) == 0) {
      seller_store=seller%>%html_node("td.tl")%>%html_node("a")%>%html_attr("href");
      print(seller_store);
      store_page= read_html(seller_store);
      #Seller Page
      seller_page=store_page%>%html_node("div.mbg")%>%html_node("a")%>%html_attr("href");
      seller_page=read_html(trimws(seller_page));
      seller_code=seller_page%>%html_node("a.mbg-id")
      seller_code=html_text(seller_code,trim = TRUE);
      seller_code=sub(regex,"",seller_code);
      seller_code=trimws(seller_code);
      score_divs= seller_page%>%html_nodes("div.score");
      for (score_div in score_divs) {
        score_type=score_div%>%html_node("span.txt");
        score_type=html_text(score_type,trim = TRUE);
        score_type=tolower(score_type);
        if (length(score_type) == 0) {
          break();
        }
        if (grepl("positive",score_type)) {
          pos_ratings=score_div%>%html_node("span.num");
          pos_ratings=html_text(pos_ratings,trim = TRUE);
          pos_ratings=gsub(",","",pos_ratings);
          pos_ratings=as.numeric(pos_ratings);
        }
        
        if (grepl("negative",score_type)) {
          neg_ratings=score_div%>%html_node("span.num");
          neg_ratings=html_text(neg_ratings,trim = TRUE);
          neg_ratings=gsub(",","",neg_ratings);
          neg_ratings=as.numeric(neg_ratings);
        }
        
        if (grepl("neutral",score_type)) {
          neu_ratings=score_div%>%html_node("span.num");
          neu_ratings=html_text(neu_ratings,trim = TRUE);
          neu_ratings=gsub(",","",neu_ratings);
          neu_ratings=as.numeric(neu_ratings);
        }
      }
      dsr=seller_page%>%html_node("div.dsr")%>%html_nodes("div.fl");
      if (length(dsr) >0) {
        desc_rating=dsr[[1]]%>%html_node("span.rating_ov");
        desc_rating=html_attr(desc_rating,name = "title");
        desc_rating= strsplit(desc_rating,split = "/");
        desc_rating=desc_rating[[1]][1];
        desc_rating=as.numeric(desc_rating);
        
        comm_rating=dsr[[2]]%>%html_node("span.rating_ov");
        comm_rating=html_attr(comm_rating,name = "title");
        comm_rating= strsplit(comm_rating,split = "/");
        comm_rating=comm_rating[[1]][1];
        comm_rating=as.numeric(comm_rating);
        
        shipping_rating=dsr[[3]]%>%html_node("span.rating_ov");
        shipping_rating=html_attr(shipping_rating,name = "title");
        shipping_rating= strsplit(shipping_rating,split = "/");
        shipping_rating=shipping_rating[[1]][1];
        shipping_rating=as.numeric(shipping_rating);
        
        handling_rating=dsr[[4]]%>%html_node("span.rating_ov");
        handling_rating=html_attr(handling_rating,name = "title");
        handling_rating= strsplit(handling_rating,split = "/");
        handling_rating=handling_rating[[1]][1];
        handling_rating=as.numeric(handling_rating);
      }
      seller_followers=seller_page%>%html_node("div.mem_info")%>%html_nodes("span.hide");
      seller_followers=seller_followers[[1]];
      seller_followers=html_attr(seller_followers,name = "contentstring");
      seller_followers=gsub(",","",seller_followers,ignore.case = TRUE);
      seller_followers=strapply(seller_followers,"\\d+",as.numeric,ignore.case = TRUE);
      if (length(seller_followers) == 1) {
        seller_followers=seller_followers[[1]];
      }
      
      #All feedbacks
      allfbpage=seller_page%>%html_node("span.all_fb")
      if (is.na(allfbpage)) {
        #do nothing
      }else {
        allfbpage=allfbpage%>%html_node("a")%>%html_attr(name = "href");
        allfbpage=read_html(allfbpage)
        allfb= allfbpage%>%html_node("table.FbOuterYukon")%>%html_nodes("tr");
        #+/- Feedback
        fbpages=0;
        while(!is.null(allfb) && fbpages <=5){
          for (row in allfb) {
            if (is.na(html_attr(row,"class"))) {
              fb=row%>%html_nodes("td");
              if (length(fb) >=2) {
                fbtype=fb[[1]]%>%html_node("img")%>%html_attr("alt");
                fbdata=html_text(fb[[2]],trim = TRUE);
                if (grepl("positive",fbtype,ignore.case = TRUE) == TRUE) {
                  pos_feedback=paste(pos_feedback,fbdata);
                }else if (grepl("negative",fbtype,ignore.case = TRUE) == TRUE) {
                  neg_feedback=paste(neg_feedback,fbdata);
                }
              }
            }
          }
          allfbpage=allfbpage%>%html_node("b.pg-rp")%>%html_node("a")%>%html_attr("href");
          if (is.na(allfbpage)) {
            break();
          }
          allfbpage=read_html(allfbpage);
          allfb=allfbpage%>%html_node("table.FbOuterYukon")%>%html_nodes("tr");
          fbpages=fbpages+1;
        }
      }
      
      #Visit Seller Store
      product_cnt=store_page%>%html_node("span.countClass")
      if (is.na(product_cnt)) {
        #do nothing
        product_cnt=store_page%>%html_node("div.res-cnt");
        if (!is.na(product_cnt)) {
          product_cnt=html_text(product_cnt,trim = TRUE);
          product_cnt=gsub(",","",product_cnt,ignore.case = TRUE);
          product_cnt=strapply(product_cnt,"\\d+",as.numeric);
          product_cnt=product_cnt[[1]];
        }else {
          product_cnt=0;
        }
      }else {
        product_cnt=html_text(product_cnt,trim = TRUE);
        product_cnt=gsub(",","",product_cnt);
        product_cnt=as.numeric(product_cnt);
      }
      seller_rating=store_page%>%html_node("a.mbg-fb")
      if (!is.null(seller_rating) && length(seller_rating) > 0) {
        seller_rating=html_text(seller_rating,trim = TRUE)
        seller_rating=strapply(seller_rating,"\\d+",as.numeric);
        seller_rating=seller_rating[[1]];
      }else {
        seller_rating=0;
      }
      categories=store_page%>%html_node("ul.lev1");
      if (length(categories) > 0) {
        categories= categories%>%html_nodes("li");
        for (category in categories) {
          seller_category= category%>%html_node("a");
          seller_category=html_text(seller_category,trim = TRUE);
          seller_df = rbind(seller_df,data.frame(seller_code,seller_name,seller_store,
                                                 seller_rating,seller_category,seller_followers,
                                                 shipping_rating,handling_rating,
                                                 desc_rating,
                                                 comm_rating,
                                                 pos_ratings,
                                                 neg_ratings,
                                                 neu_ratings,
                                                 pos_feedback,
                                                 neg_feedback,
                                                 product_cnt,row.names = NULL,stringsAsFactors = FALSE));
          
        }
      }else {
        seller_category= NA;
        seller_df = rbind(seller_df,data.frame(seller_code,seller_name,seller_store,
                                               seller_rating,seller_category,seller_followers,
                                               shipping_rating,handling_rating,
                                               desc_rating,
                                               comm_rating,
                                               pos_ratings,
                                               neg_ratings,
                                               neu_ratings,
                                               pos_feedback,
                                               neg_feedback,
                                               product_cnt,row.names = NULL,stringsAsFactors = FALSE));
        
        
      }
    }
  }
  ebay_store= ebay_store%>%html_node("td.next")%>%html_node("a.enabled");
  if (length(ebay_store) > 0) {
    ebay_store= html_attr(ebay_store,name = "href");
    ebay_store= paste(ebay_url,ebay_store,sep = "");
    print(ebay_store);
    ebay_store=read_html(ebay_store);
    seller_list= ebay_store%>%html_nodes("div.s_item");
  }else {
    break();
  }
  
}

write.csv(seller_df,"ebay.csv",row.names = FALSE,col.names = FALSE)

