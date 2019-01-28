library("rvest")
library(gsubfn)

homePage= read_html("https://www.snapdeal.com/")
todayDate=format(Sys.Date(),format="%d-%m-%Y")
categoryList= homePage%>%html_nodes("li.navlink")

for (categoryXml in categoryList) {
  categoryMenuLink= categoryXml%>%html_node("a.leftCategoriesProduct")
  categoryName= html_text(categoryMenuLink%>%html_node("span.catText"))
  print(paste("Category Name:",categoryName));
    if (categoryName != "All Offers" && categoryName != "See All Categories") {
    categoryDataXml= categoryXml%>%html_node("div.leftNavigationRightBlock");
    categoryInnerBlocks= categoryDataXml%>%html_nodes("div.colDataInnerBlk");
    sellerData.dataFrame=NULL;
    for (categoryInnerBlockXml in categoryInnerBlocks) {
      categoryRightMenuLinksXml= categoryInnerBlockXml%>%html_nodes("p");
      subCategory1Text=NaN;        
       for(pCategory in categoryRightMenuLinksXml){
        shiftHeadTopCategory = html_nodes(pCategory,"a.shiftHeadTop");
        if (length(shiftHeadTopCategory) == 0) {
          subCategory2Text= html_text(pCategory%>%html_node("span"),trim = TRUE);
          print(paste("Subcategory 2:",subCategory2Text));
          subCategory2Link=html_nodes(pCategory,"a.rightMenuLink");
          subCategoryLink2Href= html_attr(subCategory2Link,"href");
          subCategoryLink2Href= paste(subCategoryLink2Href,"sort=plrty",sep = "?");
          print(paste("Subcategory 2 Link:",subCategoryLink2Href));
          subCategory2ProductsPage= read_html(subCategoryLink2Href);
          subCategory2ProductCountDiv= html_node(subCategory2ProductsPage,"div.jsNumberFound");
          subCategory2ProductCountText= html_text(subCategory2ProductCountDiv,trim = TRUE);
          totalProductCount=as.numeric(subCategory2ProductCountText);
          subCategory2CatIdRegex=".*catId\\s*: *(.*?) *,\\s*resultsCt.*";
          subCategory2CatIdRegion= gsub(subCategory2CatIdRegex,"\\1",subCategory2ProductsPage,ignore.case = TRUE)
          subCategory2CategoryId=strapply(subCategory2CatIdRegion, "\\d+", as.numeric, simplify = TRUE)
          subCategory2BrandRegex= ".*brand\\s*:*(.*?) *,\\s*searchEventId.*"
          subCategory2BrandRegion=gsub(subCategory2BrandRegex,"\\1",subCategory2ProductsPage,ignore.case = TRUE)
          subCategory2BrandText=gsub('\"',"",subCategory2BrandRegion);
          subCategory2BrandText=trimws(subCategory2BrandText)
          subCategory2SortRegex= ".*sort\\s*:*(.*?) *,\\s*labelUrl.*"
          subCategory2SortRegion=gsub(subCategory2SortRegex,"\\1",subCategory2ProductsPage,ignore.case = TRUE)
          subCategory2SortText=gsub('\"',"",subCategory2SortRegion);
          subCategory2SortText=trimws(subCategory2SortText)
          nextPageProductsUrl="https://www.snapdeal.com/acors/json/brand/product/get/search";
          nextPageProductsSortParam=paste("sort",subCategory2SortText,sep = "=");
          nextPageProductsKeywordParam="q=";
          nextPageProductsPreviousParam=paste("previousRequest","true",sep = "=");
          nextPageProductsBrandParam=paste("brandName",subCategory2BrandText,sep = "=");
          nextPageProductsPageParam=paste("page","bcp",sep = "=")
          nextPageProductsUrlOptions=paste(nextPageProductsKeywordParam,nextPageProductsSortParam,nextPageProductsPreviousParam,nextPageProductsBrandParam,nextPageProductsPageParam,sep = "&")
          defaultProductCount=20;
          totalProductCount=20;
          currentProductCount = 0;
          productRatingDivList= subCategory2ProductsPage%>%html_nodes("div.product-desc-rating");
            for (productRatingDiv in productRatingDivList) {
              dpWidgetLink = html_node(productRatingDiv,"a.dp-widget-link");
              productPageHref=trimws(html_attr(dpWidgetLink,"href"));
              print(paste("Product page link:",productPageHref));
              productPage= read_html(productPageHref);
              productDetailCardRight= productPage%>%html_node("div.product-detail-card-right");
              pdpSellerInfo= html_node(productDetailCardRight,"a.pdp-e-seller-info-name");
              pdpSellerName= html_text(pdpSellerInfo,trim = TRUE);
              pdpSellerHref =trimws(html_attr(pdpSellerInfo,"href"));
              sellerStoreLink = paste("https://www.snapdeal.com",pdpSellerHref,sep = "");
              print (paste("Seller store link: ",sellerStoreLink,sep = ""));
              pdpSellerScoreInfoDiv= html_node(productDetailCardRight,"div.pdp-e-seller-info-score");
              newSellerSpan= pdpSellerScoreInfoDiv%>%html_node("span.newSeller");
              newSellerInfo= html_text(newSellerSpan,trim = TRUE);
              print (paste("New Seller Info:",newSellerInfo));
              buyLinkDiv= html_node(productPage,"div.buyLink");
              sellerScoreText= trimws(html_attr(buyLinkDiv,"vendorscore"));
              sellerScoreNum= as.numeric(sellerScoreText);
              print(paste("Seller Score:",sellerScoreText));
              sellerCodeText= trimws(html_attr(buyLinkDiv,"vendorcode"));
              print(paste("Seller code:",sellerCodeText));
              sellerSdPlusRegex= ".*sellerSdPlus\\s*: *(.*?) *,\\s*supc.*";
              sellerSdPlusInfoRegion= gsub(sellerSdPlusRegex,"\\1",productPage,ignore.case = TRUE);
              sellerSdPlusQuotesString= gsub("\t*\n*\\s*","",sellerSdPlusInfoRegion);
              sellerSdPlusString=gsub('\"',"",sellerSdPlusQuotesString);
              print(paste("Seller is Sdplus: ",sellerSdPlusString));
              sellerSdPlusValue=0;
              if (sellerSdPlusString == "true") {
                sellerSdPlusValue = 1;
              }
              if (sellerSdPlusString == "false") {
                sellerSdPlusValue=0;
              }
              snapdealFulfilledIconSpan = html_node(productPage,"span.sdFulfilledIcon");
              snapdealFulfilledIconClassAttr=trimws(html_attr(snapdealFulfilledIconSpan,"class"));
              snapdealFulfillmentValue= 0;
              sellerFulfillmentValue=0;
              if (grepl("hidden",snapdealFulfilledIconClassAttr)==TRUE) {
                snapdealFulfillmentValue =0;
                sellerFulfillmentValue=1;
              }else {
                snapdealFulfillmentValue=1;
                sellerFulfillmentValue=0;
              }
              productAvgRatingSpan= productPage%>%html_node("span.avrg-rating");
              productAvgRatingText= html_text(productAvgRatingSpan,trim = TRUE);
              productAvgRatingValue= as.numeric(gsub("[()]","",productAvgRatingText));
              print(paste("Product Average Rating:",productAvgRatingValue));
              productRatingCountSpan=html_node(productPage,"span.total-rating");
              productRatingCountText=html_text(productRatingCountSpan);
              productRatingCountValue=strapply(productRatingCountText,"\\d+",as.numeric,simplify = TRUE);
              productReviewCountSpan= html_node(productPage,"span.numbr-review");
              productReviewCounta= productReviewCountSpan%>%html_node("a");
              productReviewCountText=html_text(productReviewCounta);
              productReviewCountValue=strapply(productReviewCountText,"\\d+",as.numeric,simplify = TRUE);
              
              productUsrReviewList= html_nodes(productPage,"div.user-review");
              productUsrReviewText="";
              for (productReviewDiv in productUsrReviewList) {
                productReviewHead= productReviewDiv%>%html_node("div.head");
                productReviewHeadText= html_text(productReviewHead,trim = TRUE);
                productReviewHeadText=sub("\n*\t*\\s*","",productReviewHeadText,ignore.case = TRUE);
                productReviewP= productReviewDiv%>%html_node("p")
                productReviewPText=html_text(productReviewP,trim = TRUE);
                productReviewPText=sub("\n*\t*\\s*","",productReviewPText,ignore.case = TRUE);
                productReviewPText=trimws(productReviewPText);
                productReviewHeadText= trimws(productReviewHeadText);
                productUsrReviewText = paste(productUsrReviewText,productReviewHeadText,productReviewPText,sep = "");
              }
              
              productTitlePelement=html_node(productPage,"p.product-title");        
              productTitleText= html_text(productTitlePelement,trim = TRUE);
              print(paste("Product Name: ",productTitleText));
              productOfferPricePelement= html_node(productPage,"p.product-offer-price");
              productOfferPriceText= html_text(productOfferPricePelement,trim= TRUE);
              productOfferPriceRegex= ".*Rs.\\s*";
              productOfferPriceRegion=gsub(productOfferPriceRegex,"\\1",productOfferPriceText);
              productOfferPriceValue= trimws(productOfferPriceRegion);
              print(paste("Product Offer Price: ",productOfferPriceValue));
              sellerStorePage = read_html(sellerStoreLink);
              sellerOrderRowDiv= sellerStorePage%>%html_node("div.seller-order-row");
              sellerOrdersProcessInfoText= html_text(sellerOrderRowDiv);
              sellerOrdersProcessInfoText=tolower(sellerOrdersProcessInfoText); 
              sellerOrdersProcessInfoVector= noquote(strsplit(sellerOrdersProcessInfoText," "));
              sellerOrdersProcessedCountText= sellerOrdersProcessInfoVector[[1]][1];
              sellerOrdersProcessedCountInt= as.numeric(sellerOrdersProcessedCountText);
              print(paste("Seller Orders Process Count:",sellerOrdersProcessedCountInt));
              sellerOrdersProcessedDuration =0;  
              if (grepl("one year",sellerOrdersProcessInfoText)==TRUE) {
                print(paste("Duration of Orders Processed: ",1));
                sellerOrdersProcessedDuration =1;  
              }
              sellerSubCategoryCountHead= sellerStorePage%>%html_node("h2.seller-product-count-head");
              sellerSubCategoryCountText= html_text(sellerSubCategoryCountHead,trim = TRUE);
              sellerSubCategoryCountText=tolower(sellerSubCategoryCountText);
              sellerProductCountInt= strapply(sellerSubCategoryCountText, "\\d+", as.numeric, simplify = TRUE)
              print(paste("Seller Product Count:",sellerProductCountInt));
              currentProductCount=currentProductCount+1;
              currentSellerData.dataFrame=data.frame(SellerCode=c(sellerCodeText),
                                                     SellerName=c(pdpSellerName),
                                                     SellerStore=c(sellerStoreLink),
                                                     NewSeller= c(newSellerValue),
                                                     SellerRatingDate= c(todayDate),
                                                     SellerRating=as.numeric(c(sellerScoreNum)),
                                                     SellerSdPlus=as.numeric(c(sellerSdPlusValue)),
                                                     SnapdealFulfilled=as.numeric(c(snapdealFulfillmentValue)),
                                                     SellerFulfillment=as.numeric(c(sellerFulfillmentValue)),
                                                     Category=c(categoryName),
                                                     SubCategory1=c(subCategory1Text),
                                                     SubCategory2=c(subCategory2Text),
                                                     SubCategory2Link=c(subCategoryLink2Href),
                                                     ProductPageLink=c(productPageHref),
                                                     ProductName=c(productTitleText),
                                                     ProductPrice=c(productOfferPriceValue),
                                                     OrdersProcessed=as.numeric(c(sellerOrdersProcessedCountInt)),
                                                     SellerDuration=as.numeric(c(sellerOrdersProcessedDuration)),
                                                     ProductCount=as.numeric(c(sellerProductCountInt)),
                                                     ProductAvgRate=as.numeric(c(productAvgRatingValue)),
                                                     ProductRatingCount=as.numeric(c(productRatingCountValue)),
                                                     ProductReviewCount=as.numeric(c(productReviewCountValue)),
                                                     ProductReviewText=c(productUsrReviewText),
                                                     row.names = NULL,
                                                     stringsAsFactors = FALSE
              );
              sellerData.dataFrame=rbind(sellerData.dataFrame,currentSellerData.dataFrame);
            }
        }else{
          subCategory1Text= html_text(pCategory%>%html_node("span"),trim = TRUE);
          print(paste("Subcategory 1:",subCategory1Text));
        }
      }     
    }
    dataFilename=paste(paste(todayDate,"Snapdeal",categoryName,sep = "_"),"csv",sep = ".")
    write.csv(sellerData.dataFrame,file = dataFilename,row.names = FALSE)
  }  
}
