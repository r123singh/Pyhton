sellerData.dataFrame=NULL;
sellerCodeText="Sd7819";
pdpSellerName="2Bro solutions";
sellerStoreLink="https://www.snapdeal.com/seller/Sd7819"
newSellerInfo="New Seller"
todayDate=format(Sys.Date(),format="%d-%m-%Y")
sellerScoreNum=3.8
sellerSdPlusValue=1
snapdealFulfillmentValue=1
sellerFulfillmentValue= 0 
categoryName="Mobile"
subCategory1Text="Mobile & Cases"
subCategory2Text="Smartphones"
subCategoryLink2Href="https://www.snapdeal.com/products/mobiles?sort=plrty#bcrumbLabelId:676"
nextSubCategory2ProductsPageFinalUrl="https://www.snapdeal.com/acors/json/product/get/search/12/60/20?q=&sort=plrty&brandPageUrl=&keyword=&searchState=previousRequest=true|serviceabilityUsed=false|filterState=null&pincode=&vc=&webpageName=categoryPage&campaignId=&brandName=&isMC=false&clickSrc=unknown&showAds=true&cartId=&page=cp"
productPageHref="https://www.snapdeal.com/product/gionee-a1-64-mb-white/6341068918754327942#bcrumbLabelId:12"
productTitleText="Gionee A1 (4GB, 64 GB) Selfiestan"
productOfferPriceValue="12,628"
sellerOrdersProcessedCountInt=1101
sellerOrdersProcessedDuration=0
sellerProductCountInt=26
i=0
while (i<2) {
  currentSellerData.dataFrame=data.frame(SellerCode=c(sellerCodeText),
                                         SellerName=c(pdpSellerName),
                                         SellerStore=c(sellerStoreLink),
                                         NewSeller=c(newSellerInfo),
                                         SellerRatingDate=c(todayDate),
                                         SellerRating=as.numeric(c(sellerScoreNum)),
                                         SellerSdPlus=as.numeric(c(sellerSdPlusValue)),
                                         SnapdealFulfilled=as.numeric(c(snapdealFulfillmentValue)),
                                         SellerFulfillment=as.numeric(c(sellerFulfillmentValue)),
                                         Category=c(categoryName),
                                         SubCategory1=c(subCategory1Text),
                                         SubCategory2=c(subCategory2Text),
                                         SubCategory2Link=c(subCategoryLink2Href),
                                         SubCategory2NextPage=c(nextSubCategory2ProductsPageFinalUrl),
                                         ProductPageLink=c(productPageHref),
                                         ProductName=c(productTitleText),
                                         ProductPrice=c(productOfferPriceValue),
                                         OrdersProcessed=as.numeric(c(sellerOrdersProcessedCountInt)),
                                         SellerDuration=as.numeric(c(sellerOrdersProcessedDuration)),
                                         ProductCount=as.numeric(c(sellerProductCountInt)),
                                         row.names = NULL,
                                         stringsAsFactors = FALSE
  );
  sellerData.dataFrame=rbind(sellerData.dataFrame,currentSellerData.dataFrame);
  i=i+1;
}
print(sellerData.dataFrame)
dataFilename=paste(paste(todayDate,"Snapdeal",categoryName,sep = "_"),"csv",sep = ".")
write.csv(sellerData.dataFrame,file = dataFilename,row.names = FALSE)
