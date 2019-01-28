s= noquote(strsplit("1213 b c"," "))
as.numeric(s[[1]][1])
grepl("one year","last one year")
tolower("Ramandeep")


?grep()
txt <- c("The", "licenses", "for", "most", "software", "are",
         "designed", "to", "take", "away", "your", "freedom",
         "to", "share", "and", "change", "it.",
         "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
         "is", "intended", "to", "guarantee", "your", "freedom", "to",
         "share", "and", "change", "free", "software", "--",
         "to", "make", "sure", "the", "software", "is",
         "free", "for", "all", "its", "users")
regexpr("en", txt)
gregexpr("e", txt)
txt<-"Products by this seller (16)"
gregexpr("\\d+",txt)
txt <- "a test of capitalizing"
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", txt, perl=TRUE)

txt <- noquote(strsplit("Products by this seller (16)"," "))
if(length(i <- grep("\\(", txt))){
  cat("'foo' appears at least once in\n\t", txt, "\n")}
i # 2 and 4
txt

x<-"Products by this seller (16)"

st=strapply(x, "\\d+", as.numeric, simplify = TRUE)
print(st)

txt=read_html("https://www.snapdeal.com/product/xolo-era-2-8gb-black/5188147410095998352")

s <- "sellerSdPlus    :   'true'         	 ,	 supc	"
txt=sub(".*sellerSdPlus\\s*: *(.*?) *,\\s*supc.*", "\\1", txt,ignore.case = TRUE)
txt=gsub("\t*\n*\\s*","",txt)
noquote(txt)


price="Rs. 3,899"
pr= sub(".*Rs.\\s*","\\1",price,ignore.case = TRUE)
trimws(noquote(pr))
