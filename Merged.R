#merged ratings

# Load packages
library(readr)
library(dplyr)
library(ggplot2)
library(sparklyr)
library(extrafont)
library(scales)
library(grid)
library(RColorBrewer)
library(digest)
library(readr)
library(stringr)

fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}


n=5
weight_flipkart = 2
weight_snapdeal=1

flip_f= read.csv("21-02-2018_Flipkart.csv",stringsAsFactors = FALSE)
snap_f=read.csv("21-02-2018_Snapdeal.csv",stringsAsFactors = FALSE)

flipdf= data.frame(Code=flip_f$SellerCode,Name=tolower(flip_f$SellerName),Rating=flip_f$SellerRating)
snapdf= data.frame(Code=snap_f$SellerCode,Name=tolower(snap_f$SellerName),Rating=snap_f$SellerRating)
nrow(flipdf)
nrow(snapdf)

flipdf_clean = flipdf[!duplicated(flipdf),]
snapdf_clean=snapdf[!duplicated(snapdf),]

nrow(flipdf_clean)
nrow(snapdf_clean)

merged_df=merge(flipdf_clean[,c("Code","Rating","Name")],snapdf_clean[,c("Code","Rating","Name")],by.x ="Name",by.y ="Name")
nrow(merged_df)
mergedcodes= merged_df[,"Code"]

flipdf_unmerged= flipdf_clean[!flipdf_clean$Code %in% mergedcodes,]
snapdf_unmerged=snapdf_clean[!snapdf_clean$Code%in%mergedcodes,]
nrow(flipdf_unmerged)
nrow(snapdf_unmerged)

merged_df$WeightedScore=(merged_df$Rating.x*weight_flipkart+merged_df$Rating.y*weight_snapdeal)/(weight_flipkart+weight_snapdeal)

finaldf_unmerged=rbind(flipdf_unmerged,snapdf_unmerged)
finaldf_unmerged

finaldf_merged= data.frame(Code=merged_df$Code,Name=merged_df$Name.x,Rating=merged_df$WeightedScore,stringsAsFactors = FALSE)
finaldf_merged

finaldf= rbind(finaldf_merged,finaldf_unmerged)
finaldf
nrow(finaldf)
#top 10 sellers by scores
finaldf=finaldf[order(finaldf$Rating,decreasing = TRUE),]
ggplot(data=finaldf[c(1:n),], aes(Name, Rating)) + geom_bar(stat="identity") + coord_flip() + geom_text(aes(label=round(Rating, 2)), hjust=2, size=2, color="white") + fte_theme() + labs(x="Seller Name", y="Average Weighted Score", title="Top Rated Sellers By Online Seller")



