#23-02-2018
#Ratings based on category

rate_colors = c(brewer.pal(9, "Reds")[c(7,6,5)],brewer.pal(9, "Greens")[c(6,7)])
fte_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  theme_bw(base_size=9) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=10,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=10,color=color.axis.title, vjust=1.25)) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
df<-read.csv(file = "20-02-2018_Snapdeal.csv",stringsAsFactors = FALSE)
df$SellerRating=round(df$SellerRating,0)
#De-deuplicating
df=df%>%group_by(SellerCode,Category)%>%filter(row_number(Category)==1)%>%ungroup()
#grouping
df_category=df%>%group_by(Category,SellerRating)%>%summarise(count=n())%>%arrange(Category,SellerRating)%>%collect()
df_category_avg=df%>%group_by(Category)%>%summarise(avg_seller_rating=mean(SellerRating),sd_ratings= sd(SellerRating))%>%arrange(desc(avg_seller_rating))%>%collect()
df_category_avg
#make the factor out of category
df_category$Category=factor(df_category$Category,levels = rev(df_category_avg$Category))
cat_plot=ggplot(df_category,aes(x=Category,y=count,fill=as.factor(SellerRating)))+geom_bar(stat = "identity",position = "fill")+
  fte_theme()+coord_flip()+
  scale_y_continuous(labels = percent)+
  theme(legend.title = element_blank(),legend.position = "top",legend.direction = "horizontal",
  legend.key.width = unit(0.5,"cm"),legend.key.height = unit(0.0,"cm"),plot.title = element_text(vjust = 0,hjust = 1.5),legend.margin = margin(0,0,-0.2,-3,"cm"),axis.title.y = element_blank())+
  scale_fill_manual(values =rate_colors,labels("1 star","2 star","3 start","4 star","5 star") )+
  labs(title="Breakdown of Seller ratings by Category",ylab="Proportion")

  