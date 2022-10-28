{
  #required packages
  library(Cairo)
  library(extrafont)
  library(extrafontdb)
  library(ggplot2)
  library(openxlsx)
}


#importing data(_xls_ file has been converted to _xlsx_ )
data<-read.xlsx("C:\\Users\\Guest1\\Desktop\\211220-PRABIN\\Lab test\\Prabin Sabat - CGHS_life_saving_drugs.xlsx")



#The data has only 2 set of columns 
##where the values in the first column represents different medicines life saving drugs approved by CGHS
###where the second column talks about what type the drug in column 1 is.
####as the data is categorical and have only 7 types of values in column2.
#####I will be making one bar_plot visualizing the different types and the number of drugs approved under them.



#finding out the no. of occurrences of the values in the second column
table(data$typeName)

#creating a data frame for our graph from the table.(The table had missed the Value NA, which represents the medicines does not fall under any type category, so we added all the values in the table and subtracted it with the number of rows to obtain the number of NA values in the second column of our main data set.)
df<-data.frame(types=c("CAP","INJ","PATCH","PFS","SYP","TAB","NA"),
               count=c(25,159,7,2,2,68,35))


#writing directly in  cairo_pdf for better output.
{
pdf_file<-"assignment.pdf"
cairo_pdf(bg="grey98", pdf_file,width=9,height=6.5)
par(omi=c(0.65,0.25,0.75,0.75),mai=c(0.3,0.75,0.35,0),mgp=c(3,3,0),
    family="Lato Light", las=1)



#_________creating chart__________
x<-barplot(df$count,names.arg=F,horiz=T,border=NA,xlim=c(0,200),
           col=c("Hot Pink","#00FFFF","#99F443",'#EC449B',"#2C7873","#AA96DA","#EEA47FFF"), cex.names=0.85,axes=F)


for (i in 1:length(df$count))
{
  if (df$types[i] %in% c("INJ","TAB","NA"))
    {myFont<-"Lato Black"
    text(-8,x[i],df$types[i],xpd=T,adj=1,cex=1.1,family=myFont)
    text(df$count[i]+10,x[i],df$count[i],xpd=T,adj=1,cex=0.85,family=myFont)} 
  else 
    {myFont<-"Lato Light"
    text(-8,x[i],df$types[i],xpd=T,adj=1,cex=1.05,family=myFont)
    text(df$count[i]+7,x[i],df$count[i],xpd=T,adj=1,cex=0.85,family=myFont)}
}

loc<-c(0,20,40,60,80,100,120,140,160,180,200)
for(i in 2:9)
{
arrows(loc[i],0,loc[i],10,lwd=1.5,lty=3,length=0,xpd=F,col="skyblue3")
}
text(42,9,"NA stands for No Category",adj=1,xpd=T,cex=0.85,font=3)
mtext(c(0,20,40,60,80,100,120,140,160,180,200),at=c(0,20,40,60,80,100,120,140,160,180,200),1,line=0,cex=0.80)
mtext("Number of CGHS approved medicines under different types: ",3,line=2,adj=0,cex=1.4,family="Lato Black",outer=T)
mtext("Total number of medicines=298",3,line=0.8,adj=0,cex=0.9,outer=T)
mtext("Source:- xls file given in assignment",1,line=1,adj=1.09,cex=0.65,outer=T,font=3)
dev.off()
}
  
