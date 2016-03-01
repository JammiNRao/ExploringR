library(ggplot2)
library(gridExtra)

stdate <- "01/Jan/2006"
today <- as.character(Sys.Date(), format="%d/%b/%Y")

boe.URL <- paste("http://www.bankofengland.co.uk/
             boeapps/iadb/fromshowcolumns.asp?csv.x=yes
             &Datefrom=",stdate,"&Dateto=",today,"&SeriesCodes=XUDLBK64,XUDLBK97,XUDLBK73,XUDLBK89 
             &CSVF=TN&UsingCodes=Y&VPD=Y&VFD=N", sep="")

inr <- read.csv(url(boe.URL))
inr$DATE <- as.Date(as.character(inr$DATE), format="%d %b %Y")
colnames(inr) <- c("date", "Rupee_to_Dollar", "Rupee_to_Sterling", 
                   "Yuan_to_Dollar", "Yuan_to_Sterling")
legend.text <- paste("Bank of England data to", 
                     as.character(inr$date[nrow(inr)], format="%d %b %Y"))

latest <- paste("Latest:",as.character(inr$Rupee_to_Dollar[nrow(inr)]))
p1 <- ggplot(data=inr, aes(x=date,y=Rupee_to_Dollar))+labs(title=latest)+
      geom_line(col="black")+scale_y_reverse()
p1 <- p1 + theme(axis.title.x=element_blank(),
          axis.title=element_text(size=16)) #+

latest <- paste("Latest:",as.character(inr$Rupee_to_Sterling[nrow(inr)]))
p2 <- ggplot(data=inr, aes(x=date,y=Rupee_to_Sterling))+labs(title=latest)+
      geom_line(col="black")+scale_y_reverse()
p2 <- p2 + theme(axis.title.x=element_blank(),
          axis.title=element_text(size=16))

latest <- paste("Latest:",as.character(inr$Yuan_to_Dollar[nrow(inr)]))
p3 <- ggplot(data=inr, aes(x=date,y=Yuan_to_Dollar))+labs(title=latest)+
      geom_line(col="red")+scale_y_reverse()
p3 <- p3 + theme(axis.title.x=element_blank(),
          axis.title=element_text(size=16))

latest <- paste("Latest:",as.character(inr$Yuan_to_Sterling[nrow(inr)]))
p4 <- ggplot(data=inr, aes(x=date,y=Yuan_to_Sterling))+labs(title=latest)+
      geom_line(col="red")+scale_y_reverse()
p4 <- p4 + theme(axis.title.x=element_blank(),
          axis.title=element_text(size=16))

#png("Rupee-Yuan movements.png")
grid.arrange(p1,p2,p3,p4, ncol=2, top=legend.text, bottom = "©Gorway Global")
#dev.off()
