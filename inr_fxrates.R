library(tidyverse)
library(ggplot2)
library(ggthemes)

# set up constants
stdate <- "01/Jan/2010"; today <- as.character(Sys.Date(), format="%d/%b/%Y")
boe.url <- "http://www.bankofengland.co.uk/boeapps/iadb/fromshowcolumns.asp?csv.x=yes"
boe.url <- paste0(boe.url,"&Datefrom=",stdate,"&Dateto=",today,
          "&SeriesCodes=XUDLBK64,XUDLERD,XUDLGBD,XUDLBK73&CSVF=TN&UsingCodes=Y&VPD=Y&VFD=N")
Colors <- c("Rupee"="#D55E00", "Euro" = "#009E73", "Sterling" ="#0072B2", "Yuan" = "#E69F00" ) 

#get the data
inr <- read.csv(url(boe.url))


#define constants for charting
caption.text <- paste("Bank of England daily Fx rates from",
                      gsub("/", " ",stdate), "to", inr$DATE[nrow(inr)])
latest <- paste("Latest:",as.character(inr$XUDLBK64[nrow(inr)]),"on",as.character(inr$DATE[nrow(inr)], format="%d %b %Y") )

fx_theme <- theme_economist() +theme(legend.position = "none",
                                    axis.title.y = element_text(size=16),
                                    plot.title = element_text(size=22),
                                    plot.subtitle = element_text(size=14),
                                    plot.caption = element_text(size=12),
                                    axis.text = element_text(size=14),
                                    strip.text.x = element_text(size=14)
)
inr.monthly.average <- inr %>% 
  mutate(date = as.Date(DATE, format = "%d %b %Y")) %>%
  #rename(Rupee = XUDLBK64, Euro = XUDLERD, Sterling = XUDLGBD, Yuan = XUDLBK73) %>%
  group_by("Month_Year" = substr(DATE,4,11)) %>%
  summarise(Rupee = mean(XUDLBK64), 
            Euro = mean(XUDLERD), 
            Sterling = mean(XUDLGBD), 
            Yuan = mean(XUDLBK73)
            ) %>%
  mutate(date = as.Date(paste("15",Month_Year), format = "%d %b %Y")) %>%
  arrange(date) 
## *********************************    change this date
start.point <- as.Date("2010-05-01")    # and rerun the code
## ***********************************  to get a new chart

inr.monthly.average %>%
  filter(date > start.point) %>%
  gather('Rupee':'Yuan',key = "Currency", value = "Exchange.Rate") %>%
  ggplot(aes(x=date,y=Exchange.Rate, group = Currency)) +
  geom_line(aes(colour = factor(Currency)), size = 0.8) + 
  scale_y_reverse() +
  facet_wrap(~Currency, scales = "free_y") + 
  scale_colour_manual(values = Colors) +
  labs(title = "US Dollar Exchange Rate for 4 currencies 2010 to present",
       x = "",
       y = "Value of 1 US Dollar - inverted scale",
       subtitle = paste("Indian Rupee","(", latest ,")","and 3 leading Currencies averaged over each month"),
       caption = paste( "Bank of England daily Fx rates from",
                         as.character(start.point, format="%b %Y"), "to", inr$DATE[nrow(inr)]  ,
          "  © JayEnAar")) +
  fx_theme 


 
ggsave(filename = paste0("USD_4_Curr_MonthlyAverage",inr$DATE[nrow(inr)],".png"),
       path = "charts", device = "png",width = 10, height = 8, units = "in" )


base100 <- function(x) {(x*100/x[1])}
base100.rev <- function(x) {(x[1]*100/x)}

#start.point <- as.Date("2010-01-01")
inr.monthly.average %>%
  filter(date > start.point) %>%
  transmute(Rupee = base100(Rupee),
            Euro = base100(Euro),
            Sterling = base100(Sterling),
            Yuan = base100(Yuan),
            date = date) %>%
  ggplot(aes( x= date))+
  scale_color_manual(name="", values =  c("Rupee"="#D55E00", 
                                          "Euro" = "#009E73", "Sterling" ="#0072B2", 
                                          "Yuan" = "black" )  ) +  #"#E69F00"
  geom_line(aes(y=Rupee, colour = "Rupee"), size=1.1)+
  geom_line(aes(y=Euro, colour = "Euro"), size= 0.75) +
  geom_line(aes(y=Sterling, colour = "Sterling"), size = 0.8)+
  geom_line(aes(y=Yuan, colour = "Yuan"), size = 0.9) +
  scale_y_reverse()+
  labs(title = "Movement in value of selected currencies in terms of US Dollar",
       subtitle = paste( "Indian Rupee, Euro, Sterling and Chinese Yuan; base", 
                         as.character(start.point, format="%b %Y") , 
                         "set to 100"),
       caption = paste("Bank of England daily Fx data averaged over month and standardised to", 
                       as.character(start.point, format="%b %Y"), 
                       ";  © JayEnAar"),
       x = paste(as.character(start.point, format="%b %Y"), "to", as.character(inr$DATE[nrow(inr)], format="%d %b %Y")),
       y = paste("Standardised value; 100 =",as.character(start.point, format="%b %Y"))) + fx_theme +
  theme(legend.position = c(0.2,0.25) )

ggsave(filename = paste0("USD_4_standardised",inr$DATE[nrow(inr)],".png"),
       path = "charts", device = "png",width = 10, height = 8, units = "in" )
