library(tidyverse)
library(ggthemes)

c <- read.csv("Currency_In_Circ_to_17Aug_2018.csv") %>%
    mutate(Week.Ending = as.Date(c$Week.Ending, format = "%d %b %Y")) %>%
    select(Week.Ending,Notes_Issued,Notes_in_Circulation) %>%
    arrange(desc(Week.Ending)) 

c.demon <- c %>% filter(Week.Ending >= as.Date("2016-11-01") & 
                        Week.Ending <= as.Date("2017-01-07"))

plot1 <- ggplot(data = c,
             aes(x = Week.Ending, y = Notes_in_Circulation)) +
        geom_line(stat="identity", colour = "#0072B2", size=1.1) +
        geom_line( data=c.demon, aes(x=Week.Ending, y=Notes_Issued), 
                   stat = "identity", colour = "#E69F00", size=1.2) +
        scale_x_date(date_breaks = "20 weeks",date_labels = "%b %y") +
        scale_y_continuous( 
            name="Trillions (= Lakh crores) INR",
            limits =c(6000, 20000),
            breaks = c(8000, 10000, 12000, 14000, 16000, 18000, 20000),
            labels = c("8 trn", "10 trn", "12 trn", "14 trn", "16 trn", "18 trn", "20 trn" )
            ) +
        labs(title ="Weekly movement in RBI liability due to Currency Notes in Circulation",
            subtitle = "Currency Notes in issue closely tracks Notes in Ciculation",
            x = "Week Ending... dates range from 25 Dec 2015 to 17 Aug 2018",
            caption = "Source of data: RBI Balance sheet; https://dbie.rbi.org.in/DBIE/dbie.rbi?site=home", size = 4) +
        annotate("text", x= as.Date("2017-08-24"), y= 7000, label = "(c)JayEnAar", size=4) +
        theme_economist()
plot1
