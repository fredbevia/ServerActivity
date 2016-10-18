library(plotly)
library(lubridate)
library(chron)

        
Stats <- read.csv("Stats.csv", sep=";", stringsAsFactors=FALSE)
Stats$Date <-as.POSIXct(Stats$DBegin, format = "%d/%m/%Y")
Stats$DBegin <-as.POSIXct(Stats$DBegin, format = "%d/%m/%Y %H:%M")
Stats$DEnd <-as.POSIXct(Stats$DEnd, format = "%d/%m/%Y %H:%M")
Stats$Duration<-chron(times=Stats$Duration)

Stats<- arrange(Stats,Server,Date,Apps)
ts <- summarise(group_by(Stats,Server,Date),total=sum(Duration),min = min(Duration, na.rm = TRUE),m = mean(Duration, na.rm = TRUE),max = max(Duration, na.rm = TRUE))

# reusable function for highlighting a particular Serveur or app
layer_server <- function(plot, name) {
  plot %>% filter(Server == name) %>% add_lines(name = name)
}

# reusable function for plotting overall average duration and min/max

layer_stats <- function(plot) {
 
    plot %>%
    add_lines(y = ~m, name = "mean", color = I("Green")) %>%
    add_ribbons(ymin = ~min, ymax = ~max, name = "Min and Max", color = I("black"))
}



allServers <- ts %>%
  plot_ly(x = ~Date, y = ~total) %>%
  add_lines(alpha = 0.5, name = "Applidis Servers", hoverinfo = "none")



allServers %>%
  add_fun(layer_stats)  %>%
  add_fun(layer_server, "SRVMV-COMMUN001") %>%
  add_fun(layer_server, "SRVMV-PDEP001")





