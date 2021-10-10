daf <- read.delim("C:/Users/Admin/Desktop/cet333/CET333 Laone Tsheole/logfile.txt")

colnames(daf)<-c("IP", "Time","URL", "Status")

View(daf)

library(tidyr)
library(lubridate)
library(dplyr)

#preprocessing
#separating date and time column
df= df%>% mutate(Time = dmy_hms(Time))

df$Date2 <- NA
class(df$Date)
df$Date<-sub(".*:", " ", df$Date)

df$Date <- format(as.Date(df$Date),"%Y:%M:%D")
df$Time<-  format(as.POSIXct(df$Time, format= "%Y:%M:%D %H:%M:%S"),"%H:%M:%S")

library(ApacheLogProcessor)
library(stringr)
#separating method from URL
df$Method<- NA

df$Method<- clear.urls(df$URL, remove_http_method = FALSE)
sub("/.", " ", df$Method)
df$Method= str_extract(df$Method, "GET|POST")

df$URL<- clear.urls(df$URL, remove_http_method = TRUE)
View(df)
library(DT)
library(ggplot2)
library(tidyverse)

a= table(df$Status)
status = as.data.frame(a)
names(status)[1] = "status"
datatable(status)

ggplot(data= status, aes(x=Var1, y=Freq, group=1)) +
  geom_line() + xlab('Status') + ylab('Occurance') 

library(DT)
library(stringr)
library(ApacheLogProcessor)
b= table(df$URL)
pages= as.data.frame(b)
colnames(pages)= c("pages", "frequency")
datatable(pages)

pageweb = cbind(df, str_match(df[,"URL"], "\\.com\\/([^\\.]+\\.(php|html))"))
pageweb

library(anytime)
requests = as.data.frame(table(df$Date))
date <- as.Date(requests$Var1, format = "%m.%d.%Y")

library(lubridate)
requests$Var1 <- mdy(requests$Var1)

class(requests$Var1)
View(requests)


library(ggplot2)
library(ggplot)
library(dplyr)
ggpolt(data= requests, aes(x=as.Date(Var1), y=Freq)) +geom_line() + xlab('Date') + ylab('No of requests') + opts(title= 'Traffic')

ggplot(data= requests, aes(x=Var1, y=Freq)) + geom_line() + xlab('Date') + ylab('No of requests') + title= ('Traffic')

ggplot(data= requests, aes(x=Var1, y=Freq, group =1)) +
  geom_line() + xlab('Date') + ylab('No of requests') 


month_a<- as.data.frame(table(format(df$Date, "%m/%Y")))
month_a
ggplot(data = requests, aes(x=month_a$Var1, y=month_a$Freq)) +
  geom_line()
month_a<- as.data.frame(table(format(df$Date, "%m/%Y")))
ggplot(data = month_a, aes(x = Var1, y = Freq, group=1)) +
  geom_line() + xlab('Date') + ylab('No of requests')

library(urltools)
library(rvest)
urls1<- df$URL

strip_html<- function(urls){
  html_text(read_html(urls))
}

library(qdapRegex)
rm_url(urls1)
ex_url(urls1)

r<-url_parse(urls)
df$Date<-sub(".*:", " ", df$Date)
r$path<-sub("*.", " ", r$path)
View(r)

library(rgeolocate)
file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
results <- maxmind("196.200.60.51", file, c("continent_name", "country_code", "country_name"))
results

library(dygraphs)
library(xts)
don <- xts(x = df$URL, order.by = ac$Var1)

ac<- as.data.frame(table(df$Time))
ac$Freq <- sort(ac$Freq, decreasing = FALSE)
View(ac)
ac$Var1 <- hms(ac$Var1)

p <- ggplot(df, aes(x=requests$Var1, y=requests$Freq)) +
  geom_line() + 
  xlab("")
p



class(ac$Var1)
requests = as.data.frame(table(df$Date))
ggplot(data= requests, aes(x=Var1, y=Freq)) +
  geom_line() + xlab('Date') + ylab('No of requests')

group_by(Var1)
ggplot(data= requests, aes(x=Freq, y=Var1)) +
  geom_line()
df%>% 
  select(df$URL, df$Date)%>%
  pivot_longer(df$URL)%>%
  group_by(df$URL)%>%
  mutate(value = scale(value))%>%
  ungroup()%>%
  ggplot(aes(x= value, y=df$URL, fill= df$Date))
  geom_density_ridges()

ggplot2(data= df, aes(x=format(status))) + geom_bar() + xlab('status') + ylab('freq') + opts(title= 'Status')

library(dplyr)
library(tidyr)
library(stringr)




# some ip addresses 
ip_lst= df$IP
accesskey= '98c5b82231b0c3d23f964da5ad487f5b'
# a simple function
ip_locate <- function(ip_lst, accesskey) {
  
  ip <- your_vector_of_ip_addresses
  
  map_df(ip, ~{
    out <- httr::GET(url = paste0("http://api.ipstack.com/", .,
                                  "?access_key=", accesskey))
    resp <- fromJSON(httr::content(out, "text"), flatten = TRUE)
    tibble::tibble(ip = resp$ip, 
                   country = resp$country_name, 
                   region = resp$region_name, 
                   city = resp$city, 
                   zip = resp$zip, 
                   lat = resp$latitude, 
                   lng = resp$longitude)
    
  })
  
}

ip_info<- ip_locate(your_vector_of_ip_addresses= ip_lst, access_key= accesskey)



devtools::install_github("hrbrmstr/ipstack")
library(ipstack)
# current verison
packageVersion("ipstack")

ip_lst= table(df$IP)
ip_lst = as.data.frame(ip_lst)
names(ip_lst)[1] = "IP"

bulk_lookup(ip_lst$IP, ipstack_api_key = '98c5b82231b0c3d23f964da5ad487f5b')

ip_lst= table(df$IP)
ip_lst = as.data.frame(ip_lst)
names(ip_lst)[1] = "IP"

View(d)

library
d<- subset(locdf, select = -c(IP, Latitude, Longitude))
d<- d %>% group_by(Country)


devtools::install_github("dkahle/ggmap")
ggmap::register_google(key = "AIzaSyCGFAWVm4QGGjtUFg-0Yh376U-Ity2nwqs")

  
devtools::install_github("ironholds/rgeolocate")
library(ip2location)
location_df<- get_all(df$IP)

py_run_string(ip2location(
  ips = df$IP,
  file = "./location_db.csv",
  fields = c("lat", "long", "country_name")
))

library(reticulate)
library(ip2location)

py_run_string("import IP2Location")
py_run_string("IP2LocObj = IP2Location.IP2Location()")
py_run_string("IP2LocObj.open('IP2LOCATION-LITE-DB5.BIN')")
py_run_string("rec = IP2LocObj.get_all('94.164.200.219')")
py_run_string("print(rec.country_long)")
py_run_string("print(rec.latitude)")
py_run_string("print(rec.longitude)")

ip<- c("101.168.78.59", "111.101.66.248", "115.14.56.140", "118.206.244.2", "119.220.210.247", "131.41.78.243", "177.147.139.228", "185.124.38.12", "201.175.225.161", "241.91.16.200", "69.96.155.21", "79.202.86.209", "84.146.117.229", "91.68.162.55", "94.164.200.219")
country<-c("Australia", "Japan", "Korea (Republic of)", "China", "Korea (Republic of)", "United States of America", "Brazil", "Spain", "Mexico", "No name", "United States of America", "Germany","Germany", "France", "Italy")
lat<-c("-37.813999", "35.689507", "37.56826", "28.200001", "37.56826", "39.966381", "-18.165831", "40.4165", "19.417219", "0.0", "40.735661", "53.059631", "51.5", "48.853409", "44.22361")
long<-c("144.963318", "139.691696", "126.977829", "112.966667", "126.977829", "-83.012772", "-47.946388", "-3.70256", "-99.156937", "0.0", "-74.172371", "14.28154", "12.0", "2.3488", "12.05278")

ip_lst= table(df$IP)
ip_lst = as.data.frame(ip_lst)
names(ip_lst)[1] = "IP"

df_location<- data.frame(ip, country, as.numeric(lat), as.numeric(long))
names(df_location)<- c("IP", "Country", "Latitude", "Longitude")
View(df_location)

locdf<- merge(df_location, ip_lst, by= "IP")
View(locdf)
class(locdf$Latitude)

library(leaflet)
leaflet(locdf) %>% addTiles() %>%
  addCircleMarkers(radius= 0.5,lng= ~Longitude, lat = ~Latitude, popup = ~Country, ~Freq)



requests = as.data.frame(table(format(df$Date, "%d")))
ggplot(data= requests, aes(x=Var1, y=Freq, group=1)) +
  geom_line() + xlab('Day of month') + ylab('No of requests')

library(ggplotly)

library(plotly)
p1 <- plot_ly(requests, x = ~Var1, y = ~Freq, group=1) %>% 
  add_lines(name = "Traffic by Day of month")
p2 <- plot_ly(month_a, x = ~Var1, y = ~Freq, group=1) %>% 
  add_lines(name = "Traffic by month of year")
subplot(p1, p2)

requests = as.data.frame(table(format(df$Date, "%d")))
ggplot(data= requests, aes(x=Var1, y=Freq, group=1)) +
  geom_line() + xlab('Day of month') + ylab('No of requests') 

month_a<- as.data.frame(table(format(df$Date, "%m/%Y")))
ggplot(data = month_a, aes(x = Var1, y = Freq, group=1)) +
  geom_line() + xlab('Month') + ylab('No of requests')