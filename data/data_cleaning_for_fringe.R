
library(readr)
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(tidyr)

library(dplyr)
library(readxl)
data_path <- "data//events_data-2025-01-15-16-07-31.xlsx"
file.exists(data_path)
fringe_data <- read_xlsx(data_path, skip = 2)
head(fringe_data)
names(fringe_data)



numeric_version <- c("Performers #",
                      "Performances #",
                      "Lowest full price",
                      "Lowest concession price")  


fringe_data <- fringe_data %>%
  mutate(across(all_of(numeric_version), as.numeric))

# AGE CATEGORY NA GELİYOR DÜZELTEMEDİM.
summary(fringe_data$`Age category`)    
class(fringe_data$`Age category`)
glimpse(fringe_data)

str(fringe_data)
head(fringe_data$`Age category`)

# 
head(fringe_data$`Performers #`)

which(is.na(fringe_data$`Performers #`))

which(is.na(fringe_data$`Performances #`))

which(is.na(fringe_data$`Lowest full price`))

which(is.na(fringe_data$`Lowest concession price`))

sum(is.na(fringe_data$`Lowest concession price`))
nrow(fringe_data)


# since the # of na is large i'll first try to replace with median.
# skewness is 10.19245 so using median is the safest choice

#to use the skewness command
library(e1071)
skewness(fringe_data$`Lowest concession price`)

fringe_data <- fringe_data %>%
  mutate(`Lowest concession price` = replace_na(`Lowest concession price`, median(`Lowest concession price`, na.rm =  T)))

summary(fringe_data$`Lowest concession price`)
glimpse(fringe_data$`Lowest concession price`)
hist(fringe_data$`Lowest concession price`)

# log transformations

fringe_data <- fringe_data %>%
  mutate(`Lowest concession price` = replace_na(`Lowest concession price`, median(`Lowest concession price`, na.rm =  T)))
ggplot(fringe_data, aes( fringe_data$`Lowest concession price`))+
  geom_histogram(bins = 30)

ggplot(fringe_data, aes( log(fringe_data$`Lowest concession price`)))+
  geom_histogram(bins = 30)

kurtosis(fringe_data$`Lowest concession price`)
#çok yüksek değerler geldi. 241 diyor

x <- fringe_data$`Performances #`
y <- fringe_data$`Performers #`

lm <- lm(y ~x, data= fringe_data)

qqplot(x, y)
  
abline(lm, lwd=2)

sum(which(is.na(clean_third$`Performances #`)))
sum(which(fringe_data$`Performers #`== 0))


qqplot(clean_third$`Performances #`, y= clean_third$`Performers #`)

## Aynı kuantil noktalarına en-iyi uyum çizgisi
abline(lm(clean_third$`Performers #` ~ clean_third$`Performances #`), col = "red", lwd = 2)     

coef(lm(qq$y ~ qq$x))

#ikiside normal değil
shapiro.test(fringe_data$`Performances #`)
shapiro.test(fringe_data$`Performers #`)

# log değişimiyle de normalleşmiyorlar.
shapiro.test(log(fringe_data$`Performances #`))
shapiro.test(log(fringe_data$`Performers #`))

# spearman check
scor.test(fringe_data$`Performances #`,
         fringe_data$`Performers #`,
         method = "spearman")   
#kendal tau
cor.test(fringe_data$`Performances #`,
         fringe_data$`Performers #`,
         method = "kendall")  

names(fringe_data)
#düzenleme başarız


#changing the dates to a usable format

library(dplyr)
library(lubridate)

fringe_data$`Last performance date` <- as.Date(fringe_data$`Last performance date`)
fringe_data$`First performance date`<- as.Date(fringe_data$`First performance date`)


##  added the new performance_days_total column

fringe_data <- fringe_data %>%
  mutate(performance_days_total=as.integer((`Last performance date`-  `First performance date`+1)))
names(fringe_data)

ggplot(fringe_data, aes(y=`Performances #`, x=`performance_days_total`))+
  geom_point()


# OUTLIERS


#install.packages("rstatix")
library(rstatix)

outliers <- fringe_data %>%
  identify_outliers(performance_days_total)


names(outliers)

outliers %>% 
  select(performance_days_total)  

# these are incorrect data firnge is only 30 days long so i am dropping them.



fringe_data_clean_first <- fringe_data%>%
  filter(performance_days_total < 31)

summary(fringe_data_clean_first$performance_days_total)

ggplot(fringe_data_clean_first, aes(y=`Performances #`, x=`performance_days_total`))+
  geom_point()

ggplot(fringe_data_clean_first, aes(y=`Performances #`, x=`performance_days_total`))+
  geom_point()

#now i need to check # of performances

outliers2 <- fringe_data%>%
  identify_outliers(`Performances #`)
outliers2%>%
  select(`Performances #`)

# 30 günde gösteri sayısının 300 lere yaklaşması çok zor . 3.IQR ye kadar alıyorum. ektremleri trimlemek için

summary(fringe_data_clean_first$`Performances #`)


cut_3rd_quartile<- quantile(fringe_data_clean_first$`Performances #`, .75)+
  1.5*IQR(fringe_data_clean_first$`Performances #`)

clean_second <- fringe_data_clean_first %>%
  filter(`Performances #`<= cut_3rd_quartile)

cut_3rd_quartile_price<- quantile(clean_second$`Lowest full price`, .95)


### WINSORIZATIN DENİYCEM
## tavanı yazdığım sayıya yakınlaştırıyor , o sayıdan fazla olanlar direk o sayısıya eşit geliyorlar.
## veri kaybetmiyoruz sadece etkisini azaltıyoruz grafiklerde


clean_third <- clean_second %>% 
  mutate(`Lowest full price` = pmin(`Lowest full price`, cut_3rd_quartile_price))
summary(clean_third$`Lowest full price`)



outliers3 <- clean_second %>%
  identify_outliers(`Lowest full price`)
outliers3 %>%
  select(`Lowest full price`)


.Renviron
usethis::edit_r_environ()
#------------------------------------------------------------
sum(which(is.na(clean_second$`Artist type`)))
library(ggplot2)
ggplot(data= clean_second, aes(x= `Performances #`, y= `Artist type`))+
  geom_boxplot(outlier.colour = "red", outlier.shape = 21, fill = "lightblue") +
  labs(x = "Number of Performances", y = "Artist type #",
       title = "Gösteri Sayısı ~ Sanatçı Türü (box plot)") +
  theme_minimal()


#----------------------------------------------------------
ggplot(data=clean_third, aes( x= `Lowest full price`, y= `Performances #`))+
  geom_point(size=2, alpha= 0.7)+
  labs(x = "Lowest full price (£)",
       y = "Performances #",
       title = "Bilet Fiyatı ile Gösteri Sayısı İlişkisi") +
  theme_minimal()
 
qqplot(clean_second$`Lowest full price`, clean_second$`Performances #`)
#-------------------------------------------------------
  
library(ggplot2)

ggplot(clean_third,
       aes(y = reorder(Title, `Performances #`),   # azdan çoğa sırala
           x = `Performances #`)) +
  geom_col() +                                    # = geom_bar(stat = "identity")
  labs(x = "Performances #",
       y = "Title",
       title = "Her Gösterinin Toplam Performans Sayısı") +
  theme_minimal()

#----------------------------------------
library(dplyr)
library(ggplot2)

clean_third %>% 
  arrange(desc(`Performances #`)) %>%        
  slice_head(n = 20) %>%                     
  ggplot(aes(x = `Performances #`,
             y = reorder(Title, `Performances #`))) +
  geom_col() +                               
  labs(x = "Performances #",
       y = "Title (Top 20)",
       title = "En Yüksek 20 Gösteri – Performans Sayısı") +
  theme_minimal()
#--------------------------------------------------------------
  
ggplot(data= clean_third, aes(x= `Genre`, y= `Performances #` ))+
  geom_boxplot(outlier.colour = "black", outlier.shape = 21, fill = "pink")+
  labs(x= " Genre", y= "# of Performances")
         
## events diye ayrın bri genre çıkıyor ????

#-------------------------------------------------------------------------
  
  
ggplot(data=clean_third, aes(x= `Genre`, y= `Artist type`, fill = `Artist type`))+
  geom_col(position = "stack", width = 0.8)+
  labs(x= "genre", y= " artist type", title = "artist type by genre")+
  coord_flip() +
  theme_bw()

ggplot(data=clean_third, aes(x= `Genre`, fill = `Artist type`))+
  geom_bar(position = "stack", width = 0.8)+
  labs(x= "genre", y= " artist type", title = "artist type by genre")+
  coord_flip() +
  theme_bw()


#------------------------------------------------
library(dplyr)
library(ggplot2)
library(forcats)   # fct_reorder
library(scales)    # percent_format

clean_third %>% 
  count(`Genre`, `Artist type`) %>%                # her kombinasyonu say
  ggplot(aes( x = n,                             # yatay yüzde için x
              y = fct_reorder(`Genre`, n, sum),    # genre’leri toplamına göre sırala
              fill = `Artist type`)) +
  geom_col(position = "fill", width = .8) +      # "fill"  ⇒ yüzde yığma
  scale_x_continuous(labels = percent_format()) +
  coord_flip() +                                 # yan (horizontal) yap
  labs(title = "Artist type payları (%%) – Genre bazında",
       x = "% içinde Artist type",
       y = NULL,
       fill = "Artist type") +
  theme_minimal()



#----------------------------------------
  #map
  
library(osmdata)  #openstreet map data
library(tmap)              
library(sf)                
library(rnaturalearth)     
library(dplyr)            

map_for_fringe <- st_as_sf(clean_third, coords = c("Longitude", "Latitude"),
                          crs= 4326)

scottish_map<- ne_states(country = "United Kingdom",
                         geounit = "Scotland",
                         returnclass = "sf") |>st_transform(4326)

edinburgh_boundary <- getbb("Edinburgh, Scotland",
                            format_out = "sf_polygon")$multipolygon |>
  st_transform(4326)

map_for_fringe <- st_filter(map_for_fringe, edinburgh_boundary, .predicate = st_within)



tmap_mode("plot")

map_plot <- tm_shape(scottish_map) +
  tm_polygons(col = "black", border.col= "grey") +
  
  tm_shape(map_for_fringe) +
  tm_dots(
    fill ="Performance #",        
    palette = "viridis",
    style = "quantile",     # sayılsa “cat” yerine “quantile” iyi durur; kategorikse style = "cat"http://127.0.0.1:19927/graphics/1c48a6c3-814e-468a-940b-c47928dd0cdf.png
    n= 5,
    size = 0.07,
    legend.size.show = FALSE)+
            
  tm_layout(title = "FRİNGE SCOTLAND")

m_view

#------------------------------------------------
library(tmap)        # v4+ olmalı
library(sf)
library(osmdata)     # OSM’den sınır
library(geodata)     # GADM yedeği
library(dplyr)

# ------------------------------------------------------------
# 1) Veriyi sf'ye dönüştür
# ------------------------------------------------------------
# clean_third <- read.csv("veriniz.csv")        # gerekirse dosyadan oku
map_for_fringe <- st_as_sf(clean_third,
                           coords = c("Longitude", "Latitude"),
                           crs    = 4326)

# ------------------------------------------------------------
# 2) Edinburgh sınırı (önce OSM, yoksa GADM’le yedekle)
# ------------------------------------------------------------
get_edinburgh <- function(){
  # a) OSM dene
  ed <- tryCatch({
    osmdata::getbb("Edinburgh, Scotland",
                   format_out = "sf_polygon")
  }, error = function(e) NULL)
  
  if (!is.null(ed) && inherits(ed, "sf")) return(st_transform(ed, 4326))
  
  # b) GADM yedeği
  message("OSM bulamadı, GADM kullanılıyor…")
  uk2 <- gadm("GBR", level = 2, path = tempdir())
  ed  <- uk2[uk2$NAME_2 == "Edinburgh, City of", ]
  st_transform(ed, 4326)
}

edinburgh_boundary <- get_edinburgh()

# ------------------------------------------------------------
# 3) Noktaları Edinburgh içinde filtrele
# ------------------------------------------------------------
inside <- st_within(map_for_fringe, edinburgh_boundary,
                    sparse = FALSE)[, 1]       # mantıksal vektör
map_for_fringe <- map_for_fringe[inside, ]

# ------------------------------------------------------------
# 4) tmap  –  ETKİLEŞİMLİ + STATİK
# ------------------------------------------------------------
## 4A) Etkileşimli
tmap_mode("view")


library(tmap)        # v4+ olmalı
library(sf)
library(osmdata)     # OSM’den sınır
library(geodata)     # GADM yedeği
library(dplyr)

# ------------------------------------------------------------
# 1) Veriyi sf'ye dönüştür
# ------------------------------------------------------------
# clean_third <- read.csv("veriniz.csv")        # gerekirse dosyadan oku
map_for_fringe <- st_as_sf(clean_third,
                           coords = c("Longitude", "Latitude"),
                           crs    = 4326)

# ------------------------------------------------------------
# 2) Edinburgh sınırı (önce OSM, yoksa GADM’le yedekle)
# ------------------------------------------------------------
get_edinburgh <- function(){
  # a) OSM dene
  ed <- tryCatch({
    osmdata::getbb("Edinburgh, Scotland",
                   format_out = "sf_polygon")
  }, error = function(e) NULL)
  
  if (!is.null(ed) && inherits(ed, "sf")) return(st_transform(ed, 4326))
  
  # b) GADM yedeği
  message("OSM bulamadı, GADM kullanılıyor…")
  uk2 <- gadm("GBR", level = 2, path = tempdir())
  ed  <- uk2[uk2$NAME_2 == "Edinburgh, City of", ]
  st_transform(ed, 4326)
}

edinburgh_boundary <- get_edinburgh()

# ------------------------------------------------------------
# 3) Noktaları Edinburgh içinde filtrele
# ------------------------------------------------------------
inside <- st_within(map_for_fringe, edinburgh_boundary,
                    sparse = FALSE)[, 1]       # mantıksal vektör
map_for_fringe <- map_for_fringe[inside, ]

# ------------------------------------------------------------
# 4) tmap  –  ETKİLEŞİMLİ + STATİK
# ------------------------------------------------------------
## 4A) Etkileşimli
tmap_mode("view")
library(tmap)
tmap_mode("view")   # etkileşimli

m_view <- tm_shape(edinburgh_boundary) +
  tm_borders(col = "grey40", lwd = 1.3) +
  
  tm_shape(map_for_fringe) +
  tm_dots(
    # ----- RENK -----
    fill     = "Performances #",     # sayısal değişken
    palette  = "heat",             # açık → koyu kırmızı
    style    = "cont",
    colorNA = NULL,
    # sürekli skala
    
    # ----- BOYUT -----
    size     = "Performances #",     # değişkene bağla
    scale    = 1.5,                # (ayarlayarak büyüt/küçült)
    
    # ----- POP-UP -----
    popup.vars = c(
      "Performans" = "Performances #"
    ),
    
    clustering = TRUE
  ) +
  tm_layout(
    title     = "Edinburgh | Performans Yoğunluğu",
    legend.outside = TRUE
  )

m_view
 # çok da beğenmedim bunları 

#------------------------------
library(ggplot2)
library(forcats)
sum(which(is.na(clean_third$Country)))

# EN ÇOK SPEROFRMANSI GERÇEKLEŞTİREN ÜLKELER
ggplot(data = clean_third, aes(x= fct_rev(fct_lump_n(fct_infreq(Country),20 )),y= `Performances #`))+
  geom_col()+
  coord_flip()


sum(clean_third$`Performances #`)

## YÜZDELİK OLARAK MOST PEROFRMANCES BY COUNTRY
ggplot(data = clean_third, aes(x= fct_rev(fct_lump_n(fct_infreq(Country),20 )),y= `Performances #`/50573*100))+
  geom_col()+
  coord_flip()

# bnunn genre yla kıyaslanma haline bak faceting yap 
ggplot(data=clean_third, aes(x=(fct_reorder(Genre, `Performances #`, .desc=F )), y= `Performances #`))+
  geom_col()+
  coord_flip()

ggplot(data=clean_third, aes(x=(fct_reorder(Genre, `Performances #`, .desc=TRUE )), y= `Performances #`))+
  stat_summary(fun= sum, geom = "col")+
  coord_flip()+
  theme_light()
#BUNLAR DÜZELMEDİ

#-------------------------------------------------------
 
# from data to viz e bak 
names(clean_third)[sapply(clean_third,is.numeric)]
#---------------------------------------------------
#DENSITY
#---------------------------------------------------

ggplot(data= clean_third, aes(x= `Lowest full price`))+
  geom_density(fill= "lightblue", alpha= 0.5)+
  labs( title= "dur ",
        x= "lowwest price",
        y= " density")+
  theme_bw()


#-------------------------------------------------------------------------------
#HISTOGRAMS
#-------------------------------------------------------------------------------
# full price vs concession price
head(clean_third$`Lowest concession price`)
summary(clean_third$`Lowest full price`)

ggplot(clean_third)+
  geom_histogram(aes(x= `Lowest full price`),
                 fill= "pink", alpha= 0.5, color= "black", bins= 30)+
  geom_histogram(aes(x= `Lowest concession price`),
                 fill= "lightblue", alpha= 0.5, color= "black", bins=30)+
  labs(x= "lowest price",
       y= "lowest concession price")+
  xlim(c(1,25))+
  theme_minimal()


# total number of performances per days
ggplot(clean_third)+
  geom_histogram(aes(x= performance_days_total),
                 fill= "darkgreen", alpha =0.6, bins = 20, color= "black")+
  labs(title = "total days of performance",,
       x= "performance days",
       y= "count")+
  theme_minimal()


#
library(grid)
library(gridExtra)
plot1<- ggplot(clean_third)+
  geom_histogram(aes(x= `Performances #`),
                 fill= "blue", bins = 20, color= "black")+
  theme_classic()

plot2 <- ggplot(clean_third)+
  geom_histogram(aes(x= `Performers #`),
               fill= "pink", bins = 20, color= "black")+
  theme_classic()


plot_grid(plot1 ,plot2 , nrow= 1)
summary(clean_third$`Performers #`)

which(clean_third$`Performers #`>100)



cap_perf       <- quantile(clean_third$`Performances #`, .99, na.rm = TRUE)
cap_performers <- quantile(clean_third$`Performers #`,   .99, na.rm = TRUE)

df2 <- clean_third %>%
  filter(`Performances #` <= cap_perf,
         `Performers #`   <= cap_performers)

p1 <- ggplot(df2, aes(x = `Performances #`)) +
  geom_histogram(binwidth = 1, fill = "royalblue", color = "black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Performances #", y = "Count") +
  theme_classic()

p2 <- ggplot(df2, aes(x = `Performers #`)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = "Performers #", y = "Count") +
  theme_classic()

plot_grid(p1, p2, nrow = 1)

#bence güzel dğil bu

#VIOLIN

ggplot(clean_third, aes(x=Genre, y= `Lowest full price`))+
  geom_violin(fill= "#b2df8a", alpha=0.7, color= "black")+
  geom_boxplot(width=0.125, fill= "#fdd0a2", outlier.colour = "red")+
  labs(title= "Lowest price by genre",
       x= "Genre",
       y= "Lowest Price in pound")
  theme_light()

  
ggplot(clean_third, aes(x=Genre, y= performance_days_total))+
  geom_violin(fill= "lightpink", alpha= 0.7, color = "black")+
  geom_boxplot(width= 0.1, fill= "#cbb67c", outlier.colour = "red")+
  labs(title= " Distirbution of total days of performance by genre",
       x= "Genre",
       y= "Total days of perofrmance")+
  theme_minimal()

# age cateogries made 
clean_fourth<- clean_third%>%
  mutate(
    age_number=as.numeric(gsub("\\+", "", `Age category`))
  )
clean_fourth<- clean_fourth%>%
  mutate(
    age_group=case_when(
      age_number==0 ~ "All ages",
      age_number<8 ~ "Kids",
      age_number>=8 & age_number<13 ~ "Pre Teens",
      age_number>= 13 & age_number<18 ~ "Teens",
      age_number>=18 ~ "Adults"
      ) ) %>%
  mutate(
    age_group = factor(age_group,
                       levels = c("All ages", "Kids", "Pre Teens", "Teens", "Adults"))
  )




# bu güzelll

ggplot(clean_fourth, aes(x= age_group, y= `Performances #`))+
  geom_violin(fill= "blue", color="black", alpha= 0.4)+
  geom_boxplot(width=0.4, fill= "lightblue", outlier.color= "red")+
  labs(title= "  # perofrmances by age categories",
       x= "age categories",
       y= "# of performance")+
  theme_minimal()

#--------------------------------------------------
#SCATTER DENEMELERİ
#---------------------------------------------------------------

# bu güzel gelmiycek gibi
ggplot(clean_fourth,aes(x=`Lowest full price`,y= `Performances #`))+
  geom_point()

# üstüne genreyle renk ekle ve direk age group la falan faceting ekle bak ''UNUTMAA


qqplot(clean_fourth$`Lowest full price`,clean_fourth$`Performances #`)
lm<- lm(clean_fourth$`Lowest full price`~clean_fourth$`Performances #`)
abline(lm)


# performer vs performances
ggplot(clean_fourth, aes(x=`Performers #`, y= `Performances #`))+
  geom_point()
#bir tane outlier var gibi dueruyor outlierlerı  atıp deniyorum

clean_fifth <- clean_fourth%>%
  mutate(`Performers #`<100)

outliers4 <- clean_fourth%>%
  identify_outliers(`Performers #`)
outliers4%>%
  select(`Performers #`)

#outlier sayısı beklediğimden çok.datanın 95% sini alıyorum

clean_fifth<- clean_fourth$`Performers #` %>%
  mutate(outlier.alpha =.5)


#MOST USED VENUES 
names(clean_fifth)

head(clean_fifth$`Artist type`)
library(treemap)
library(ggplot2)
#------------------------------------------
# TREEMAPS
#-------------------------------------------------
library(RColorBrewer)
library(viridis)


cols <- viridis(length(unique(clean_fifth$Genre)), option = "A")
treemap(clean_fifth,
        index = c( "Genre", "Artist type"),
        vSize = "Performances #",
        vColor = "Genre",
        type = "index" ,
        title = "total perofrmance by genre",
        palette = cols,
        
        border.lwds = c(0,1),
        bg.labels = 0,
        border.col = "black",
        fontsize.labels = c(1,20),
        fontcolor.labels =c(1,1),
        fontfamily.labels = "Times",
        fontface.labels = c(3,2),
        align.labels = list(c("center", "center"), c("center","center")),
        inflate.labels = F,
        position.legend = "bottom",
        fontsize.legend = 8)

plot(tm)




library(treemap)
library(RColorBrewer)

# Matching palette to your genres
cols <- brewer.pal(length(unique(clean_fifth$Genre)), "Set3")

treemap(clean_fifth,
        index = c("Artist type", "Genre"),
        vSize = "Performances #",
        vColor = "Artist type",
        type = "categorical",
        palette = cols,
        
        border.lwds = c(0,1),
        border.col = "black",
        
        # Label settings
        fontsize.labels = c(1, 1),       # 0 for Genre, 7 for Artist type
        fontcolor.labels = c(1, 1),
        fontfamily.labels = "Times",
        fontface.labels = c(3, 2),
        align.labels = list(c("center", "center"),
                            c("center", "center")),
        inflate.labels = FALSE,
        bg.labels = 0,
        
        # Legend settings
        position.legend = "right",       # or "bottom"
        fontsize.legend = 8,             # smaller legend text
        title = "Total Performance by Genre")

