library("ggplot2")
library("knitr")
library("latticeExtra")
## Loading required package: RColorBrewer
## Loading required package: lattice
## 
## Attaching package: 'latticeExtra'
## 
## The following object is masked from 'package:ggplot2':
## 
##     layer
library("dplyr")
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
library("hexbin")
# barchart showing the number of shots of each shot type of kobe bryant
getwd()
file_upd<- file.choose()
mydata = read.csv(file_upd)
barchart(sort(table(mydata$combined_shot_type))
         , col = "lightsalmon4"
         , border = "transparent"
         , xlim = c(0, 27000)
         , xlab = "Number of shots"
         , main = "Number of shots by shot type"
         , panel = function(...){
           panel.abline(v = seq(0, 26000, 1000), col = "gray90")
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

#comparing the type of shots of kobe bryant during regular season and playoffs
comparision <- as.data.frame(xtabs(~ playoffs + combined_shot_type, mydata))
comparision $combined_shot_type <- factor(comparision $combined_shot_type, levels(comparision $c
                                                                                  ombined_shot_type)[c(1, 3, 6, 2, 5, 4)])
levels(comparision $playoffs) <- c("Regular season", "Playoffs")
levels(comparision $playoffs) <- paste0(levels(comparision $playoffs), ": "
                                        , table(mydata$playoffs), " (", round(prop.table(table(m
                                                                                               ydata$playoffs)), 4)*100, "%)")
barchart(combined_shot_type ~ Freq | playoffs
         , col = "#0080ff"
         , border = "transparent"
         , scales = list(x = "free")
         , xlim = list(c(0, 27000), c(0, 4500))
         , strip = strip.custom(bg = "white")
         , xlab = "Number of shots"
         , main = "Number of shots, regular season vs playoffs"
         , comparision
         , panel = function(...){
           args <- list(...)
           panel.text(
             args$x, args$y, paste0(args$x, " (", round(prop.table(args$x), 3)*100, "%)")
             , pos = 4)
           panel.barchart(...)})

# visualizing kobe bryant's hits vs misses data
hitvsmiss <- ggplot(data = mydata) +
  geom_hex(aes(x=loc_x, y=loc_y), binwidth = c(15,15)) +
  scale_fill_gradient(trans = "log", low = "darkorchid", high = "olivedrab1") +
  theme(legend.position="none") +
  facet_wrap(~ shot_made_flag) +
  coord_fixed() +
  ggtitle(paste("Misses vs Hits"))

# hitvsmiss graph
hitvsmiss

#visualizing the plot that maps position by feature
shotplot <- function(feat) {
  feat <- substitute(feat)
  mydata %>%
    ggplot(aes(x = lon, y = lat)) +
    geom_point(aes_q(color = feat), alpha = 0.7, size = 3) +
    ylim(c(33.7, 34.0883)) +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    ggtitle(paste(feat))
}
#position by feature plot
shotplot (combined_shot_type)


