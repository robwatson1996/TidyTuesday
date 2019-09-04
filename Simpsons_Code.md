Tidy Tuesday - Simpson Guest Appearances
================

R Markdown
----------

Import data and load packages
=============================

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages ----------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  2.1.3     v dplyr   0.8.1
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.3.1     v forcats 0.3.0

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## -- Conflicts -------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggridges)
```

    ## Warning: package 'ggridges' was built under R version 3.5.3

    ## 
    ## Attaching package: 'ggridges'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     scale_discrete_manual

``` r
simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")
```

    ## Parsed with column specification:
    ## cols(
    ##   season = col_character(),
    ##   number = col_character(),
    ##   production_code = col_character(),
    ##   episode_title = col_character(),
    ##   guest_star = col_character(),
    ##   role = col_character()
    ## )

``` r
summary(simpsons)
```

    ##     season             number          production_code   
    ##  Length:1386        Length:1386        Length:1386       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##  episode_title       guest_star            role          
    ##  Length:1386        Length:1386        Length:1386       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character

SPlit roles column into multiple columns
========================================

``` r
simpsons2<-simpsons%>%
          separate(role,c("Role1","Role2","Role3"), sep=';')%>%
          filter(season!="Movie")
```

    ## Warning: Expected 3 pieces. Additional pieces discarded in 5 rows [22, 50,
    ## 116, 205, 632].

    ## Warning: Expected 3 pieces. Missing pieces filled with `NA` in 1370
    ## rows [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20,
    ## 21, ...].

``` r
#Calculating the top guest appearances
top_apps <- simpsons2 %>%
          filter(season!="movie")%>%
          group_by(guest_star)%>%
          summarise(count=n())%>%
          arrange(desc(count))%>%
          head(5)

simpsons2$season=as.numeric(simpsons2$season)
```

Filtering on top 5 guest appearances
====================================

``` r
Top5<- simpsons2%>%
       inner_join(top_apps,by="guest_star")%>%
        group_by(season,guest_star)%>%
        summarise(counts=n())%>%
        arrange(season)
```

``` r
#Image of Edna 

library(magick)
```

    ## Warning: package 'magick' was built under R version 3.5.3

    ## Linking to ImageMagick 6.9.9.14
    ## Enabled features: cairo, freetype, fftw, ghostscript, lcms, pango, rsvg, webp
    ## Disabled features: fontconfig, x11

``` r
library(here) # For making the script run without a wd
```

    ## Warning: package 'here' was built under R version 3.5.3

    ## here() starts at Z:/23. Data Team Admin/Other/Tidy Tuesday/20190904 - Simpsons

``` r
library(magrittr)
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
img <- png::readPNG("Z:/23. Data Team Admin/Other/Tidy Tuesday/20190904 - Simpsons/Edna.png")

edna<-image_read("Z:/23. Data Team Admin/Other/Tidy Tuesday/20190904 - Simpsons/Edna.png")

print(edna)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG      166    400 sRGB       TRUE     17214 72x72

<img src="Simpsons_Code_files/figure-markdown_github/unnamed-chunk-2-1.png" width="166" />

``` r
edna_png <- image_convert(edna, "png")
image_info(edna_png)
```

    ## # A tibble: 1 x 7
    ##   format width height colorspace matte filesize density
    ##   <chr>  <int>  <int> <chr>      <lgl>    <int> <chr>  
    ## 1 PNG      166    400 sRGB       TRUE         0 72x72

``` r
image_scale(edna_png, "x100")
```

<img src="Simpsons_Code_files/figure-markdown_github/unnamed-chunk-2-2.png" width="42" />

Creating Plot
=============

``` r
Chart <- ggplot(Top5,aes(x=season,y=guest_star,fill=guest_star))+
           geom_density_ridges(scale=2,aes(
      
      point_fill = guest_star,
      point_shape = 21
    ),
     jittered_points = TRUE,
    alpha=0.5,
    point_alpha=1,
    size=0.75
    ) +
  theme_ridges() + 
  theme(legend.position = "none")+
   scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30),
                     limits = c(0, 30))+
  expand_limits(x = 0, y = 0)+
  
    theme(plot.background = element_rect(fill = "cornflowerblue"),
        panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text = element_text( hjust = 0.5, colour = "yellow"),
  axis.title = element_text(colour="yellow"),
  plot.title = element_text(color="yellow", size=14, face="bold",hjust = 3),
  plot.subtitle = element_text(color="yellow",size=10,hjust=-0.25),
  title = element_text("yellow"),
  axis.ticks = element_blank(),
  axis.title.x = element_text(hjust=0.5),
  axis.title.y = element_text(hjust=0.5)
    )+
  
  labs(x="Season",
         y="Guest Star",
         title="Simpsons Guest Appearances by Top Appearing Guests",
       subtitle="Marcia Wallace has the greatest number of guest appearances playing Edna
       ")




  

Chart
```

    ## Picking joint bandwidth of 2.9

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x
    ## $y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

![](Simpsons_Code_files/figure-markdown_github/Creating%20Graph-1.png)

``` r
ggsave("Simpsons.png",width=12,height=7)
```

    ## Picking joint bandwidth of 2.9

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call.graphics(C_text, as.graphicsAnnot(x$label), x$x, x
    ## $y, : font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database

    ## Warning in grid.Call(C_textBounds, as.graphicsAnnot(x$label), x$x, x$y, :
    ## font family not found in Windows font database
