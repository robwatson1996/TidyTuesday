Tidy Tuesday Video Games
================

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 3.5.3

    ## -- Attaching packages ----------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.2.0     v purrr   0.3.2
    ## v tibble  2.1.1     v dplyr   0.8.1
    ## v tidyr   0.8.3     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.4.0

    ## Warning: package 'ggplot2' was built under R version 3.5.3

    ## Warning: package 'tibble' was built under R version 3.5.3

    ## Warning: package 'tidyr' was built under R version 3.5.3

    ## Warning: package 'readr' was built under R version 3.5.3

    ## Warning: package 'purrr' was built under R version 3.5.3

    ## Warning: package 'dplyr' was built under R version 3.5.3

    ## Warning: package 'stringr' was built under R version 3.5.3

    ## Warning: package 'forcats' was built under R version 3.5.3

    ## -- Conflicts -------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 3.5.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   number = col_double(),
    ##   game = col_character(),
    ##   release_date = col_character(),
    ##   price = col_double(),
    ##   owners = col_character(),
    ##   developer = col_character(),
    ##   publisher = col_character(),
    ##   average_playtime = col_double(),
    ##   median_playtime = col_double(),
    ##   metascore = col_double()
    ## )

``` r
summary(video_games)
```

    ##      number         game           release_date           price        
    ##  Min.   :   1   Length:26688       Length:26688       Min.   :  0.490  
    ##  1st Qu.: 821   Class :character   Class :character   1st Qu.:  2.990  
    ##  Median :2356   Mode  :character   Mode  :character   Median :  5.990  
    ##  Mean   :2904                                         Mean   :  8.947  
    ##  3rd Qu.:4523                                         3rd Qu.:  9.990  
    ##  Max.   :8846                                         Max.   :595.990  
    ##                                                       NA's   :3095     
    ##     owners           developer          publisher        
    ##  Length:26688       Length:26688       Length:26688      
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ##                                                          
    ##                                                          
    ##  average_playtime   median_playtime     metascore    
    ##  Min.   :   0.000   Min.   :   0.00   Min.   :20.00  
    ##  1st Qu.:   0.000   1st Qu.:   0.00   1st Qu.:66.00  
    ##  Median :   0.000   Median :   0.00   Median :73.00  
    ##  Mean   :   9.057   Mean   :   5.16   Mean   :71.89  
    ##  3rd Qu.:   0.000   3rd Qu.:   0.00   3rd Qu.:80.00  
    ##  Max.   :5670.000   Max.   :3293.00   Max.   :98.00  
    ##  NA's   :9          NA's   :12        NA's   :23838

``` r
data <- video_games %>%
  mutate(release_date = mdy(release_date))
```

    ## Warning: 1 failed to parse.

``` r
df <- data%>%
  select(publisher,release_date)%>%
  group_by(publisher) %>%
  summarise(max_date=max(release_date), min_date=min(release_date),counts=as.numeric(n()))%>%
  mutate(lag=as.numeric((formatC((max_date-min_date)/365,digits=2, format="f"))))%>% 
  arrange(desc(counts))
```

    ## Warning: NAs introduced by coercion

``` r
df<-na.omit(df) 
  
df2 <- head(df,20) 
```

``` r
graph <- ggplot(df2, aes(x=lag,y=counts))+
  geom_point()+
  geom_label(label=paste(df2$publisher,":",df2$counts), fill="red", color='white', check_overlap = TRUE, size=2,alpha=0.25)+
  labs(x="Years Active", y = "Number of Video Games Released",title="Does Number of Years Active Affect the Number of Video Games Released?")+
  xlim(-2,17.5)+
 theme(panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(),
       panel.background = element_rect(fill = 'black'),
       plot.background = element_rect(fill = 'black'),
       axis.title.y = element_text(color ='white'),
       axis.title.x = element_text(color ='white'),
       axis.text = element_text( color = "white"),
       plot.title = element_text(colour = 'white',face="bold",size=12))
```

    ## Warning: Ignoring unknown parameters: check_overlap

``` r
graph
```

![](Tidy_Tuesday_Video_Games_files/figure-markdown_github/Scatter%20Diagram%20looking%20at%20years%20active-1.png)

``` r
ggsave("videogames_tidytuesday.png", dpi = "retina")
```

    ## Saving 7 x 5 in image
