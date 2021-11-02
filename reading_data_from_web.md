reading\_data\_from\_the\_web.Rmd
================

``` r
library(tidyverse)
library(rvest)
library(httr)
```

### Read URL:

``` r
url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

drug_use_html = read_html(url) %>%
  html_table()


table_marj = 
  drug_use_html %>% 
  first() %>%
  slice(-1) 

table_marj
```

    ## # A tibble: 56 × 16
    ##    State      `12+(2013-2014)` `12+(2014-2015)` `12+(P Value)` `12-17(2013-2014…
    ##    <chr>      <chr>            <chr>            <chr>          <chr>            
    ##  1 Total U.S. 12.90a           13.36            0.002          13.28b           
    ##  2 Northeast  13.88a           14.66            0.005          13.98            
    ##  3 Midwest    12.40b           12.76            0.082          12.45            
    ##  4 South      11.24a           11.64            0.029          12.02            
    ##  5 West       15.27            15.62            0.262          15.53a           
    ##  6 Alabama    9.98             9.60             0.426          9.90             
    ##  7 Alaska     19.60a           21.92            0.010          17.30            
    ##  8 Arizona    13.69            13.12            0.364          15.12            
    ##  9 Arkansas   11.37            11.59            0.678          12.79            
    ## 10 California 14.49            15.25            0.103          15.03            
    ## # … with 46 more rows, and 11 more variables: 12-17(2014-2015) <chr>,
    ## #   12-17(P Value) <chr>, 18-25(2013-2014) <chr>, 18-25(2014-2015) <chr>,
    ## #   18-25(P Value) <chr>, 26+(2013-2014) <chr>, 26+(2014-2015) <chr>,
    ## #   26+(P Value) <chr>, 18+(2013-2014) <chr>, 18+(2014-2015) <chr>,
    ## #   18+(P Value) <chr>

### learning assesement:

``` r
ny_living = read_html("https://www.bestplaces.net/cost_of_living/city/new_york/new_york") %>% 
  html_table(header = TRUE) %>% 
  first()
```

``` r
swm_html = 
  read_html("https://www.imdb.com/list/ls070150896/")

title_vec = 
  swm_html %>%
  html_elements(".lister-item-header a") %>%
  html_text()

gross_rev_vec = 
  swm_html %>%
  html_elements(".text-small:nth-child(7) span:nth-child(5)") %>%
  html_text()

runtime_vec = 
  swm_html %>%
  html_elements(".runtime") %>%
  html_text()

swm_df = 
  tibble(
    title = title_vec,
    rev = gross_rev_vec,
    runtime = runtime_vec)

votes = 
  swm_html %>%
  html_elements(".mode-detail:nth-child(9) span , .mode-detail:nth-child(8) span , .mode-detail:nth-child(7) span , .mode-detail:nth-child(6) span , .mode-detail:nth-child(5) span , .mode-detail:nth-child(3) span , .mode-detail:nth-child(1) span") %>%
  html_text()
```

### Napolean Dynamite:

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars
)
```

``` r
nyc_water = 
  GET("https://data.cityofnewyork.us/resource/ia2d-e54m.csv") %>% 
  content("parsed")
```
