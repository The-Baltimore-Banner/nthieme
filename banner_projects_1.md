Racial disparities in the tax lien system and initial profits
================
Nick Thieme
2023-01-25

## Introduction

This is the GitHub that goes along with the Baltimore Banner story
`An unpaid tax bill can quickly land a Baltimore home in a tax sale. But at what cost to homeowners — and the city?`
located [here](thebaltimorebanner.com) by Nick Thieme and Sophie
Kasakove. It is one of two files that creates the analysis. The second
file located [here]() is titled “creating_tax_lien_data.R” and performs
necessary pre-processing. I recommend reading that file for a fuller
understanding of the project and how to reproduce it in other
jurisdictions.

## Library

We load our functions and data in here. Because of the way that names
change when writing sf files, I also write out name files.

### EDA and plots

Here we calculated the number of liened homes, number of liened homes in
the Black Butterfly, the percent of liened homes in Black Butterfly, the
percent of total homes in the Black Butterfly, and the percent of sold
homes in Black Butterfly. The tax lien system, as described in the
article, disproportionately affects Black Baltimoreans. This section
gives some evidence of that, as well as providing correct comparison
points for that evidence.

    ## [1] 40988

    ## [1] 34282

    ## [1] 0.8363911

    ## [1] 0.6390777

    ## [1] 1

We calculate the percent of total homes liened and sold in different
neighborhoods, and write out the data for a Datawrapper chart. This
requires joining CSAs from the BNIA with the shapefiles of properties
and liened properties to calculate liened property percent per
neighborhood. This is, as far as I can tell, the first calculation of
liened buildings in Baltimore that uses the number of buildings as the
denominator.

    ## # A tibble: 56 × 4
    ##    Community                       n_homes n_homes_liened perc_liened
    ##    <chr>                             <int>          <int>       <dbl>
    ##  1 Southwest Baltimore                9541           4366        45.8
    ##  2 Sandtown-Winchester/Harlem Park    7561           3190        42.2
    ##  3 Clifton-Berea                      5349           2117        39.6
    ##  4 Midway/Coldstream                  4634           1648        35.6
    ##  5 Greater Rosemont                   7887           2706        34.3
    ##  6 Greenmount East                    5560           1851        33.3
    ##  7 Madison/East End                   3606           1128        31.3
    ##  8 Penn North/Reservoir Hill          3426           1006        29.4
    ##  9 Greater Mondawmin                  3620           1061        29.3
    ## 10 Southern Park Heights              4802           1351        28.1
    ## # … with 46 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

    ## # A tibble: 56 × 4
    ##    Community                       n_homes n_homes_liened perc_liened
    ##    <chr>                             <int>          <int>       <dbl>
    ##  1 Southwest Baltimore                9541            265        2.78
    ##  2 Sandtown-Winchester/Harlem Park    7561            166        2.20
    ##  3 Clifton-Berea                      5349            111        2.08
    ##  4 Pimlico/Arlington/Hilltop          4382             90        2.05
    ##  5 Midway/Coldstream                  4634             95        2.05
    ##  6 Greater Rosemont                   7887            160        2.03
    ##  7 Southern Park Heights              4802             94        1.96
    ##  8 Madison/East End                   3606             65        1.80
    ##  9 Upton/Druid Heights                3397             51        1.50
    ## 10 Greenmount East                    5560             81        1.46
    ## # … with 46 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

    ## Deleting source `/Users/nickthieme/Desktop/banner_projects/real_estate/butterfly_lien.geojson' using driver `GeoJSON'
    ## Writing layer `precinct' to data source 
    ##   `/Users/nickthieme/Desktop/banner_projects/real_estate/butterfly_lien.geojson' using driver `GeoJSON'
    ## Writing 56 features with 6 fields and geometry type Polygon.

How many homes had their deed delivered to someone else?

    ## [1] 1763

We need to properly format the dates to be able to find the second sale
after a tax sale.

Another bit of evidence of the racially disparate effects of the tax
lien system is shown by the plot of the percent of white residents in a
Census tract against the % of liened homes in the tract. At low
percentages of white residents, homes may be liened at any rate.
However, at high white %s homes are almost never liened.

``` r
Bmore_acs<-get_acs(geography = "tract", state = "MD",county = "Baltimore city",
                              variables=c(med_inc="B19013_001",white = "B02001_002", 
                                          black = "B02001_003", 
                                          poverty = "B17001_002"), geometry = T, summary_var = "B01001_001"
                   )
```

    ## Getting data from the 2016-2020 5-year ACS

    ## Downloading feature geometry from the Census website.  To cache shapefiles for use in future sessions, set `options(tigris_use_cache = TRUE)`.

``` r
Bmore_acs_wide<-Bmore_acs %>% pivot_wider(-c(GEOID, moe),names_from = "variable", values_from = "estimate") %>% 
  mutate(blk_perc = black/summary_est, wht_perc = white/summary_est, pov_rate = poverty/summary_est) %>% st_as_sf

Bmore_acs_wide<-st_transform(Bmore_acs_wide, st_crs(liened_properties))

st_crs(Bmore_acs_wide)<-st_crs(liened_properties)
st_crs(j_city_state_data_sf)<-st_crs(liened_properties)

liened_with_census<-liened_properties %>% st_join(Bmore_acs_wide) #we lost 9000 here out of 70000
full_city_with_census<-j_city_state_data_sf %>% st_join(Bmore_acs_wide) # we lose almost none here, about 800 out of 240

liened_with_census_j<-liened_with_census %>% mutate(
  lien_status = case_when(str_detect(redemption, "deed")~"sold",
            str_detect(redemption, "deed")==FALSE~"liened",
            is.na(redemption)~"liened")
) %>% data.frame %>% select(-geometry) %>% 
  group_by(NAME, lien_status) %>% 
  summarise(n = n())
```

    ## `summarise()` has grouped output by 'NAME'. You can override using the
    ## `.groups` argument.

``` r
full_city_with_census_f<-full_city_with_census %>% data.frame %>% select(-geometry) %>% 
  group_by(NAME) %>% 
  summarise(n = n(), med_inc = med_inc[1], blk_perc = blk_perc[1], wht_perc = wht_perc[1], pov_rate = pov_rate[1])

total_joined_f<-liened_with_census_j %>% left_join(full_city_with_census_f, by = "NAME") %>% 
  #mutate(perc_change = n.x/n.y) %>% 
  pivot_wider(names_from = lien_status, values_from = n.x) %>%
  mutate(sold = replace_na(sold, 0),
         liened = replace_na(liened, 0),
         sold_p = sold / n.y,
         liened_p = liened / n.y,
         sold_liened = sold+liened,
         neither = n.y-sold_liened,
         neither_p = neither / n.y) %>% 
  ungroup %>% 
  select( wht_perc, liened_p, sold_p, neither_p) %>% 
  mutate(wht_perc = wht_perc*100, liened_p= liened_p*100, sold_p = sold_p*100, neither_p = neither_p*100) %>% 
  pivot_longer(-wht_perc, names_to = "type_of_lien", values_to = "percentage") %>% 
  mutate(percentage = case_when(percentage>100~100, percentage<0~0, between(percentage,0,100)~percentage))

total_joined_f %>% filter(type_of_lien!="sold_p") %>% write_csv("~/Desktop/banner_projects/real_estate/dw_corrected_perc_wht_lien.csv")
```

This loops finds the nearest sale after tax auction. We then take the
difference in the amount bid and the next sale to calculate the profits
made by investors flipping the home. This is the right amount because
the when purchasers overbid, they pay the full amount of their bid, with
the excess being deposited in the city’s bid balance account.

With the results of the previous loop, we can calcuate the amount of
money made from flipping homes as the difference between the tax sale
price and the subsequent auction.

    ## [1] 27169058

We also need to calculate the amount of money made off of interest.
There are a few little kinks here.

For a home to result in interest income for an investor, by definition,
the home must be redeemed by the owner. So, we limit ourselves to homes
that were redeemed.

Next, the interest rate is 12% or 18% depending on whether a home is
owner occupied or not. And in either case, the interest is simple annual
interest, not compound interest. We handle this in the code.

Third, some of the dates included by Department of Finance are far into
the future. To deal with that, we filter out properties with redemption
dates after 2023.

Finally, we also don’t want to include homes purchased by the City of
Baltimore, as we’re focused on investor profits. So we filter out homes
purchased by MCC.

``` r
liened_properties_f_2<-liened_properties_f%>% 
  mutate(amt_bid = as.numeric(amt_bid), tot_assess = as.numeric(tot_assess), total_liens = as.numeric(total_liens), tot_assess = as.numeric(tot_assess),
         year = year(sale_date))

liened_properties_f_2_2<-liened_properties_f_2 %>%ungroup %>% 
  filter(bidder_type=="PRIV") %>% 
    filter(is.na(redem_date)==FALSE, 
           str_detect(redemption, "deed")==FALSE, 
           str_detect(redemption, "void")==FALSE, 
           str_detect(redemption, "no case filed")==FALSE,
           str_detect(redemption, "dismissed")==FALSE,
    ) %>%
    mutate(year = year(sale_date)-1,
           prev_july = str_c("-07-01"),
           prev_july=str_c(year,prev_july) %>% ymd,
      months_before = ((sale_date - prev_july)/30) %>% as.numeric ,
      months_after = ((redem_date - sale_date)/30) %>% as.numeric ) %>% 
  filter(months_after>0, redem_date<ymd("2023-01-01")) 

#fee simple interest

interest_on_non_owner<-liened_properties_f_2_2 %>% 
  filter(owner_occ_code%in%c("N","n"))%>% 
  mutate(interest_tot = total_liens*(.015)*(months_after))%>% 
  pull(interest_tot) %>%
  sum(na.rm = T)

#$2m
interest_on_owner<-liened_properties_f_2_2 %>% 
  filter(owner_occ_code=="H")%>% 
  mutate(interest_tot = total_liens*(.01)*(months_after)) %>% 
  pull(interest_tot) %>%
  sum(na.rm = T)

interest_on_non_owner
```

    ## [1] 8213601

``` r
interest_on_owner
```

    ## [1] 1412095

Here, we calculate the rate of misclassified owner-occupied homes. We do
this by standardizing the property owner’s address and the liened
property’s address using postmastr. Then, we look for homes where those
addresses match. Intuitively, homes where the owner lives at the
property should be classified as owner-occupied. This methodology was
confirmed by officials at Baltimore City’s Department of Finance and The
Department of Housing and Community Development. We then check the SDAT
owner-occupancy code for homes we’ve identified to be “owner-occupied,”
looking for homes that are classified as non-owner-occupied. We treat
these homes are misclassified.

We also look in the opposite direction for homes that should be listed
as non-homeowner-occupied, but are classified as owner-occupied in SDAT.
This is important as the supposed prevalance of these sorts of homes was
the impetus for a 2007 law requiring homeowners to file for owner
status.

``` r
liened_properties_f_id<-liened_properties_f  %>% pm_identify(var = "owner_addr")

owner_addr_match_add<-liened_properties_f_id%>% pm_prep(var = "owner_addr", type = "street") 
dirsDict <- pm_dictionary(type = "directional", locale = "us")

owner_addr_match_add_unit<-owner_addr_match_add %>% 
  pm_house_parse() %>% pm_unit_parse() %>% select(pm.uid, pm.unit) %>% na.omit

owner_addr_match_add_f<-owner_addr_match_add %>% 
  pm_house_parse() %>% 
  pm_streetDir_parse(dictionary = dirsDict) %>%
  pm_streetSuf_parse()%>%
  filter(is.na(pm.house)==FALSE,is.na(pm.address)==FALSE) %>% 
  mutate(pm.address = pm.address %>% str_split("#") %>% 
           lapply(.,
                  function(x){
                    return(x[1])
                  }
                    ) %>% unlist %>% trimws %>% str_remove("\\.")
         )
```

    ## Warning in stri_sub(string, from = start, to = end): argument is not an atomic
    ## vector; coercing

``` r
owner_addr_match_add_f_3<-owner_addr_match_add_f %>% left_join(owner_addr_match_add_unit)%>%
  mutate(pm.house = pm.house %>% str_remove(., "^0+") %>% str_remove(., ">"))%>% 
  mutate(pm.house=str_split(pm.house,"-") %>% lapply(function(x)return(x[1])) %>% unlist,
         pm.preDir=replace_na(pm.preDir,""),
         pm.streetSuf=replace_na(pm.streetSuf,""),
         pm.sufDir=replace_na(pm.sufDir,""),
         pm.unit=replace_na(pm.unit,""),
         pm.addr.full = str_c(pm.house, " ", " ",pm.address," ", pm.streetSuf, " ", pm.sufDir) %>% trimws %>% 
           str_replace("  "," ") %>% str_replace("  "," ") %>% tolower) %>% select(pm.uid,pm.addr.full, pm.address)%>%
  mutate(pm.addr.full = str_remove(pm.addr.full, "\\#+[0-9]*") %>% trimws %>% str_remove_all("\\."))
```

    ## Joining, by = "pm.uid"

``` r
owner_addr_match_f<-liened_properties_f_id %>% left_join(owner_addr_match_add_f_3, by = "pm.uid") %>% 
  select(-pm.id,-pm.uid, -pm.type, -id_lien)%>% add_column(id_lien = 1:nrow(.)) %>% as.data.frame

##doing it again for the property addr
owner_addr_match_f_2<-owner_addr_match_f  %>% pm_identify(var = "property_address")

owner_addr_match_f_add<-owner_addr_match_f_2%>% pm_prep(var = "property_address", type = "street") 
dirsDict <- pm_dictionary(type = "directional", locale = "us")

owner_addr_match_f_add_unit<-owner_addr_match_f_add %>% 
  pm_house_parse() %>% pm_unit_parse() %>% select(pm.uid, pm.unit) %>% na.omit

owner_addr_match_f_add_f_2<-owner_addr_match_f_add %>% 
  pm_house_parse() %>% 
  pm_streetDir_parse(dictionary = dirsDict) %>%
  pm_streetSuf_parse()%>%
  filter(is.na(pm.house)==FALSE,is.na(pm.address)==FALSE) %>% 
  mutate(pm.address = pm.address %>% str_split("#") %>% 
           lapply(.,
                  function(x){
                    return(x[1])
                  }
                    ) %>% unlist %>% trimws %>% str_remove("\\.")
         )

owner_addr_match_f_add_f_4<-owner_addr_match_f_add_f_2 %>% left_join(owner_addr_match_f_add_unit)%>%
  mutate(pm.house = pm.house %>% str_remove(., "^0+") %>% str_remove(., ">"))%>% 
  mutate(pm.house=str_split(pm.house,"-") %>% lapply(function(x)return(x[1])) %>% unlist,
         pm.address = pm.address%>% str_remove(., "^0+") %>% str_remove(., ">"),
         pm.preDir=replace_na(pm.preDir,""),
         pm.streetSuf=replace_na(pm.streetSuf,""),
         pm.sufDir=replace_na(pm.sufDir,""),
         pm.unit=replace_na(pm.unit,""),
         pm.addr.full = str_c(pm.house, " ", " ",pm.address," ", pm.streetSuf, " ", pm.sufDir) %>% trimws %>% 
           str_replace("  "," ") %>% str_replace("  "," ") %>% tolower) %>% select(pm.uid,pm.addr.full, pm.address)%>%
  mutate(pm.addr.full = str_remove(pm.addr.full, "\\#+[0-9]*") %>% trimws %>% str_remove_all("\\."))
```

    ## Joining, by = "pm.uid"

``` r
owner_addr_match_add_f_new<-owner_addr_match_f_2 %>% left_join(owner_addr_match_f_add_f_4, by = "pm.uid") %>% 
  select(-pm.id,-pm.uid, -pm.type, -id_lien)%>% add_column(id_lien = 1:nrow(.)) %>% as.data.frame

#this is uh pretty bad. only 28% of the owner occupied homes show as that
addr_match<-owner_addr_match_add_f_new %>% filter(pm.addr.full.x==pm.addr.full.y) %>% select(owner_occ_code, pm.addr.full.x, pm.addr.full.y) %>% distinct() %>% group_by(owner_occ_code) %>% summarise(n= n()) 

(addr_match %>% filter(owner_occ_code == "H") %>% pull(n))/(addr_match$n %>% sum)
```

    ## [1] 0.3794447

``` r
post_2021_addr_match<-owner_addr_match_add_f_new %>% filter(sale_date>ymd("2021-01-01")) %>% filter(pm.addr.full.x==pm.addr.full.y) %>% select(owner_occ_code, pm.addr.full.x, pm.addr.full.y) %>% distinct() %>% group_by(owner_occ_code) %>% summarise(n= n()) 

#15% of the post 2021s
(post_2021_addr_match %>% filter(owner_occ_code == "H") %>% pull(n))/(post_2021_addr_match$n %>% sum)
```

    ## [1] 0.1523774

``` r
know_sold_addr_match<-owner_addr_match_add_f_new %>% filter(sale_date>sales_date_1, sale_date>sales_date_2, sale_date>sales_date_3,pm.addr.full.x==pm.addr.full.y)%>% select(owner_occ_code, pm.addr.full.x, pm.addr.full.y) %>% distinct() %>% group_by(owner_occ_code) %>% summarise(n= n()) 

#35% of the homes that we know sold
(know_sold_addr_match %>% filter(owner_occ_code == "H") %>% pull(n))/(know_sold_addr_match$n %>% sum)
```

    ## [1] 0.3501247

Here we look for homes that have cycled through the property tax lien
system in Baltimore.

    ## # A tibble: 6 × 3
    ##       n n_years  liens
    ##   <int>   <int>  <dbl>
    ## 1     1   21667  5489.
    ## 2     2   14656 38171.
    ## 3     3    4384 33161.
    ## 4     4     407  4777.
    ## 5     5      56  4729.
    ## 6     6       9  6596.

And the amount of time after tax auction but before the subsequent sale.

    ## [1] 0.06890894

    ## [1] 0.2420016

    ## [1] 0.4922067

    ## [1] 0.6185398
