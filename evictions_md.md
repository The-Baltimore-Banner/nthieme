Evictions in MD
================
2022-08-29

### Library and data

There are two datasets that we use for this story. One is [high-level
statistical count data](https://www.lsctracker.org/virginia/state-wide)
aggregated by the Legal Services Corporation, a non-profit that provides
legal aid to low-income Americans. The other is a dataset of actual
administrative case records from Anne Arundel County that my colleague
Ryan Little scraped from the [Maryland Case
Search](https://casesearch.courts.state.md.us/casesearch/). A brief word
on the choice of Anne Arundel County. There is very little
standardization for the electronic data storage of administrative
eviction records in Maryland. Many counties do things very differently,
and many post next to none of their eviction case records online.
Baltimore City, for example, makes none of its data available online or
in any mass format.

Thanks to a long-standing court scraping project in Maryland [called
Case Harvester](https://mdcaseexplorer.com/), however, we were able to
see which counties posted a substantial number of their Failure to Pay
Rent cases on the Maryland Case Search. Anne Arundel County posted by
far the largest number of cases, so we directed our scraping efforts
towards this county, and focused the story there also.

### Story points

Average filing rate in Maryland and its neighbors:

``` r
D_state %>% group_by(state, year) %>% summarise(tot=sum(filed), num_mon = month %>% unique %>% length) %>% 
  mutate(tot_per_mo = tot/num_mon) %>% arrange(desc(tot_per_mo)) %>% filter(year==2022)
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 4 × 5
    ## # Groups:   state [4]
    ##   state         year    tot num_mon tot_per_mo
    ##   <chr>        <dbl>  <dbl>   <int>      <dbl>
    ## 1 Maryland      2022 116893       5     23379.
    ## 2 Pennsylvania  2022  39574       5      7915.
    ## 3 Virginia      2022  26237       5      5247.
    ## 4 Delaware      2022   4587       5       917.

``` r
D_state %>% group_by(state, year) %>% summarise(tot=sum(filed))%>% filter(state=="Maryland", year ==2019)
```

    ## `summarise()` has grouped output by 'state'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 1 × 3
    ## # Groups:   state [1]
    ##   state     year    tot
    ##   <chr>    <dbl>  <dbl>
    ## 1 Maryland  2019 671052

We re-format the Anne Arundel data to be able to look at flows. this
grouped_ftpr object is what get sent out to create the flow chart in the
story.

``` r
ftpr_cases[,any(has_complaint), case_id]$V1 %>% table
```

    ## .
    ## FALSE  TRUE 
    ##    14 68046

``` r
to_rm<-ftpr_cases[,any(has_complaint), case_id][V1==FALSE]$case_id

ftpr_cases_1 <- ftpr_cases[case_id%in%to_rm==FALSE]#[case_id%in%to_rm_date_f]

grouped_ftpr=ftpr_cases_1[,
                          .(
                            has_disposition= any(has_disposition), 
                            has_warrant = any(has_warrant), 
                            has_dismiss = any(has_dismiss),
                            has_warrant_expired = any(has_warrant_expired),
                            has_warrant_cancelled = any(has_warrant_cancelled),
                            has_warrant_evicted = any(has_warrant_evicted)
                          )
                          ,
                          by = .(case_id)
                          ]

grouped_val<-grouped_ftpr[,.N, .(has_disposition, has_dismiss, has_warrant,has_warrant_expired,has_warrant_cancelled,has_warrant_evicted)] 

flow_tibble<-tibble(source = "Case filed", 
       destination = "Disposition filed", 
       value = grouped_val[has_disposition==TRUE]$N %>% sum,
       step_from =0,
       step_to=1) %>% 
  add_row(
       tibble(
         source = "Case filed", 
         destination = "Dismissed", 
         value = grouped_val[has_dismiss==TRUE &has_disposition==FALSE&has_warrant==FALSE]$N,
         step_from=0,
         step_to=1)
       ) %>% 
  add_row(
    tibble(
      source = "Case filed", 
      destination = "Attrition", 
      value = grouped_val[has_dismiss==FALSE &has_disposition==FALSE&has_warrant==FALSE]$N,
      step_from =0,
      step_to=1)
  ) %>% 
  add_row(
    tibble(
      source = "Disposition filed", 
      destination = "Warrant filed", 
      value = grouped_val[has_disposition==TRUE&has_warrant==TRUE]$N %>% sum,
      step_from =1,
      step_to=2)
  )%>% 
  add_row(
    tibble(
      source = "Disposition filed", 
      destination = "Attrition or pending", 
      value = grouped_val[has_disposition==TRUE&has_warrant==FALSE]$N %>% sum,
      step_from =1,
      step_to=2)
  )%>% 
  add_row(
    tibble(
      source = "Warrant filed", 
      destination = "Warrant cancelled", 
      value = grouped_val[has_disposition==TRUE&has_warrant==TRUE&has_warrant_cancelled==TRUE]$N %>% sum,
      step_from =2,
      step_to=3)
  )%>% 
  add_row(
    tibble(
      source = "Warrant filed", 
      destination = "Tenant evicted", 
      value = grouped_val[has_disposition==TRUE&has_warrant==TRUE&has_warrant_evicted==TRUE]$N %>% sum,
      step_from =2,
      step_to=3)
  )%>% 
  add_row(
    tibble(
      source = "Warrant filed", 
      destination = "Warrant expired", 
      value = grouped_val[has_disposition==TRUE&has_warrant==TRUE&has_warrant_expired==TRUE]$N %>% sum,
      step_from =2,
      step_to=3)
  ) %>% 
  mutate(value = comma(value))

dismissed = flow_tibble %>% filter(source=="Case filed", destination=="Dismissed") %>% pull(value) %>% str_remove(",") %>% as.numeric

warrant_filed = flow_tibble %>% filter(source=="Disposition filed", destination=="Warrant filed") %>% pull(value) %>% str_remove(",") %>% as.numeric

evicted = flow_tibble %>% filter(source=="Warrant filed", destination=="Tenant evicted") %>% pull(value) %>% str_remove(",") %>% as.numeric

#almost 20%
dismissed/nrow(grouped_ftpr)
```

    ## [1] 0.1864033

``` r
#under 40%
warrant_filed/nrow(grouped_ftpr)
```

    ## [1] 0.3783176

``` r
#3.29%
evicted/nrow(grouped_ftpr)
```

    ## [1] 0.03299239

We also look at how the pandemic affected eviction filing rates. A 96%
drop during the beginning of the pandemic, followed by a rapid rebound.

``` r
D_state %>% filter(between(month, ymd("2020-02-01"), ymd("2020-06-01")), state=="Maryland") %>% group_by(.) %>% 
  summarise(max_n=max(filed), min_n = min(filed)) %>% mutate(1-min_n/max_n)
```

    ## # A tibble: 1 × 3
    ##   max_n min_n `1 - min_n/max_n`
    ##   <dbl> <dbl>             <dbl>
    ## 1 55477  2193             0.960

``` r
D_state %>% filter(month==ymd("2020-10-01"), state=="Maryland") %>% pull(filed)
```

    ## [1] 33575

``` r
D_state %>% filter(month>=ymd("2020-10-01"), period == "moratorium", state == "Maryland") %>% pull(evictions) %>% median
```

    ## [1] 576.5
