Tax lien investors
================
Nick Thieme
2022-10-05

## Introduction

This is the white paper that goes along with the story about companies
that invest in property tax liens in Baltimore City. It’s the third
article in a Baltimore Banner series about tax liens in Baltimore, and
identifies individual companies that have benefited and are benefiting
from the current tax auction system.

## Library

### EDA and plots

``` r
n_months <- 12

short_ents_1<-which(liened_properties$sales_date_1 %>% nchar==7)
short_ents_2<-which(liened_properties$sales_date_2 %>% nchar==7)
short_ents_3<-which(liened_properties$sales_date_3 %>% nchar==7)

liened_properties$sales_date_1[short_ents_1]<-str_c(0,liened_properties$sales_date_1[short_ents_1])
liened_properties$sales_date_2[short_ents_2]<-str_c(0,liened_properties$sales_date_2[short_ents_2])
liened_properties$sales_date_3[short_ents_3]<-str_c(0,liened_properties$sales_date_3[short_ents_3])

liened_properties_f<-liened_properties %>%as.data.frame  %>% mutate(sales_date_1 = mdy(sales_date_1),
                                                                    sales_date_2 = mdy(sales_date_2),
                                                                    sales_date_3 = mdy(sales_date_3))

liened_properties_f_2<-liened_properties_f%>% 
  mutate(amt_bid = as.numeric(amt_bid), tot_assess = as.numeric(tot_assess), total_liens = as.numeric(total_liens), tot_assess = as.numeric(tot_assess),
         year = year(sale_date))
```

As established in the first article, most homes that go through the tax
sale process don’t end up changing hands. Their debt is sold at auction
and the owners redeemed. However, a substantial amount of money is made
through both redemption interest and by flipping houses. Given that,
it’s useful to understand the differences in how companies approach tax
sale as an investment opportunity.

There are some investor groups that bid on thousands of properties but
have only ever received a handful of deeds through the tax system. There
are others that pick up close to 20% of the properties they bid on.
Examining this requires standardizing bidders’ addresses and names
because investors often use multiple names and multiple addresses,
presumably for some sort of market advantage. Standerdizing these takes
a little work because it isn’t a priori clear whether a company has a
single address and multiple names, multiple names but a single address,
or multiple of both.

However, it turns out that because of the way some attorneys both manage
their own tax lien investment firms and act as registered agents for
other firms, it isn’t possible to perform this process automatically.
Doing so combines large national investment firms that use local
attorneys with the smaller firms run by those attorneys. Because of the
lack of transparency surrounding corporate governance in Maryland, we
can’t disentangle the two. In the end, we combine subsidiaries for the
10 most profitable companies by hand, and leave the rest the way they
are.

It is, however, still worthwhile, to explain what we did here
originally. We parsed by both address and name, looking for all
addresses associated with a name (to get an address list) and all names
associated with an address (to get a name list). After that, we found
all the addresses linked to the name list and all names linked to
address list, and treated those second lists as the standardized
quantities. The dual usage of addresses as business address for a
company and registered agent address is what causes our problem. It
would be simple for the city to require bidders to list their business
address instead of their registered agent’s address, and increase
transparency.

``` r
liened_properties_f_pm<-liened_properties_f %>% pm_identify(var = "bidder_addr")

liened_properties_f_pm_2<-liened_properties_f_pm%>% pm_prep(var = "bidder_addr", type = "street") %>% 
  mutate(pm.address=pm.address %>% str_remove_all("\\.")) 

dirsDict <- pm_dictionary(type = "directional", locale = "us")

liened_properties_f_pm_2_unit_2<-liened_properties_f_pm_2 %>% 
  pm_house_parse() %>% pm_unit_parse() %>% select(pm.uid, pm.unit) %>% 
  mutate(pm.unit = str_c("unit ", pm.unit))

addr_po_box<-liened_properties_f_pm_2 %>%
  mutate(pm.address = str_replace(pm.address,"P O", "PO") %>%  str_replace("POST OFFICE", "PO")) %>% 
  rename(pm.addr.full = pm.address) %>% 
  filter(str_detect(pm.addr.full, "BOX"))

liened_properties_f_pm_2_add_f<-liened_properties_f_pm_2 %>% 
  pm_house_parse() %>% 
  pm_unit_parse() %>% 
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

liened_properties_f_match_3<-liened_properties_f_pm_2_add_f %>% left_join(liened_properties_f_pm_2_unit_2)%>%
  mutate(pm.house = pm.house %>% str_remove(., "^0+") %>% str_remove(., ">"),
         pm.house=str_split(pm.house,"-") %>% lapply(function(x)return(x[1])) %>% unlist,
         pm.preDir=replace_na(pm.preDir,""),
         pm.streetSuf=replace_na(pm.streetSuf,""),
         pm.sufDir=replace_na(pm.sufDir,""),
         pm.unit=replace_na(pm.unit,""),
         pm.addr.full = str_c(pm.house, " ",pm.preDir, " ",pm.address," ", pm.streetSuf, " ", pm.sufDir, pm.unit) %>% trimws %>% 
           str_replace("  "," ") %>% str_replace("  "," ") %>% tolower) %>% select(pm.uid,pm.addr.full)%>%
  mutate(pm.addr.full = str_remove(pm.addr.full, "\\#+[0-9]*") %>% trimws %>% str_remove_all("\\.")) %>% 
  rbind(addr_po_box) %>% 
  rename(pm.addr.full.bidder = pm.addr.full)
```

    ## Joining, by = "pm.uid"

``` r
liened_properties_match_f<-liened_properties_f_pm %>% left_join(liened_properties_f_match_3, by = "pm.uid") %>% 
  select(-pm.id,-pm.uid, -pm.type) %>% as.data.frame %>% 
  mutate(bidder_name = str_remove_all(bidder_name, "\\."))

liened_properties_match_f_list_col<-liened_properties_match_f %>%
  group_by(bidder_name) %>% 
  summarise(addr_list = list(pm.addr.full.bidder %>% unique %>% na.omit))

name_l_vec <- vector(mode = "list", length=nrow(liened_properties_match_f_list_col))

for(i in 1:nrow(liened_properties_match_f_list_col)){
  name_l_vec[[i]]<-liened_properties_match_f %>% filter(pm.addr.full.bidder%in%liened_properties_match_f_list_col$addr_list[i][[1]]) %>% pull(bidder_name) %>%
    unique %>% na.omit
}

liened_properties_match_f_list_col$bidder_names_post<-name_l_vec
liened_properties_match_f_list_col$final_names <- NA
liened_properties_match_f_list_col$final_addrs <- NA

for(i in 1:nrow(liened_properties_match_f_list_col)){
  names_to_grab = liened_properties_match_f_list_col$bidder_names_post[i][[1]]
  
  for(j in 1:length(names_to_grab)){
    
    name_inds<-lapply(liened_properties_match_f_list_col$bidder_names_post, 
           function(x){
             return(names_to_grab[j]%in%x)
           }
    ) %>% unlist %>%  {which(.)}
    
    full_addrs<-liened_properties_match_f_list_col[name_inds,] %>% pull(addr_list) %>%na.omit %>%  unlist %>% unique %>% str_c(.,collapse = "|") 
    full_names<-liened_properties_match_f_list_col[name_inds,] %>% pull(bidder_names_post) %>% na.omit %>% unlist %>% unique%>% str_c(.,collapse = "|")
    
    liened_properties_match_f_list_col$final_names[name_inds]<-full_names
    liened_properties_match_f_list_col$final_addrs[name_inds]<-full_addrs
    
  }
}

standardized_names<-liened_properties_match_f_list_col %>% 
 select(bidder_name, final_names, final_addrs) %>% distinct

liened_properties_match_f_2<-liened_properties_match_f %>% left_join(standardized_names, by = "bidder_name")
```

We can now examine the different strategies used by investment
companies. The vast majority of companies that bought more than 100
certificates almost never took title, however, some companies take deeds
far more often than others. TLMD Capital LLC has the highest percentage
of debts purchased that led to homes transfered at 14%. This looks like
Neil Eskin’s company, a lawyer commonly seen in the transfer data. He is
the registered agent and was listed as the director at one point.

On the other hand, Pluto LLC has the lowest deed delivered percentage,
purchasing debt on nearly 1,000 homes but never having taken ownership.
Companies like this and Municipal Investments LLC (5,255 debts
purchased, 10 transferred) follow a different strategy of collecting the
interest payments and attorneys fees.

Near the bottom of the list is an interesting company called Midaro
Investments LLC. Midaro purchased 1,300 liens on properties, but only
received the deed to one. They target expensive homes that will likely
be redeemed. Their average bid price is \$312k for homes worth \$481k on
average. It looks like they just bid on expensive homes to get
redemptions and if they happen to get a nice house out of it, all the
better. They were sued for evicting UMD students from housing commonly
used by students in College Park.

``` r
liened_props_shifted<-liened_properties_match_f_2%>%
  mutate(year = year(sale_date),
         deed_deli_q = str_detect(redemption, "deed"))%>%
  group_by(final_names) %>% 
  summarise(n = n(),
            assessed_values = sum(as.numeric(tot_assess)), 
            amount_bid = sum(as.numeric(amt_bid)), 
            total_liens = sum(as.numeric(total_liens)),
            num_years = length(unique(year)),
            avg_per_year = assessed_values/num_years,
            deed_n = length(which(deed_deli_q))) %>% 
  mutate(diff_in_value = assessed_values - amount_bid) 

q = liened_properties_match_f_2 %>% ungroup %>% mutate(
  sold =
  case_when(
    (str_detect(redemption, "deed")|is.na(dd_date)==FALSE)~"sold",
    ((str_detect(redemption, "deed")==FALSE)|(is.na(dd_date)))~"not_sold"
    )
  ) %>%
  group_by(final_names) %>% 
  summarise(num_sold = length(which(sold=="sold")),
            num_not_sold = length(which(sold=="not_sold"))) %>% 
  mutate(perc_sold = num_sold/(num_sold+num_not_sold)) %>% 
  filter(num_not_sold>100) %>% 
  arrange(desc(perc_sold))
```

Arx I LLC, a Maryland-based investment firm, transfers the second-most
homes and has the highest percentage of homes transfered for companies
that buy more than 1,000 debts. It isn’t clear what happens to these
properties and whether they’re renovated by purchasers because we don’t
have historical vacancy data, but what we can do is ask which of the
homes they purchased have been assessed since the date of purchase.
Assessment provides an opportunity for updating vacancy data, so
assessed homes that are still vacant are likely not have been renovated
after purchase. 85% of Arx’s homes that were assessed after tax sale
purchase remain vacant.

``` r
liened_properties_match_f_2 %>% filter(str_detect(final_names, "arx")) %>% mutate(assess_date = str_c("010", date_assessed) %>% dmy()) %>% filter(assess_date>sale_date) %>% pull(VACIND) %>% table
```

    ## .
    ##   N   Y 
    ##  86 484

We can compare the percentage of Arx’s vacant homes to the percentage on
the full list, and see that their percentage is much higher.

``` r
liened_properties_f  %>% 
  mutate(assess_date = str_c("010", date_assessed) %>% dmy()) %>% 
  filter(assess_date>sale_date) %>% pull(VACIND) %>% table
```

    ## .
    ##     N     Y 
    ##  7224 12657

``` r
liened_properties_match_f_2  %>%
  mutate(assess_date = str_c("010", date_assessed) %>% dmy()) %>% 
  filter(assess_date>sale_date) %>%
  group_by(final_names, VACIND) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = VACIND, values_from = n) %>% 
  mutate(N=replace_na(N, 0),
         none = replace_na(`NA`, 0),
         Y = replace_na(Y, 0),
         vac_perc = Y/(Y+N)) %>% 
  arrange(desc(vac_perc)) %>% 
  filter(N+Y+none>100) %>% head(10)
```

    ## # A tibble: 10 × 6
    ## # Groups:   final_names [10]
    ##    final_names                                       N  `NA`     Y  none vac_p…¹
    ##    <chr>                                         <int> <int> <int> <int>   <dbl>
    ##  1 acf development llc|tark development llc|tjc…    74   507   514   507   0.874
    ##  2 valens llc|arx i llc|totum ltd|velox ltd|tem…    86   591   484   591   0.849
    ##  3 hobart holdings llc|ashman investments llc|c…    73   548   360   548   0.831
    ##  4 tlmd capital llc                                 52   517   167   517   0.763
    ##  5 interstate holdings llc|mtsif llc|palomino h…   155  1489   482  1489   0.757
    ##  6 immocap|immobilier cap ks ltd                    24   159    71   159   0.747
    ##  7 palm tsf llc|maple tsf llc                      117  1175   285  1175   0.709
    ##  8 hickory tsf llc                                  88   551   162   551   0.648
    ##  9 ultra-safe fund llc|mac  llc|land research a…   169  1751   292  1751   0.633
    ## 10 mayor and city council of baltimore            5097  4182  8297  4182   0.619
    ## # … with abbreviated variable name ¹​vac_perc

We see a smaller percentage (64%) of vacant homes in the full list than
in ARX I LLC’s list (85%). Some companies, like Pluto LLC (39%), hold
far fewer vacant properties. It may be that ARX I LLC is just collecting
properties and leaving them vacant the way some researchers accuse these
investment companies of doing.

``` r
sold_homes <- liened_properties_match_f_2 %>% filter(str_detect(redemption, "deed")|(is.na(dd_date)==FALSE))%>% 
  mutate(amt_bid = as.numeric(amt_bid), tot_assess = as.numeric(tot_assess), total_liens = as.numeric(total_liens), tot_assess = as.numeric(tot_assess),
         year = year(sale_date))

##we know that for sold homes all of these homes sold at the acution. that means that the amount that was bid acutino will be the sales price for the deed transfer. so we find the floor
#because that's the rounded amount that appears in the sdat data. then we can match which was the most recent sale. or confirm it rather
sold_homes_2<-sold_homes%>% mutate(amt_bid = floor(amt_bid))

next_price <- rep(0, nrow(sold_homes_2))
min_date <- rep(0, nrow(sold_homes_2))


##what does this loop do?

for(i in 1:nrow(sold_homes_2)){
  check_vec <- c(sold_homes_2$sales_price_1[i], sold_homes_2$sales_price_2[i], sold_homes_2$sales_price_3[i])
  bid_sale<-which(check_vec==sold_homes_2$amt_bid[i])
  
  if(length(bid_sale)==0){  #if there are no matching bids, we assign it the earliest sale, under the assumption that the house has sold more than two times
    next_price[i]=check_vec[3]
    min_date[i] = "3"
  }else if(length(bid_sale)>1){ #if there are more than one with the same price, almost certainly the house was sold from one arm of a company or relationship to another. take the second
    next_price[i]=check_vec[min(bid_sale)]
    min_date[i] = min(bid_sale)
  }else if(bid_sale ==1){ #if it's the most recent sale, then the last person to get the house is the tax sale purchaser
    next_price[i]="still owned"
    min_date[i] = bid_sale-1
  }else{ #this is the non-degenerate case. it sold, and we look for the next sale
    next_price[i]=check_vec[bid_sale-1]
    min_date[i] = bid_sale-1
  }
  
}

#intermediate step
sold_homes_2_f<- sold_homes_2  %>% add_column(min_date =min_date) %>% mutate(
  post_sale_date = case_when(
    min_date=="0"~"still_owned",
    min_date=="1"~sales_date_1%>% as.character(), 
    min_date=="2"~sales_date_2%>% as.character(),
    min_date=="3"~sales_date_3%>% as.character()),
  post_sale_price = case_when(
    min_date=="0"~"still_owned",
    min_date=="1"~sales_price_1%>% as.character(), 
    min_date=="2"~sales_price_2%>% as.character(),
    min_date=="3"~sales_price_3%>% as.character()),
  post_sale_assess = case_when(
    min_date=="0"~"still_owned",
    min_date=="1"~tot_assess %>% as.character(), 
    min_date=="2"~tot_assess_2%>% as.character(),
    min_date=="3"~tot_assess_3%>% as.character()),
  pre_sale_assess = case_when(
    min_date=="0"~"still_owned",
    min_date=="1"~tot_assess_2 %>% as.character(), 
    min_date=="2"~tot_assess_2%>% as.character(),
    min_date=="3"~tot_assess_3%>% as.character()))


assessment_differences_for_sold_homes <- rep(0, nrow(sold_homes_2_f))

for(i in 1:nrow(sold_homes_2)){
  assess_1 <- sold_homes_2_f[i,]$tot_assess %>% as.numeric
  assess_2 <- sold_homes_2_f[i,]$tot_assess_2 %>% as.numeric
  assess_vec <- c(assess_1, assess_2)
  
  if(any(is.na(assess_vec))){
    next
  }
  
  ind_vec <- c(1,2)
  max_ind<-which.max(assess_vec)
  other_ind <- ind_vec[-max_ind]
  assessment_differences_for_sold_homes[i]<-assess_vec[max_ind]-assess_vec[other_ind]
}
  
sold_homes_2_f$diff_in_assess <- assessment_differences_for_sold_homes

#sold_homes_2 %>% filter(is.na(tot_assess)==FALSE, is.na(tot_assess_2)==FALSE)
  
sold_homes_3<-sold_homes_2_f%>% 
  select(sale_date,property_address, owner_addr,total_liens, bidder_addr, bidder_type,
         amt_bid, bidder_name, grantor_1, grantor_2, grantor_3,sales_price_1, sales_price_2, sales_price_3,redemption,
         redem_date,sales_date_1, sales_date_2, sales_date_3,post_sale_price, post_sale_date,post_sale_assess,VACIND, min_date, owner_occ_code, diff_in_assess,
         final_names
  ) %>% # filter(bidder_name!="mayor and city council of baltimore") %>%
  mutate(diff_date = (ymd(redem_date)-ymd(post_sale_date)) %>% as.numeric) %>% 
  filter(min_date!="3") #i think we need to remove properties where we don't observe the property sale in SDAT because we don't know that the profits are their profits or that the time
```

    ## Warning: 79 failed to parse.

``` r
#difference is their difference

sold_homes_3_interest <- sold_homes_2_f%>% 
  select(sale_date,property_address, owner_addr,total_liens, bidder_addr, bidder_type,
         amt_bid, bidder_name, grantor_1, grantor_2, grantor_3,sales_price_1, sales_price_2, sales_price_3,
         redem_date,sales_date_1, sales_date_2, sales_date_3,post_sale_price, post_sale_date,post_sale_assess,VACIND, min_date, owner_occ_code, diff_in_assess,
         redemption
  ) %>% # filter(bidder_name!="mayor and city council of baltimore") %>%
  mutate(diff_date = (ymd(redem_date)-ymd(post_sale_date)) %>% as.numeric) 
```

    ## Warning: 79 failed to parse.

``` r
sold_homes_4<-sold_homes_3 %>% filter(redem_date<sales_date_1)%>% mutate(post_sale_profit = as.numeric(post_sale_price)-as.numeric(amt_bid),  
                        post_sale_profit_perc = post_sale_profit/as.numeric(amt_bid),
                        post_sale_profit_assess = as.numeric(post_sale_price)-as.numeric(post_sale_assess)) 
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

### Who’s making money off of this?

In the previous story, we established that neighborhoods with higher
poverty rates and smaller proportions of white residents lose the most
in the tax sale process. Here, we want to say something about who gains
from the process. To do that, we calculate the sales price of each home
sold through the process as well as the subsequent sales price. The
difference between these numbers tells us the amount of money an
investor made by buying a house at auction and selling it later. It does
not take into account repairs that may have been performed after
purchase, but we show that this doesn’t seem to be as influential as one
might think.

``` r
#we lose 200 homes here because we filter out the homes that the mayor and city council purchased

sold_homes_4<-sold_homes_3 %>% filter(redem_date<=sales_date_1)%>% mutate(post_sale_profit = as.numeric(post_sale_price)-as.numeric(amt_bid),  
                        post_sale_profit_perc = post_sale_profit/as.numeric(amt_bid),
                        post_sale_profit_assess = as.numeric(post_sale_price)-as.numeric(post_sale_assess)) 
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
liened_properties_f_2<-liened_properties_f%>% 
  mutate(amt_bid = as.numeric(amt_bid), tot_assess = as.numeric(tot_assess), total_liens = as.numeric(total_liens), tot_assess = as.numeric(tot_assess),
         year = year(sale_date))

liened_properties_f_2_2<-liened_properties_match_f_2 %>%ungroup %>% 
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
```

Some companies are much more involved in the tax lien process that
others. About 530 companies have purchased property tax debts through
the lien process over the last 6 years, and some have purchased many
more than others. The top 10 companies have purchased 73% of the
properties that sold and those same companies have received 60% the
total profits of the last six years, each earning at least a million
dollars in profits from flipping tax sale properties and interest. Only
13 companies have received deeds to more than 10 houses. 6of the top 10
companies all held the homes they purchased for fewer than 30 days, with
the 3 of the 4 remaining being companies that specialize in interest
collection and only infrequently take title to homes.

``` r
sold_homes_4_2<-sold_homes_4

nainan<-"naiman and naiman pa"

which_nainan<-which(sold_homes_4_2$bidder_name%in%(nainan %>% str_split("\\|") %>% unlist))

caret_bay <- "caret bay llc|caret bey llc|caret bay holdings llc"
which_caret<-which(sold_homes_4_2$bidder_name%in%(caret_bay %>% str_split("\\|") %>% unlist))

arc_llc <- "valens llc|arx i llc|totum ltd|velox ltd|tempest llc|novalux md  llc|visus ltd|u llc"
which_bryan <- which(sold_homes_4_2$bidder_name%in%(arc_llc %>% str_split("\\|") %>% unlist))

tark<-"acf development llc|tark development llc|tjci development llc"
which_anthony <- which(sold_homes_4_2$bidder_name%in%(tark %>% str_split("\\|") %>% unlist))

fna<-"fna dz llc|fna maryland llc|fna dz llc fbo wsfs"
which_fna <- which(sold_homes_4_2$bidder_name%in%(fna %>% str_split("\\|") %>% unlist))

stone <-"mdinv llc|t rocho llc|stonefield investment fund iv llc|stonefield invesmtent fund iv llc"
which_stone <- which(sold_homes_4_2$bidder_name%in%(stone %>% str_split("\\|") %>% unlist))

sold_homes_4_2$bidder_name[which_nainan]<-nainan
sold_homes_4_2$bidder_name[which_caret]<-caret_bay
sold_homes_4_2$bidder_name[which_bryan]<-arc_llc
sold_homes_4_2$bidder_name[which_anthony]<-tark
sold_homes_4_2$bidder_name[which_fna]<-fna
sold_homes_4_2$bidder_name[which_stone]<-stone

liened_properties_f_2_3<-liened_properties_f_2_2

which_nainan<-which(liened_properties_f_2_3$bidder_name%in%(nainan %>% str_split("\\|") %>% unlist))
which_caret<-which(liened_properties_f_2_3$bidder_name%in%(caret_bay %>% str_split("\\|") %>% unlist))
which_arc<-which(liened_properties_f_2_3$bidder_name%in%(arc_llc %>% str_split("\\|") %>% unlist))
which_tark<-which(liened_properties_f_2_3$bidder_name%in%(tark %>% str_split("\\|") %>% unlist))
which_fna<-which(liened_properties_f_2_3$bidder_name%in%(fna %>% str_split("\\|") %>% unlist))
which_stone<-which(liened_properties_f_2_3$bidder_name%in%(stone %>% str_split("\\|") %>% unlist))

liened_properties_f_2_3$bidder_name[which_nainan]<-nainan
liened_properties_f_2_3$bidder_name[which_caret]<-caret_bay
liened_properties_f_2_3$bidder_name[which_arc]<-arc_llc
liened_properties_f_2_3$bidder_name[which_tark]<-tark
liened_properties_f_2_3$bidder_name[which_fna]<-fna
liened_properties_f_2_3$bidder_name[which_stone]<-stone

# 
# liened_prop_agg<-sold_homes_4 %>%rowwise  %>% 
#   group_by(final_names) %>% 
#   summarise(n = n(), 
#             diff_amt = sum(post_sale_profit, na.rm = T),
#             time_held = median(diff_date, na.rm =T),
#             assessed_values = sum(as.numeric(post_sale_assess), na.rm = T), 
#             amount_bid = sum(as.numeric(amt_bid), na.rm = T),
#             total_liens = sum(as.numeric(total_liens), na.rm = T), 
#             post_sale_price = sum(as.numeric(post_sale_price), na.rm = T),
#             num_years = length(unique(year(redem_date)))
#             ) 
# 
# interest_on_non_owner<-liened_properties_f_2_2 %>% 
#   filter(owner_occ_code%in%c("N","n"))%>% 
#   mutate(interest_tot = total_liens*(.015)*(months_after))%>% 
#   group_by(final_names) %>% 
#   summarise(amt_non_own = sum(interest_tot))
# 
# #$2m
# interest_on_owner<-liened_properties_f_2_2 %>% 
#   filter(owner_occ_code=="H")%>% 
#   mutate(interest_tot = total_liens*(.01)*(months_after)) %>% 
#   group_by(final_names) %>% 
#   summarise(amt_own = sum(interest_tot))
# 
# joined_interest<-interest_on_non_owner %>% 
#   full_join(interest_on_owner, by = c("final_names")) %>% 
#   mutate(amt_own = replace_na(amt_own, 0),
#          amt_non_own = replace_na(amt_non_own, 0)
#          )
# 
# liened_prop_agg_f<-joined_interest %>% 
#   left_join(liened_prop_agg, by = "final_names") %>% 
#   mutate(across(.cols = where(is.numeric), 
#                 function(x)return(replace_na(x,0))
#                 )
#          )
# 
# liened_prop_agg_f_j_2<-liened_prop_agg_f %>% 
#   rowwise %>%  
#   mutate(total = diff_amt+amt_non_own+amt_own) %>% 
#   select(final_names, n, diff_amt, total, amt_non_own, amt_own, time_held) %>% 
#   arrange(desc(total)) 
# 
# #top 10 get 84% of the homes
# cum_sum_purch<-(liened_prop_agg_f_j$n %>% cumsum)/max((liened_prop_agg_f_j$n %>% cumsum)) 
# 
# #this shows that the top 10 companies account for 75% of the profits. the bottom 262 account for just 1% 
# cum_sum_perc<-(liened_prop_agg_f_j$total %>% cumsum)/max((liened_prop_agg_f_j$total %>% cumsum)) 
# 
# liened_prop_agg_f_j$cumulative_perc_prof <- cum_sum_perc
# liened_prop_agg_f_j$cumulative_perc_n <- cum_sum_purch
# liened_prop_agg_f_j %>% head(10) %>% View
# liened_prop_agg_f_j_2 %>% head(20) %>% View

liened_prop_agg<-sold_homes_4_2 %>%rowwise  %>% 
  group_by(bidder_name) %>% 
  summarise(n = n(), 
            diff_amt = sum(post_sale_profit, na.rm = T),
            time_held = median(diff_date, na.rm =T),
            assessed_values = sum(as.numeric(post_sale_assess), na.rm = T), 
            amount_bid = sum(as.numeric(amt_bid), na.rm = T),
            total_liens = sum(as.numeric(total_liens), na.rm = T), 
            post_sale_price = sum(as.numeric(post_sale_price), na.rm = T),
            num_years = length(unique(year(redem_date)))
            ) 

interest_on_non_owner<-liened_properties_f_2_3 %>% 
  filter(owner_occ_code%in%c("N","n"))%>% 
  mutate(interest_tot = total_liens*(.015)*(months_after))%>% 
  group_by(bidder_name) %>% 
  summarise(amt_non_own = sum(interest_tot),
            tot_non_own = n())

#$2m
interest_on_owner<-liened_properties_f_2_3 %>% 
  filter(owner_occ_code=="H")%>% 
  mutate(interest_tot = total_liens*(.01)*(months_after)) %>% 
  group_by(bidder_name) %>% 
  summarise(amt_own = sum(interest_tot),
            tot_own = n())

joined_interest<-interest_on_non_owner %>% 
  full_join(interest_on_owner, by = c("bidder_name")) %>% 
  mutate(amt_own_int = replace_na(amt_own, 0),
         amt_non_own_int = replace_na(amt_non_own, 0),
         n_own_int = replace_na(tot_own,0),
         n_non_own_int = replace_na(tot_non_own, 0),
         tot_amt_int = amt_own_int+amt_non_own_int,
         tot_n_int = n_own_int+n_non_own_int
         ) %>% 
  select( -tot_own, -tot_non_own,-amt_non_own, -amt_own)

liened_prop_agg_f<-joined_interest %>% 
  left_join(liened_prop_agg, by = "bidder_name") %>% 
  mutate(across(.cols = where(is.numeric), 
                function(x)return(replace_na(x,0))
                )
         )

liened_prop_agg_f_j_3<-liened_prop_agg_f %>% 
  rowwise %>%  
  mutate(total = diff_amt+tot_amt_int,
         owner_int_perc = amt_own_int/tot_amt_int) %>% 
  select(bidder_name, num_years, total,time_held, tot_n_sold = n,tot_n_int, tot_amt_sold=diff_amt, tot_amt_int, total,amt_own_int, amt_non_own_int, n_own_int,n_non_own_int,owner_int_perc) %>% 
  arrange(desc(owner_int_perc)) %>% ungroup%>% arrange(desc(total))

names_to_add<- liened_prop_agg_f_j_3$bidder_name

liened_prop_agg_f_j_4<-liened_prop_agg_f_j_3 %>% select(-bidder_name) %>% data.frame 
row.names(liened_prop_agg_f_j_4)<-str_sub(names_to_add, 1,50)

#datatable(liened_prop_agg_f_j_4, options=list(rownames.print = TRUE))

liened_prop_agg_f_j_4 %>% head(30)
```

    ##                                                    num_years     total
    ## mdinv llc|t rocho llc|stonefield investment fund i         6 6908169.8
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l         5 2261754.8
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs            4 2174205.2
    ## maple tsf llc                                              4 2033042.6
    ## acf development llc|tark development llc|tjci deve         5 1814044.3
    ## hobart holdings llc                                        6 1772765.4
    ## thornton mellon llc                                        3 1756911.7
    ## md tax properties  llc                                     5 1384965.6
    ## baltax  llc                                                4 1347329.1
    ## palm tsf llc                                               4 1213162.4
    ## municipal investments llc                                  5 1153121.2
    ## hickory tsf llc                                            3 1097921.0
    ## ultra-safe fund llc                                        4 1042902.1
    ## tlmd capital llc                                           4 1002458.8
    ## income one llc                                             3  964118.1
    ## caret bay llc|caret bey llc|caret bay holdings llc         4  729369.1
    ## tax properties one llc                                     3  591848.2
    ## mtsif llc                                                  3  554600.9
    ## park & menlo llc                                           3  447336.7
    ## bts  llc c/o j scott morse                                 3  363497.7
    ## tax sale holdings llc                                      2  300796.5
    ## rite investments llc                                       2  239116.5
    ## fig as custodian for fig md llc and se                     1  228931.8
    ## us bank as cust for tower dbw vi trust                     2  202895.0
    ## midaro investments  llc                                    1  197357.3
    ## ashland new homes iv llc                                   4  192450.0
    ## labri llc                                                  3  187579.7
    ## mtag as custodian for mgd-md llc                           4  181744.7
    ## immobilier cap ks ltd                                      4  179860.3
    ## golden ashland services llc                                3  170625.0
    ##                                                    time_held tot_n_sold
    ## mdinv llc|t rocho llc|stonefield investment fund i     -19.0        187
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l     -28.0        150
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs        -89.0         26
    ## maple tsf llc                                           -8.0        116
    ## acf development llc|tark development llc|tjci deve     -35.0        119
    ## hobart holdings llc                                    -19.5         92
    ## thornton mellon llc                                   -139.0         57
    ## md tax properties  llc                                 -11.0         66
    ## baltax  llc                                             -6.5         64
    ## palm tsf llc                                           -24.0         53
    ## municipal investments llc                             -217.0          9
    ## hickory tsf llc                                        -87.5         32
    ## ultra-safe fund llc                                    -87.5         20
    ## tlmd capital llc                                        -3.0         85
    ## income one llc                                        -199.5          6
    ## caret bay llc|caret bey llc|caret bay holdings llc     -83.0         17
    ## tax properties one llc                                  -7.0          7
    ## mtsif llc                                              -15.0         25
    ## park & menlo llc                                        -7.0         10
    ## bts  llc c/o j scott morse                             -12.0         11
    ## tax sale holdings llc                                  -10.0          5
    ## rite investments llc                                  -155.5          2
    ## fig as custodian for fig md llc and se                -137.0          1
    ## us bank as cust for tower dbw vi trust                 -97.5          4
    ## midaro investments  llc                                  0.0          1
    ## ashland new homes iv llc                               -84.0          4
    ## labri llc                                               -1.0          4
    ## mtag as custodian for mgd-md llc                      -105.0          5
    ## immobilier cap ks ltd                                  -62.0         14
    ## golden ashland services llc                            -13.0          5
    ##                                                    tot_n_int tot_amt_sold
    ## mdinv llc|t rocho llc|stonefield investment fund i      2729      6064442
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l      1118      2003348
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs         1370       525131
    ## maple tsf llc                                            800      1899400
    ## acf development llc|tark development llc|tjci deve       937      1586040
    ## hobart holdings llc                                      464      1643321
    ## thornton mellon llc                                     1523      1298440
    ## md tax properties  llc                                   389      1292737
    ## baltax  llc                                              506      1244392
    ## palm tsf llc                                             441      1140336
    ## municipal investments llc                               4398       219579
    ## hickory tsf llc                                          769       823120
    ## ultra-safe fund llc                                     1906       427740
    ## tlmd capital llc                                         581       918237
    ## income one llc                                            79       918188
    ## caret bay llc|caret bey llc|caret bay holdings llc       693       607206
    ## tax properties one llc                                   239       382298
    ## mtsif llc                                                233       518319
    ## park & menlo llc                                          28       424486
    ## bts  llc c/o j scott morse                                92       341201
    ## tax sale holdings llc                                     19       290054
    ## rite investments llc                                      10       217046
    ## fig as custodian for fig md llc and se                   411         9318
    ## us bank as cust for tower dbw vi trust                   158       119471
    ## midaro investments  llc                                  982            0
    ## ashland new homes iv llc                                   6       184601
    ## labri llc                                                 15       182265
    ## mtag as custodian for mgd-md llc                         149       126963
    ## immobilier cap ks ltd                                    134       165890
    ## golden ashland services llc                                8       167372
    ##                                                    tot_amt_int amt_own_int
    ## mdinv llc|t rocho llc|stonefield investment fund i  843727.771  117851.795
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l  258406.849   15996.958
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs    1649074.225  114815.858
    ## maple tsf llc                                       133642.552   22792.797
    ## acf development llc|tark development llc|tjci deve  228004.278   17765.270
    ## hobart holdings llc                                 129444.368   12158.620
    ## thornton mellon llc                                 458471.699  128327.551
    ## md tax properties  llc                               92228.614   14141.012
    ## baltax  llc                                         102937.090   18186.950
    ## palm tsf llc                                         72826.432    7432.168
    ## municipal investments llc                           933542.187  307266.017
    ## hickory tsf llc                                     274801.036   30733.909
    ## ultra-safe fund llc                                 615162.064  101371.582
    ## tlmd capital llc                                     84221.848    7356.202
    ## income one llc                                       45930.114    6541.959
    ## caret bay llc|caret bey llc|caret bay holdings llc  122163.125   15145.423
    ## tax properties one llc                              209550.219   11850.175
    ## mtsif llc                                            36281.888    1523.072
    ## park & menlo llc                                     22850.707    5124.962
    ## bts  llc c/o j scott morse                           22296.660    3531.232
    ## tax sale holdings llc                                10742.545    3384.183
    ## rite investments llc                                 22070.507    8942.521
    ## fig as custodian for fig md llc and se              219613.751   32027.026
    ## us bank as cust for tower dbw vi trust               83424.008   19317.715
    ## midaro investments  llc                             197357.269   60916.460
    ## ashland new homes iv llc                              7848.963       0.000
    ## labri llc                                             5314.749    2160.712
    ## mtag as custodian for mgd-md llc                     54781.678   11812.248
    ## immobilier cap ks ltd                                13970.327     626.286
    ## golden ashland services llc                           3253.018     659.239
    ##                                                    amt_non_own_int n_own_int
    ## mdinv llc|t rocho llc|stonefield investment fund i      725875.976       528
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l      242409.892        98
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs        1534258.367       247
    ## maple tsf llc                                           110849.754       155
    ## acf development llc|tark development llc|tjci deve      210239.008        99
    ## hobart holdings llc                                     117285.747        57
    ## thornton mellon llc                                     330144.147       572
    ## md tax properties  llc                                   78087.602        67
    ## baltax  llc                                              84750.140        95
    ## palm tsf llc                                             65394.264        51
    ## municipal investments llc                               626276.170      1945
    ## hickory tsf llc                                         244067.127       102
    ## ultra-safe fund llc                                     513790.481       439
    ## tlmd capital llc                                         76865.646        74
    ## income one llc                                           39388.155        24
    ## caret bay llc|caret bey llc|caret bay holdings llc      107017.702        82
    ## tax properties one llc                                  197700.044        26
    ## mtsif llc                                                34758.816        16
    ## park & menlo llc                                         17725.745         5
    ## bts  llc c/o j scott morse                               18765.427        22
    ## tax sale holdings llc                                     7358.361         6
    ## rite investments llc                                     13127.986         5
    ## fig as custodian for fig md llc and se                  187586.725        93
    ## us bank as cust for tower dbw vi trust                   64106.293        61
    ## midaro investments  llc                                 136440.809       359
    ## ashland new homes iv llc                                  7848.963         0
    ## labri llc                                                 3154.038         5
    ## mtag as custodian for mgd-md llc                         42969.430        55
    ## immobilier cap ks ltd                                    13344.040        15
    ## golden ashland services llc                               2593.779         3
    ##                                                    n_non_own_int owner_int_perc
    ## mdinv llc|t rocho llc|stonefield investment fund i          2201     0.13967988
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l          1020     0.06190609
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs             1123     0.06962443
    ## maple tsf llc                                                645     0.17055045
    ## acf development llc|tark development llc|tjci deve           838     0.07791639
    ## hobart holdings llc                                          407     0.09392931
    ## thornton mellon llc                                          951     0.27990289
    ## md tax properties  llc                                       322     0.15332565
    ## baltax  llc                                                  411     0.17668024
    ## palm tsf llc                                                 390     0.10205316
    ## municipal investments llc                                   2453     0.32913994
    ## hickory tsf llc                                              667     0.11184059
    ## ultra-safe fund llc                                         1467     0.16478842
    ## tlmd capital llc                                             507     0.08734316
    ## income one llc                                                55     0.14243289
    ## caret bay llc|caret bey llc|caret bay holdings llc           611     0.12397705
    ## tax properties one llc                                       213     0.05655052
    ## mtsif llc                                                    217     0.04197885
    ## park & menlo llc                                              23     0.22428025
    ## bts  llc c/o j scott morse                                    70     0.15837494
    ## tax sale holdings llc                                         13     0.31502623
    ## rite investments llc                                           5     0.40517969
    ## fig as custodian for fig md llc and se                       318     0.14583343
    ## us bank as cust for tower dbw vi trust                        97     0.23156062
    ## midaro investments  llc                                      623     0.30866084
    ## ashland new homes iv llc                                       6     0.00000000
    ## labri llc                                                     10     0.40655003
    ## mtag as custodian for mgd-md llc                              94     0.21562406
    ## immobilier cap ks ltd                                        119     0.04482973
    ## golden ashland services llc                                    5     0.20265461

``` r
#liened_prop_agg_f_j_3 %>% write_csv("~/Desktop/banner_projects/real_estate/dw_table_data.csv")
#liened_prop_agg_f_j_3 %>% arrange(desc(tot_n_sold)) %>% pull(tot_n_sold) %>% cumsum %>% {./1282}

#liened_prop_agg_f_j_3 %>% arrange(desc(total)) %>% pull(total) %>% cumsum %>% {./37525005}
```

The company that has profited the most from the tax sale system is the
local arm of Stonefield Investment Fund, a New Jersey based investor.
Stonefield has garnered nearly \$7 million dollars by investing in tax
sale in Baltimore. Most of their income has come in the form of property
aquisitions. They have received the deeds to 187 homes and collected
interest on another 2,700, turning those homes into that into \$6
million dollars of profits fromp flips and about \$840k from interest
payments. There is a lot to say about Stonefield’s corporate structure
and the way they operate in local markets.

Two very interesting companies are the FNA group (most likely First
National Assets, an Illinois-based affiliate of Denver-based investment
firm Arrowmark Partners) and Municipal Investments LLC (Heidi Kenny’s
tax lien group). These are the two companies that have taken the most in
interest over the last 6 years. FNA has taken \$1.6m in interest
payments, while Municipal Investments has taken \$934k. Despite
collecting from thousands of owners, neither has collected more than 30
deeds in the the last 6 years. FNA is interesting primarily because they
treat interest collection as a pure investment scheme.

And, while most companies truly do focus primarily on non owner occupied
properties, that isn’t exactly true for Municipal Investments.

32% of the interest collected by Municipal Investments was gotten from
owner-occupied properties, making it one of 6 companies with over \$100k
total over the last 6 years who took more than 30% of their interest
payments from homeowners. That’s the third highest percentage of all
investment groups.
