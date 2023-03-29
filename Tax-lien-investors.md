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

    ## Joining, by = "pm.uid"

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
  filter(N+Y+none>100) 
```

    ## # A tibble: 28 × 6
    ## # Groups:   final_names [28]
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
    ## # … with 18 more rows, and abbreviated variable name ¹​vac_perc
    ## # ℹ Use `print(n = ...)` to see more rows

We see a smaller percentage (64%) of vacant homes in the full list than
in ARX I LLC’s list (85%). Some companies, like Pluto LLC (39%), hold
far fewer vacant properties. It may be that ARX I LLC is just collecting
properties and leaving them vacant the way some researchers accuse these
investment companies of doing.

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
others. About 440 companies have purchased property tax debts through
the lien process over the last 6 years, and some have purchased many
more than others. The top 10 companies have purchased 80% of the
properties that sold and those same companies have received 64% the
total profits of the last six years, each earning at least a million
dollars in profits from flipping tax sale properties and interest. Only
13 companies have received deeds to more than 10 houses. 6of the top 10
companies all held the homes they purchased for fewer than 30 days, with
the 3 of the 4 remaining being companies that specialize in interest
collection and only infrequently take title to homes.

    ##                                                    num_years         total
    ## mdinv llc|t rocho llc|stonefield investment fund i         6  6.908170e+06
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l         5  2.261755e+06
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs            4  2.174205e+06
    ## maple tsf llc                                              4  2.033043e+06
    ## acf development llc|tark development llc|tjci deve         5  1.814044e+06
    ## hobart holdings llc                                        6  1.772765e+06
    ## thornton mellon llc                                        3  1.756912e+06
    ## md tax properties  llc                                     5  1.384966e+06
    ## baltax  llc                                                4  1.347329e+06
    ## palm tsf llc                                               4  1.213162e+06
    ## municipal investments llc                                  5  1.153121e+06
    ## hickory tsf llc                                            3  1.097921e+06
    ## ultra-safe fund llc                                        4  1.042902e+06
    ## tlmd capital llc                                           4  1.002459e+06
    ## income one llc                                             3  9.641181e+05
    ## caret bay llc|caret bey llc|caret bay holdings llc         4  7.293691e+05
    ## tax properties one llc                                     3  5.918482e+05
    ## mtsif llc                                                  3  5.546009e+05
    ## park & menlo llc                                           3  4.473367e+05
    ## bts  llc c/o j scott morse                                 3  3.634977e+05
    ## tax sale holdings llc                                      2  3.007965e+05
    ## rite investments llc                                       2  2.391165e+05
    ## fig as custodian for fig md llc and se                     1  2.289318e+05
    ## us bank as cust for tower dbw vi trust                     2  2.028950e+05
    ## midaro investments  llc                                    1  1.973573e+05
    ## ashland new homes iv llc                                   4  1.924500e+05
    ## labri llc                                                  3  1.875797e+05
    ## mtag as custodian for mgd-md llc                           4  1.817447e+05
    ## immobilier cap ks ltd                                      4  1.798603e+05
    ## golden ashland services llc                                3  1.706250e+05
    ## baltrentals dot com  llc                                   1  1.646391e+05
    ## maryland tax  llc                                          2  1.491803e+05
    ## bts  llc                                                   2  1.314962e+05
    ## palomino holdings llc                                      1  1.245565e+05
    ## jcrtl llc                                                  1  1.189585e+05
    ## patapsco property management llc                           1  1.104123e+05
    ## ram tax lien fund lp                                       0  1.100603e+05
    ## mtag as custodian for atcf ii maryland                     1  1.045269e+05
    ## pine valley one real estate llc                            0  1.040082e+05
    ## municipal lien fund llc                                    0  1.025017e+05
    ## atcf ii maryland llc taxserv cust                          0  1.012761e+05
    ## u s liens k trust                                          2  9.948207e+04
    ## baltimore tax  llc                                         3  9.759972e+04
    ## tloa of md llc                                             0  9.566488e+04
    ## pluto  llc                                                 0  9.385322e+04
    ## newline holdings llc                                       0  9.178896e+04
    ## ande investments llc                                       3  8.870702e+04
    ## integrity homes unlimited llc                              2  8.728987e+04
    ## vasantha bpillai                                           1  8.471885e+04
    ## redwood tsf llc                                            0  8.319098e+04
    ## ande properties llc                                        3  8.128352e+04
    ## karaninc solo k trust                                      1  7.928819e+04
    ## locke capital llc c/o eskin law llc                        0  7.875797e+04
    ## interstate holdings llc                                    0  7.841339e+04
    ## ashland holdings llc                                       2  7.742306e+04
    ## da property llc                                            2  7.376549e+04
    ## barbay llc c/o eskin law llc                               0  7.354866e+04
    ## wilbarger llc                                              1  7.322025e+04
    ## us bank cust for tower db ix trust -                       0  7.296373e+04
    ## atcf ii maryland llc lumentum cust                         0  6.885884e+04
    ## kairos real estate llc                                     2  6.830249e+04
    ## us bank cust for tower db viii trust                       0  6.399901e+04
    ## us bank cust for tower db vii trust                        0  6.374272e+04
    ## point holdings llc                                         0  6.262018e+04
    ## lmg llc                                                    1  6.224395e+04
    ## dnh group llc                                              1  6.190495e+04
    ## muir ridge properties llc                                  1  6.153445e+04
    ## cust fig series holdings llc fbo sec pty                   0  5.902470e+04
    ## innovative property solutions llc                          1  5.627378e+04
    ## u s liens llc                                              0  5.285449e+04
    ## md tl llc rai custodian                                    0  4.994443e+04
    ## north star realty management inc                           1  4.992755e+04
    ## serna psp                                                  0  4.883242e+04
    ## investments holding inc                                    1  4.573119e+04
    ## ips holdings llc                                           0  4.541126e+04
    ## granite  llc                                               2  4.407362e+04
    ## ssc-md llc                                                 0  4.177848e+04
    ## stages llc                                                 1  3.998890e+04
    ## hobart capital llc                                         1  3.984522e+04
    ## copperfield holdings llc                                   2  3.898125e+04
    ## hometrust tax liens llc                                    0  3.894522e+04
    ## win-wal properties llc                                     1  3.236978e+04
    ## dane equities  llc                                         2  3.073642e+04
    ## equiant financial services as custodian                    0  2.801482e+04
    ## dane equities llc                                          1  2.525628e+04
    ## usbank cust tower dbxi                                     0  2.251970e+04
    ## ghulam sarwar                                              0  2.150185e+04
    ## dean properties  llc                                       1  1.975573e+04
    ## bay assets llc                                             0  1.894952e+04
    ## reo properties servicing trust                             1  1.867928e+04
    ## horseshoe trust llc                                        2  1.832196e+04
    ## sws holdings llc                                           0  1.829408e+04
    ## rtlf-md llc                                                0  1.814273e+04
    ## mtag cust for empire viii md portfolio                     0  1.693903e+04
    ## locus and grove llc                                        0  1.593331e+04
    ## delinquent asset mgmt & maintenance llc                    0  1.547393e+04
    ## us bank c/f actlien holding                                1  1.463986e+04
    ## east coast tax auction llc                                 0  1.357522e+04
    ## bsd realty llc                                             0  1.353851e+04
    ## rp management llc                                          0  1.294563e+04
    ## reo property servicing trust                               1  1.259138e+04
    ## serna psp k plan                                           0  1.223964e+04
    ## sunflower gardens llc                                      0  1.175561e+04
    ## karan garewal                                              0  1.166785e+04
    ## ultra-safe fund  llc                                       0  1.166453e+04
    ## boi real llc                                               0  1.164525e+04
    ## immocap                                                    0  1.101634e+04
    ## crystal homes llc                                          0  1.063050e+04
    ## subhash c gupta                                            0  1.014989e+04
    ## jmj holdings llc                                           0  9.873702e+03
    ## raymond cr investment corp                                 0  9.743338e+03
    ## gt&t properties llc                                        0  9.566875e+03
    ## stone town investors llc                                   0  9.435862e+03
    ## babu jacob                                                 0  9.021800e+03
    ## ashland llc                                                0  8.879794e+03
    ## empire viii maryland portfolio llc                         0  8.792957e+03
    ## david streit                                               1  8.448002e+03
    ## howard fine                                                0  7.842194e+03
    ## mls equity llc                                             1  7.057228e+03
    ## ipb management group llc                                   0  6.745647e+03
    ## els realty inc                                             0  6.569287e+03
    ## naveen malhotra                                            0  6.342819e+03
    ## largent llc                                                0  5.720195e+03
    ## us bank as custodian for actlien holdi                     0  5.146546e+03
    ## xli baltimore fund llc                                     0  5.145931e+03
    ## housing our future inc                                     0  4.920183e+03
    ## us bank cust actlien holding                               1  4.751067e+03
    ## liengps  llc                                               0  4.634829e+03
    ## josephine alexander llc                                    0  4.103775e+03
    ## us liens k trust                                           0  4.093090e+03
    ## nexus holdings llc                                         0  3.968142e+03
    ## unique homes llc                                           0  3.808535e+03
    ## express funding llc                                        0  3.634014e+03
    ## oneeno llc                                                 0  3.575503e+03
    ## taxsale                                                    0  3.559540e+03
    ## chesapeake liens llc                                       0  3.514665e+03
    ## us bank cust for tower db x trust -                        0  3.498314e+03
    ## scattered sites ii llc                                     0  3.408488e+03
    ## pbi holdings inc                                           0  3.309405e+03
    ## crab properties llc                                        0  3.234051e+03
    ## naiman and naiman pa                                       0  3.229342e+03
    ## daley difranco & co llc                                    0  3.180418e+03
    ## wells properties llc                                       0  3.161269e+03
    ## plainview financial services ltd                           0  3.083321e+03
    ## day investment and consulting llc                          0  3.034901e+03
    ## liberty international investments inc                      0  2.953277e+03
    ## mirona investment group llc                                0  2.946200e+03
    ## dean investments llc                                       0  2.868254e+03
    ## brookins investments llc                                   0  2.825385e+03
    ## gnj investor marketing llc                                 0  2.646513e+03
    ## bbic real estate llc                                       0  2.597926e+03
    ## dean investments  llc                                      0  2.592554e+03
    ## kyle sampson                                               0  2.575001e+03
    ## us liens llc                                               0  2.561402e+03
    ## propitious properties llc                                  0  2.530130e+03
    ## yosemite ventures llc                                      0  2.511587e+03
    ## land research associates llc                               0  2.427369e+03
    ## samy gupta                                                 0  2.395077e+03
    ## baltimore arts realty corporation                          0  2.328700e+03
    ## hampstead hills llc                                        0  2.279612e+03
    ## brownstone investments llc                                 0  2.179909e+03
    ## bmoreprop llc                                              0  2.164098e+03
    ## h land co llc                                              0  2.052043e+03
    ## adam skordinski                                            0  2.012084e+03
    ## carey & marciniak properties llc                           0  1.820279e+03
    ## watson economic development associates                     1  1.814099e+03
    ## tyi and lillllc                                            0  1.797080e+03
    ## benjamin j rubin                                           0  1.765342e+03
    ## monarch development llc                                    0  1.755445e+03
    ## wendi henry                                                0  1.702632e+03
    ## arkad capital                                              0  1.695713e+03
    ## sanabani properties llc                                    0  1.682484e+03
    ## gang li                                                    0  1.655714e+03
    ## h&h real ventures company llc                              0  1.628901e+03
    ## jeffrey harris                                             0  1.616187e+03
    ## r healthcare                                               0  1.596126e+03
    ## dean investments                                           0  1.514785e+03
    ## dainan bramble                                             0  1.481047e+03
    ## sandor mester                                              0  1.465885e+03
    ## eric harris                                                0  1.465153e+03
    ## chad newkirk                                               0  1.464284e+03
    ## shahrad shawn yazdani                                      1  1.425859e+03
    ## advance iii llc                                            1  1.369925e+03
    ## asratu assefa checol                                       0  1.342743e+03
    ## kevin digrazia                                             0  1.339639e+03
    ## louis bailey                                               0  1.333392e+03
    ## nanbar solo k trust                                        0  1.323434e+03
    ## patricia r lewis                                           0  1.321223e+03
    ## charter meridian                                           0  1.316092e+03
    ## strategic properties llc                                   0  1.312558e+03
    ## dawn veltman                                               0  1.306059e+03
    ## g imperios                                                 0  1.257721e+03
    ## hometrust franchise llc                                    0  1.220003e+03
    ## housing our future together                                0  1.194843e+03
    ## sweet p s joyceful assistant living llc                    0  1.193587e+03
    ## stellar prime investment real estate                       0  1.184812e+03
    ## hilton properties llc                                      0  1.135026e+03
    ## abode keys llc                                             0  1.127159e+03
    ## bwi home llc                                               0  1.093623e+03
    ## natalex solo k plan                                        0  1.087463e+03
    ## cedr llc                                                   0  1.079046e+03
    ## cynthia brown                                              0  1.077835e+03
    ## christopher perry                                          0  1.030126e+03
    ## feruz boboev                                               0  1.017506e+03
    ## ankh literacy initiative                                   0  1.011070e+03
    ## dorman dennis                                              0  1.003071e+03
    ## marissa arguijo                                            0  9.882112e+02
    ## bellatruth commercial properties llc                       0  9.803363e+02
    ## bridgette perkins                                          0  9.743797e+02
    ## baybrook llc                                               0  9.653751e+02
    ## alchymist holdings llc                                     0  9.597750e+02
    ## natalya antonenko                                          0  9.509491e+02
    ## cabata family enterprises llc                              0  9.458911e+02
    ## resilience real estate l c                                 0  9.207949e+02
    ## preeminent management group llc                            0  9.031641e+02
    ## key realty group- i llc                                    0  9.018875e+02
    ## zippy realty llc                                           0  8.977946e+02
    ## myhome properties llc                                      0  8.787843e+02
    ## frucasion fruit boutique                                   0  8.585885e+02
    ## east investment group llc                                  0  8.492864e+02
    ## homeland valley llc                                        0  8.404192e+02
    ## marcus franz                                               0  8.209860e+02
    ## mission hill md llc                                        0  8.189550e+02
    ## timothy belcher                                            0  8.094809e+02
    ## technical services llc                                     0  8.074357e+02
    ## charter meridian llc                                       1  7.381648e+02
    ## jw estates llc                                             0  7.159822e+02
    ## williamsville                                              0  7.130100e+02
    ## denise abrams                                              0  6.346492e+02
    ## forest burrell industries                                  0  6.130498e+02
    ## housing our future                                         0  5.923001e+02
    ## platinum solutions llc                                     0  5.815638e+02
    ## martavious washington                                      0  5.761096e+02
    ## one  eno llc                                               0  5.724297e+02
    ## the leverage firm                                          0  5.678707e+02
    ## coit real estate llc                                       1  5.663347e+02
    ## erudition                                                  0  5.621497e+02
    ## atcf ii maryland llc                                       0  5.593295e+02
    ## american home group llc                                    0  5.551563e+02
    ## global international group                                 0  5.499118e+02
    ## hasson barnes                                              0  5.265727e+02
    ## erica seaman                                               0  5.164960e+02
    ## omin                                                       0  5.125506e+02
    ## taf holdings llc                                           0  5.125204e+02
    ## reginald best ii                                           0  5.123866e+02
    ## noovah llc                                                 0  5.104045e+02
    ## ross chamberlain                                           0  5.082253e+02
    ## bde homes llc                                              0  5.078784e+02
    ## brandon burrell                                            0  5.076476e+02
    ## robert braddock  sonya braddock                            0  4.392577e+02
    ## jmj residential holdings llc                               0  4.387496e+02
    ## kimberly green                                             0  4.358037e+02
    ## hpa holdings llc                                           0  4.260369e+02
    ## response ability investing llc                             0  4.231731e+02
    ## evans wilson                                               0  4.202904e+02
    ## sahar algahdari                                            0  4.164203e+02
    ## osage advisors llc                                         0  4.086311e+02
    ## departed llc                                               0  4.080730e+02
    ## mansur abdul-malik                                         0  3.965657e+02
    ## robert martinez                                            0  3.896982e+02
    ## twin mills investments llc                                 0  3.847539e+02
    ## clay wilson iv                                             0  3.784132e+02
    ## wj investments llc                                         0  3.780027e+02
    ## patricia lewis                                             0  3.751695e+02
    ## accent professional services llc                           0  3.728272e+02
    ## bay capital llc                                            0  3.717541e+02
    ## bright venture holdings llc                                0  3.698701e+02
    ## hayk hagopian                                              0  3.655605e+02
    ## anthony brand                                              0  3.652137e+02
    ## mts venture  llc                                           0  3.540819e+02
    ## lewis s davis                                              0  3.404387e+02
    ## jon magsaysay                                              0  3.395929e+02
    ## investments for good international                         0  3.346254e+02
    ## hometrust on the top llc                                   0  3.329441e+02
    ## harout doukmajian                                          0  3.284395e+02
    ## nemo llc                                                   0  3.282705e+02
    ## astute real estate investments llc                         1  3.274785e+02
    ## john evans                                                 0  3.167665e+02
    ## highrise real estate groups llc                            0  3.159905e+02
    ## amber crump                                                0  3.098366e+02
    ## beyond change                                              0  3.057565e+02
    ## asratu checol                                              1  2.999683e+02
    ## gonzalez videla                                            0  2.988975e+02
    ## joseph building restoration                                0  2.921522e+02
    ## reginald l ready                                           1  2.902144e+02
    ## terrence brown                                             0  2.857527e+02
    ## wsfs as custodian for rearden llc f/b/o                    0  2.854675e+02
    ## the curry team llc                                         0  2.850877e+02
    ## peggy m israel                                             0  2.844412e+02
    ## harvey ross                                                0  2.749754e+02
    ## deborah zebina                                             0  2.719197e+02
    ## leverage law firm                                          0  2.716924e+02
    ## ade estates llc                                            0  2.700526e+02
    ## the bean group                                             0  2.619141e+02
    ## glenn c rogers jr                                          0  2.611584e+02
    ## secured futures funding                                    0  2.602065e+02
    ## latia morton                                               0  2.590010e+02
    ## darron waller                                              0  2.569610e+02
    ## mikeen investments no  llc                                 0  2.545601e+02
    ## kenneth futrell                                            0  2.508216e+02
    ## michael martin                                             0  2.449722e+02
    ## emerson lane llc                                           0  2.339891e+02
    ## bwe enterprises llc                                        0  2.338220e+02
    ## melissa singletary                                         0  2.257954e+02
    ## eagle homes usa llc                                        1  2.250049e+02
    ## lawrence e callis jr                                       0  2.187894e+02
    ## diamond flip investments llc                               0  2.180338e+02
    ## vine and miller llc                                        0  2.134912e+02
    ## erickson miller                                            0  2.112716e+02
    ## nelson fu                                                  0  2.051909e+02
    ## danielle natasha green                                     0  2.051709e+02
    ## hometrust sanay llc                                        0  2.013651e+02
    ## suchgor llc                                                0  1.990357e+02
    ## gaberell moore                                             0  1.967647e+02
    ## moniladae adewoyin                                         0  1.959322e+02
    ## gedion teklemariam                                         0  1.869214e+02
    ## mellow investor inc                                        0  1.838601e+02
    ## cmc properties llc                                         0  1.827815e+02
    ## rmx                                                        0  1.815833e+02
    ## abrams real estate development                             0  1.800586e+02
    ## sharon mokhtari                                            0  1.790781e+02
    ## housing our future too                                     0  1.779822e+02
    ## tlh tax auctions llc                                       0  1.773919e+02
    ## crystal session                                            0  1.762573e+02
    ## grand development llc                                      0  1.729380e+02
    ## courtly group management llc                               0  1.718561e+02
    ## girard curry                                               0  1.715291e+02
    ## deandre holley                                             0  1.705327e+02
    ## baltax                                                     0  1.701403e+02
    ## princess valentine                                         0  1.602008e+02
    ## buddy roth llc                                             0  1.518634e+02
    ## jacqueline d dennis                                        0  1.500380e+02
    ## rognell llc                                                0  1.498735e+02
    ## infinite kingdom llc                                       0  1.440566e+02
    ## raymond c r investment corp                                0  1.417043e+02
    ## latina dawkins                                             0  1.390945e+02
    ## antwan lipscomb                                            0  1.352952e+02
    ## lisa tisdale                                               0  1.352057e+02
    ## lynn moaney                                                0  1.336101e+02
    ## go green incentive llc                                     0  1.314578e+02
    ## amber woodruff                                             0  1.297703e+02
    ## mac  llc                                                   0  1.281198e+02
    ## jamal adams                                                0  1.254600e+02
    ## lights                                                     0  1.181200e+02
    ## nile community and family services llc                     0  1.178387e+02
    ## cvc realty holdings llc                                    0  1.161982e+02
    ## true north advisory llc                                    0  1.138217e+02
    ## danielle alston                                            0  1.136118e+02
    ## premier fitness training                                   0  1.109230e+02
    ## michael steininger                                         0  1.084817e+02
    ## kalunga holdings llc                                       0  1.080576e+02
    ## renaud p batcho                                            0  1.068868e+02
    ## shanice crowder                                            0  1.065202e+02
    ## visionary enterprises                                      0  1.063669e+02
    ## melba chambers                                             0  1.014142e+02
    ## rmx llc                                                    0  1.000612e+02
    ## kendal l joyce                                             0  1.000152e+02
    ## ukoha ukoha                                                0  9.966933e+01
    ## dch realty                                                 0  9.893400e+01
    ## trebor investments                                         0  9.877411e+01
    ## gnj investor marketing                                     1  9.690023e+01
    ## maria bernardez / lucas polanco                            0  9.612022e+01
    ## gilad enterprisescom                                       0  9.461642e+01
    ## loch raven investments llc                                 0  9.450189e+01
    ## ronnie lewis                                               0  9.356863e+01
    ## jeron p stokes                                             0  9.159780e+01
    ## gulden llc                                                 0  9.039233e+01
    ## young law group                                            0  8.951209e+01
    ## acrobat llc                                                0  8.943133e+01
    ## dream builders asset capital llc                           0  8.905328e+01
    ## william hammond                                            0  8.684850e+01
    ## brad lindsey                                               0  8.600311e+01
    ## baltimore appraisal & realty company in                    1  8.423080e+01
    ## aerica lake                                                0  8.245367e+01
    ## limitless homes                                            0  8.126697e+01
    ## chevette wilson                                            0  7.856413e+01
    ## spectrum soft consultants llc                              0  7.818875e+01
    ## dalyn clarence allen                                       0  6.935796e+01
    ## peter v faris                                              0  6.840939e+01
    ## open ocean investments llc                                 0  6.366432e+01
    ## cory nelson                                                0  6.324434e+01
    ## brighter daye llc                                          0  6.317472e+01
    ## rachel scott                                               0  6.138737e+01
    ## cav capital llc                                            0  6.119100e+01
    ## terrance evans                                             0  5.987406e+01
    ## quentin holley                                             0  5.856573e+01
    ## black women build llc                                      0  5.835375e+01
    ## tyrone smith                                               0  5.835280e+01
    ## unclaimed funds unit                                       0  5.708905e+01
    ## callis legacy renovations llc                              0  5.658960e+01
    ## brook-lyn corp llc                                         0  5.551325e+01
    ## bnote  llc                                                 0  5.199463e+01
    ## ashman investments llc                                     0  5.110177e+01
    ## dietra norton                                              0  4.980030e+01
    ## lands and liens                                            0  4.798269e+01
    ## loudons place llc                                          0  4.770667e+01
    ## carl tilghman                                              0  4.617177e+01
    ## alpha zone investment llc                                  0  4.612223e+01
    ## bora mpinja                                                0  4.445841e+01
    ## pbi holdings llc                                           0  4.408509e+01
    ## cynthia gross                                              0  4.293276e+01
    ## tiffany fennoy                                             0  4.098316e+01
    ## ying zhang                                                 0  4.089166e+01
    ## temeshia johnson                                           0  3.897075e+01
    ## deon mcgehee                                               0  3.889013e+01
    ## rda llc                                                    0  3.855148e+01
    ## steven messmer                                             0  3.588445e+01
    ## glenn curtis rogers jr                                     0  3.579105e+01
    ## atkinsonsmith llc                                          0  3.559416e+01
    ## averil eugene rosenburgh                                   0  3.550150e+01
    ## castilian properties llc                                   0  3.472078e+01
    ## billy may                                                  0  3.455673e+01
    ## cle deals                                                  0  3.096087e+01
    ## assurance communications teknologies                       0  2.864970e+01
    ## alfred r daniels                                           0  2.848785e+01
    ## jjj turner investments llc                                 0  2.801450e+01
    ## step  llc                                                  0  2.598268e+01
    ## rtlf-nj llc                                                0  2.586155e+01
    ## taneisha pauliono                                          0  2.565130e+01
    ## kj productions                                             0  2.288507e+01
    ## c&j investments                                            0  2.236683e+01
    ## takeshia campbell                                          0  2.195268e+01
    ## almarin properties llc                                     0  2.126328e+01
    ## ijd paralegal services llc                                 0  2.123212e+01
    ## sag investment trust                                       0  2.096185e+01
    ## all american tax liens llc                                 0  2.008195e+01
    ## llc                                                        0  1.808982e+01
    ## nadeem butt                                                0  1.681408e+01
    ## atash properties                                           0  1.591630e+01
    ## st funding group llc                                       0  1.436820e+01
    ## stephenson development llc                                 0  1.368929e+01
    ## melvin l freeman                                           0  1.325541e+01
    ## brian batson                                               0  1.169196e+01
    ## cynthia d gross                                            0  1.166400e+01
    ## ronald reedy brown                                         0  9.455150e+00
    ## harris homes llc                                           0  8.579560e+00
    ## the miller group                                           0  8.487285e+00
    ## jarrell winston                                            0  7.779840e+00
    ## sonya m bynum                                              0  7.071870e+00
    ## noteman r/e investments llc                                0  6.360433e+00
    ## lws investments llc                                        0  6.085320e+00
    ## haynes consulting                                          0  5.758380e+00
    ## heber brown iii                                            0  4.673200e+00
    ## hathaway property management llc                           0  3.840800e+00
    ## new country properties llc                                 0  3.489600e+00
    ## focus forward property group llc                           0  3.312450e+00
    ## kevin jean-pierre                                          0  3.241305e+00
    ## st nicholas  llc                                           0  2.650470e+00
    ## monique r mack                                             0  2.577100e+00
    ## real estate census                                         0  2.009880e+00
    ## steadfast structural engineers                             0  1.956430e+00
    ## jamari development llc                                     0  6.537300e-01
    ## max daddy llc                                              2 -1.090770e+04
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
    ## baltrentals dot com  llc                              -404.0          1
    ## maryland tax  llc                                      -49.0          3
    ## bts  llc                                              -100.5          2
    ## palomino holdings llc                                 -113.0          2
    ## jcrtl llc                                             -675.0          1
    ## patapsco property management llc                      -935.0          1
    ## ram tax lien fund lp                                     0.0          0
    ## mtag as custodian for atcf ii maryland                 -35.0          1
    ## pine valley one real estate llc                          0.0          0
    ## municipal lien fund llc                                  0.0          0
    ## atcf ii maryland llc taxserv cust                        0.0          0
    ## u s liens k trust                                     -536.5          2
    ## baltimore tax  llc                                     -69.0          5
    ## tloa of md llc                                           0.0          0
    ## pluto  llc                                               0.0          0
    ## newline holdings llc                                     0.0          0
    ## ande investments llc                                   -13.5          4
    ## integrity homes unlimited llc                         -769.0          3
    ## vasantha bpillai                                      -164.0          1
    ## redwood tsf llc                                          0.0          0
    ## ande properties llc                                    -25.0          4
    ## karaninc solo k trust                                 -552.0          1
    ## locke capital llc c/o eskin law llc                      0.0          0
    ## interstate holdings llc                                  0.0          0
    ## ashland holdings llc                                   -30.0          3
    ## da property llc                                        -64.5          2
    ## barbay llc c/o eskin law llc                             0.0          0
    ## wilbarger llc                                         -505.0          1
    ## us bank cust for tower db ix trust -                     0.0          0
    ## atcf ii maryland llc lumentum cust                       0.0          0
    ## kairos real estate llc                                -495.0          3
    ## us bank cust for tower db viii trust                     0.0          0
    ## us bank cust for tower db vii trust                      0.0          0
    ## point holdings llc                                       0.0          0
    ## lmg llc                                                -88.0          1
    ## dnh group llc                                         -809.0          1
    ## muir ridge properties llc                             -398.0          2
    ## cust fig series holdings llc fbo sec pty                 0.0          0
    ## innovative property solutions llc                       -3.0          2
    ## u s liens llc                                            0.0          0
    ## md tl llc rai custodian                                  0.0          0
    ## north star realty management inc                       -62.0          1
    ## serna psp                                                0.0          0
    ## investments holding inc                                -38.0          1
    ## ips holdings llc                                         0.0          0
    ## granite  llc                                           -61.5          3
    ## ssc-md llc                                               0.0          0
    ## stages llc                                            -563.0          1
    ## hobart capital llc                                    -127.0          1
    ## copperfield holdings llc                               -54.0          2
    ## hometrust tax liens llc                                  0.0          0
    ## win-wal properties llc                                -134.0          1
    ## dane equities  llc                                    -153.0          2
    ## equiant financial services as custodian                  0.0          0
    ## dane equities llc                                      -11.0          1
    ## usbank cust tower dbxi                                   0.0          0
    ## ghulam sarwar                                            0.0          0
    ## dean properties  llc                                   -58.0          1
    ## bay assets llc                                           0.0          0
    ## reo properties servicing trust                         -10.5          2
    ## horseshoe trust llc                                   -106.0          2
    ## sws holdings llc                                         0.0          0
    ## rtlf-md llc                                              0.0          0
    ## mtag cust for empire viii md portfolio                   0.0          0
    ## locus and grove llc                                      0.0          0
    ## delinquent asset mgmt & maintenance llc                  0.0          0
    ## us bank c/f actlien holding                              0.0          1
    ## east coast tax auction llc                               0.0          0
    ## bsd realty llc                                           0.0          0
    ## rp management llc                                        0.0          0
    ## reo property servicing trust                            -9.0          1
    ## serna psp k plan                                         0.0          0
    ## sunflower gardens llc                                    0.0          0
    ## karan garewal                                            0.0          0
    ## ultra-safe fund  llc                                     0.0          0
    ## boi real llc                                             0.0          0
    ## immocap                                                  0.0          0
    ## crystal homes llc                                        0.0          0
    ## subhash c gupta                                          0.0          0
    ## jmj holdings llc                                         0.0          0
    ## raymond cr investment corp                               0.0          0
    ## gt&t properties llc                                      0.0          0
    ## stone town investors llc                                 0.0          0
    ## babu jacob                                               0.0          0
    ## ashland llc                                              0.0          0
    ## empire viii maryland portfolio llc                       0.0          0
    ## david streit                                           -75.0          1
    ## howard fine                                              0.0          0
    ## mls equity llc                                           0.0          1
    ## ipb management group llc                                 0.0          0
    ## els realty inc                                           0.0          0
    ## naveen malhotra                                          0.0          0
    ## largent llc                                              0.0          0
    ## us bank as custodian for actlien holdi                   0.0          0
    ## xli baltimore fund llc                                   0.0          0
    ## housing our future inc                                   0.0          0
    ## us bank cust actlien holding                             0.0          1
    ## liengps  llc                                             0.0          0
    ## josephine alexander llc                                  0.0          0
    ## us liens k trust                                         0.0          0
    ## nexus holdings llc                                       0.0          0
    ## unique homes llc                                         0.0          0
    ## express funding llc                                      0.0          0
    ## oneeno llc                                               0.0          0
    ## taxsale                                                  0.0          0
    ## chesapeake liens llc                                     0.0          0
    ## us bank cust for tower db x trust -                      0.0          0
    ## scattered sites ii llc                                   0.0          0
    ## pbi holdings inc                                         0.0          0
    ## crab properties llc                                      0.0          0
    ## naiman and naiman pa                                     0.0          0
    ## daley difranco & co llc                                  0.0          0
    ## wells properties llc                                     0.0          0
    ## plainview financial services ltd                         0.0          0
    ## day investment and consulting llc                        0.0          0
    ## liberty international investments inc                    0.0          0
    ## mirona investment group llc                              0.0          0
    ## dean investments llc                                     0.0          0
    ## brookins investments llc                                 0.0          0
    ## gnj investor marketing llc                               0.0          0
    ## bbic real estate llc                                     0.0          0
    ## dean investments  llc                                    0.0          0
    ## kyle sampson                                             0.0          0
    ## us liens llc                                             0.0          0
    ## propitious properties llc                                0.0          0
    ## yosemite ventures llc                                    0.0          0
    ## land research associates llc                             0.0          0
    ## samy gupta                                               0.0          0
    ## baltimore arts realty corporation                        0.0          0
    ## hampstead hills llc                                      0.0          0
    ## brownstone investments llc                               0.0          0
    ## bmoreprop llc                                            0.0          0
    ## h land co llc                                            0.0          0
    ## adam skordinski                                          0.0          0
    ## carey & marciniak properties llc                         0.0          0
    ## watson economic development associates                   0.0          1
    ## tyi and lillllc                                          0.0          0
    ## benjamin j rubin                                         0.0          0
    ## monarch development llc                                  0.0          0
    ## wendi henry                                              0.0          0
    ## arkad capital                                            0.0          0
    ## sanabani properties llc                                  0.0          0
    ## gang li                                                  0.0          0
    ## h&h real ventures company llc                            0.0          0
    ## jeffrey harris                                           0.0          0
    ## r healthcare                                             0.0          0
    ## dean investments                                         0.0          0
    ## dainan bramble                                           0.0          0
    ## sandor mester                                            0.0          0
    ## eric harris                                              0.0          0
    ## chad newkirk                                             0.0          0
    ## shahrad shawn yazdani                                    0.0          1
    ## advance iii llc                                          0.0          1
    ## asratu assefa checol                                     0.0          0
    ## kevin digrazia                                           0.0          0
    ## louis bailey                                             0.0          0
    ## nanbar solo k trust                                      0.0          0
    ## patricia r lewis                                         0.0          0
    ## charter meridian                                         0.0          0
    ## strategic properties llc                                 0.0          0
    ## dawn veltman                                             0.0          0
    ## g imperios                                               0.0          0
    ## hometrust franchise llc                                  0.0          0
    ## housing our future together                              0.0          0
    ## sweet p s joyceful assistant living llc                  0.0          0
    ## stellar prime investment real estate                     0.0          0
    ## hilton properties llc                                    0.0          0
    ## abode keys llc                                           0.0          0
    ## bwi home llc                                             0.0          0
    ## natalex solo k plan                                      0.0          0
    ## cedr llc                                                 0.0          0
    ## cynthia brown                                            0.0          0
    ## christopher perry                                        0.0          0
    ## feruz boboev                                             0.0          0
    ## ankh literacy initiative                                 0.0          0
    ## dorman dennis                                            0.0          0
    ## marissa arguijo                                          0.0          0
    ## bellatruth commercial properties llc                     0.0          0
    ## bridgette perkins                                        0.0          0
    ## baybrook llc                                             0.0          0
    ## alchymist holdings llc                                   0.0          0
    ## natalya antonenko                                        0.0          0
    ## cabata family enterprises llc                            0.0          0
    ## resilience real estate l c                               0.0          0
    ## preeminent management group llc                          0.0          0
    ## key realty group- i llc                                  0.0          0
    ## zippy realty llc                                         0.0          0
    ## myhome properties llc                                    0.0          0
    ## frucasion fruit boutique                                 0.0          0
    ## east investment group llc                                0.0          0
    ## homeland valley llc                                      0.0          0
    ## marcus franz                                             0.0          0
    ## mission hill md llc                                      0.0          0
    ## timothy belcher                                          0.0          0
    ## technical services llc                                   0.0          0
    ## charter meridian llc                                     0.0          2
    ## jw estates llc                                           0.0          0
    ## williamsville                                            0.0          0
    ## denise abrams                                            0.0          0
    ## forest burrell industries                                0.0          0
    ## housing our future                                       0.0          0
    ## platinum solutions llc                                   0.0          0
    ## martavious washington                                    0.0          0
    ## one  eno llc                                             0.0          0
    ## the leverage firm                                        0.0          0
    ## coit real estate llc                                     0.0          1
    ## erudition                                                0.0          0
    ## atcf ii maryland llc                                     0.0          0
    ## american home group llc                                  0.0          0
    ## global international group                               0.0          0
    ## hasson barnes                                            0.0          0
    ## erica seaman                                             0.0          0
    ## omin                                                     0.0          0
    ## taf holdings llc                                         0.0          0
    ## reginald best ii                                         0.0          0
    ## noovah llc                                               0.0          0
    ## ross chamberlain                                         0.0          0
    ## bde homes llc                                            0.0          0
    ## brandon burrell                                          0.0          0
    ## robert braddock  sonya braddock                          0.0          0
    ## jmj residential holdings llc                             0.0          0
    ## kimberly green                                           0.0          0
    ## hpa holdings llc                                         0.0          0
    ## response ability investing llc                           0.0          0
    ## evans wilson                                             0.0          0
    ## sahar algahdari                                          0.0          0
    ## osage advisors llc                                       0.0          0
    ## departed llc                                             0.0          0
    ## mansur abdul-malik                                       0.0          0
    ## robert martinez                                          0.0          0
    ## twin mills investments llc                               0.0          0
    ## clay wilson iv                                           0.0          0
    ## wj investments llc                                       0.0          0
    ## patricia lewis                                           0.0          0
    ## accent professional services llc                         0.0          0
    ## bay capital llc                                          0.0          0
    ## bright venture holdings llc                              0.0          0
    ## hayk hagopian                                            0.0          0
    ## anthony brand                                            0.0          0
    ## mts venture  llc                                         0.0          0
    ## lewis s davis                                            0.0          0
    ## jon magsaysay                                            0.0          0
    ## investments for good international                       0.0          0
    ## hometrust on the top llc                                 0.0          0
    ## harout doukmajian                                        0.0          0
    ## nemo llc                                                 0.0          0
    ## astute real estate investments llc                       0.0          1
    ## john evans                                               0.0          0
    ## highrise real estate groups llc                          0.0          0
    ## amber crump                                              0.0          0
    ## beyond change                                            0.0          0
    ## asratu checol                                            0.0          1
    ## gonzalez videla                                          0.0          0
    ## joseph building restoration                              0.0          0
    ## reginald l ready                                         0.0          1
    ## terrence brown                                           0.0          0
    ## wsfs as custodian for rearden llc f/b/o                  0.0          0
    ## the curry team llc                                       0.0          0
    ## peggy m israel                                           0.0          0
    ## harvey ross                                              0.0          0
    ## deborah zebina                                           0.0          0
    ## leverage law firm                                        0.0          0
    ## ade estates llc                                          0.0          0
    ## the bean group                                           0.0          0
    ## glenn c rogers jr                                        0.0          0
    ## secured futures funding                                  0.0          0
    ## latia morton                                             0.0          0
    ## darron waller                                            0.0          0
    ## mikeen investments no  llc                               0.0          0
    ## kenneth futrell                                          0.0          0
    ## michael martin                                           0.0          0
    ## emerson lane llc                                         0.0          0
    ## bwe enterprises llc                                      0.0          0
    ## melissa singletary                                       0.0          0
    ## eagle homes usa llc                                    -49.0          1
    ## lawrence e callis jr                                     0.0          0
    ## diamond flip investments llc                             0.0          0
    ## vine and miller llc                                      0.0          0
    ## erickson miller                                          0.0          0
    ## nelson fu                                                0.0          0
    ## danielle natasha green                                   0.0          0
    ## hometrust sanay llc                                      0.0          0
    ## suchgor llc                                              0.0          0
    ## gaberell moore                                           0.0          0
    ## moniladae adewoyin                                       0.0          0
    ## gedion teklemariam                                       0.0          0
    ## mellow investor inc                                      0.0          0
    ## cmc properties llc                                       0.0          0
    ## rmx                                                      0.0          0
    ## abrams real estate development                           0.0          0
    ## sharon mokhtari                                          0.0          0
    ## housing our future too                                   0.0          0
    ## tlh tax auctions llc                                     0.0          0
    ## crystal session                                          0.0          0
    ## grand development llc                                    0.0          0
    ## courtly group management llc                             0.0          0
    ## girard curry                                             0.0          0
    ## deandre holley                                           0.0          0
    ## baltax                                                   0.0          0
    ## princess valentine                                       0.0          0
    ## buddy roth llc                                           0.0          0
    ## jacqueline d dennis                                      0.0          0
    ## rognell llc                                              0.0          0
    ## infinite kingdom llc                                     0.0          0
    ## raymond c r investment corp                              0.0          0
    ## latina dawkins                                           0.0          0
    ## antwan lipscomb                                          0.0          0
    ## lisa tisdale                                             0.0          0
    ## lynn moaney                                              0.0          0
    ## go green incentive llc                                   0.0          0
    ## amber woodruff                                           0.0          0
    ## mac  llc                                                 0.0          0
    ## jamal adams                                              0.0          0
    ## lights                                                   0.0          0
    ## nile community and family services llc                   0.0          0
    ## cvc realty holdings llc                                  0.0          0
    ## true north advisory llc                                  0.0          0
    ## danielle alston                                          0.0          0
    ## premier fitness training                                 0.0          0
    ## michael steininger                                       0.0          0
    ## kalunga holdings llc                                     0.0          0
    ## renaud p batcho                                          0.0          0
    ## shanice crowder                                          0.0          0
    ## visionary enterprises                                    0.0          0
    ## melba chambers                                           0.0          0
    ## rmx llc                                                  0.0          0
    ## kendal l joyce                                           0.0          0
    ## ukoha ukoha                                              0.0          0
    ## dch realty                                               0.0          0
    ## trebor investments                                       0.0          0
    ## gnj investor marketing                                   0.0          2
    ## maria bernardez / lucas polanco                          0.0          0
    ## gilad enterprisescom                                     0.0          0
    ## loch raven investments llc                               0.0          0
    ## ronnie lewis                                             0.0          0
    ## jeron p stokes                                           0.0          0
    ## gulden llc                                               0.0          0
    ## young law group                                          0.0          0
    ## acrobat llc                                              0.0          0
    ## dream builders asset capital llc                         0.0          0
    ## william hammond                                          0.0          0
    ## brad lindsey                                             0.0          0
    ## baltimore appraisal & realty company in                  0.0          1
    ## aerica lake                                              0.0          0
    ## limitless homes                                          0.0          0
    ## chevette wilson                                          0.0          0
    ## spectrum soft consultants llc                            0.0          0
    ## dalyn clarence allen                                     0.0          0
    ## peter v faris                                            0.0          0
    ## open ocean investments llc                               0.0          0
    ## cory nelson                                              0.0          0
    ## brighter daye llc                                        0.0          0
    ## rachel scott                                             0.0          0
    ## cav capital llc                                          0.0          0
    ## terrance evans                                           0.0          0
    ## quentin holley                                           0.0          0
    ## black women build llc                                    0.0          0
    ## tyrone smith                                             0.0          0
    ## unclaimed funds unit                                     0.0          0
    ## callis legacy renovations llc                            0.0          0
    ## brook-lyn corp llc                                       0.0          0
    ## bnote  llc                                               0.0          0
    ## ashman investments llc                                   0.0          0
    ## dietra norton                                            0.0          0
    ## lands and liens                                          0.0          0
    ## loudons place llc                                        0.0          0
    ## carl tilghman                                            0.0          0
    ## alpha zone investment llc                                0.0          0
    ## bora mpinja                                              0.0          0
    ## pbi holdings llc                                         0.0          0
    ## cynthia gross                                            0.0          0
    ## tiffany fennoy                                           0.0          0
    ## ying zhang                                               0.0          0
    ## temeshia johnson                                         0.0          0
    ## deon mcgehee                                             0.0          0
    ## rda llc                                                  0.0          0
    ## steven messmer                                           0.0          0
    ## glenn curtis rogers jr                                   0.0          0
    ## atkinsonsmith llc                                        0.0          0
    ## averil eugene rosenburgh                                 0.0          0
    ## castilian properties llc                                 0.0          0
    ## billy may                                                0.0          0
    ## cle deals                                                0.0          0
    ## assurance communications teknologies                     0.0          0
    ## alfred r daniels                                         0.0          0
    ## jjj turner investments llc                               0.0          0
    ## step  llc                                                0.0          0
    ## rtlf-nj llc                                              0.0          0
    ## taneisha pauliono                                        0.0          0
    ## kj productions                                           0.0          0
    ## c&j investments                                          0.0          0
    ## takeshia campbell                                        0.0          0
    ## almarin properties llc                                   0.0          0
    ## ijd paralegal services llc                               0.0          0
    ## sag investment trust                                     0.0          0
    ## all american tax liens llc                               0.0          0
    ## llc                                                      0.0          0
    ## nadeem butt                                              0.0          0
    ## atash properties                                         0.0          0
    ## st funding group llc                                     0.0          0
    ## stephenson development llc                               0.0          0
    ## melvin l freeman                                         0.0          0
    ## brian batson                                             0.0          0
    ## cynthia d gross                                          0.0          0
    ## ronald reedy brown                                       0.0          0
    ## harris homes llc                                         0.0          0
    ## the miller group                                         0.0          0
    ## jarrell winston                                          0.0          0
    ## sonya m bynum                                            0.0          0
    ## noteman r/e investments llc                              0.0          0
    ## lws investments llc                                      0.0          0
    ## haynes consulting                                        0.0          0
    ## heber brown iii                                          0.0          0
    ## hathaway property management llc                         0.0          0
    ## new country properties llc                               0.0          0
    ## focus forward property group llc                         0.0          0
    ## kevin jean-pierre                                        0.0          0
    ## st nicholas  llc                                         0.0          0
    ## monique r mack                                           0.0          0
    ## real estate census                                       0.0          0
    ## steadfast structural engineers                           0.0          0
    ## jamari development llc                                   0.0          0
    ## max daddy llc                                         -910.0          3
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
    ## baltrentals dot com  llc                                   2       160889
    ## maryland tax  llc                                         39       129003
    ## bts  llc                                                  11       127719
    ## palomino holdings llc                                    225        67298
    ## jcrtl llc                                                 29       105900
    ## patapsco property management llc                           6       108678
    ## ram tax lien fund lp                                      92            0
    ## mtag as custodian for atcf ii maryland                   136        36168
    ## pine valley one real estate llc                          161            0
    ## municipal lien fund llc                                  476            0
    ## atcf ii maryland llc taxserv cust                         71            0
    ## u s liens k trust                                         32        95786
    ## baltimore tax  llc                                        32        86831
    ## tloa of md llc                                           145            0
    ## pluto  llc                                               920            0
    ## newline holdings llc                                      32            0
    ## ande investments llc                                       8        87465
    ## integrity homes unlimited llc                              1        86780
    ## vasantha bpillai                                           3        84400
    ## redwood tsf llc                                          307            0
    ## ande properties llc                                       10        75862
    ## karaninc solo k trust                                      1        79014
    ## locke capital llc c/o eskin law llc                      487            0
    ## interstate holdings llc                                  549            0
    ## ashland holdings llc                                      18        70060
    ## da property llc                                            5        71205
    ## barbay llc c/o eskin law llc                             486            0
    ## wilbarger llc                                             28        61900
    ## us bank cust for tower db ix trust -                      58            0
    ## atcf ii maryland llc lumentum cust                        73            0
    ## kairos real estate llc                                    18        66019
    ## us bank cust for tower db viii trust                     110            0
    ## us bank cust for tower db vii trust                      198            0
    ## point holdings llc                                       244            0
    ## lmg llc                                                    6        60834
    ## dnh group llc                                              1        61440
    ## muir ridge properties llc                                 23        54843
    ## cust fig series holdings llc fbo sec pty                 100            0
    ## innovative property solutions llc                         12        52737
    ## u s liens llc                                            251            0
    ## md tl llc rai custodian                                   34            0
    ## north star realty management inc                           4        37789
    ## serna psp                                                 21            0
    ## investments holding inc                                    3        37433
    ## ips holdings llc                                         276            0
    ## granite  llc                                               5        43394
    ## ssc-md llc                                                56            0
    ## stages llc                                                 3        39912
    ## hobart capital llc                                       175         7940
    ## copperfield holdings llc                                  13        33763
    ## hometrust tax liens llc                                   10            0
    ## win-wal properties llc                                     5        29275
    ## dane equities  llc                                        20        24276
    ## equiant financial services as custodian                   64            0
    ## dane equities llc                                         12        21141
    ## usbank cust tower dbxi                                    31            0
    ## ghulam sarwar                                             37            0
    ## dean properties  llc                                       6        17868
    ## bay assets llc                                            55            0
    ## reo properties servicing trust                            10        13961
    ## horseshoe trust llc                                        2        17750
    ## sws holdings llc                                          14            0
    ## rtlf-md llc                                               41            0
    ## mtag cust for empire viii md portfolio                    13            0
    ## locus and grove llc                                       21            0
    ## delinquent asset mgmt & maintenance llc                   79            0
    ## us bank c/f actlien holding                               44            0
    ## east coast tax auction llc                                34            0
    ## bsd realty llc                                            76            0
    ## rp management llc                                         20            0
    ## reo property servicing trust                              11         4010
    ## serna psp k plan                                          11            0
    ## sunflower gardens llc                                     12            0
    ## karan garewal                                             22            0
    ## ultra-safe fund  llc                                      58            0
    ## boi real llc                                               9            0
    ## immocap                                                  127            0
    ## crystal homes llc                                          3            0
    ## subhash c gupta                                           38            0
    ## jmj holdings llc                                           1            0
    ## raymond cr investment corp                                 3            0
    ## gt&t properties llc                                       24            0
    ## stone town investors llc                                  21            0
    ## babu jacob                                                 4            0
    ## ashland llc                                                5            0
    ## empire viii maryland portfolio llc                        10            0
    ## david streit                                               7         7485
    ## howard fine                                                1            0
    ## mls equity llc                                            31            0
    ## ipb management group llc                                  61            0
    ## els realty inc                                            40            0
    ## naveen malhotra                                           38            0
    ## largent llc                                               17            0
    ## us bank as custodian for actlien holdi                    30            0
    ## xli baltimore fund llc                                    46            0
    ## housing our future inc                                    41            0
    ## us bank cust actlien holding                              15            0
    ## liengps  llc                                              11            0
    ## josephine alexander llc                                    6            0
    ## us liens k trust                                          21            0
    ## nexus holdings llc                                         8            0
    ## unique homes llc                                           4            0
    ## express funding llc                                        9            0
    ## oneeno llc                                                 4            0
    ## taxsale                                                    2            0
    ## chesapeake liens llc                                       8            0
    ## us bank cust for tower db x trust -                        5            0
    ## scattered sites ii llc                                     6            0
    ## pbi holdings inc                                          19            0
    ## crab properties llc                                        9            0
    ## naiman and naiman pa                                      12            0
    ## daley difranco & co llc                                   14            0
    ## wells properties llc                                       1            0
    ## plainview financial services ltd                          29            0
    ## day investment and consulting llc                         17            0
    ## liberty international investments inc                      7            0
    ## mirona investment group llc                               10            0
    ## dean investments llc                                      11            0
    ## brookins investments llc                                  37            0
    ## gnj investor marketing llc                                 3            0
    ## bbic real estate llc                                      25            0
    ## dean investments  llc                                      9            0
    ## kyle sampson                                               5            0
    ## us liens llc                                              29            0
    ## propitious properties llc                                 12            0
    ## yosemite ventures llc                                      1            0
    ## land research associates llc                              23            0
    ## samy gupta                                                20            0
    ## baltimore arts realty corporation                          1            0
    ## hampstead hills llc                                        7            0
    ## brownstone investments llc                                 1            0
    ## bmoreprop llc                                             11            0
    ## h land co llc                                              1            0
    ## adam skordinski                                            3            0
    ## carey & marciniak properties llc                           4            0
    ## watson economic development associates                     7            0
    ## tyi and lillllc                                            6            0
    ## benjamin j rubin                                           1            0
    ## monarch development llc                                    5            0
    ## wendi henry                                               12            0
    ## arkad capital                                              1            0
    ## sanabani properties llc                                    7            0
    ## gang li                                                    6            0
    ## h&h real ventures company llc                              1            0
    ## jeffrey harris                                             2            0
    ## r healthcare                                               8            0
    ## dean investments                                           7            0
    ## dainan bramble                                             2            0
    ## sandor mester                                              2            0
    ## eric harris                                               10            0
    ## chad newkirk                                               3            0
    ## shahrad shawn yazdani                                      4            0
    ## advance iii llc                                            8            0
    ## asratu assefa checol                                       1            0
    ## kevin digrazia                                             9            0
    ## louis bailey                                               2            0
    ## nanbar solo k trust                                        3            0
    ## patricia r lewis                                           9            0
    ## charter meridian                                           8            0
    ## strategic properties llc                                   1            0
    ## dawn veltman                                               5            0
    ## g imperios                                                 1            0
    ## hometrust franchise llc                                    7            0
    ## housing our future together                               15            0
    ## sweet p s joyceful assistant living llc                    3            0
    ## stellar prime investment real estate                       3            0
    ## hilton properties llc                                      3            0
    ## abode keys llc                                             1            0
    ## bwi home llc                                               7            0
    ## natalex solo k plan                                        4            0
    ## cedr llc                                                   5            0
    ## cynthia brown                                              5            0
    ## christopher perry                                          3            0
    ## feruz boboev                                               6            0
    ## ankh literacy initiative                                   5            0
    ## dorman dennis                                              1            0
    ## marissa arguijo                                            1            0
    ## bellatruth commercial properties llc                       1            0
    ## bridgette perkins                                          2            0
    ## baybrook llc                                               1            0
    ## alchymist holdings llc                                     1            0
    ## natalya antonenko                                          8            0
    ## cabata family enterprises llc                              2            0
    ## resilience real estate l c                                 7            0
    ## preeminent management group llc                            1            0
    ## key realty group- i llc                                    1            0
    ## zippy realty llc                                           7            0
    ## myhome properties llc                                      2            0
    ## frucasion fruit boutique                                   1            0
    ## east investment group llc                                  1            0
    ## homeland valley llc                                        4            0
    ## marcus franz                                               1            0
    ## mission hill md llc                                       17            0
    ## timothy belcher                                            2            0
    ## technical services llc                                     1            0
    ## charter meridian llc                                       8            0
    ## jw estates llc                                             1            0
    ## williamsville                                              2            0
    ## denise abrams                                              2            0
    ## forest burrell industries                                  1            0
    ## housing our future                                         3            0
    ## platinum solutions llc                                     2            0
    ## martavious washington                                      7            0
    ## one  eno llc                                               5            0
    ## the leverage firm                                          2            0
    ## coit real estate llc                                       2            0
    ## erudition                                                  3            0
    ## atcf ii maryland llc                                       3            0
    ## american home group llc                                    5            0
    ## global international group                                10            0
    ## hasson barnes                                              2            0
    ## erica seaman                                               5            0
    ## omin                                                       4            0
    ## taf holdings llc                                           4            0
    ## reginald best ii                                           1            0
    ## noovah llc                                                 1            0
    ## ross chamberlain                                           3            0
    ## bde homes llc                                              2            0
    ## brandon burrell                                            2            0
    ## robert braddock  sonya braddock                            1            0
    ## jmj residential holdings llc                               1            0
    ## kimberly green                                             1            0
    ## hpa holdings llc                                           1            0
    ## response ability investing llc                             2            0
    ## evans wilson                                               5            0
    ## sahar algahdari                                            1            0
    ## osage advisors llc                                         3            0
    ## departed llc                                              24            0
    ## mansur abdul-malik                                         1            0
    ## robert martinez                                            1            0
    ## twin mills investments llc                                 7            0
    ## clay wilson iv                                             1            0
    ## wj investments llc                                         2            0
    ## patricia lewis                                             4            0
    ## accent professional services llc                           6            0
    ## bay capital llc                                            2            0
    ## bright venture holdings llc                                6            0
    ## hayk hagopian                                              2            0
    ## anthony brand                                              1            0
    ## mts venture  llc                                           1            0
    ## lewis s davis                                              4            0
    ## jon magsaysay                                              1            0
    ## investments for good international                         1            0
    ## hometrust on the top llc                                   3            0
    ## harout doukmajian                                          2            0
    ## nemo llc                                                   1            0
    ## astute real estate investments llc                         4            0
    ## john evans                                                 1            0
    ## highrise real estate groups llc                            3            0
    ## amber crump                                                1            0
    ## beyond change                                              1            0
    ## asratu checol                                              3            0
    ## gonzalez videla                                            2            0
    ## joseph building restoration                                2            0
    ## reginald l ready                                           3            0
    ## terrence brown                                             1            0
    ## wsfs as custodian for rearden llc f/b/o                   19            0
    ## the curry team llc                                         1            0
    ## peggy m israel                                             2            0
    ## harvey ross                                                6            0
    ## deborah zebina                                             2            0
    ## leverage law firm                                          4            0
    ## ade estates llc                                            3            0
    ## the bean group                                             1            0
    ## glenn c rogers jr                                          2            0
    ## secured futures funding                                    9            0
    ## latia morton                                               1            0
    ## darron waller                                              2            0
    ## mikeen investments no  llc                                 1            0
    ## kenneth futrell                                            2            0
    ## michael martin                                             1            0
    ## emerson lane llc                                           1            0
    ## bwe enterprises llc                                        2            0
    ## melissa singletary                                         4            0
    ## eagle homes usa llc                                        1            0
    ## lawrence e callis jr                                       6            0
    ## diamond flip investments llc                               2            0
    ## vine and miller llc                                        1            0
    ## erickson miller                                            3            0
    ## nelson fu                                                  4            0
    ## danielle natasha green                                     1            0
    ## hometrust sanay llc                                        3            0
    ## suchgor llc                                                2            0
    ## gaberell moore                                             3            0
    ## moniladae adewoyin                                         2            0
    ## gedion teklemariam                                         4            0
    ## mellow investor inc                                        3            0
    ## cmc properties llc                                         1            0
    ## rmx                                                        3            0
    ## abrams real estate development                             3            0
    ## sharon mokhtari                                            2            0
    ## housing our future too                                     1            0
    ## tlh tax auctions llc                                       7            0
    ## crystal session                                            1            0
    ## grand development llc                                      1            0
    ## courtly group management llc                               2            0
    ## girard curry                                               2            0
    ## deandre holley                                             1            0
    ## baltax                                                     2            0
    ## princess valentine                                         1            0
    ## buddy roth llc                                             1            0
    ## jacqueline d dennis                                        1            0
    ## rognell llc                                                2            0
    ## infinite kingdom llc                                       2            0
    ## raymond c r investment corp                                1            0
    ## latina dawkins                                             1            0
    ## antwan lipscomb                                            2            0
    ## lisa tisdale                                               1            0
    ## lynn moaney                                                3            0
    ## go green incentive llc                                     1            0
    ## amber woodruff                                             1            0
    ## mac  llc                                                   9            0
    ## jamal adams                                                1            0
    ## lights                                                     4            0
    ## nile community and family services llc                     2            0
    ## cvc realty holdings llc                                    1            0
    ## true north advisory llc                                    1            0
    ## danielle alston                                            2            0
    ## premier fitness training                                   3            0
    ## michael steininger                                         3            0
    ## kalunga holdings llc                                       1            0
    ## renaud p batcho                                            1            0
    ## shanice crowder                                            3            0
    ## visionary enterprises                                      1            0
    ## melba chambers                                             1            0
    ## rmx llc                                                    2            0
    ## kendal l joyce                                             1            0
    ## ukoha ukoha                                                1            0
    ## dch realty                                                 1            0
    ## trebor investments                                         3            0
    ## gnj investor marketing                                     3            0
    ## maria bernardez / lucas polanco                            1            0
    ## gilad enterprisescom                                       2            0
    ## loch raven investments llc                                 1            0
    ## ronnie lewis                                               1            0
    ## jeron p stokes                                             1            0
    ## gulden llc                                                 2            0
    ## young law group                                            1            0
    ## acrobat llc                                                1            0
    ## dream builders asset capital llc                           2            0
    ## william hammond                                            3            0
    ## brad lindsey                                               6            0
    ## baltimore appraisal & realty company in                    1            0
    ## aerica lake                                                1            0
    ## limitless homes                                            1            0
    ## chevette wilson                                            2            0
    ## spectrum soft consultants llc                              1            0
    ## dalyn clarence allen                                       1            0
    ## peter v faris                                              1            0
    ## open ocean investments llc                                 2            0
    ## cory nelson                                                2            0
    ## brighter daye llc                                          1            0
    ## rachel scott                                               2            0
    ## cav capital llc                                            1            0
    ## terrance evans                                             1            0
    ## quentin holley                                             3            0
    ## black women build llc                                      2            0
    ## tyrone smith                                               1            0
    ## unclaimed funds unit                                       1            0
    ## callis legacy renovations llc                              2            0
    ## brook-lyn corp llc                                         1            0
    ## bnote  llc                                                 1            0
    ## ashman investments llc                                     7            0
    ## dietra norton                                              1            0
    ## lands and liens                                            2            0
    ## loudons place llc                                          1            0
    ## carl tilghman                                              1            0
    ## alpha zone investment llc                                  1            0
    ## bora mpinja                                                2            0
    ## pbi holdings llc                                           2            0
    ## cynthia gross                                              1            0
    ## tiffany fennoy                                             1            0
    ## ying zhang                                                 1            0
    ## temeshia johnson                                           1            0
    ## deon mcgehee                                               1            0
    ## rda llc                                                    1            0
    ## steven messmer                                             1            0
    ## glenn curtis rogers jr                                     1            0
    ## atkinsonsmith llc                                          2            0
    ## averil eugene rosenburgh                                   1            0
    ## castilian properties llc                                   2            0
    ## billy may                                                  1            0
    ## cle deals                                                  1            0
    ## assurance communications teknologies                       1            0
    ## alfred r daniels                                           1            0
    ## jjj turner investments llc                                 1            0
    ## step  llc                                                  3            0
    ## rtlf-nj llc                                                1            0
    ## taneisha pauliono                                          1            0
    ## kj productions                                             2            0
    ## c&j investments                                            1            0
    ## takeshia campbell                                          1            0
    ## almarin properties llc                                     1            0
    ## ijd paralegal services llc                                 1            0
    ## sag investment trust                                       1            0
    ## all american tax liens llc                                 1            0
    ## llc                                                        1            0
    ## nadeem butt                                                1            0
    ## atash properties                                           1            0
    ## st funding group llc                                       1            0
    ## stephenson development llc                                 2            0
    ## melvin l freeman                                           1            0
    ## brian batson                                               1            0
    ## cynthia d gross                                            1            0
    ## ronald reedy brown                                         2            0
    ## harris homes llc                                           1            0
    ## the miller group                                           1            0
    ## jarrell winston                                            1            0
    ## sonya m bynum                                              1            0
    ## noteman r/e investments llc                                1            0
    ## lws investments llc                                        1            0
    ## haynes consulting                                          1            0
    ## heber brown iii                                            1            0
    ## hathaway property management llc                           1            0
    ## new country properties llc                                 1            0
    ## focus forward property group llc                           1            0
    ## kevin jean-pierre                                          1            0
    ## st nicholas  llc                                           1            0
    ## monique r mack                                             1            0
    ## real estate census                                         1            0
    ## steadfast structural engineers                             1            0
    ## jamari development llc                                     1            0
    ## max daddy llc                                              2       -12000
    ##                                                     tot_amt_int  amt_own_int
    ## mdinv llc|t rocho llc|stonefield investment fund i 8.437278e+05 1.178518e+05
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l 2.584068e+05 1.599696e+04
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs    1.649074e+06 1.148159e+05
    ## maple tsf llc                                      1.336426e+05 2.279280e+04
    ## acf development llc|tark development llc|tjci deve 2.280043e+05 1.776527e+04
    ## hobart holdings llc                                1.294444e+05 1.215862e+04
    ## thornton mellon llc                                4.584717e+05 1.283276e+05
    ## md tax properties  llc                             9.222861e+04 1.414101e+04
    ## baltax  llc                                        1.029371e+05 1.818695e+04
    ## palm tsf llc                                       7.282643e+04 7.432168e+03
    ## municipal investments llc                          9.335422e+05 3.072660e+05
    ## hickory tsf llc                                    2.748010e+05 3.073391e+04
    ## ultra-safe fund llc                                6.151621e+05 1.013716e+05
    ## tlmd capital llc                                   8.422185e+04 7.356202e+03
    ## income one llc                                     4.593011e+04 6.541959e+03
    ## caret bay llc|caret bey llc|caret bay holdings llc 1.221631e+05 1.514542e+04
    ## tax properties one llc                             2.095502e+05 1.185017e+04
    ## mtsif llc                                          3.628189e+04 1.523072e+03
    ## park & menlo llc                                   2.285071e+04 5.124962e+03
    ## bts  llc c/o j scott morse                         2.229666e+04 3.531232e+03
    ## tax sale holdings llc                              1.074254e+04 3.384183e+03
    ## rite investments llc                               2.207051e+04 8.942521e+03
    ## fig as custodian for fig md llc and se             2.196138e+05 3.202703e+04
    ## us bank as cust for tower dbw vi trust             8.342401e+04 1.931771e+04
    ## midaro investments  llc                            1.973573e+05 6.091646e+04
    ## ashland new homes iv llc                           7.848963e+03 0.000000e+00
    ## labri llc                                          5.314749e+03 2.160712e+03
    ## mtag as custodian for mgd-md llc                   5.478168e+04 1.181225e+04
    ## immobilier cap ks ltd                              1.397033e+04 6.262860e+02
    ## golden ashland services llc                        3.253018e+03 6.592390e+02
    ## baltrentals dot com  llc                           3.750142e+03 0.000000e+00
    ## maryland tax  llc                                  2.017727e+04 1.079077e+03
    ## bts  llc                                           3.777191e+03 1.597660e+01
    ## palomino holdings llc                              5.725845e+04 4.844658e+03
    ## jcrtl llc                                          1.305854e+04 1.732268e+03
    ## patapsco property management llc                   1.734317e+03 1.301992e+02
    ## ram tax lien fund lp                               1.100603e+05 2.968647e+03
    ## mtag as custodian for atcf ii maryland             6.835891e+04 2.081577e+04
    ## pine valley one real estate llc                    1.040082e+05 3.484483e+03
    ## municipal lien fund llc                            1.025017e+05 2.502060e+04
    ## atcf ii maryland llc taxserv cust                  1.012761e+05 3.996692e+03
    ## u s liens k trust                                  3.696069e+03 9.322250e+02
    ## baltimore tax  llc                                 1.076872e+04 1.282984e+03
    ## tloa of md llc                                     9.566488e+04 4.813703e+03
    ## pluto  llc                                         9.385322e+04 2.450550e+04
    ## newline holdings llc                               9.178896e+04 1.610167e+03
    ## ande investments llc                               1.242022e+03 6.086303e+02
    ## integrity homes unlimited llc                      5.098719e+02 0.000000e+00
    ## vasantha bpillai                                   3.188452e+02 3.051620e+02
    ## redwood tsf llc                                    8.319098e+04 1.502233e+04
    ## ande properties llc                                5.421522e+03 8.558094e+01
    ## karaninc solo k trust                              2.741904e+02 0.000000e+00
    ## locke capital llc c/o eskin law llc                7.875797e+04 5.952318e+03
    ## interstate holdings llc                            7.841339e+04 9.240899e+03
    ## ashland holdings llc                               7.363056e+03 8.300139e+02
    ## da property llc                                    2.560489e+03 5.298862e+02
    ## barbay llc c/o eskin law llc                       7.354866e+04 5.666181e+03
    ## wilbarger llc                                      1.132025e+04 1.033787e+02
    ## us bank cust for tower db ix trust -               7.296373e+04 4.198947e+03
    ## atcf ii maryland llc lumentum cust                 6.885884e+04 1.608516e+03
    ## kairos real estate llc                             2.283490e+03 2.299360e+01
    ## us bank cust for tower db viii trust               6.399901e+04 1.152443e+04
    ## us bank cust for tower db vii trust                6.374272e+04 2.385863e+04
    ## point holdings llc                                 6.262018e+04 9.924894e+03
    ## lmg llc                                            1.409947e+03 0.000000e+00
    ## dnh group llc                                      4.649526e+02 0.000000e+00
    ## muir ridge properties llc                          6.691452e+03 2.014873e+03
    ## cust fig series holdings llc fbo sec pty           5.902470e+04 0.000000e+00
    ## innovative property solutions llc                  3.536779e+03 1.366955e+03
    ## u s liens llc                                      5.285449e+04 6.789989e+03
    ## md tl llc rai custodian                            4.994443e+04 4.929835e+03
    ## north star realty management inc                   1.213855e+04 6.737355e+02
    ## serna psp                                          4.883242e+04 1.976622e+03
    ## investments holding inc                            8.298192e+03 4.882562e+02
    ## ips holdings llc                                   4.541126e+04 4.099641e+03
    ## granite  llc                                       6.796218e+02 2.430440e+02
    ## ssc-md llc                                         4.177848e+04 6.862999e+03
    ## stages llc                                         7.690184e+01 1.261420e+01
    ## hobart capital llc                                 3.190522e+04 5.109080e+03
    ## copperfield holdings llc                           5.218252e+03 0.000000e+00
    ## hometrust tax liens llc                            3.894522e+04 0.000000e+00
    ## win-wal properties llc                             3.094783e+03 0.000000e+00
    ## dane equities  llc                                 6.460418e+03 1.616641e+03
    ## equiant financial services as custodian            2.801482e+04 4.104285e+03
    ## dane equities llc                                  4.115283e+03 4.406241e+02
    ## usbank cust tower dbxi                             2.251970e+04 4.216730e+03
    ## ghulam sarwar                                      2.150185e+04 0.000000e+00
    ## dean properties  llc                               1.887726e+03 6.867387e+02
    ## bay assets llc                                     1.894952e+04 6.187786e+03
    ## reo properties servicing trust                     4.718284e+03 3.595485e+01
    ## horseshoe trust llc                                5.719616e+02 0.000000e+00
    ## sws holdings llc                                   1.829408e+04 1.015045e+03
    ## rtlf-md llc                                        1.814273e+04 1.962950e+03
    ## mtag cust for empire viii md portfolio             1.693903e+04 6.059049e+02
    ## locus and grove llc                                1.593331e+04 8.098543e+02
    ## delinquent asset mgmt & maintenance llc            1.547393e+04 1.044497e+03
    ## us bank c/f actlien holding                        1.463986e+04 4.336631e+03
    ## east coast tax auction llc                         1.357522e+04 3.131049e+03
    ## bsd realty llc                                     1.353851e+04 2.835083e+02
    ## rp management llc                                  1.294563e+04 9.179519e+02
    ## reo property servicing trust                       8.581375e+03 2.016443e+03
    ## serna psp k plan                                   1.223964e+04 0.000000e+00
    ## sunflower gardens llc                              1.175561e+04 4.587310e+02
    ## karan garewal                                      1.166785e+04 1.793215e+03
    ## ultra-safe fund  llc                               1.166453e+04 8.045851e+02
    ## boi real llc                                       1.164525e+04 8.734518e+01
    ## immocap                                            1.101634e+04 1.910576e+02
    ## crystal homes llc                                  1.063050e+04 0.000000e+00
    ## subhash c gupta                                    1.014989e+04 1.717855e+02
    ## jmj holdings llc                                   9.873702e+03 9.873702e+03
    ## raymond cr investment corp                         9.743338e+03 0.000000e+00
    ## gt&t properties llc                                9.566875e+03 1.369591e+03
    ## stone town investors llc                           9.435862e+03 7.761710e+01
    ## babu jacob                                         9.021800e+03 0.000000e+00
    ## ashland llc                                        8.879794e+03 0.000000e+00
    ## empire viii maryland portfolio llc                 8.792957e+03 4.413753e+03
    ## david streit                                       9.630021e+02 4.976594e+01
    ## howard fine                                        7.842194e+03 0.000000e+00
    ## mls equity llc                                     7.057228e+03 1.706502e+03
    ## ipb management group llc                           6.745647e+03 9.388931e+01
    ## els realty inc                                     6.569287e+03 2.352333e+02
    ## naveen malhotra                                    6.342819e+03 4.409767e+02
    ## largent llc                                        5.720195e+03 3.218197e+02
    ## us bank as custodian for actlien holdi             5.146546e+03 1.894394e+03
    ## xli baltimore fund llc                             5.145931e+03 2.741819e+02
    ## housing our future inc                             4.920183e+03 4.159061e+02
    ## us bank cust actlien holding                       4.751067e+03 2.693971e+03
    ## liengps  llc                                       4.634829e+03 4.293441e+02
    ## josephine alexander llc                            4.103775e+03 1.323532e+03
    ## us liens k trust                                   4.093090e+03 1.284441e+03
    ## nexus holdings llc                                 3.968142e+03 6.807100e+02
    ## unique homes llc                                   3.808535e+03 4.937543e+02
    ## express funding llc                                3.634014e+03 3.870218e+02
    ## oneeno llc                                         3.575503e+03 0.000000e+00
    ## taxsale                                            3.559540e+03 0.000000e+00
    ## chesapeake liens llc                               3.514665e+03 3.281259e+02
    ## us bank cust for tower db x trust -                3.498314e+03 8.388483e+02
    ## scattered sites ii llc                             3.408488e+03 6.089168e+02
    ## pbi holdings inc                                   3.309405e+03 0.000000e+00
    ## crab properties llc                                3.234051e+03 0.000000e+00
    ## naiman and naiman pa                               3.229342e+03 3.714022e+02
    ## daley difranco & co llc                            3.180418e+03 9.290077e+02
    ## wells properties llc                               3.161269e+03 0.000000e+00
    ## plainview financial services ltd                   3.083321e+03 4.142541e+02
    ## day investment and consulting llc                  3.034901e+03 0.000000e+00
    ## liberty international investments inc              2.953277e+03 2.953277e+03
    ## mirona investment group llc                        2.946200e+03 5.189910e+02
    ## dean investments llc                               2.868254e+03 6.994658e+01
    ## brookins investments llc                           2.825385e+03 1.400058e+02
    ## gnj investor marketing llc                         2.646513e+03 0.000000e+00
    ## bbic real estate llc                               2.597926e+03 5.058827e+00
    ## dean investments  llc                              2.592554e+03 7.817099e+02
    ## kyle sampson                                       2.575001e+03 0.000000e+00
    ## us liens llc                                       2.561402e+03 5.989854e+01
    ## propitious properties llc                          2.530130e+03 6.862554e+02
    ## yosemite ventures llc                              2.511587e+03 0.000000e+00
    ## land research associates llc                       2.427369e+03 2.378670e+02
    ## samy gupta                                         2.395077e+03 4.446246e+02
    ## baltimore arts realty corporation                  2.328700e+03 0.000000e+00
    ## hampstead hills llc                                2.279612e+03 0.000000e+00
    ## brownstone investments llc                         2.179909e+03 0.000000e+00
    ## bmoreprop llc                                      2.164098e+03 5.235335e+02
    ## h land co llc                                      2.052043e+03 0.000000e+00
    ## adam skordinski                                    2.012084e+03 1.272630e+03
    ## carey & marciniak properties llc                   1.820279e+03 1.371467e+02
    ## watson economic development associates             1.814099e+03 3.543753e+01
    ## tyi and lillllc                                    1.797080e+03 8.508000e+00
    ## benjamin j rubin                                   1.765342e+03 0.000000e+00
    ## monarch development llc                            1.755445e+03 0.000000e+00
    ## wendi henry                                        1.702632e+03 0.000000e+00
    ## arkad capital                                      1.695713e+03 0.000000e+00
    ## sanabani properties llc                            1.682484e+03 2.234966e+02
    ## gang li                                            1.655714e+03 6.921500e+00
    ## h&h real ventures company llc                      1.628901e+03 0.000000e+00
    ## jeffrey harris                                     1.616187e+03 0.000000e+00
    ## r healthcare                                       1.596126e+03 5.233667e+02
    ## dean investments                                   1.514785e+03 5.282105e+01
    ## dainan bramble                                     1.481047e+03 1.385503e+03
    ## sandor mester                                      1.465885e+03 0.000000e+00
    ## eric harris                                        1.465153e+03 1.900538e+02
    ## chad newkirk                                       1.464284e+03 1.080794e+02
    ## shahrad shawn yazdani                              1.425859e+03 0.000000e+00
    ## advance iii llc                                    1.369925e+03 0.000000e+00
    ## asratu assefa checol                               1.342743e+03 0.000000e+00
    ## kevin digrazia                                     1.339639e+03 7.436607e+01
    ## louis bailey                                       1.333392e+03 8.191418e+02
    ## nanbar solo k trust                                1.323434e+03 0.000000e+00
    ## patricia r lewis                                   1.321223e+03 2.798180e+00
    ## charter meridian                                   1.316092e+03 9.337149e+01
    ## strategic properties llc                           1.312558e+03 0.000000e+00
    ## dawn veltman                                       1.306059e+03 0.000000e+00
    ## g imperios                                         1.257721e+03 0.000000e+00
    ## hometrust franchise llc                            1.220003e+03 3.907515e+01
    ## housing our future together                        1.194843e+03 3.346572e+02
    ## sweet p s joyceful assistant living llc            1.193587e+03 0.000000e+00
    ## stellar prime investment real estate               1.184812e+03 3.871075e+02
    ## hilton properties llc                              1.135026e+03 4.474520e+01
    ## abode keys llc                                     1.127159e+03 0.000000e+00
    ## bwi home llc                                       1.093623e+03 2.521450e+02
    ## natalex solo k plan                                1.087463e+03 0.000000e+00
    ## cedr llc                                           1.079046e+03 0.000000e+00
    ## cynthia brown                                      1.077835e+03 3.829547e+01
    ## christopher perry                                  1.030126e+03 0.000000e+00
    ## feruz boboev                                       1.017506e+03 0.000000e+00
    ## ankh literacy initiative                           1.011070e+03 4.577993e+02
    ## dorman dennis                                      1.003071e+03 1.003071e+03
    ## marissa arguijo                                    9.882112e+02 0.000000e+00
    ## bellatruth commercial properties llc               9.803363e+02 0.000000e+00
    ## bridgette perkins                                  9.743797e+02 0.000000e+00
    ## baybrook llc                                       9.653751e+02 9.653751e+02
    ## alchymist holdings llc                             9.597750e+02 0.000000e+00
    ## natalya antonenko                                  9.509491e+02 1.105410e+02
    ## cabata family enterprises llc                      9.458911e+02 0.000000e+00
    ## resilience real estate l c                         9.207949e+02 0.000000e+00
    ## preeminent management group llc                    9.031641e+02 0.000000e+00
    ## key realty group- i llc                            9.018875e+02 0.000000e+00
    ## zippy realty llc                                   8.977946e+02 2.832790e+02
    ## myhome properties llc                              8.787843e+02 8.787843e+02
    ## frucasion fruit boutique                           8.585885e+02 0.000000e+00
    ## east investment group llc                          8.492864e+02 0.000000e+00
    ## homeland valley llc                                8.404192e+02 5.797995e+01
    ## marcus franz                                       8.209860e+02 0.000000e+00
    ## mission hill md llc                                8.189550e+02 0.000000e+00
    ## timothy belcher                                    8.094809e+02 0.000000e+00
    ## technical services llc                             8.074357e+02 0.000000e+00
    ## charter meridian llc                               7.381648e+02 0.000000e+00
    ## jw estates llc                                     7.159822e+02 0.000000e+00
    ## williamsville                                      7.130100e+02 0.000000e+00
    ## denise abrams                                      6.346492e+02 3.011604e+02
    ## forest burrell industries                          6.130498e+02 0.000000e+00
    ## housing our future                                 5.923001e+02 4.225963e+01
    ## platinum solutions llc                             5.815638e+02 0.000000e+00
    ## martavious washington                              5.761096e+02 0.000000e+00
    ## one  eno llc                                       5.724297e+02 0.000000e+00
    ## the leverage firm                                  5.678707e+02 0.000000e+00
    ## coit real estate llc                               5.663347e+02 0.000000e+00
    ## erudition                                          5.621497e+02 0.000000e+00
    ## atcf ii maryland llc                               5.593295e+02 0.000000e+00
    ## american home group llc                            5.551563e+02 0.000000e+00
    ## global international group                         5.499118e+02 0.000000e+00
    ## hasson barnes                                      5.265727e+02 5.167565e+01
    ## erica seaman                                       5.164960e+02 3.651397e+02
    ## omin                                               5.125506e+02 0.000000e+00
    ## taf holdings llc                                   5.125204e+02 3.209045e+02
    ## reginald best ii                                   5.123866e+02 0.000000e+00
    ## noovah llc                                         5.104045e+02 0.000000e+00
    ## ross chamberlain                                   5.082253e+02 0.000000e+00
    ## bde homes llc                                      5.078784e+02 5.078784e+02
    ## brandon burrell                                    5.076476e+02 0.000000e+00
    ## robert braddock  sonya braddock                    4.392577e+02 4.392577e+02
    ## jmj residential holdings llc                       4.387496e+02 4.387496e+02
    ## kimberly green                                     4.358037e+02 4.358037e+02
    ## hpa holdings llc                                   4.260369e+02 0.000000e+00
    ## response ability investing llc                     4.231731e+02 0.000000e+00
    ## evans wilson                                       4.202904e+02 2.418571e+01
    ## sahar algahdari                                    4.164203e+02 0.000000e+00
    ## osage advisors llc                                 4.086311e+02 0.000000e+00
    ## departed llc                                       4.080730e+02 0.000000e+00
    ## mansur abdul-malik                                 3.965657e+02 0.000000e+00
    ## robert martinez                                    3.896982e+02 0.000000e+00
    ## twin mills investments llc                         3.847539e+02 0.000000e+00
    ## clay wilson iv                                     3.784132e+02 0.000000e+00
    ## wj investments llc                                 3.780027e+02 0.000000e+00
    ## patricia lewis                                     3.751695e+02 1.040320e+02
    ## accent professional services llc                   3.728272e+02 0.000000e+00
    ## bay capital llc                                    3.717541e+02 0.000000e+00
    ## bright venture holdings llc                        3.698701e+02 0.000000e+00
    ## hayk hagopian                                      3.655605e+02 2.767498e+01
    ## anthony brand                                      3.652137e+02 0.000000e+00
    ## mts venture  llc                                   3.540819e+02 0.000000e+00
    ## lewis s davis                                      3.404387e+02 2.589836e+02
    ## jon magsaysay                                      3.395929e+02 0.000000e+00
    ## investments for good international                 3.346254e+02 0.000000e+00
    ## hometrust on the top llc                           3.329441e+02 0.000000e+00
    ## harout doukmajian                                  3.284395e+02 0.000000e+00
    ## nemo llc                                           3.282705e+02 0.000000e+00
    ## astute real estate investments llc                 3.274785e+02 1.634352e+02
    ## john evans                                         3.167665e+02 3.167665e+02
    ## highrise real estate groups llc                    3.159905e+02 0.000000e+00
    ## amber crump                                        3.098366e+02 0.000000e+00
    ## beyond change                                      3.057565e+02 0.000000e+00
    ## asratu checol                                      2.999683e+02 0.000000e+00
    ## gonzalez videla                                    2.988975e+02 0.000000e+00
    ## joseph building restoration                        2.921522e+02 0.000000e+00
    ## reginald l ready                                   2.902144e+02 8.929008e+01
    ## terrence brown                                     2.857527e+02 2.857527e+02
    ## wsfs as custodian for rearden llc f/b/o            2.854675e+02 0.000000e+00
    ## the curry team llc                                 2.850877e+02 0.000000e+00
    ## peggy m israel                                     2.844412e+02 2.220072e+02
    ## harvey ross                                        2.749754e+02 0.000000e+00
    ## deborah zebina                                     2.719197e+02 0.000000e+00
    ## leverage law firm                                  2.716924e+02 0.000000e+00
    ## ade estates llc                                    2.700526e+02 2.631864e+01
    ## the bean group                                     2.619141e+02 2.619141e+02
    ## glenn c rogers jr                                  2.611584e+02 0.000000e+00
    ## secured futures funding                            2.602065e+02 8.732493e+00
    ## latia morton                                       2.590010e+02 2.590010e+02
    ## darron waller                                      2.569610e+02 0.000000e+00
    ## mikeen investments no  llc                         2.545601e+02 0.000000e+00
    ## kenneth futrell                                    2.508216e+02 0.000000e+00
    ## michael martin                                     2.449722e+02 2.449722e+02
    ## emerson lane llc                                   2.339891e+02 0.000000e+00
    ## bwe enterprises llc                                2.338220e+02 0.000000e+00
    ## melissa singletary                                 2.257954e+02 0.000000e+00
    ## eagle homes usa llc                                2.250049e+02 0.000000e+00
    ## lawrence e callis jr                               2.187894e+02 0.000000e+00
    ## diamond flip investments llc                       2.180338e+02 0.000000e+00
    ## vine and miller llc                                2.134912e+02 0.000000e+00
    ## erickson miller                                    2.112716e+02 0.000000e+00
    ## nelson fu                                          2.051909e+02 0.000000e+00
    ## danielle natasha green                             2.051709e+02 0.000000e+00
    ## hometrust sanay llc                                2.013651e+02 7.408642e+01
    ## suchgor llc                                        1.990357e+02 0.000000e+00
    ## gaberell moore                                     1.967647e+02 3.822270e+01
    ## moniladae adewoyin                                 1.959322e+02 0.000000e+00
    ## gedion teklemariam                                 1.869214e+02 0.000000e+00
    ## mellow investor inc                                1.838601e+02 0.000000e+00
    ## cmc properties llc                                 1.827815e+02 0.000000e+00
    ## rmx                                                1.815833e+02 0.000000e+00
    ## abrams real estate development                     1.800586e+02 0.000000e+00
    ## sharon mokhtari                                    1.790781e+02 0.000000e+00
    ## housing our future too                             1.779822e+02 0.000000e+00
    ## tlh tax auctions llc                               1.773919e+02 0.000000e+00
    ## crystal session                                    1.762573e+02 0.000000e+00
    ## grand development llc                              1.729380e+02 0.000000e+00
    ## courtly group management llc                       1.718561e+02 0.000000e+00
    ## girard curry                                       1.715291e+02 0.000000e+00
    ## deandre holley                                     1.705327e+02 0.000000e+00
    ## baltax                                             1.701403e+02 0.000000e+00
    ## princess valentine                                 1.602008e+02 1.602008e+02
    ## buddy roth llc                                     1.518634e+02 0.000000e+00
    ## jacqueline d dennis                                1.500380e+02 0.000000e+00
    ## rognell llc                                        1.498735e+02 0.000000e+00
    ## infinite kingdom llc                               1.440566e+02 0.000000e+00
    ## raymond c r investment corp                        1.417043e+02 0.000000e+00
    ## latina dawkins                                     1.390945e+02 0.000000e+00
    ## antwan lipscomb                                    1.352952e+02 0.000000e+00
    ## lisa tisdale                                       1.352057e+02 0.000000e+00
    ## lynn moaney                                        1.336101e+02 0.000000e+00
    ## go green incentive llc                             1.314578e+02 0.000000e+00
    ## amber woodruff                                     1.297703e+02 0.000000e+00
    ## mac  llc                                           1.281198e+02 0.000000e+00
    ## jamal adams                                        1.254600e+02 0.000000e+00
    ## lights                                             1.181200e+02 0.000000e+00
    ## nile community and family services llc             1.178387e+02 0.000000e+00
    ## cvc realty holdings llc                            1.161982e+02 0.000000e+00
    ## true north advisory llc                            1.138217e+02 0.000000e+00
    ## danielle alston                                    1.136118e+02 0.000000e+00
    ## premier fitness training                           1.109230e+02 0.000000e+00
    ## michael steininger                                 1.084817e+02 5.694539e+01
    ## kalunga holdings llc                               1.080576e+02 0.000000e+00
    ## renaud p batcho                                    1.068868e+02 1.068868e+02
    ## shanice crowder                                    1.065202e+02 0.000000e+00
    ## visionary enterprises                              1.063669e+02 0.000000e+00
    ## melba chambers                                     1.014142e+02 0.000000e+00
    ## rmx llc                                            1.000612e+02 0.000000e+00
    ## kendal l joyce                                     1.000152e+02 0.000000e+00
    ## ukoha ukoha                                        9.966933e+01 0.000000e+00
    ## dch realty                                         9.893400e+01 0.000000e+00
    ## trebor investments                                 9.877411e+01 0.000000e+00
    ## gnj investor marketing                             9.690023e+01 0.000000e+00
    ## maria bernardez / lucas polanco                    9.612022e+01 0.000000e+00
    ## gilad enterprisescom                               9.461642e+01 7.545787e+01
    ## loch raven investments llc                         9.450189e+01 0.000000e+00
    ## ronnie lewis                                       9.356863e+01 0.000000e+00
    ## jeron p stokes                                     9.159780e+01 0.000000e+00
    ## gulden llc                                         9.039233e+01 0.000000e+00
    ## young law group                                    8.951209e+01 0.000000e+00
    ## acrobat llc                                        8.943133e+01 8.943133e+01
    ## dream builders asset capital llc                   8.905328e+01 0.000000e+00
    ## william hammond                                    8.684850e+01 0.000000e+00
    ## brad lindsey                                       8.600311e+01 1.018442e+01
    ## baltimore appraisal & realty company in            8.423080e+01 0.000000e+00
    ## aerica lake                                        8.245367e+01 0.000000e+00
    ## limitless homes                                    8.126697e+01 0.000000e+00
    ## chevette wilson                                    7.856413e+01 7.493008e+01
    ## spectrum soft consultants llc                      7.818875e+01 0.000000e+00
    ## dalyn clarence allen                               6.935796e+01 0.000000e+00
    ## peter v faris                                      6.840939e+01 0.000000e+00
    ## open ocean investments llc                         6.366432e+01 0.000000e+00
    ## cory nelson                                        6.324434e+01 0.000000e+00
    ## brighter daye llc                                  6.317472e+01 6.317472e+01
    ## rachel scott                                       6.138737e+01 3.791209e+01
    ## cav capital llc                                    6.119100e+01 0.000000e+00
    ## terrance evans                                     5.987406e+01 5.987406e+01
    ## quentin holley                                     5.856573e+01 0.000000e+00
    ## black women build llc                              5.835375e+01 0.000000e+00
    ## tyrone smith                                       5.835280e+01 0.000000e+00
    ## unclaimed funds unit                               5.708905e+01 0.000000e+00
    ## callis legacy renovations llc                      5.658960e+01 0.000000e+00
    ## brook-lyn corp llc                                 5.551325e+01 0.000000e+00
    ## bnote  llc                                         5.199463e+01 0.000000e+00
    ## ashman investments llc                             5.110177e+01 0.000000e+00
    ## dietra norton                                      4.980030e+01 0.000000e+00
    ## lands and liens                                    4.798269e+01 0.000000e+00
    ## loudons place llc                                  4.770667e+01 0.000000e+00
    ## carl tilghman                                      4.617177e+01 0.000000e+00
    ## alpha zone investment llc                          4.612223e+01 4.612223e+01
    ## bora mpinja                                        4.445841e+01 0.000000e+00
    ## pbi holdings llc                                   4.408509e+01 0.000000e+00
    ## cynthia gross                                      4.293276e+01 0.000000e+00
    ## tiffany fennoy                                     4.098316e+01 0.000000e+00
    ## ying zhang                                         4.089166e+01 0.000000e+00
    ## temeshia johnson                                   3.897075e+01 0.000000e+00
    ## deon mcgehee                                       3.889013e+01 3.889013e+01
    ## rda llc                                            3.855148e+01 0.000000e+00
    ## steven messmer                                     3.588445e+01 0.000000e+00
    ## glenn curtis rogers jr                             3.579105e+01 0.000000e+00
    ## atkinsonsmith llc                                  3.559416e+01 0.000000e+00
    ## averil eugene rosenburgh                           3.550150e+01 0.000000e+00
    ## castilian properties llc                           3.472078e+01 0.000000e+00
    ## billy may                                          3.455673e+01 0.000000e+00
    ## cle deals                                          3.096087e+01 0.000000e+00
    ## assurance communications teknologies               2.864970e+01 0.000000e+00
    ## alfred r daniels                                   2.848785e+01 0.000000e+00
    ## jjj turner investments llc                         2.801450e+01 0.000000e+00
    ## step  llc                                          2.598268e+01 0.000000e+00
    ## rtlf-nj llc                                        2.586155e+01 0.000000e+00
    ## taneisha pauliono                                  2.565130e+01 0.000000e+00
    ## kj productions                                     2.288507e+01 0.000000e+00
    ## c&j investments                                    2.236683e+01 0.000000e+00
    ## takeshia campbell                                  2.195268e+01 2.195268e+01
    ## almarin properties llc                             2.126328e+01 0.000000e+00
    ## ijd paralegal services llc                         2.123212e+01 0.000000e+00
    ## sag investment trust                               2.096185e+01 0.000000e+00
    ## all american tax liens llc                         2.008195e+01 0.000000e+00
    ## llc                                                1.808982e+01 1.808982e+01
    ## nadeem butt                                        1.681408e+01 0.000000e+00
    ## atash properties                                   1.591630e+01 1.591630e+01
    ## st funding group llc                               1.436820e+01 0.000000e+00
    ## stephenson development llc                         1.368929e+01 0.000000e+00
    ## melvin l freeman                                   1.325541e+01 0.000000e+00
    ## brian batson                                       1.169196e+01 0.000000e+00
    ## cynthia d gross                                    1.166400e+01 0.000000e+00
    ## ronald reedy brown                                 9.455150e+00 0.000000e+00
    ## harris homes llc                                   8.579560e+00 0.000000e+00
    ## the miller group                                   8.487285e+00 0.000000e+00
    ## jarrell winston                                    7.779840e+00 0.000000e+00
    ## sonya m bynum                                      7.071870e+00 0.000000e+00
    ## noteman r/e investments llc                        6.360433e+00 6.360433e+00
    ## lws investments llc                                6.085320e+00 0.000000e+00
    ## haynes consulting                                  5.758380e+00 0.000000e+00
    ## heber brown iii                                    4.673200e+00 0.000000e+00
    ## hathaway property management llc                   3.840800e+00 3.840800e+00
    ## new country properties llc                         3.489600e+00 0.000000e+00
    ## focus forward property group llc                   3.312450e+00 0.000000e+00
    ## kevin jean-pierre                                  3.241305e+00 0.000000e+00
    ## st nicholas  llc                                   2.650470e+00 0.000000e+00
    ## monique r mack                                     2.577100e+00 0.000000e+00
    ## real estate census                                 2.009880e+00 0.000000e+00
    ## steadfast structural engineers                     1.956430e+00 0.000000e+00
    ## jamari development llc                             6.537300e-01 0.000000e+00
    ## max daddy llc                                      1.092299e+03 0.000000e+00
    ##                                                    amt_non_own_int n_own_int
    ## mdinv llc|t rocho llc|stonefield investment fund i    7.258760e+05       528
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l    2.424099e+05        98
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs       1.534258e+06       247
    ## maple tsf llc                                         1.108498e+05       155
    ## acf development llc|tark development llc|tjci deve    2.102390e+05        99
    ## hobart holdings llc                                   1.172857e+05        57
    ## thornton mellon llc                                   3.301441e+05       572
    ## md tax properties  llc                                7.808760e+04        67
    ## baltax  llc                                           8.475014e+04        95
    ## palm tsf llc                                          6.539426e+04        51
    ## municipal investments llc                             6.262762e+05      1945
    ## hickory tsf llc                                       2.440671e+05       102
    ## ultra-safe fund llc                                   5.137905e+05       439
    ## tlmd capital llc                                      7.686565e+04        74
    ## income one llc                                        3.938815e+04        24
    ## caret bay llc|caret bey llc|caret bay holdings llc    1.070177e+05        82
    ## tax properties one llc                                1.977000e+05        26
    ## mtsif llc                                             3.475882e+04        16
    ## park & menlo llc                                      1.772574e+04         5
    ## bts  llc c/o j scott morse                            1.876543e+04        22
    ## tax sale holdings llc                                 7.358361e+03         6
    ## rite investments llc                                  1.312799e+04         5
    ## fig as custodian for fig md llc and se                1.875867e+05        93
    ## us bank as cust for tower dbw vi trust                6.410629e+04        61
    ## midaro investments  llc                               1.364408e+05       359
    ## ashland new homes iv llc                              7.848963e+03         0
    ## labri llc                                             3.154038e+03         5
    ## mtag as custodian for mgd-md llc                      4.296943e+04        55
    ## immobilier cap ks ltd                                 1.334404e+04        15
    ## golden ashland services llc                           2.593779e+03         3
    ## baltrentals dot com  llc                              3.750142e+03         0
    ## maryland tax  llc                                     1.909820e+04         6
    ## bts  llc                                              3.761215e+03         2
    ## palomino holdings llc                                 5.241380e+04        28
    ## jcrtl llc                                             1.132627e+04         5
    ## patapsco property management llc                      1.604118e+03         2
    ## ram tax lien fund lp                                  1.070917e+05        13
    ## mtag as custodian for atcf ii maryland                4.754314e+04        58
    ## pine valley one real estate llc                       1.005237e+05        16
    ## municipal lien fund llc                               7.748111e+04       156
    ## atcf ii maryland llc taxserv cust                     9.727936e+04        14
    ## u s liens k trust                                     2.763844e+03         5
    ## baltimore tax  llc                                    9.485739e+03         8
    ## tloa of md llc                                        9.085118e+04        14
    ## pluto  llc                                            6.934771e+04       307
    ## newline holdings llc                                  9.017880e+04         7
    ## ande investments llc                                  6.333914e+02         4
    ## integrity homes unlimited llc                         5.098719e+02         0
    ## vasantha bpillai                                      1.368325e+01         2
    ## redwood tsf llc                                       6.816866e+04        69
    ## ande properties llc                                   5.335941e+03         1
    ## karaninc solo k trust                                 2.741904e+02         0
    ## locke capital llc c/o eskin law llc                   7.280565e+04        64
    ## interstate holdings llc                               6.917249e+04        74
    ## ashland holdings llc                                  6.533042e+03         6
    ## da property llc                                       2.030603e+03         1
    ## barbay llc c/o eskin law llc                          6.788248e+04        58
    ## wilbarger llc                                         1.121687e+04         2
    ## us bank cust for tower db ix trust -                  6.876478e+04        12
    ## atcf ii maryland llc lumentum cust                    6.725032e+04         6
    ## kairos real estate llc                                2.260496e+03         1
    ## us bank cust for tower db viii trust                  5.247457e+04        39
    ## us bank cust for tower db vii trust                   3.988409e+04       103
    ## point holdings llc                                    5.269528e+04        47
    ## lmg llc                                               1.409947e+03         0
    ## dnh group llc                                         4.649526e+02         0
    ## muir ridge properties llc                             4.676578e+03         7
    ## cust fig series holdings llc fbo sec pty              5.902470e+04         0
    ## innovative property solutions llc                     2.169824e+03         6
    ## u s liens llc                                         4.606450e+04        45
    ## md tl llc rai custodian                               4.501460e+04        10
    ## north star realty management inc                      1.146482e+04         1
    ## serna psp                                             4.685579e+04         2
    ## investments holding inc                               7.809936e+03         2
    ## ips holdings llc                                      4.131161e+04        47
    ## granite  llc                                          4.365778e+02         3
    ## ssc-md llc                                            3.491548e+04        17
    ## stages llc                                            6.428764e+01         1
    ## hobart capital llc                                    2.679614e+04        31
    ## copperfield holdings llc                              5.218252e+03         0
    ## hometrust tax liens llc                               3.894522e+04         0
    ## win-wal properties llc                                3.094783e+03         0
    ## dane equities  llc                                    4.843777e+03         5
    ## equiant financial services as custodian               2.391053e+04        16
    ## dane equities llc                                     3.674659e+03         3
    ## usbank cust tower dbxi                                1.830297e+04        13
    ## ghulam sarwar                                         2.150185e+04         0
    ## dean properties  llc                                  1.200988e+03         1
    ## bay assets llc                                        1.276174e+04        24
    ## reo properties servicing trust                        4.682330e+03         1
    ## horseshoe trust llc                                   5.719616e+02         0
    ## sws holdings llc                                      1.727904e+04         5
    ## rtlf-md llc                                           1.617978e+04         7
    ## mtag cust for empire viii md portfolio                1.633312e+04         1
    ## locus and grove llc                                   1.512345e+04         7
    ## delinquent asset mgmt & maintenance llc               1.442943e+04         9
    ## us bank c/f actlien holding                           1.030323e+04        25
    ## east coast tax auction llc                            1.044417e+04        11
    ## bsd realty llc                                        1.325500e+04         4
    ## rp management llc                                     1.202768e+04         4
    ## reo property servicing trust                          6.564932e+03         1
    ## serna psp k plan                                      1.223964e+04         0
    ## sunflower gardens llc                                 1.129688e+04         1
    ## karan garewal                                         9.874634e+03         6
    ## ultra-safe fund  llc                                  1.085994e+04         7
    ## boi real llc                                          1.155791e+04         1
    ## immocap                                               1.082528e+04         4
    ## crystal homes llc                                     1.063050e+04         0
    ## subhash c gupta                                       9.978106e+03         3
    ## jmj holdings llc                                      0.000000e+00         1
    ## raymond cr investment corp                            9.743338e+03         0
    ## gt&t properties llc                                   8.197283e+03         5
    ## stone town investors llc                              9.358245e+03         1
    ## babu jacob                                            9.021800e+03         0
    ## ashland llc                                           8.879794e+03         0
    ## empire viii maryland portfolio llc                    4.379204e+03         4
    ## david streit                                          9.132361e+02         1
    ## howard fine                                           7.842194e+03         0
    ## mls equity llc                                        5.350725e+03         4
    ## ipb management group llc                              6.651758e+03         1
    ## els realty inc                                        6.334053e+03         2
    ## naveen malhotra                                       5.901842e+03         2
    ## largent llc                                           5.398376e+03         2
    ## us bank as custodian for actlien holdi                3.252152e+03        15
    ## xli baltimore fund llc                                4.871749e+03         5
    ## housing our future inc                                4.504276e+03         8
    ## us bank cust actlien holding                          2.057096e+03         9
    ## liengps  llc                                          4.205485e+03         3
    ## josephine alexander llc                               2.780242e+03         2
    ## us liens k trust                                      2.808649e+03         6
    ## nexus holdings llc                                    3.287432e+03         2
    ## unique homes llc                                      3.314781e+03         2
    ## express funding llc                                   3.246992e+03         2
    ## oneeno llc                                            3.575503e+03         0
    ## taxsale                                               3.559540e+03         0
    ## chesapeake liens llc                                  3.186539e+03         2
    ## us bank cust for tower db x trust -                   2.659466e+03         1
    ## scattered sites ii llc                                2.799571e+03         2
    ## pbi holdings inc                                      3.309405e+03         0
    ## crab properties llc                                   3.234051e+03         0
    ## naiman and naiman pa                                  2.857940e+03         2
    ## daley difranco & co llc                               2.251411e+03         1
    ## wells properties llc                                  3.161269e+03         0
    ## plainview financial services ltd                      2.669067e+03         4
    ## day investment and consulting llc                     3.034901e+03         0
    ## liberty international investments inc                 0.000000e+00         7
    ## mirona investment group llc                           2.427209e+03         3
    ## dean investments llc                                  2.798307e+03         3
    ## brookins investments llc                              2.685379e+03         2
    ## gnj investor marketing llc                            2.646513e+03         0
    ## bbic real estate llc                                  2.592867e+03         1
    ## dean investments  llc                                 1.810845e+03         2
    ## kyle sampson                                          2.575001e+03         0
    ## us liens llc                                          2.501503e+03         2
    ## propitious properties llc                             1.843875e+03         1
    ## yosemite ventures llc                                 2.511587e+03         0
    ## land research associates llc                          2.189502e+03         2
    ## samy gupta                                            1.950452e+03         6
    ## baltimore arts realty corporation                     2.328700e+03         0
    ## hampstead hills llc                                   2.279612e+03         0
    ## brownstone investments llc                            2.179909e+03         0
    ## bmoreprop llc                                         1.640565e+03         3
    ## h land co llc                                         2.052043e+03         0
    ## adam skordinski                                       7.394539e+02         2
    ## carey & marciniak properties llc                      1.683132e+03         2
    ## watson economic development associates                1.778662e+03         1
    ## tyi and lillllc                                       1.788572e+03         1
    ## benjamin j rubin                                      1.765342e+03         0
    ## monarch development llc                               1.755445e+03         0
    ## wendi henry                                           1.702632e+03         0
    ## arkad capital                                         1.695713e+03         0
    ## sanabani properties llc                               1.458987e+03         1
    ## gang li                                               1.648792e+03         1
    ## h&h real ventures company llc                         1.628901e+03         0
    ## jeffrey harris                                        1.616187e+03         0
    ## r healthcare                                          1.072759e+03         1
    ## dean investments                                      1.461964e+03         1
    ## dainan bramble                                        9.554451e+01         1
    ## sandor mester                                         1.465885e+03         0
    ## eric harris                                           1.275099e+03         2
    ## chad newkirk                                          1.356205e+03         1
    ## shahrad shawn yazdani                                 1.425859e+03         0
    ## advance iii llc                                       1.369925e+03         0
    ## asratu assefa checol                                  1.342743e+03         0
    ## kevin digrazia                                        1.265273e+03         1
    ## louis bailey                                          5.142504e+02         1
    ## nanbar solo k trust                                   1.323434e+03         0
    ## patricia r lewis                                      1.318425e+03         1
    ## charter meridian                                      1.222720e+03         1
    ## strategic properties llc                              1.312558e+03         0
    ## dawn veltman                                          1.306059e+03         0
    ## g imperios                                            1.257721e+03         0
    ## hometrust franchise llc                               1.180928e+03         1
    ## housing our future together                           8.601857e+02         5
    ## sweet p s joyceful assistant living llc               1.193587e+03         0
    ## stellar prime investment real estate                  7.977041e+02         1
    ## hilton properties llc                                 1.090280e+03         1
    ## abode keys llc                                        1.127159e+03         0
    ## bwi home llc                                          8.414780e+02         1
    ## natalex solo k plan                                   1.087463e+03         0
    ## cedr llc                                              1.079046e+03         0
    ## cynthia brown                                         1.039539e+03         1
    ## christopher perry                                     1.030126e+03         0
    ## feruz boboev                                          1.017506e+03         0
    ## ankh literacy initiative                              5.532705e+02         2
    ## dorman dennis                                         0.000000e+00         1
    ## marissa arguijo                                       9.882112e+02         0
    ## bellatruth commercial properties llc                  9.803363e+02         0
    ## bridgette perkins                                     9.743797e+02         0
    ## baybrook llc                                          0.000000e+00         1
    ## alchymist holdings llc                                9.597750e+02         0
    ## natalya antonenko                                     8.404081e+02         2
    ## cabata family enterprises llc                         9.458911e+02         0
    ## resilience real estate l c                            9.207949e+02         0
    ## preeminent management group llc                       9.031641e+02         0
    ## key realty group- i llc                               9.018875e+02         0
    ## zippy realty llc                                      6.145156e+02         1
    ## myhome properties llc                                 0.000000e+00         2
    ## frucasion fruit boutique                              8.585885e+02         0
    ## east investment group llc                             8.492864e+02         0
    ## homeland valley llc                                   7.824392e+02         1
    ## marcus franz                                          8.209860e+02         0
    ## mission hill md llc                                   8.189550e+02         0
    ## timothy belcher                                       8.094809e+02         0
    ## technical services llc                                8.074357e+02         0
    ## charter meridian llc                                  7.381648e+02         0
    ## jw estates llc                                        7.159822e+02         0
    ## williamsville                                         7.130100e+02         0
    ## denise abrams                                         3.334888e+02         1
    ## forest burrell industries                             6.130498e+02         0
    ## housing our future                                    5.500404e+02         1
    ## platinum solutions llc                                5.815638e+02         0
    ## martavious washington                                 5.761096e+02         0
    ## one  eno llc                                          5.724297e+02         0
    ## the leverage firm                                     5.678707e+02         0
    ## coit real estate llc                                  5.663347e+02         0
    ## erudition                                             5.621497e+02         0
    ## atcf ii maryland llc                                  5.593295e+02         0
    ## american home group llc                               5.551563e+02         0
    ## global international group                            5.499118e+02         0
    ## hasson barnes                                         4.748971e+02         1
    ## erica seaman                                          1.513564e+02         2
    ## omin                                                  5.125506e+02         0
    ## taf holdings llc                                      1.916159e+02         2
    ## reginald best ii                                      5.123866e+02         0
    ## noovah llc                                            5.104045e+02         0
    ## ross chamberlain                                      5.082253e+02         0
    ## bde homes llc                                         0.000000e+00         2
    ## brandon burrell                                       5.076476e+02         0
    ## robert braddock  sonya braddock                       0.000000e+00         1
    ## jmj residential holdings llc                          0.000000e+00         1
    ## kimberly green                                        0.000000e+00         1
    ## hpa holdings llc                                      4.260369e+02         0
    ## response ability investing llc                        4.231731e+02         0
    ## evans wilson                                          3.961047e+02         1
    ## sahar algahdari                                       4.164203e+02         0
    ## osage advisors llc                                    4.086311e+02         0
    ## departed llc                                          4.080730e+02         0
    ## mansur abdul-malik                                    3.965657e+02         0
    ## robert martinez                                       3.896982e+02         0
    ## twin mills investments llc                            3.847539e+02         0
    ## clay wilson iv                                        3.784132e+02         0
    ## wj investments llc                                    3.780027e+02         0
    ## patricia lewis                                        2.711375e+02         1
    ## accent professional services llc                      3.728272e+02         0
    ## bay capital llc                                       3.717541e+02         0
    ## bright venture holdings llc                           3.698701e+02         0
    ## hayk hagopian                                         3.378855e+02         1
    ## anthony brand                                         3.652137e+02         0
    ## mts venture  llc                                      3.540819e+02         0
    ## lewis s davis                                         8.145517e+01         3
    ## jon magsaysay                                         3.395929e+02         0
    ## investments for good international                    3.346254e+02         0
    ## hometrust on the top llc                              3.329441e+02         0
    ## harout doukmajian                                     3.284395e+02         0
    ## nemo llc                                              3.282705e+02         0
    ## astute real estate investments llc                    1.640432e+02         1
    ## john evans                                            0.000000e+00         1
    ## highrise real estate groups llc                       3.159905e+02         0
    ## amber crump                                           3.098366e+02         0
    ## beyond change                                         3.057565e+02         0
    ## asratu checol                                         2.999683e+02         0
    ## gonzalez videla                                       2.988975e+02         0
    ## joseph building restoration                           2.921522e+02         0
    ## reginald l ready                                      2.009244e+02         2
    ## terrence brown                                        0.000000e+00         1
    ## wsfs as custodian for rearden llc f/b/o               2.854675e+02         0
    ## the curry team llc                                    2.850877e+02         0
    ## peggy m israel                                        6.243393e+01         1
    ## harvey ross                                           2.749754e+02         0
    ## deborah zebina                                        2.719197e+02         0
    ## leverage law firm                                     2.716924e+02         0
    ## ade estates llc                                       2.437340e+02         1
    ## the bean group                                        0.000000e+00         1
    ## glenn c rogers jr                                     2.611584e+02         0
    ## secured futures funding                               2.514740e+02         1
    ## latia morton                                          0.000000e+00         1
    ## darron waller                                         2.569610e+02         0
    ## mikeen investments no  llc                            2.545601e+02         0
    ## kenneth futrell                                       2.508216e+02         0
    ## michael martin                                        0.000000e+00         1
    ## emerson lane llc                                      2.339891e+02         0
    ## bwe enterprises llc                                   2.338220e+02         0
    ## melissa singletary                                    2.257954e+02         0
    ## eagle homes usa llc                                   2.250049e+02         0
    ## lawrence e callis jr                                  2.187894e+02         0
    ## diamond flip investments llc                          2.180338e+02         0
    ## vine and miller llc                                   2.134912e+02         0
    ## erickson miller                                       2.112716e+02         0
    ## nelson fu                                             2.051909e+02         0
    ## danielle natasha green                                2.051709e+02         0
    ## hometrust sanay llc                                   1.272787e+02         1
    ## suchgor llc                                           1.990357e+02         0
    ## gaberell moore                                        1.585420e+02         1
    ## moniladae adewoyin                                    1.959322e+02         0
    ## gedion teklemariam                                    1.869214e+02         0
    ## mellow investor inc                                   1.838601e+02         0
    ## cmc properties llc                                    1.827815e+02         0
    ## rmx                                                   1.815833e+02         0
    ## abrams real estate development                        1.800586e+02         0
    ## sharon mokhtari                                       1.790781e+02         0
    ## housing our future too                                1.779822e+02         0
    ## tlh tax auctions llc                                  1.773919e+02         0
    ## crystal session                                       1.762573e+02         0
    ## grand development llc                                 1.729380e+02         0
    ## courtly group management llc                          1.718561e+02         0
    ## girard curry                                          1.715291e+02         0
    ## deandre holley                                        1.705327e+02         0
    ## baltax                                                1.701403e+02         0
    ## princess valentine                                    0.000000e+00         1
    ## buddy roth llc                                        1.518634e+02         0
    ## jacqueline d dennis                                   1.500380e+02         0
    ## rognell llc                                           1.498735e+02         0
    ## infinite kingdom llc                                  1.440566e+02         0
    ## raymond c r investment corp                           1.417043e+02         0
    ## latina dawkins                                        1.390945e+02         0
    ## antwan lipscomb                                       1.352952e+02         0
    ## lisa tisdale                                          1.352057e+02         0
    ## lynn moaney                                           1.336101e+02         0
    ## go green incentive llc                                1.314578e+02         0
    ## amber woodruff                                        1.297703e+02         0
    ## mac  llc                                              1.281198e+02         0
    ## jamal adams                                           1.254600e+02         0
    ## lights                                                1.181200e+02         0
    ## nile community and family services llc                1.178387e+02         0
    ## cvc realty holdings llc                               1.161982e+02         0
    ## true north advisory llc                               1.138217e+02         0
    ## danielle alston                                       1.136118e+02         0
    ## premier fitness training                              1.109230e+02         0
    ## michael steininger                                    5.153628e+01         1
    ## kalunga holdings llc                                  1.080576e+02         0
    ## renaud p batcho                                       0.000000e+00         1
    ## shanice crowder                                       1.065202e+02         0
    ## visionary enterprises                                 1.063669e+02         0
    ## melba chambers                                        1.014142e+02         0
    ## rmx llc                                               1.000612e+02         0
    ## kendal l joyce                                        1.000152e+02         0
    ## ukoha ukoha                                           9.966933e+01         0
    ## dch realty                                            9.893400e+01         0
    ## trebor investments                                    9.877411e+01         0
    ## gnj investor marketing                                9.690023e+01         0
    ## maria bernardez / lucas polanco                       9.612022e+01         0
    ## gilad enterprisescom                                  1.915855e+01         1
    ## loch raven investments llc                            9.450189e+01         0
    ## ronnie lewis                                          9.356863e+01         0
    ## jeron p stokes                                        9.159780e+01         0
    ## gulden llc                                            9.039233e+01         0
    ## young law group                                       8.951209e+01         0
    ## acrobat llc                                           0.000000e+00         1
    ## dream builders asset capital llc                      8.905328e+01         0
    ## william hammond                                       8.684850e+01         0
    ## brad lindsey                                          7.581869e+01         1
    ## baltimore appraisal & realty company in               8.423080e+01         0
    ## aerica lake                                           8.245367e+01         0
    ## limitless homes                                       8.126697e+01         0
    ## chevette wilson                                       3.634050e+00         1
    ## spectrum soft consultants llc                         7.818875e+01         0
    ## dalyn clarence allen                                  6.935796e+01         0
    ## peter v faris                                         6.840939e+01         0
    ## open ocean investments llc                            6.366432e+01         0
    ## cory nelson                                           6.324434e+01         0
    ## brighter daye llc                                     0.000000e+00         1
    ## rachel scott                                          2.347528e+01         1
    ## cav capital llc                                       6.119100e+01         0
    ## terrance evans                                        0.000000e+00         1
    ## quentin holley                                        5.856573e+01         0
    ## black women build llc                                 5.835375e+01         0
    ## tyrone smith                                          5.835280e+01         0
    ## unclaimed funds unit                                  5.708905e+01         0
    ## callis legacy renovations llc                         5.658960e+01         0
    ## brook-lyn corp llc                                    5.551325e+01         0
    ## bnote  llc                                            5.199463e+01         0
    ## ashman investments llc                                5.110177e+01         0
    ## dietra norton                                         4.980030e+01         0
    ## lands and liens                                       4.798269e+01         0
    ## loudons place llc                                     4.770667e+01         0
    ## carl tilghman                                         4.617177e+01         0
    ## alpha zone investment llc                             0.000000e+00         1
    ## bora mpinja                                           4.445841e+01         0
    ## pbi holdings llc                                      4.408509e+01         0
    ## cynthia gross                                         4.293276e+01         0
    ## tiffany fennoy                                        4.098316e+01         0
    ## ying zhang                                            4.089166e+01         0
    ## temeshia johnson                                      3.897075e+01         0
    ## deon mcgehee                                          0.000000e+00         1
    ## rda llc                                               3.855148e+01         0
    ## steven messmer                                        3.588445e+01         0
    ## glenn curtis rogers jr                                3.579105e+01         0
    ## atkinsonsmith llc                                     3.559416e+01         0
    ## averil eugene rosenburgh                              3.550150e+01         0
    ## castilian properties llc                              3.472078e+01         0
    ## billy may                                             3.455673e+01         0
    ## cle deals                                             3.096087e+01         0
    ## assurance communications teknologies                  2.864970e+01         0
    ## alfred r daniels                                      2.848785e+01         0
    ## jjj turner investments llc                            2.801450e+01         0
    ## step  llc                                             2.598268e+01         0
    ## rtlf-nj llc                                           2.586155e+01         0
    ## taneisha pauliono                                     2.565130e+01         0
    ## kj productions                                        2.288507e+01         0
    ## c&j investments                                       2.236683e+01         0
    ## takeshia campbell                                     0.000000e+00         1
    ## almarin properties llc                                2.126328e+01         0
    ## ijd paralegal services llc                            2.123212e+01         0
    ## sag investment trust                                  2.096185e+01         0
    ## all american tax liens llc                            2.008195e+01         0
    ## llc                                                   0.000000e+00         1
    ## nadeem butt                                           1.681408e+01         0
    ## atash properties                                      0.000000e+00         1
    ## st funding group llc                                  1.436820e+01         0
    ## stephenson development llc                            1.368929e+01         0
    ## melvin l freeman                                      1.325541e+01         0
    ## brian batson                                          1.169196e+01         0
    ## cynthia d gross                                       1.166400e+01         0
    ## ronald reedy brown                                    9.455150e+00         0
    ## harris homes llc                                      8.579560e+00         0
    ## the miller group                                      8.487285e+00         0
    ## jarrell winston                                       7.779840e+00         0
    ## sonya m bynum                                         7.071870e+00         0
    ## noteman r/e investments llc                           0.000000e+00         1
    ## lws investments llc                                   6.085320e+00         0
    ## haynes consulting                                     5.758380e+00         0
    ## heber brown iii                                       4.673200e+00         0
    ## hathaway property management llc                      0.000000e+00         1
    ## new country properties llc                            3.489600e+00         0
    ## focus forward property group llc                      3.312450e+00         0
    ## kevin jean-pierre                                     3.241305e+00         0
    ## st nicholas  llc                                      2.650470e+00         0
    ## monique r mack                                        2.577100e+00         0
    ## real estate census                                    2.009880e+00         0
    ## steadfast structural engineers                        1.956430e+00         0
    ## jamari development llc                                6.537300e-01         0
    ## max daddy llc                                         1.092299e+03         0
    ##                                                    n_non_own_int owner_int_perc
    ## mdinv llc|t rocho llc|stonefield investment fund i          2201    0.139679881
    ## valens llc|arx i llc|totum ltd|velox ltd|tempest l          1020    0.061906091
    ## fna dz llc|fna maryland llc|fna dz llc fbo wsfs             1123    0.069624433
    ## maple tsf llc                                                645    0.170550451
    ## acf development llc|tark development llc|tjci deve           838    0.077916387
    ## hobart holdings llc                                          407    0.093929311
    ## thornton mellon llc                                          951    0.279902886
    ## md tax properties  llc                                       322    0.153325650
    ## baltax  llc                                                  411    0.176680239
    ## palm tsf llc                                                 390    0.102053162
    ## municipal investments llc                                   2453    0.329139937
    ## hickory tsf llc                                              667    0.111840586
    ## ultra-safe fund llc                                         1467    0.164788417
    ## tlmd capital llc                                             507    0.087343157
    ## income one llc                                                55    0.142432894
    ## caret bay llc|caret bey llc|caret bay holdings llc           611    0.123977046
    ## tax properties one llc                                       213    0.056550525
    ## mtsif llc                                                    217    0.041978846
    ## park & menlo llc                                              23    0.224280254
    ## bts  llc c/o j scott morse                                    70    0.158374938
    ## tax sale holdings llc                                         13    0.315026227
    ## rite investments llc                                           5    0.405179691
    ## fig as custodian for fig md llc and se                       318    0.145833428
    ## us bank as cust for tower dbw vi trust                        97    0.231560616
    ## midaro investments  llc                                      623    0.308660837
    ## ashland new homes iv llc                                       6    0.000000000
    ## labri llc                                                     10    0.406550028
    ## mtag as custodian for mgd-md llc                              94    0.215624065
    ## immobilier cap ks ltd                                        119    0.044829734
    ## golden ashland services llc                                    5    0.202654613
    ## baltrentals dot com  llc                                       2    0.000000000
    ## maryland tax  llc                                             33    0.053479845
    ## bts  llc                                                       9    0.004229757
    ## palomino holdings llc                                        197    0.084610354
    ## jcrtl llc                                                     24    0.132653995
    ## patapsco property management llc                               4    0.075072279
    ## ram tax lien fund lp                                          79    0.026972912
    ## mtag as custodian for atcf ii maryland                        78    0.304507084
    ## pine valley one real estate llc                              145    0.033502013
    ## municipal lien fund llc                                      320    0.244099321
    ## atcf ii maryland llc taxserv cust                             57    0.039463347
    ## u s liens k trust                                             27    0.252220678
    ## baltimore tax  llc                                            24    0.119139824
    ## tloa of md llc                                               131    0.050318392
    ## pluto  llc                                                   613    0.261104577
    ## newline holdings llc                                          25    0.017542050
    ## ande investments llc                                           4    0.490031952
    ## integrity homes unlimited llc                                  1    0.000000000
    ## vasantha bpillai                                               1    0.957084979
    ## redwood tsf llc                                              238    0.180576363
    ## ande properties llc                                            9    0.015785408
    ## karaninc solo k trust                                          1    0.000000000
    ## locke capital llc c/o eskin law llc                          423    0.075577338
    ## interstate holdings llc                                      475    0.117848476
    ## ashland holdings llc                                          12    0.112726823
    ## da property llc                                                4    0.206947231
    ## barbay llc c/o eskin law llc                                 428    0.077039896
    ## wilbarger llc                                                 26    0.009132199
    ## us bank cust for tower db ix trust -                          46    0.057548412
    ## atcf ii maryland llc lumentum cust                            67    0.023359613
    ## kairos real estate llc                                        17    0.010069499
    ## us bank cust for tower db viii trust                          71    0.180072071
    ## us bank cust for tower db vii trust                           95    0.374295787
    ## point holdings llc                                           197    0.158493544
    ## lmg llc                                                        6    0.000000000
    ## dnh group llc                                                  1    0.000000000
    ## muir ridge properties llc                                     16    0.301111542
    ## cust fig series holdings llc fbo sec pty                     100    0.000000000
    ## innovative property solutions llc                              6    0.386497070
    ## u s liens llc                                                206    0.128465703
    ## md tl llc rai custodian                                       24    0.098706406
    ## north star realty management inc                               3    0.055503778
    ## serna psp                                                     19    0.040477661
    ## investments holding inc                                        1    0.058838861
    ## ips holdings llc                                             229    0.090278075
    ## granite  llc                                                   2    0.357616527
    ## ssc-md llc                                                    39    0.164271132
    ## stages llc                                                     2    0.164029838
    ## hobart capital llc                                           144    0.160133046
    ## copperfield holdings llc                                      13    0.000000000
    ## hometrust tax liens llc                                       10    0.000000000
    ## win-wal properties llc                                         5    0.000000000
    ## dane equities  llc                                            15    0.250237762
    ## equiant financial services as custodian                       48    0.146504091
    ## dane equities llc                                              9    0.107070185
    ## usbank cust tower dbxi                                        18    0.187246296
    ## ghulam sarwar                                                 37    0.000000000
    ## dean properties  llc                                           5    0.363791420
    ## bay assets llc                                                31    0.326540499
    ## reo properties servicing trust                                 9    0.007620323
    ## horseshoe trust llc                                            2    0.000000000
    ## sws holdings llc                                               9    0.055484892
    ## rtlf-md llc                                                   34    0.108194866
    ## mtag cust for empire viii md portfolio                        12    0.035769761
    ## locus and grove llc                                           14    0.050827765
    ## delinquent asset mgmt & maintenance llc                       70    0.067500429
    ## us bank c/f actlien holding                                   19    0.296220782
    ## east coast tax auction llc                                    23    0.230644433
    ## bsd realty llc                                                72    0.020940884
    ## rp management llc                                             16    0.070908221
    ## reo property servicing trust                                  10    0.234979041
    ## serna psp k plan                                              11    0.000000000
    ## sunflower gardens llc                                         11    0.039022301
    ## karan garewal                                                 16    0.153688580
    ## ultra-safe fund  llc                                          51    0.068977074
    ## boi real llc                                                   8    0.007500496
    ## immocap                                                      123    0.017343108
    ## crystal homes llc                                              3    0.000000000
    ## subhash c gupta                                               35    0.016924863
    ## jmj holdings llc                                               0    1.000000000
    ## raymond cr investment corp                                     3    0.000000000
    ## gt&t properties llc                                           19    0.143159748
    ## stone town investors llc                                      20    0.008225756
    ## babu jacob                                                     4    0.000000000
    ## ashland llc                                                    5    0.000000000
    ## empire viii maryland portfolio llc                             6    0.501964610
    ## david streit                                                   6    0.051677916
    ## howard fine                                                    1    0.000000000
    ## mls equity llc                                                27    0.241809163
    ## ipb management group llc                                      60    0.013918503
    ## els realty inc                                                38    0.035808041
    ## naveen malhotra                                               36    0.069523776
    ## largent llc                                                   15    0.056260261
    ## us bank as custodian for actlien holdi                        15    0.368090362
    ## xli baltimore fund llc                                        41    0.053281305
    ## housing our future inc                                        33    0.084530631
    ## us bank cust actlien holding                                   6    0.567024359
    ## liengps  llc                                                   8    0.092634291
    ## josephine alexander llc                                        4    0.322515888
    ## us liens k trust                                              15    0.313807182
    ## nexus holdings llc                                             6    0.171543789
    ## unique homes llc                                               2    0.129644132
    ## express funding llc                                            7    0.106499815
    ## oneeno llc                                                     4    0.000000000
    ## taxsale                                                        2    0.000000000
    ## chesapeake liens llc                                           6    0.093359070
    ## us bank cust for tower db x trust -                            4    0.239786465
    ## scattered sites ii llc                                         4    0.178647166
    ## pbi holdings inc                                              19    0.000000000
    ## crab properties llc                                            9    0.000000000
    ## naiman and naiman pa                                          10    0.115008634
    ## daley difranco & co llc                                       13    0.292102357
    ## wells properties llc                                           1    0.000000000
    ## plainview financial services ltd                              25    0.134353212
    ## day investment and consulting llc                             17    0.000000000
    ## liberty international investments inc                          0    1.000000000
    ## mirona investment group llc                                    7    0.176156053
    ## dean investments llc                                           8    0.024386470
    ## brookins investments llc                                      35    0.049552824
    ## gnj investor marketing llc                                     3    0.000000000
    ## bbic real estate llc                                          24    0.001947256
    ## dean investments  llc                                          7    0.301521110
    ## kyle sampson                                                   5    0.000000000
    ## us liens llc                                                  27    0.023385061
    ## propitious properties llc                                     11    0.271233249
    ## yosemite ventures llc                                          1    0.000000000
    ## land research associates llc                                  21    0.097993739
    ## samy gupta                                                    14    0.185641080
    ## baltimore arts realty corporation                              1    0.000000000
    ## hampstead hills llc                                            7    0.000000000
    ## brownstone investments llc                                     1    0.000000000
    ## bmoreprop llc                                                  8    0.241917601
    ## h land co llc                                                  1    0.000000000
    ## adam skordinski                                                1    0.632493557
    ## carey & marciniak properties llc                               2    0.075343772
    ## watson economic development associates                         6    0.019534507
    ## tyi and lillllc                                                5    0.004734347
    ## benjamin j rubin                                               1    0.000000000
    ## monarch development llc                                        5    0.000000000
    ## wendi henry                                                   12    0.000000000
    ## arkad capital                                                  1    0.000000000
    ## sanabani properties llc                                        6    0.132837300
    ## gang li                                                        5    0.004180373
    ## h&h real ventures company llc                                  1    0.000000000
    ## jeffrey harris                                                 2    0.000000000
    ## r healthcare                                                   7    0.327898127
    ## dean investments                                               6    0.034870328
    ## dainan bramble                                                 1    0.935488541
    ## sandor mester                                                  2    0.000000000
    ## eric harris                                                    8    0.129716004
    ## chad newkirk                                                   2    0.073810359
    ## shahrad shawn yazdani                                          4    0.000000000
    ## advance iii llc                                                8    0.000000000
    ## asratu assefa checol                                           1    0.000000000
    ## kevin digrazia                                                 8    0.055512027
    ## louis bailey                                                   1    0.614329222
    ## nanbar solo k trust                                            3    0.000000000
    ## patricia r lewis                                               8    0.002117871
    ## charter meridian                                               7    0.070946026
    ## strategic properties llc                                       1    0.000000000
    ## dawn veltman                                                   5    0.000000000
    ## g imperios                                                     1    0.000000000
    ## hometrust franchise llc                                        6    0.032028726
    ## housing our future together                                   10    0.280084711
    ## sweet p s joyceful assistant living llc                        3    0.000000000
    ## stellar prime investment real estate                           2    0.326724941
    ## hilton properties llc                                          2    0.039422191
    ## abode keys llc                                                 1    0.000000000
    ## bwi home llc                                                   6    0.230559317
    ## natalex solo k plan                                            4    0.000000000
    ## cedr llc                                                       5    0.000000000
    ## cynthia brown                                                  4    0.035530000
    ## christopher perry                                              3    0.000000000
    ## feruz boboev                                                   6    0.000000000
    ## ankh literacy initiative                                       3    0.452787059
    ## dorman dennis                                                  0    1.000000000
    ## marissa arguijo                                                1    0.000000000
    ## bellatruth commercial properties llc                           1    0.000000000
    ## bridgette perkins                                              2    0.000000000
    ## baybrook llc                                                   0    1.000000000
    ## alchymist holdings llc                                         1    0.000000000
    ## natalya antonenko                                              6    0.116242768
    ## cabata family enterprises llc                                  2    0.000000000
    ## resilience real estate l c                                     7    0.000000000
    ## preeminent management group llc                                1    0.000000000
    ## key realty group- i llc                                        1    0.000000000
    ## zippy realty llc                                               6    0.315527668
    ## myhome properties llc                                          0    1.000000000
    ## frucasion fruit boutique                                       1    0.000000000
    ## east investment group llc                                      1    0.000000000
    ## homeland valley llc                                            3    0.068989323
    ## marcus franz                                                   1    0.000000000
    ## mission hill md llc                                           17    0.000000000
    ## timothy belcher                                                2    0.000000000
    ## technical services llc                                         1    0.000000000
    ## charter meridian llc                                           8    0.000000000
    ## jw estates llc                                                 1    0.000000000
    ## williamsville                                                  2    0.000000000
    ## denise abrams                                                  1    0.474530473
    ## forest burrell industries                                      1    0.000000000
    ## housing our future                                             2    0.071348340
    ## platinum solutions llc                                         2    0.000000000
    ## martavious washington                                          7    0.000000000
    ## one  eno llc                                                   5    0.000000000
    ## the leverage firm                                              2    0.000000000
    ## coit real estate llc                                           2    0.000000000
    ## erudition                                                      3    0.000000000
    ## atcf ii maryland llc                                           3    0.000000000
    ## american home group llc                                        5    0.000000000
    ## global international group                                    10    0.000000000
    ## hasson barnes                                                  1    0.098135836
    ## erica seaman                                                   3    0.706955437
    ## omin                                                           4    0.000000000
    ## taf holdings llc                                               2    0.626130157
    ## reginald best ii                                               1    0.000000000
    ## noovah llc                                                     1    0.000000000
    ## ross chamberlain                                               3    0.000000000
    ## bde homes llc                                                  0    1.000000000
    ## brandon burrell                                                2    0.000000000
    ## robert braddock  sonya braddock                                0    1.000000000
    ## jmj residential holdings llc                                   0    1.000000000
    ## kimberly green                                                 0    1.000000000
    ## hpa holdings llc                                               1    0.000000000
    ## response ability investing llc                                 2    0.000000000
    ## evans wilson                                                   4    0.057545229
    ## sahar algahdari                                                1    0.000000000
    ## osage advisors llc                                             3    0.000000000
    ## departed llc                                                  24    0.000000000
    ## mansur abdul-malik                                             1    0.000000000
    ## robert martinez                                                1    0.000000000
    ## twin mills investments llc                                     7    0.000000000
    ## clay wilson iv                                                 1    0.000000000
    ## wj investments llc                                             2    0.000000000
    ## patricia lewis                                                 3    0.277293326
    ## accent professional services llc                               6    0.000000000
    ## bay capital llc                                                2    0.000000000
    ## bright venture holdings llc                                    6    0.000000000
    ## hayk hagopian                                                  1    0.075705617
    ## anthony brand                                                  1    0.000000000
    ## mts venture  llc                                               1    0.000000000
    ## lewis s davis                                                  1    0.760734707
    ## jon magsaysay                                                  1    0.000000000
    ## investments for good international                             1    0.000000000
    ## hometrust on the top llc                                       3    0.000000000
    ## harout doukmajian                                              2    0.000000000
    ## nemo llc                                                       1    0.000000000
    ## astute real estate investments llc                             3    0.499071740
    ## john evans                                                     0    1.000000000
    ## highrise real estate groups llc                                3    0.000000000
    ## amber crump                                                    1    0.000000000
    ## beyond change                                                  1    0.000000000
    ## asratu checol                                                  3    0.000000000
    ## gonzalez videla                                                2    0.000000000
    ## joseph building restoration                                    2    0.000000000
    ## reginald l ready                                               1    0.307669322
    ## terrence brown                                                 0    1.000000000
    ## wsfs as custodian for rearden llc f/b/o                       19    0.000000000
    ## the curry team llc                                             1    0.000000000
    ## peggy m israel                                                 1    0.780503188
    ## harvey ross                                                    6    0.000000000
    ## deborah zebina                                                 2    0.000000000
    ## leverage law firm                                              4    0.000000000
    ## ade estates llc                                                2    0.097457444
    ## the bean group                                                 0    1.000000000
    ## glenn c rogers jr                                              2    0.000000000
    ## secured futures funding                                        8    0.033559862
    ## latia morton                                                   0    1.000000000
    ## darron waller                                                  2    0.000000000
    ## mikeen investments no  llc                                     1    0.000000000
    ## kenneth futrell                                                2    0.000000000
    ## michael martin                                                 0    1.000000000
    ## emerson lane llc                                               1    0.000000000
    ## bwe enterprises llc                                            2    0.000000000
    ## melissa singletary                                             4    0.000000000
    ## eagle homes usa llc                                            1    0.000000000
    ## lawrence e callis jr                                           6    0.000000000
    ## diamond flip investments llc                                   2    0.000000000
    ## vine and miller llc                                            1    0.000000000
    ## erickson miller                                                3    0.000000000
    ## nelson fu                                                      4    0.000000000
    ## danielle natasha green                                         1    0.000000000
    ## hometrust sanay llc                                            2    0.367920838
    ## suchgor llc                                                    2    0.000000000
    ## gaberell moore                                                 2    0.194255900
    ## moniladae adewoyin                                             2    0.000000000
    ## gedion teklemariam                                             4    0.000000000
    ## mellow investor inc                                            3    0.000000000
    ## cmc properties llc                                             1    0.000000000
    ## rmx                                                            3    0.000000000
    ## abrams real estate development                                 3    0.000000000
    ## sharon mokhtari                                                2    0.000000000
    ## housing our future too                                         1    0.000000000
    ## tlh tax auctions llc                                           7    0.000000000
    ## crystal session                                                1    0.000000000
    ## grand development llc                                          1    0.000000000
    ## courtly group management llc                                   2    0.000000000
    ## girard curry                                                   2    0.000000000
    ## deandre holley                                                 1    0.000000000
    ## baltax                                                         2    0.000000000
    ## princess valentine                                             0    1.000000000
    ## buddy roth llc                                                 1    0.000000000
    ## jacqueline d dennis                                            1    0.000000000
    ## rognell llc                                                    2    0.000000000
    ## infinite kingdom llc                                           2    0.000000000
    ## raymond c r investment corp                                    1    0.000000000
    ## latina dawkins                                                 1    0.000000000
    ## antwan lipscomb                                                2    0.000000000
    ## lisa tisdale                                                   1    0.000000000
    ## lynn moaney                                                    3    0.000000000
    ## go green incentive llc                                         1    0.000000000
    ## amber woodruff                                                 1    0.000000000
    ## mac  llc                                                       9    0.000000000
    ## jamal adams                                                    1    0.000000000
    ## lights                                                         4    0.000000000
    ## nile community and family services llc                         2    0.000000000
    ## cvc realty holdings llc                                        1    0.000000000
    ## true north advisory llc                                        1    0.000000000
    ## danielle alston                                                2    0.000000000
    ## premier fitness training                                       3    0.000000000
    ## michael steininger                                             2    0.524931001
    ## kalunga holdings llc                                           1    0.000000000
    ## renaud p batcho                                                0    1.000000000
    ## shanice crowder                                                3    0.000000000
    ## visionary enterprises                                          1    0.000000000
    ## melba chambers                                                 1    0.000000000
    ## rmx llc                                                        2    0.000000000
    ## kendal l joyce                                                 1    0.000000000
    ## ukoha ukoha                                                    1    0.000000000
    ## dch realty                                                     1    0.000000000
    ## trebor investments                                             3    0.000000000
    ## gnj investor marketing                                         3    0.000000000
    ## maria bernardez / lucas polanco                                1    0.000000000
    ## gilad enterprisescom                                           1    0.797513469
    ## loch raven investments llc                                     1    0.000000000
    ## ronnie lewis                                                   1    0.000000000
    ## jeron p stokes                                                 1    0.000000000
    ## gulden llc                                                     2    0.000000000
    ## young law group                                                1    0.000000000
    ## acrobat llc                                                    0    1.000000000
    ## dream builders asset capital llc                               2    0.000000000
    ## william hammond                                                3    0.000000000
    ## brad lindsey                                                   5    0.118419172
    ## baltimore appraisal & realty company in                        1    0.000000000
    ## aerica lake                                                    1    0.000000000
    ## limitless homes                                                1    0.000000000
    ## chevette wilson                                                1    0.953744158
    ## spectrum soft consultants llc                                  1    0.000000000
    ## dalyn clarence allen                                           1    0.000000000
    ## peter v faris                                                  1    0.000000000
    ## open ocean investments llc                                     2    0.000000000
    ## cory nelson                                                    2    0.000000000
    ## brighter daye llc                                              0    1.000000000
    ## rachel scott                                                   1    0.617587766
    ## cav capital llc                                                1    0.000000000
    ## terrance evans                                                 0    1.000000000
    ## quentin holley                                                 3    0.000000000
    ## black women build llc                                          2    0.000000000
    ## tyrone smith                                                   1    0.000000000
    ## unclaimed funds unit                                           1    0.000000000
    ## callis legacy renovations llc                                  2    0.000000000
    ## brook-lyn corp llc                                             1    0.000000000
    ## bnote  llc                                                     1    0.000000000
    ## ashman investments llc                                         7    0.000000000
    ## dietra norton                                                  1    0.000000000
    ## lands and liens                                                2    0.000000000
    ## loudons place llc                                              1    0.000000000
    ## carl tilghman                                                  1    0.000000000
    ## alpha zone investment llc                                      0    1.000000000
    ## bora mpinja                                                    2    0.000000000
    ## pbi holdings llc                                               2    0.000000000
    ## cynthia gross                                                  1    0.000000000
    ## tiffany fennoy                                                 1    0.000000000
    ## ying zhang                                                     1    0.000000000
    ## temeshia johnson                                               1    0.000000000
    ## deon mcgehee                                                   0    1.000000000
    ## rda llc                                                        1    0.000000000
    ## steven messmer                                                 1    0.000000000
    ## glenn curtis rogers jr                                         1    0.000000000
    ## atkinsonsmith llc                                              2    0.000000000
    ## averil eugene rosenburgh                                       1    0.000000000
    ## castilian properties llc                                       2    0.000000000
    ## billy may                                                      1    0.000000000
    ## cle deals                                                      1    0.000000000
    ## assurance communications teknologies                           1    0.000000000
    ## alfred r daniels                                               1    0.000000000
    ## jjj turner investments llc                                     1    0.000000000
    ## step  llc                                                      3    0.000000000
    ## rtlf-nj llc                                                    1    0.000000000
    ## taneisha pauliono                                              1    0.000000000
    ## kj productions                                                 2    0.000000000
    ## c&j investments                                                1    0.000000000
    ## takeshia campbell                                              0    1.000000000
    ## almarin properties llc                                         1    0.000000000
    ## ijd paralegal services llc                                     1    0.000000000
    ## sag investment trust                                           1    0.000000000
    ## all american tax liens llc                                     1    0.000000000
    ## llc                                                            0    1.000000000
    ## nadeem butt                                                    1    0.000000000
    ## atash properties                                               0    1.000000000
    ## st funding group llc                                           1    0.000000000
    ## stephenson development llc                                     2    0.000000000
    ## melvin l freeman                                               1    0.000000000
    ## brian batson                                                   1    0.000000000
    ## cynthia d gross                                                1    0.000000000
    ## ronald reedy brown                                             2    0.000000000
    ## harris homes llc                                               1    0.000000000
    ## the miller group                                               1    0.000000000
    ## jarrell winston                                                1    0.000000000
    ## sonya m bynum                                                  1    0.000000000
    ## noteman r/e investments llc                                    0    1.000000000
    ## lws investments llc                                            1    0.000000000
    ## haynes consulting                                              1    0.000000000
    ## heber brown iii                                                1    0.000000000
    ## hathaway property management llc                               0    1.000000000
    ## new country properties llc                                     1    0.000000000
    ## focus forward property group llc                               1    0.000000000
    ## kevin jean-pierre                                              1    0.000000000
    ## st nicholas  llc                                               1    0.000000000
    ## monique r mack                                                 1    0.000000000
    ## real estate census                                             1    0.000000000
    ## steadfast structural engineers                                 1    0.000000000
    ## jamari development llc                                         1    0.000000000
    ## max daddy llc                                                  2    0.000000000

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
