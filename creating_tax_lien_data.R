###############
###Tax liens###
###############

##This is the official sinlge-file used to create the tax lien information in the write-up


### LIBRARY ###

library(sf)
library(tidyverse)
library(pdftools)
library(data.table)
library(viridis)
library(lubridate)
library(stringdist)
library(postmastr)
source("~/Desktop/banner_projects/real_estate/helper_funs.R")
sf_use_s2(FALSE)

### READ IN THE DATASETS ###

### Load in the SDAT data and turn it into cleaned property information

layout<-read_csv("~/Desktop/banner_projects/real_estate/layout.csv") %>% 
  mutate(last = str_split(POSITIONS, "-") %>% 
           lapply(function(x)
             return(
               x[length(x)]
             )
           ) %>% unlist %>% as.numeric,
         first = str_split(POSITIONS, "-") %>% 
           lapply(function(x)
             return(
               x[1]
             )
           ) %>% unlist %>% as.numeric
         
  ) %>% tail(nrow(.)-1) %>% 
  mutate(SIZE = replace_na(SIZE,0),
         last = replace_na(last,0)) %>% 
  add_column(check = cumsum(.$SIZE)) %>% filter(first!=0, last!=0)

stem <- "~/Desktop/banner_projects/real_estate/ban-real/"
list_of_counties <- list.files(stem) %>% str_remove_all(".zip") %>% unique
list_of_files<- vector(mode = "list", length = length(list_of_counties))

for(i in 1:length(list_of_counties)){
  root <- list.files(str_c(stem, list_of_counties[i]))
  root<-root[which(str_detect(root, ".txt"))]
  
  list_of_files[[i]]<-read_fwf(str_c(stem,list_of_counties[i],"/", root), 
                               fwf_positions(start = c(layout$first), end = c(layout$last), col_names =layout$DATA )) %>% 
    select(
      county_code = "County Code", district_ward = "District/Ward...2", map = Map,section = "Section", block = "Block", lot = "Lot", parcel="Parcel",
      owner_occ_code = "Owner Occupancy Code", owner = "Owner's Name", owner_2 = "Owner's Name 2nd Line",
      owner_name_key = "Name Key", addr = "PO Box/Street Address", addr_2 = "Address 2nd Line", number = "Number", 
      number_suff = "Number Suffix", direction = "Direction", addr_name = "Name", name_type = "Type", city = "City...25", state ="State Code",
      zip =  "Zip Code...26", legal_desc_1 = "Legal Description Line 1", legal_desc_2 = "Legal Description Line 2",
      legal_desc_3 = "Legal Description Line 3", condo_num = `Condominium Unit No.`, 
      
      account_num =  "Account Number...3",
      
      exempt_class = "Exempt Class", 
      land_use =  "Land Use Code", public_use_code = `BPRUC (Public Use Code)`, utilities_water =  `Utilities - Water`, 
      utilities_sewer = `Utilities - Sewer`, 
      
      grantor_1 = "Grantor Name...79", sales_type_1 = "How Conveyed Ind....86", 
      sales_date_1 ="Transfer Date (MMDDCCYY)...88", sales_price_1 = "Consideration...89",total_partial_1 = "Total/Partial Ind....87",
      mortgage_1 = "Mortgage...91",market_improve_1="Mkt Improvement Value...94", 
      
      grantor_2 ="Grantor Name...98", sales_type_2 = "How Conveyed Ind....105", 
      sales_date_2 ="Transfer Date (MMDDCCYY)...107", sales_price_2= "Consideration...108",total_partial_2 = "Total/Partial Ind....106",
      mortgage_2 = "Mortgage...110",market_improve_2 = "Mkt Improvement Value...114", market_land_2 = "Mkt Land Value...113",
      
      grantor_3 ="Grantor Name...118", sales_type_3 = "How Conveyed Ind....125", 
      sales_date_3 ="Transfer Date (MMDDCCYY)...127", sales_price_3= "Consideration...128",total_partial_3 = "Total/Partial Ind....126",
      mortgage_23 = "Mortgage...130",market_improve_3 = "Mkt Improvement Value...134", market_land_3 = "Mkt Land Value...133",
      
      county_exempt = "County Exempt Class",state_exempt="State Exempt Class", state_assess_exempt = "State Exempt Assessment", 
      municipal_exempt ="Municipal Exempt Class", municipal_exempt_assess = "Municipal Exempt Assessment",
      
      land_value = "Land Value...151", improvements_value = "Improvements Value...152", pref_land_value = "Preferential Land Value...153",
      date_assessed = "Date Assessed (MMCCYY)...155", date_inspected = "Date Inspected (MMCCYY)...156", tot_assess = "Total Assessment...158",
      
      land_value_2 = "Land Value...161", improvements_value_2 = "Improvements Value...162", pref_land_value_2 = "Preferential Land Value...163",
      date_assessed_2 = "Date Assessed (MMCCYY)...166", date_inspected_2 = "Date Inspected (MMCCYY)...165", tot_assess_2 = "Total Assessment...169",
      
      land_value_3 = "Land Value...174", improvements_value_3 = "Improvements Value...175", pref_land_value_3 = "Preferential Land Value...176",
      date_assessed_3 = "Date Assessed (MMCCYY)...166", date_inspected_3 = "Date Inspected (MMCCYY)...165", tot_assess_3 = "Total Assessment...178",
      
      year_build ="Year Built (CCYY)" , num_stories = "Number of Stories", structure_area = "Structure Area (Sq.Ft.)", land_area = "Land Area",
      land_units = "Land Unit of Measure",
      
      homestead_qual_code = "Homestead Qualification Code", homstead_qual_date = "Homestead Qualification Date"
      
    )
}

D_list <- do.call("rbind", list_of_files)
##write out an intermediate file
D_list %>% fwrite("~/Desktop/banner_projects/real_estate/property_transfer_list.csv")

#read the data back in. don't technically do the code above.

D_list_dt<-fread("~/Desktop/banner_projects/real_estate/property_transfer_list.csv") %>% 
  mutate(district_ward = district_ward %>% as.character(),
         id = 1:nrow(.))

##read in the property shapefiles for the city from the city's website. 

balt_prop_shp<-read_sf("~/Desktop/banner_projects/real_estate/Real_Property_Information/Real_Property_Information.shp")
balt_prop_shp_dt<-balt_prop_shp %>% data.table

balt_prop_shp_dt<-balt_prop_shp_dt %>% add_column(id_city = 1:nrow(.))%>%
  mutate(ward_2 = WARD, lot_2=LOT, block_2=BLOCK, WARD = as.numeric(WARD) %>%  as.character())

### This is the data that came from the city on the property liens since 2016
D_liened <- read_csv("~/Desktop/banner_projects/tax_lien/liened_houses.csv")

### We take this file and send it into Geocodio. This isn't stricly necessary because we use the shapefiles from the city to give the home locations, but i did it
### anyways 
D_liened_properties_bmore <-read_csv("~/Desktop/banner_projects/real_estate/to_geocode_geocodio_941c27214136de1ff6926593c0199ef1a8a63d58.csv") %>% 
  mutate(sale_date = mdy(`SALE-DATE`), property_address = `PROPERTY-ADDRESS`, 
         owner_name = `OWNER-NAME`, owner_addr = `OWNER-ADDRESS`, total_liens = `TOTAL-LIENS`,amt_bid=`AMT-BID`,
         bidder_type = `BIDDER-TYPE`, bidder_name = `BIDDER-NAME`, bidder_addr = `BIDDER-ADDR`,
         bidder_city=`BIDDER-CITY`, bidder_state = `BIDDER-STATE`, redemption = REDEMPTION %>% tolower,
         redem_date = ymd(`REDEM-DATE`), dd_date = ymd(`DD-DATE`), forcl_name = `FORECL-NAME`,
         forcl_desc = `FORECL-DESC`, forcl_date = `FORECL-DATE`) %>% 
  select(property_address,sale_date, owner_name, owner_addr, total_liens,amt_bid,bidder_type, bidder_name,bidder_addr, bidder_city,bidder_state,
         redemption, redem_date, dd_date, forcl_name, forcl_desc,forcl_date,latitude = Latitude, longitude = Longitude,  number= Number, 
         accuracy = `Accuracy Score`,
         street= Street,unit_type = "Unit Type", unit_number = "Unit Number", city = City) %>% 
  mutate(bidder_name = bidder_name %>% str_remove_all(",") %>% str_remove_all("[0-9]") %>%  tolower) %>% add_column(id = 1:nrow(.)) %>% 
  as.data.table() %>% mutate(addr = str_c(number," ", street) %>% toupper)

#write this out to a file that goes into the Rmd
write_csv(D_liened_properties_bmore, "~/Desktop/banner_projects/real_estate/liened_properties_cleaned.csv")

### MATCHING ###

#filter the SDAT data to baltimore and join the SDAT records with the city's property shapefiles. This match is almost perfect, but just to make sure that it is 
#I include some checking code here.

j_city_state_data<-D_list_dt[city=="BALTIMORE"][ balt_prop_shp_dt, on = .(lot=LOT, block=BLOCK,district_ward=WARD)]


id_matched_city<-j_city_state_data %>% filter(is.na(county_code)==FALSE) %>% pull(id_city)
id_matched_sdat<-j_city_state_data %>% filter(is.na(county_code)==FALSE) %>% pull(id)

city_missing<-balt_prop_shp_dt %>% filter(id_city%in%id_matched_city==FALSE)
sdat_missing<-D_list_dt%>% filter(county_code==3) %>% filter(id%in%id_matched_sdat==FALSE)

last_matched<-sdat_missing %>% left_join(city_missing, by = c("lot"="LOT", "block"="BLOCK")) %>% filter(is.na(county_code)==FALSE)
id_keep<-last_matched %>% group_by(id) %>% summarise(n = n())  %>% filter(n==1) %>% pull(id)
id_city_keep<-last_matched %>% group_by(id_city) %>% summarise(n = n())  %>% filter(n==1) %>% pull(id_city)
last_matched_f<-last_matched %>% filter(id%in%id_keep, id_city%in%id_city_keep)

j_city_state_data_f<-rbind(last_matched_f %>% select(-WARD,-ward_2, -lot_2, -block_2), 
                           j_city_state_data %>% filter(is.na(county_code)==FALSE) %>%  select(-ward_2, -lot_2, -block_2)) 

#now standardize the address for the sdat + bmore shape data

j_city_state_data_f_2<-j_city_state_data_f %>%as.data.frame %>%  filter(is.na(FULLADDR)==FALSE) %>%
  mutate(FULLADDR = case_when(is.na(UNIT_NUM)==FALSE~str_c(FULLADDR, " UNIT: ",UNIT_NUM),
                              is.na(UNIT_NUM)~FULLADDR
  )
  ) %>%  pm_identify(var = "FULLADDR")

D_postmast_addr_sdat<-j_city_state_data_f_2%>% pm_prep(var = "FULLADDR", type = "street") 

D_postmast_addr_sdat_2_unit<-D_postmast_addr_sdat %>% 
  pm_house_parse() %>% pm_unit_parse() %>% select(pm.uid, pm.unit)

dirsDict <- pm_dictionary(type = "directional", locale = "us")

D_postmast_addr_sdat_2_f<-D_postmast_addr_sdat %>% 
  pm_house_parse() %>% 
  pm_unit_parse() %>% 
  pm_streetDir_parse(dictionary = dirsDict) %>%
  pm_streetSuf_parse()%>%
  filter(is.na(pm.house)==FALSE,is.na(pm.address)==FALSE)

D_postmast_addr_sdat_3<-D_postmast_addr_sdat_2_f %>% left_join(D_postmast_addr_sdat_2_unit)%>%
  mutate(pm.house = pm.house %>% str_remove(., "^0+") %>% str_remove(., ">"))%>% 
  mutate(pm.house=str_split(pm.house,"-") %>% lapply(function(x)return(x[1])) %>% unlist,
         pm.preDir=replace_na(pm.preDir,""),
         pm.streetSuf=replace_na(pm.streetSuf,""),
         pm.sufDir=replace_na(pm.sufDir,""),
         pm.unit=replace_na(pm.unit,""),
         pm.addr.full = str_c(pm.house, " ",pm.preDir, " ",pm.address," ", pm.streetSuf, " ", pm.sufDir," ",pm.unit) %>% trimws %>% 
           str_replace("  "," ") %>% str_replace("  "," ") %>% tolower) %>% select(pm.uid,pm.addr.full)

j_city_state_data_f_3<-j_city_state_data_f_2 %>% left_join(D_postmast_addr_sdat_3, by = "pm.uid") %>% select(-pm.id,-pm.uid, -pm.type)%>% 
  filter(is.na(FULLADDR)==FALSE)  %>% as.data.frame%>% mutate(pm.addr.full.sdat = pm.addr.full)

# j_city_state_data %>% st_as_sf %>%  ggplot()+geom_sf()

j_city_state_data_sf<-j_city_state_data_f_3 %>% st_as_sf 

st_write(j_city_state_data_sf, "~/Desktop/banner_projects/real_estate/fixed_parcel_data.shp", binary = T, delete_dsn = T)

write_csv(tibble(names(j_city_state_data_sf)),"~/Desktop/banner_projects/real_estate/fixed_parcel_names.csv" )

### next we use postmaster to standardize the addressess between sdat data and the property liens data to be able to match liens with their property addresses
##this can be used to link the property shapefiles with the liened properties by connecting the SDAT components from fixed_parcel_data with the SDAT components 
##from matched_liens_sdat_2.csv

#this file is written out earlier but i'm reading it back in because that line is commeted out 
D_liened_properties_bmore<-read_csv("~/Desktop/banner_projects/real_estate/liened_properties_cleaned.csv")

#use postmastr to standerdize the addresses from the liened properties data 
D_liened_properties_bmore_sf_2<-D_liened_properties_bmore %>% pm_identify(var = "property_address")

D_postmast_addr<-D_liened_properties_bmore_sf_2%>% pm_prep(var = "property_address", type = "street") 
dirsDict <- pm_dictionary(type = "directional", locale = "us")

D_postmast_addr_2_unit<-D_postmast_addr %>% 
  pm_house_parse() %>% pm_unit_parse() %>% select(pm.uid, pm.unit)

D_postmast_addr_2_f<-D_postmast_addr %>% 
  pm_house_parse() %>% 
  pm_unit_parse() %>% 
  pm_streetDir_parse(dictionary = dirsDict) %>%
  pm_streetSuf_parse()%>%
  filter(is.na(pm.house)==FALSE,is.na(pm.address)==FALSE)

D_postmast_addr_3<-D_postmast_addr_2_f %>% left_join(D_postmast_addr_2_unit)%>%
  mutate(pm.house = pm.house %>% str_remove(., "^0+") %>% str_remove(., ">"))%>% 
  mutate(pm.house=str_split(pm.house,"-") %>% lapply(function(x)return(x[1])) %>% unlist,
         pm.preDir=replace_na(pm.preDir,""),
         pm.streetSuf=replace_na(pm.streetSuf,""),
         pm.sufDir=replace_na(pm.sufDir,""),
         pm.unit=replace_na(pm.unit,""),
         pm.addr.full = str_c(pm.house, " ",pm.preDir, " ",pm.address," ", pm.streetSuf, " ", pm.sufDir," ",pm.unit) %>% trimws %>% 
           str_replace("  "," ") %>% str_replace("  "," ") %>% tolower) %>% select(pm.uid,pm.addr.full)

D_liened_properties_bmore_sf_3<-D_liened_properties_bmore_sf_2 %>% left_join(D_postmast_addr_3, by = "pm.uid") %>% 
  select(-pm.id,-pm.uid, -pm.type)%>% add_column(id_lien = 1:nrow(.)) %>% as.data.frame

##now we do the same thing with the sdat data that has the shapes included. i'm also reading this back in, despite it being written out above, because it's 
##commented out there als. because of the way writing shapefiles can change names, i write out a name file that includes the wright names for the variables as well

j_city_state_data_sf<-st_read("~/Desktop/banner_projects/real_estate/fixed_parcel_data.shp")
name_file <- read_csv("~/Desktop/banner_projects/real_estate/fixed_parcel_names.csv") #got it
name_file[nrow(name_file),]$.<-"geometry"
name_file[nrow(name_file)-1,]$.<-"id_city"
names(j_city_state_data_sf)<-name_file$.


##after standardizing these addresses, then we joined the standerdized addresses together and check to make sure we're matching things properly.

j_balt_full_on_property<-D_liened_properties_bmore_sf_3  %>% 
  left_join(j_city_state_data_f_3, by = c("pm.addr.full")) %>% filter(is.na(owner)==FALSE) %>% select(-geometry, -id_city,-OBJECTID) %>% 
  filter(is.na(pm.addr.full.sdat)==FALSE) %>%  distinct %>% filter(is.na(pm.addr.full.sdat)==FALSE)

j_balt_full_on_property_sf<-D_liened_properties_bmore_sf_3  %>% 
  left_join(j_city_state_data_f_3, by = c("pm.addr.full")) %>% filter(is.na(owner)==FALSE) %>% select(-id_city,-OBJECTID) %>% 
  filter(is.na(pm.addr.full.sdat)==FALSE) %>%  distinct %>% filter(is.na(pm.addr.full.sdat)==FALSE) %>% st_as_sf

j_balt_full_on_property_sf %>% st_as_sf %>% st_write("~/Desktop/banner_projects/real_estate/matched_liens_sdat_2_sf.shp", delete_dsn = T)

desc_check<-j_balt_full_on_property %>% group_by(id_lien) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n))
x_check<-j_balt_full_on_property %>% group_by(id.x) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n))

# some of these are repeated and that means that the liens showed up more than once for the same property, which is fine.
y_check<-j_balt_full_on_property %>% group_by(id.y) %>% summarise(n=n()) %>% filter(n>1) %>% arrange(desc(n))
y_check_new <- y_check %>% filter(n > 3)

# this is how we check to make sure different apartments aren't being matched to the same building. they are not
j_balt_full_on_property %>% filter(id.y%in%y_check_new$id.y) %>% group_by(property_address, owner) %>% summarise(n = n()) %>%
  group_by(property_address) %>% summarise(n = n()) %>% arrange(desc(n))

#matching it that way, you only miss 3.6k of the original 69k. 
d_missing<-D_liened_properties_bmore %>% add_column(id_lien = 1:nrow(.))%>% filter(id_lien%in%j_balt_full_on_property$id_lien==FALSE)

##finding properties we know are repeated.. basically none.
repeated_properties<-j_balt_full_on_property %>% group_by(id_lien) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n >1)

j_balt_full_on_property %>% filter(id_lien%in%repeated_properties$id_lien) %>% group_by(property_address, owner) %>% summarise(n =n()) %>% 
  group_by(property_address) %>% summarise(n = n()) %>% arrange(desc(n))

#
j_balt_full_on_property %>% write_csv("~/Desktop/banner_projects/real_estate/matched_liens_sdat_2.csv")

write_csv(names(j_balt_full_on_property %>% as.data.frame) %>% as.data.frame(),
  "~/Desktop/banner_projects/real_estate/complete_prop_sale/names_to_add_1.csv")

j_balt_full_on_property_sf %>% names %>% as.data.frame %>%  write_csv("~/Desktop/banner_projects/real_estate/liens_names.csv")

