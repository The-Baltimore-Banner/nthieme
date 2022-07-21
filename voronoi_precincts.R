###2022 Maryland primary voting block calcuation###

#### load library functions

library(xml2)
library(tidyverse)
library(deldir)
library(sf)
library(tidycensus)
library(geojsonsf)
library(viridis)

##load in xml data from state website

file_list <- list.files("~/Downloads/XML_Results_2022GP (4)")
file_list_keep <- file_list %>% str_detect("Results_MD.XML") %>% {which(.)}
file_list_f<- file_list[file_list_keep]
D_tot_precs <- tibble()


DD <- read_xml("~/Downloads/XML_Results_2022GP (4)/20GP22_Results_MD_Early_Voting.XML")

##loop to parse data. could turn these loops into lapplys (like the inner one) to speed up parse
for(k in 1:length(file_list_f)){
  D_1<-read_xml(str_c("~/Downloads/XML_Results_2022GP (4)/", file_list_f[k]))
  
  races<-D_1 %>% xml_find_all("//ContestList") %>% xml_contents
  
  
  for(i in 1:length(races)){
    curr_race <- races[i]
    title <- curr_race %>% xml_attr(., "title")
    total_cast<-curr_race %>% xml_attr(., "ballotsCast")
    
    cand_list<-curr_race %>% xml_contents
    cand_names<-cand_list %>% xml_attr(.,"name")
    
    for(j in 1:length(cand_list)){
      precinct_list<-cand_list[j] %>% xml_contents
      prec_names<-precinct_list %>% xml_attr(.,"refPrecinctId")
      
      vote_tots<-lapply(precinct_list, function(x)return(x %>% xml_contents %>% xml_text()))
      vote_tots_f<-vote_tots[-which(is.na(prec_names))] %>% do.call("rbind",.)
      vote_tots_f_2<-cbind(vote_tots_f, prec_names[-which(is.na(prec_names))],cand_name = cand_names[j], race_name = title, county = k) %>% as_tibble
      names(vote_tots_f_2)<-c("Early", "election_day","Mail","tot","perc", "prec_name","cand_name","race_name","county")
      
      D_tot_precs <- rbind(D_tot_precs, vote_tots_f_2)
      
    }
    
    print( str_c("done with ", i, " of ", length(races), " races"))
  }
  
  print( str_c("done with ", k, " of ", length(file_list_f), " counties"))
}

#D_tot_precs %>% group_by(race_name, county, cand_name) %>% summarise(early = sum(as.numeric(Early)), election = sum(as.numeric(election_day)))
write_csv(D_tot_precs,"~/Desktop/banner_projects/election/precinct_level_data.csv")

###code that lets us make sure we're matching the topline results on 
#https://elections.maryland.gov/elections/2022/primary_results/county_status_page_root.html
topline_results<-D_tot_precs %>% group_by(cand_name, race_name, county) %>% 
  summarise(early = sum(as.numeric(Early)), election_day = sum(as.numeric(election_day)))

topline_results %>% filter(county==1) %>% print(n = 105)


###elsewhere, we've parsed the polling place data provided by the state and used geocodio to geocode them. moving to the polling places
D_tot_precs<-read_csv("~/Desktop/banner_projects/election/precinct_level_data.csv")

D_polling_places <- read_csv("~/Desktop/banner_projects/election/final_polling_places.csv") %>% 
  select(name, address, precinct_name,Latitude, Longitude, County) %>% st_as_sf(coords = c("Longitude", "Latitude"))

#checking if the mapping looks good. it does
D_polling_places %>% ggplot+geom_sf(size = .5)

## ultimately, what we're doing is using a voronoi tesselation of the space around polling places to partition MD into "precincts." 
##we write about this in FAQ for the article, but the idea is that each "precinct" is the space that is closest to a particular polling place. 
##there are very clear issues with this (namely precincts that cross county borders, the fact that VTDs don't match up with voronoi's exactly).
##we've decided that the drawbacks are worth seeing the geographic variation in the data. 


bbox_polygon <- function(x) {
  bb <- sf::st_bbox(x)
  
  p <- matrix(
    c(bb["xmin"], bb["ymin"], 
      bb["xmin"], bb["ymax"],
      bb["xmax"], bb["ymax"], 
      bb["xmax"], bb["ymin"], 
      bb["xmin"], bb["ymin"]),
    ncol = 2, byrow = T
  )
  
  sf::st_polygon(list(p))
}

##calculate the voronoi polygons
box <- st_sfc(bbox_polygon(D_polling_places))
v <- st_voronoi(st_union(D_polling_places), box)
MD_shape<-get_acs(state = "MD", variables = "B19013_001", geometry = T, geography = "state")
st_crs(v)<-st_crs(MD_shape)

##clip the shapes to our state and city borders
vtd_districts_fake<-st_intersection(st_cast(v), MD_shape) %>% st_sf # clip to smaller box
st_crs(vtd_districts_fake)<-st_crs(D_polling_places)
#check MD plot
vtd_districts_fake %>% ggplot()+geom_sf()

#the next difficulty is matching the XML data, which comes from individual polling places, with the polling place locations that index the 
#voronoi polygons. as always, there's a difference between the polling place names in the vote data and polling place names in the polling place
#data. who knows why! always like that though! anyways. it turns out that the differences in the naming conventions between the polling place 
#data and vote data are very predictable and we can make the two match by dropping double zero's and when we don't match like that, removing a 0
#that isn't the first or last "letter" in a "word."

vtd_districts_w_polling_info<-st_join(vtd_districts_fake, D_polling_places) %>% 
  mutate(precinct_name = str_replace(precinct_name, "00","0") %>% as.numeric %>% as.character) %>% group_by(address, precinct_name, County ) %>% 
  summarise(name = name[1])

#this is double 00 drop match
county_list<-c("Allegany County", "Anne Arundel County", "Baltimore City","Baltimore County", "Calvert County", "Caroline County","Carroll County",
               "Cecil County","Charles County","Dorchester County","Frederick County", "Garrett County","Harford County","Howard County","Kent County",
               "Montgomery County","Prince George's County","Queen Anne's County","St. Mary's County","Somerset County","Talbot County",
               "Washington County","Wicomico County","Worcester County")

D_counties <- tibble(county_n = county_list, num_county = 1:length(county_list) ) 

D_tot_precs_f<-D_tot_precs %>% left_join(D_counties, by = c("county"="num_county")) %>% mutate(prec_name = as.character(prec_name))

D_votes_joined_vornoi<-D_tot_precs_f %>% left_join(vtd_districts_w_polling_info, by = c("county_n"="County", "prec_name"="precinct_name")) %>% 
  mutate(early = as.numeric(Early), election_day = as.numeric(election_day))

D_votes_joined_vornoi_percs<-D_votes_joined_vornoi %>% group_by(race_name, county, prec_name) %>% mutate(tot_votes = sum(early+election_day)) %>% 
  mutate(vote_share_in_prec = (early+election_day)/tot_votes) %>% as_tibble

set_of_precinct_shapes <- D_votes_joined_vornoi %>% select(county, prec_name, geometry) %>% distinct

D_votes_joined_vornoi_percs_comp<-complete(D_votes_joined_vornoi_percs, nesting(cand_name, race_name), nesting(county,prec_name))

D_votes_joined_vornoi_percs_comp_f<-D_votes_joined_vornoi_percs_comp %>% mutate(Early = replace_na(Early, 0),
                                                                                election_day = replace_na(election_day, 0),
                                                                                tot = replace_na(tot, 0),
                                                                                perc = replace_na(perc, 0),
                                                                                early = replace_na(early, 0),
                                                                                tot_votes = replace_na(tot_votes, 0),
                                                                                vote_share_in_prec = replace_na(vote_share_in_prec, 0))

D_votes_joined_vornoi_percs_comp_sf<-D_votes_joined_vornoi_percs_comp_f %>%  left_join(set_of_precinct_shapes, by = c("county","prec_name"))

D_votes_joined_vornoi_percs_cox<-D_votes_joined_vornoi_percs %>% filter(cand_name=="Cox-Schifanelli") 

D_comp_cox<-complete(D_votes_joined_vornoi_percs_cox, cand_name, race_name, nesting(county,prec_name)) %>% select(-geometry) %>% 
  left_join(vtd_districts_w_polling_info, by = c("county_n"="County","prec_name"="precinct_name"))

vtd_districts_w_polling_info %>% filter(County=="Baltimore City County")

vtd_districts_w_polling_info %>% pull(County) %>% unique
D_votes_joined_vornoi_percs_cox$county_n %>%unique

##this is the drop the middle missing 0, after missing the match section

fix_prec<-function(x){
  x_1 <- str_sub(x,1,1)
  x_n <-str_sub(x,nchar(x),nchar(x))
  x_mid <- str_sub(x, 2, nchar(x)-1)
  x_mid_new<-str_remove(x_mid,"0")
  new_prec<-str_c(x_1, x_mid_new, x_n)
  return(new_prec)
}


missing_precincts<-vtd_districts_w_polling_info %>%
  anti_join(D_tot_precs_f, by = c("County"="county_n","precinct_name"="prec_name"))

missing_precincts_new<-missing_precincts %>% mutate(precinct_name = fix_prec(precinct_name))

missing_precincts_filled<-D_tot_precs_f %>%left_join(missing_precincts_new, by = c("county_n"="County","prec_name"="precinct_name"))%>% 
  filter(is.na(address)==FALSE)%>% mutate(early = as.numeric(Early), election_day = as.numeric(election_day))

D_votes_joined_vornoi<-D_tot_precs_f %>% left_join(vtd_districts_w_polling_info, by = c("county_n"="County", "prec_name"="precinct_name")) %>% 
  filter(is.na(address)==FALSE)%>% mutate(early = as.numeric(Early), election_day = as.numeric(election_day))

D_joited_joined_vornoi_2 <- rbind(D_votes_joined_vornoi,missing_precincts_filled) %>% distinct

D_votes_joined_vornoi_percs_2<-D_joited_joined_vornoi_2 %>% group_by(race_name, county, prec_name) %>% mutate(tot_votes = sum(early+election_day)) %>% 
  mutate(vote_share_in_prec = (early+election_day)/tot_votes) %>% as_tibble %>% 
  mutate(address = str_remove(address, "Unknown") %>% str_remove(., "Not Accessible") %>% trimws)

set_of_precinct_shapes_2 <- D_joited_joined_vornoi_2 %>% select(county, prec_name, geometry) %>% distinct

D_votes_joined_vornoi_percs_comp_2<-complete(D_votes_joined_vornoi_percs_2, nesting(cand_name, race_name), nesting(county,prec_name))

D_votes_joined_vornoi_percs_comp_f_2<-D_votes_joined_vornoi_percs_comp_2 %>% mutate(Early = replace_na(Early, 0),
                                                                                    election_day = replace_na(election_day, 0),
                                                                                    tot = replace_na(tot, 0),
                                                                                    perc = replace_na(perc, 0),
                                                                                    early = replace_na(early, 0),
                                                                                    tot_votes = replace_na(tot_votes, 0),
                                                                                    vote_share_in_prec = replace_na(vote_share_in_prec, 0))

D_votes_joined_vornoi_percs_comp_sf_2<-D_votes_joined_vornoi_percs_comp_f_2 %>%  left_join(set_of_precinct_shapes_2, by = c("county","prec_name"))

D_votes_joined_vornoi_percs_cox_2<-D_votes_joined_vornoi_percs_2 %>% filter(cand_name=="Cox-Schifanelli") 

##at this point the data is joined and cleaned, and we need to parse it and get it in the right shape for plotting

Baltimore_shape<-get_acs(variables = "B19013_001", geometry = T, geography = "county", state = "MD") %>% filter(str_detect(NAME, "Baltimore city"))

dem_sa_cand <- c("Ivan Bates", "Marilyn J. Mosby", "Thiru Vignarajah", "Robbie Leonard", "Scott Shellenberger")

dem_states_attorney_winning_cand<-D_votes_joined_vornoi_percs_2 %>% filter(race_name=="DEM State's Attorney")%>% filter(county%in%c(3,4)) %>% 
  mutate(zip = str_sub(address, nchar(address)-4, nchar(address))) %>% 
  select(prec_name, cand_name, county, vote_share_in_prec, geometry,zip) %>%
  pivot_wider(names_from = cand_name, values_from = vote_share_in_prec) %>% rowwise %>% 
  mutate(`Ivan Bates`=replace_na(`Ivan Bates`,0), 
         `Marilyn J. Mosby`=replace_na(`Marilyn J. Mosby`,0), 
         `Thiru Vignarajah`= replace_na(`Thiru Vignarajah`,0)
  )%>%
  mutate(winning_cand=dem_sa_cand[which.max(c(`Ivan Bates`, `Marilyn J. Mosby`, `Thiru Vignarajah`))]) %>% 
  mutate(winning_cand=case_when(winning_cand%in%c("Ivan Bates", "Marilyn J. Mosby", "Thiru Vignarajah")~winning_cand, 
                                winning_cand%in%c("Ivan Bates", "Marilyn J. Mosby", "Thiru Vignarajah")==FALSE~"Other"),
         `Ivan Bates` = round(`Ivan Bates`, digits = 3)*100,
         `Marilyn J. Mosby`= round(`Marilyn J. Mosby`, digits = 3)*100,
         `Thiru Vignarajah` = round(`Thiru Vignarajah`, digits = 3)*100,
         zip_prec = str_c(zip," - ", prec_name)
  ) %>% st_as_sf

st_crs(dem_states_attorney_winning_cand)<-st_crs(Baltimore_shape)

dem_states_attorney_winning_cand_2<-st_intersection(dem_states_attorney_winning_cand, Baltimore_shape)

dem_cand <- c("Moore-Miller", "Perez-Sneed", "Franchot-Anderson-Walker", "Baker, III-Navarro","Baron-Williams","Gansler-Hollingsworth",
              "Jaffe-Greben","Jain-Lytes","King-Daugherty Siri","Segal-Dispenza")


dem_governor_winning_cand<-D_votes_joined_vornoi_percs_2 %>% filter(race_name=="DEM Governor / Lt. Governor")%>% 
  mutate(zip = str_sub(address, nchar(address)-4, nchar(address))) %>% 
  select(prec_name, cand_name, county, vote_share_in_prec, geometry,zip) %>%
  pivot_wider(names_from = cand_name, values_from = vote_share_in_prec) %>% rowwise %>% 
  mutate(`Moore-Miller`=replace_na(`Moore-Miller`,0), `Perez-Sneed`=replace_na(`Perez-Sneed`,0), 
         `Franchot-Anderson-Walker`= replace_na(`Franchot-Anderson-Walker`,0),`Baker, III-Navarro`=replace_na(`Baker, III-Navarro`,0), 
         `Baron-Williams`=replace_na(`Baron-Williams`,0), `Gansler-Hollingsworth`= replace_na(`Gansler-Hollingsworth`,0),
         `Jaffe-Greben`= replace_na(`Jaffe-Greben`,0),`Jain-Lytes`=replace_na(`Jain-Lytes`,0), 
         `King-Daugherty Siri`=replace_na(`King-Daugherty Siri`,0), `Segal-Dispenza`= replace_na(`Segal-Dispenza`,0)
  ) %>%
  mutate(winning_cand=dem_cand[which.max(c(`Moore-Miller`, `Perez-Sneed`, `Franchot-Anderson-Walker`,`Baker, III-Navarro`,
                                           `Baron-Williams`,`Gansler-Hollingsworth`,`Jaffe-Greben`,`Jain-Lytes`,
                                           `King-Daugherty Siri`,`Segal-Dispenza` ))]) %>% 
  mutate(winning_cand=case_when(winning_cand%in%c("Moore-Miller", "Perez-Sneed", "Franchot-Anderson-Walker")~winning_cand, 
                                winning_cand%in%c("Moore-Miller", "Perez-Sneed", "Franchot-Anderson-Walker")==FALSE~"Other"),
         `Moore-Miller` = round(`Moore-Miller`, digits = 3)*100,
         `Perez-Sneed` = round(`Perez-Sneed`, digits = 3)*100,
         `Franchot-Anderson-Walker` = round(`Franchot-Anderson-Walker`, digits = 3)*100,
         zip_prec = str_c(zip," - ", prec_name)
  )

rep_cand<- c("Cox-Schifanelli", "Schulz-Woolford","Ficker-Yegge","Werner-Luong")

rep_governor_winning_cand<-D_votes_joined_vornoi_percs_2 %>% filter(race_name=="REP Governor / Lt. Governor")%>% 
  mutate(zip = str_sub(address, nchar(address)-4, nchar(address))) %>% 
  select(prec_name, cand_name, county, vote_share_in_prec, geometry,zip) %>%
  pivot_wider(names_from = cand_name, values_from = vote_share_in_prec) %>% rowwise %>% 
  mutate(`Cox-Schifanelli`=replace_na(`Cox-Schifanelli`,0), `Schulz-Woolford`=replace_na(`Schulz-Woolford`,0), 
         `Ficker-Yegge`= replace_na(`Ficker-Yegge`,0), `Werner-Luong` = replace_na(`Werner-Luong`,0)) %>% rowwise %>% 
  mutate(winning_cand=rep_cand[which.max(c(`Cox-Schifanelli`, `Schulz-Woolford`, `Ficker-Yegge`,`Werner-Luong`))])%>% 
  mutate(winning_cand=case_when(winning_cand%in%c("Cox-Schifanelli", "Schulz-Woolford")~winning_cand, 
                                winning_cand%in%c("Cox-Schifanelli", "Schulz-Woolford")==FALSE~"Other"),
         `Cox-Schifanelli` = round(`Cox-Schifanelli`, digits = 3)*100,
         `Schulz-Woolford` = round(`Schulz-Woolford`, digits = 3)*100,
         zip_prec = str_c(zip," - ", prec_name))

##writing out as GeoJSONs

st_write(dem_states_attorney_winning_cand_2, dsn = "~/Desktop/banner_projects/election/dem_states_att.GeoJSON", 
         layer = "precinct", driver = "GeoJSON",delete_dsn = T)

st_write(dem_governor_winning_cand, dsn = "~/Desktop/banner_projects/election/dem_gov.GeoJSON", 
         layer = "precinct", driver = "GeoJSON", delete_dsn = T)

st_write(rep_governor_winning_cand, dsn = "~/Desktop/banner_projects/election/rep_gov.GeoJSON", 
         layer = "precinct", driver = "GeoJSON", delete_dsn = T)

st_write(dem_states_attorney_winning_cand_2 %>% select(geometry), dsn = "~/Desktop/banner_projects/election/voronoi_shapefile.GeoJSON", 
         layer = "precinct", driver = "GeoJSON",delete_dsn = T)
