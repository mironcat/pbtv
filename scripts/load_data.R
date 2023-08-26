
#  Global Average Temperature from Li_Hu_et_al_2022
temp_avg <- read_tsv("data/Li_Hu_et_al_2022_paleotemperatures.csv")%>%
  dplyr::select(Age_MA, Surface_temperature)%>%
  rename(age=Age_MA, temp=Surface_temperature)

#paleosib polygons of climzones marked using Boucot et al. 2013 Phanerozoic Paleoclimate: An Atlas of Lithologic Indicators of Climate 
paleosib_climzones <- read.csv("data/Boucot_paleosib_zones.csv", sep=";")
# filtered stages
stages_psb<-read.csv("data/stages_psb.csv")

# raw_colls<-read.csv("data/paleosib_colls.csv")
# filtered collections with paleolatitudes see paleosib project
paleoColls <-read.csv("data/paleoColls_sibplate_only.csv")%>%left_join(stages_psb%>%dplyr::select(stage,stg,mid), by="stage")%>%
  dplyr::select(stage,stg,collection_no,projects, collection_name,early_interval,late_interval,primary_reference,stratcomments,geocomments,  mid,paleolng_scotese2016, paleolat_scotese2016)

# filtered occurrences with paleolatitudes 
occr_sib <- read.csv("data/paleoOccs_sibplate_only.csv")%>%
  dplyr::select(-X)%>%
  distinct_all()

#  filter(collection_no!=192851) #remove Tuksinskaya collection 


sib_coord_by_age_scotese<-read.csv('data/sib_coord_by_age_scotese.csv')

sibcoord_scotese<-sib_coord_by_age_scotese%>%mutate(age_dec=round(age,-1))%>%
  left_join(temp_avg,by=c("age_dec"="age"))%>%
  left_join(paleosib_climzones%>%filter(system!='additional'),by=c("stage"="stage"))%>%
  mutate(lat_range=lat_max-lat_min)%>%
  mutate(zlat_max = if_else((ztype=="lower"), lat_max-lag(persent_zone_lat)*(lat_range/100), lat_max) )%>%
  mutate(zlat_min = if_else((ztype=="upper"), lead(zlat_max), lat_min) )%>%
  filter(stage!='Hettangian'&stage!='Lochkovian')

pcclimzones_scotese<-sibcoord_scotese%>%
  dplyr::select(zones,episodes,stage,stg, age, zlat_min,zlat_max)%>%rename(lat_min=zlat_min,lat_max=zlat_max)

getPoly <- function(dat) {
  up_bound<-dat%>%rename(lat=lat_max)%>%arrange( age,desc(stg) )
  low_bound<-dat%>%rename(lat=lat_min)%>%arrange( desc(age),stg )
  return( 
    bind_rows(up_bound, low_bound)%>%dplyr::select(-lat_min,-lat_max)%>%
      distinct_all()%>%
      mutate(pointNum=row_number())
  )
}
sib_poly_scotese<-getPoly(sibcoord_scotese)
# 
# episodes<-unique(pcclimzones_scotese$episodes)
# # episodes<-c("cool_temp1")
# clim_polygons<-tibble()
# for (episode in episodes) {
#   print(episode)
#   climzone_dat<-pcclimzones_scotese%>%filter(episodes==episode)
#   clim_poly<- getPoly(climzone_dat)
#   clim_polygons<-clim_polygons%>%bind_rows(clim_poly)
# }
# clim_polygons<-clim_polygons%>%mutate(globPointNum=row_number())
# write.csv(clim_polygons, 'data/clim_polygons.csv',row.names = F)
clim_polygons<-read.csv('data/clim_polygons.csv')


# y=45-x*0.5
# y+x*0.5=45
# x=(45-y)/0.5
# geom_text(data = sibcoord_scotese, mapping = aes(x = age, y = (45-temp)/0.5, label=temp ) )+
# geom_hline(yintercept = (45-25)/0.5)

# p+
#   geom_polygon(data=clim_polygons,mapping = aes(x=age,y=lat,group=episodes, fill=zones),alpha=0.2 )+
#   geom_point(data = paleoColls, mapping = aes(x=mid,y=paleolat_scotese2016),color="orange")


occr_coll_analysis<-occr_sib%>%distinct(collection_no, paleolat_scotese2016, mid)%>%
        group_by(collection_no)%>%
        summarise(n=n())%>%
        filter(n>1)
clades<-unique(occr_sib$vernacular_name)
