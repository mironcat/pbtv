library(shiny)
library(tidyverse)
library(leaflet)
library(DT)
library(ggnewscale)
library(jsonlite)
library(markdown)

source('scripts/load_data.R')

cres <- list(
  default = leafletCRS('L.CRS.EPSG4326'),
  robin =  leafletCRS(crsClass = "L.Proj.CRS", code = "ESRI:54030",
                      proj4def = "+proj=robin +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                      resolutions = 1.5^(25:15)),
  mollewide = leafletCRS(crsClass = "L.Proj.CRS", code = "ESRI:53009",
                         proj4def = "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs",
                         resolutions = c(65536, 32768, 16384, 8192, 4096, 2048))
)

ui <- fluidPage(
    tags$head(
      tags$style(HTML("
                      p {margin-top: 15px;}
                      #loadmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #a2afbd;
                           z-index: 105;
                         }
                      
                      "))
    ),  
    titlePanel("PBTV: Paleo – Biota – Temperature – Vision"),
    fluidRow(
        column(width = 2,
               sliderInput("redsubpample", label = h3("Red subsample"), min = 0, 
                           max = 35, value = c(17, 35))
        ),        
        column(width = 2,
               sliderInput("bluesubpample", label = h3("Blue subsample"), min = 0, 
                           max = 35, value = c(1, 12))
        ),
        column(width = 2,sliderInput("tolerance", label = h3("Temp. Tolerance"), min = 0, 
                                     max = 30, value = 5)
        ),
        column(width = 2,
               checkboxInput("showred", "Show RED subsample", T),
               checkboxInput("showblue", "Show BLUE subsample", T),
               checkboxInput("showGAT", "Show Global Average Temperature (Scotese et.al, 2021)", F),
               checkboxInput("showcoll_link", "Link taxa by collection", F)
              ),
        column(width = 2,
               htmlOutput("clade_stats")
        ),
        column(width = 2,
               selectInput("vernacularFilter", "Clade:",
                           c("all", clades)),
               actionButton("showless", "show less"),
               actionButton("showall", "show more")
        )
    ),
    hr(),
    fluidRow(
        plotOutput("plot2",click = clickOpts(id = "plot2_click"), brush = brushOpts(id = "plot2_brush"), height = 800 )  
        # plotOutput("plot1", brush = brushOpts(id = "plot1_brush"), height = 400 )        
            

  
    ),
    hr(),
    fluidRow(
        column(width = 8,
               h4("Brushed points"),
               DT::dataTableOutput("brush_info"),
               #tableOutput("brush_info"),
               plotOutput("temperatureplot"),
               fluidRow(
                column(3, verbatimTextOutput("genus_info")),
                column(9, tableOutput("coll_info"),uiOutput("coll_url"))
               )
               
        ),
        column(width = 4,
               tabsetPanel(
                 tabPanel("map", leafletOutput("mymap")),
                 tabPanel("paleomap",div(id = 'myDiv',
                   selectInput(inputId = "selectedStage",label = "stages", choices = stages_psb$stage ),
                   leafletOutput("paleomap")
                 ) )
               ),
               plotOutput("trangehist"),
       )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    occr<-reactive({
        res<-tibble()
        if(input$vernacularFilter!="all"){
          occr_cl<-occr_sib%>%
            filter(vernacular_name==input$vernacularFilter)
        } else {
          occr_cl<-occr_sib
        }
        output$clade_stats<-renderText({
          paste(
            "SUBSAMPLE STATISTICS <br>",
            "colls:",nrow(paleoColls),"<br>",
            "occs:",nrow(occr_cl),"<br>",
            "genera:",length(unique(occr_cl$genus)),"<br>",
            "tmin:",min(occr_cl$tmin),"tmax:",max(occr_cl$tmax),"<br>",
            "tolerance: min-",min(occr_cl$trange), "max-",max(occr_cl$trange)
          )
        })

        if (input$showred){
            tmin_slider<-input$redsubpample[1]
            tmax_slider<-input$redsubpample[2]
            occr_red<-occr_cl%>%
                filter(tmin<=tmax_slider&tmax>=tmin_slider&trange<=input$tolerance)%>%
                mutate(tclass="red", tcolor="darkred")
            res<-bind_rows(res,occr_red)      
        } 
        if (input$showblue){
            tmin_slider<-input$bluesubpample[1]
            tmax_slider<-input$bluesubpample[2]
            occr_blue<-occr_cl%>%
                filter(tmin<=tmax_slider&tmax>=tmin_slider&trange<=input$tolerance)%>%
                mutate(tclass="blue", tcolor="darkblue")
            res<-bind_rows(res,occr_blue)
        } 
        res
    })
    
  
    observeEvent(input$showall, {
      # set parameters for show maximum data for selected clade 
      if(input$vernacularFilter!="all"){
        occr_cl<-occr_sib%>%
          filter(vernacular_name==input$vernacularFilter)
      } else {
        occr_cl<-occr_sib
      }
      updateSliderInput(session, "redsubpample", value = c( min(occr_cl$tmin), max(occr_cl$tmax) ) )
      updateSliderInput(session, "bluesubpample", value = c( min(occr_cl$tmin), max(occr_cl$tmax) ) )
      updateSliderInput(session, "tolerance", value = max(occr_cl$trange) )
    }) 
    
    observeEvent(input$showless, {
      # set parameters for show margin taxa for selected clade
      if(input$vernacularFilter!="all"){
        occr_cl<-occr_sib%>%
          filter(vernacular_name==input$vernacularFilter)
      } else {
        occr_cl<-occr_sib
      }
      occr_tr<-occr_cl%>%filter(trange<=min(occr_cl$trange))
      updateSliderInput(session, "redsubpample", value = c( max(occr_tr$tmin), 35 ) )
      updateSliderInput(session, "bluesubpample", value = c( 0, min(occr_tr$tmax) ) )
      updateSliderInput(session, "tolerance", value = min(occr_tr$trange) )
    })     
    
  output$plot2 <- renderPlot({
         occr_dat<-occr()
         occr_coll_analysis<-occr_dat%>%distinct(collection_no,tclass, paleolat_scotese2016, mid)
         

         p<-ggplot(occr_dat,mapping = aes(x=mid,y=paleolat_scotese2016))+
            geom_vline(data = stages_psb,aes(xintercept=bottom), color="gray70")+
            geom_vline(data = stages_psb,aes(xintercept=top), color="gray70")+
            geom_text(data = stages_psb, mapping = aes(x=(bottom+top)/2,y=95, label=stage, angle=90))+
            geom_polygon(data=clim_polygons,mapping = aes(x=age,y=lat,group=episodes, fill=zones),alpha=0.2 )+
            scale_fill_manual(name="paleo-Koppen Belts", breaks = rev(c("tropical", "arid_tropical", "arid", "warm_temp", "cool_temp","cold")),
                              values=rev(c("green", "orange", "yellow", "darkgreen", "blue","cyan")))+           
            new_scale_fill()
        if (input$showGAT){
             p<-p+
                 geom_line(data = sibcoord_scotese, mapping = aes(x = age_dec, y = temp*2 ),color="gray60" )+
                 geom_point(data = sibcoord_scotese, mapping = aes(x = age_dec, y = temp*2),color="gray30" )+
                 scale_y_continuous(
                     name = "Paleolatitude",
                     limits = c(5,110),
                     breaks = seq(from=10,to=120,by=10),
                     sec.axis = sec_axis( trans =~.*0.5,breaks =seq(from=10,to=30,by=5), name="Temperature (°C)")
                 )
        } else {
             p<-p+scale_y_continuous(
                 name = "Paleolatitude",
                 limits = c(5,110),
                 breaks = seq(from=10,to=120,by=10)
             )
         }
         p<-p+
            geom_point(mapping = aes(fill=tclass,shape=tclass),alpha=0.6, size=3)+
            scale_shape_manual(name = "Subsample", breaks = c("red", "blue"),
                               values = c(22, 23))+
            scale_fill_manual(name = "Subsample", breaks = c("red", "blue"),
                               values = c("darkred", "darkblue"))
         if (input$showcoll_link){
          p<-p+
           geom_line(data = occr_coll_analysis, aes(x=mid,y=paleolat_scotese2016, group=collection_no,color=tclass))
          }
          p+scale_x_continuous(breaks =seq(from=410,to=200,by=-5),trans = "reverse")+
              labs(title="",
                   x ="Age, MA", y = "paleolatitude")+
              theme_bw()+
              theme(legend.title=element_text(size=14),legend.text=element_text(size=12), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    })
  
    dat_brush<-reactive({
      dat<-brushedPoints(occr(), input$plot2_brush)
      if (nrow(nearPoints(occr(), input$plot2_click))!=0){
        dat<-nearPoints(occr(), input$plot2_click)
      }
      cur_stages<-dat%>%distinct(stage)
      updateSelectInput(session, "selectedStage",choices =cur_stages$stage, selected = cur_stages$stage[1] )
      dat
    })
   #renderDT or renderTable
    output$brush_info <- renderDT({
      # tclass,stage,mid, collection_name, genus,tmin,tmax, lat, lng, paleolat_scotese2016,paleolng_scotese2016
      dat_brush()%>%dplyr::select(tclass,stage, collection_name, genus,tmin,tmax, paleolat_scotese2016,paleolng_scotese2016)%>%
        rename(plat=paleolat_scotese2016,plng=paleolng_scotese2016)%>%
        mutate(plat=round(plat,2),plng=round(plng,2))
    }, selection = "single")
    
    
    
    # ------------------------------------ TEMPPLOT ---------------------------------- 
    output$temperatureplot <-renderPlot(
      if(nrow(dat_brush())>0){
      ggplot(dat_brush()) +
        geom_segment(
          aes(x = tmin,
              y = fct_reorder(genus, tmin),
              xend = tmax,
              yend = fct_reorder(genus, tmin),
              color=tclass
          ),size=3
        )+
        scale_color_manual(name = "subsample", breaks = c("red", "blue"),
                          values = c("darkred", "darkblue"))+
        theme_bw()
      # ggsave("tempplot.pdf")
      }
    )    

    points <- eventReactive(input$recalc, {
      cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
      if(nrow(dat_brush())>0){
            leaflet() %>%
              addTiles()%>%
              addCircleMarkers(
                group="collections",
                data = dat_brush()%>%distinct(collection_no,collection_name, lng, lat, tcolor),
                layerId= ~ collection_no,
                lng = ~ lng, lat = ~ lat,
                label = ~ collection_name,
                stroke = FALSE,
                radius = 6,
                fillColor = ~ tcolor,
                fillOpacity = 0.7
              )
        }
    })
    output$paleomap <- renderLeaflet({
      
      selected_stage<-input$selectedStage
      scotese_geojson <- readLines(paste0("data/Scotese2016geojson/scotese2016_",selected_stage,".geojson"), warn = FALSE) %>%
        paste(collapse = "\n") %>%
        fromJSON(simplifyVector = FALSE)
      # Default styles for all features
      scotese_geojson$style = list(
        weight = 1,
        fillColor = 'gray',
        color = "gray",
        opacity = 1,
        fillOpacity = 0.5
      )
      paleomap<-leaflet(
        options = leafletOptions(
          crs = cres[['default']])
      )%>%addSimpleGraticule(group = "graticule")%>% 
        addGeoJSON(group = "plates", scotese_geojson)
      if(nrow(dat_brush())>0){
        paleomap_dat<-dat_brush()%>%distinct(stage,collection_no,collection_name, paleolng_scotese2016, paleolat_scotese2016, tcolor)%>%
          filter(stage==selected_stage)
        paleomap<-paleomap%>%
          addCircleMarkers(
            group="collections",
            data = paleomap_dat,
            layerId= ~ collection_no,
            lng = ~ paleolng_scotese2016, lat = ~ paleolat_scotese2016,
            label = ~ collection_name,
            stroke = FALSE,
            radius = 6,
            fillColor = ~ tcolor,
            fillOpacity = 0.7
          )
      }
      paleomap
    })
    observe({
      rs<-input$brush_info_row_last_clicked
      if (length(rs)) {
        row<- dat_brush()[rs,]
        # cur_coll<-paleoColls%>%filter(collection_no==row$collection_no&stage==row$stage)
        leafletProxy("mymap") %>%
          clearGroup(group="selected")%>%
          addCircleMarkers(
            group="selected",
            data = row,
            layerId= ~ collection_no,
            stroke = FALSE,
            radius = 6,
            fillColor = "yellow",
            fillOpacity = 1,                
            lng = ~ lng, lat = ~ lat,
            label = ~ collection_name
          ) 
        leafletProxy("paleomap") %>%
          clearGroup(group="selected")%>%
          addCircleMarkers(
            group="selected",
            data = row,
            layerId= ~ collection_no,
            stroke = FALSE,
            radius = 6,
            fillColor = "yellow",
            fillOpacity = 1,                
            lng = ~ paleolng_scotese2016, lat = ~ paleolat_scotese2016,
            label = ~ collection_name
          )        
      }

    })
    
    
    output$genus_info <- renderPrint({
      rs<-input$brush_info_row_last_clicked
      if (length(rs)) {
       row<- dat_brush()[rs,]%>%dplyr::select(
         genus, vernacular_name, phylum,family, tmin,tmax, trange, tclass                                                                                                                                                                                  
       )
       as.data.frame(t(row))
      }
    })
    output$coll_info <- renderTable({
      rs<-input$brush_info_row_last_clicked
      if (length(rs)) {
        row<- dat_brush()[rs,]
        cur_coll<-paleoColls%>%filter(collection_no==row$collection_no&stage==row$stage)
        cur_coll%>%dplyr::select(
          collection_name,	early_interval,	late_interval,
          primary_reference,	stratcomments,	geocomments
        )
      }
    })
    output$coll_url <- renderUI({
      rs<-input$brush_info_row_last_clicked
      if (length(rs)) {
        row<- dat_brush()[rs,]
        url_text<-paste0("https://biogeolog.tk/admin/#/paleosib/collections/colls/",row$collection_no)
        url <- a(paste(url_text),target="_blank", href=url_text)
        tagList("Collection(login and password required):", url)
      }      

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
