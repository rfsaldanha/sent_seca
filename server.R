library(RColorBrewer);
library(lattice);
library(tmap);
library(tmaptools);
library(ggplot2);
library(leaflet);
library(scales);
library(dygraphs);
library(rpcdas)
library(shinybusy)
#load("/dados/htdocs/shiny.icict.fiocruz.br/sent_seca/dados_sent_seca.RData");
source("pcdas_token.R", local = TRUE)
load("dados_sent_seca.RData")

function(input, output, session) {
  
  
  ## Interactive Map ###########################################
  
  # Map data
  map_data <- reactive({
    this_time <- paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes))
    df_map <- as.data.frame(this_map)
    res <- this_map[,c("cod6", this_time)]
    res <- sf::st_drop_geometry(res)
  })
  #observe(print(head(map_data())))
  
  observeEvent(input$map_pcdasai, {
    show_modal_spinner()
    
    data_to_ai <- merge(map_data(), subset(df_map, select = c("cod6", "NOME_MUNIC")))
    data_to_ai <- format(data_to_ai, decimal.mark = ",")
    
    tryCatch({
      res <- get_text_description(
        df = data_to_ai, 
        prompt = "Este arquivo json contêm dados de precipitação na região semiárida brasileiral. Escreva um parágrafo técnico em português sobre os dados, incluindo valores. Não mencione o nome do arquivo.",
        pcdas_token = pcdas_token
      )
      
      audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
      get_audio_description(text = res, dest_file = audio_summary_file, pcdas_token = pcdas_token)
      
      remove_modal_spinner()
      
      showModal(modalDialog(
        title = "PCDaS AI",
        res, br(),
        tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA),
        tags$img(src = "logo-principal-hires.png"),
        footer = modalButton("Fechar")
      ))
      
    }, error = function(e){
      remove_modal_spinner()
      
      showModal(modalDialog(
        title = "PCDaS AI",
        "Ocorreu um erro. Tente novamente.", br(),
        footer = modalButton("Fechar")
      ))
    })
  })
  
  
  
  # Create the map
  output$map <- renderLeaflet({

    this_time<-paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes));
    
    if(input$cod_map=="Precipitação"){
      #palheta
      df_map<-as.data.frame(this_map);
      #   incProgress(2/5, detail = paste("parte ", 2));
      
      #Paleta de cores para o mapa
      popup<-paste("<b>Município :</b>", df_map$NOME_MUNIC,"-",df_map$SIGLA, "<BR><b>Precipitação :</b>",  format(df_map[,c(this_time)], digits=2),
                   "mm/mês<BR>");
      #  incProgress(3/5, detail = paste("parte ", 3));
      #560/11
      #
      
      leg <- qpal(df_map[,this_time]);
      
      #leg <-  c(levels(as.factor(leg)));
      # incProgress(4/5, detail = paste("parte ", 4));
      
      #  })

      leaflet("map", data = this_map[,c("cod6", this_time)]) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(stroke=FALSE, 
                    smoothFactor = 0.2,
                    fillOpacity = .8, 
                    popup=popup,
                    color = leg, layerId =~cod6) %>% 
	setView(lng = -37.25, lat = -10.33601, zoom = 6) %>%

        addLegend(
          position = 'topleft',
          colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
          labels = c("muito úmido","úmido",
                     "seco","muito seco","extremamente seco"), opacity = 0.5,
          title = 'Precipitação média acumulada'
        );  
    }else{
      
      if(input$cod_map=="NDVI"){
        if(as.numeric(this_time)<200301){
          
          cat("Dados do NDVI se iniciam em 2003 por isso você será realocado em 2003");
          
          this_time <- "200301";
          
        }
        #palheta
#        previewColors(colorFactor(palette = "viridis", reverse = T, domain = NULL), unique(tab7$valor[order(as.numeric(format(round(tab7$valor, 3))), decreasing = F)]))
#        tab7$valor[order(as.numeric(format(round(tab7$valor, 3))))]
        
        qpal1 <- colorQuantile("viridis", unique(tab7$valor[order(as.numeric(format(round(tab7$valor, 3))))]), reverse = T, n = 2);
        this_map <- append_data(geo,  reshape2::dcast(tab7, 'cod6 ~ tempo', value.var = 'valor', fun.aggregate = sum), 
                                key.shp = "cod6", key.data="cod6");
        
        df_map<-as.data.frame(this_map);
        #Paleta de cores para o mapa
        popup<-paste("<b>Município :</b>", df_map$NOME_MUNIC,"-",df_map$SIGLA, "<BR><b>Reflectância :</b>",  format(df_map[,c(this_time)], digits=2),
                     "<BR>");
        
        leg <- qpal1(df_map[,this_time]);
        
        leaflet("map", data = this_map[,c("cod6", this_time)]) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(stroke=FALSE, 
                      smoothFactor = 0.2,
                      fillOpacity = .8, 
                      popup=popup,
                      color = leg, layerId =~cod6) %>% 
          addLegend(
            position = 'topleft',
            colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
            labels = c("muito úmido","úmido",
                       "seco","muito seco","extremamente seco"), opacity = 0.5,
            title = 'condição da vegetação'
          );
        
      }else{
        tmp<-data.frame();
        tmp<-as.data.frame(tb_spi[,c("cod6",this_time)]);
        
        tmp[which(is.na(tmp[,this_time])),this_time]<-'S-1';
        tmp[which(tmp[,this_time] > 0),this_time]<-'S0';
        tmp[which(tmp[,this_time]< 0&tmp[,this_time]> -0.99),this_time]<-'S1';
        tmp[which(tmp[,this_time]< -1&tmp[,this_time]> -1.49),this_time]<-'S2';
        tmp[which(tmp[,this_time]< -1.5&tmp[,this_time]> -1.99),this_time]<-'S3';
        tmp[which(tmp[,this_time]<= -2),this_time]<-'S4';
        

        factpal<-colorQuantile("viridis", unique(as.numeric(as.character(tb_spi[,c(this_time)][[1]]))), reverse = T, n = 3);    
      
        leg<-factpal(tb_spi[,c(this_time)][[1]])

        this_map <- append_data(geo,  tb_spi[,c("cod6",this_time)], 
                                key.shp = "cod6", key.data="cod6");
        df_map<-(this_map);
        #Paleta de cores para o mapa
        popup<-paste("<b>Município :</b>", df_map$NOME_MUNIC,"-",df_map$SIGLA, "<BR><b>SPI :</b>",  format(df_map[,c(this_time)], digits=2),
                     "<BR>");
        

        leaflet('map', data = this_map[,c("cod6", this_time)]) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(stroke=FALSE, 
                      smoothFactor = 0.2,
                      fillOpacity = .8, 
                      popup=popup,
                      color = leg, layerId =~cod6) %>% 
  #addWebGLHeatmap(lng=this_map$x, lat=this_map$y, intensity = this_map[,this_time][[1]], map = "map") %>%        
  addLegend(
            position = 'topleft',
            colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
            labels = c("muito úmido","úmido",
                       "seco","muito seco","extremamente seco"), opacity = 0.5,
            title = 'SPI'
          );

rm(tmp)                
      }##FIM IF NDVI ENTRANDO EM SPI
      
    }
    
        
    
    

    
        })
  
 
  # Renderização dos graficos pelo mapa
  observe({
    leafletProxy("map", data = this_map[,c("cod6", this_time)]) %>% clearPopups();
    event <- input$map_shape_click
    
    if (is.null(event)){return()}
      
    isolate({

      var_munic_sel <- event$id;
    #  print(event$id);      
      
      #Muniípio selecionado pelo usuário no mapa
#      output$munic_sel <- renderPrint({
#        if(is.null(var_munic_sel)){
#          print("...")
#        }
#      pos_munic_sel<-;
      #atualiza_pos(which(df_map$cod6==var_munic_sel))
      #print(paste("posicao_munic ",pos_munic_sel))
        
#          cat(paste(df_map$NOME_MUNIC[df_map$cod6==var_munic_sel],"/", df_map$SIGLA[df_map$cod6==var_munic_sel]));
        
#      })

 #     output$munic_sel_2 <- renderPrint({
#       cat(paste(df_map$NOME_MUNIC[df_map$cod6==var_munic_sel],"/", df_map$SIGLA[df_map$cod6==var_munic_sel]));
#      })
      
      output$histCentile <- renderPlot({
        

        this_time<-paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes));
        
        nm_meso<-as.character(df_map[df_map$cod6==var_munic_sel,"NOME_MICRO"]);
        tmp<-df_map[df_map$NOME_MICRO==nm_meso,c("NOME_MUNIC","NOME_MICRO",paste0(this_time))];
        
        names(tmp)<-c("NOME_MUNIC","NOME_MICRO","valor");
        tmp$seca<-"acima";
        tmp$seca[tmp$valor<55.0]<-"abaixo";
        p<-ggplot(tmp, 
          aes(x = NOME_MUNIC, y = valor, label=NOME_MUNIC))+
          labs(x = nm_meso, y = "") +
          geom_bar(stat="identity", aes(fill=tmp$seca),width=.5)+
          scale_fill_manual(name=this_time, 
                            labels = c("< 54 mm/mês", "> 54 mm/mês"), 
                            values = c("acima"="#00ba38", "abaixo"="#FDAE61")) + 
          labs(subtitle="") + 
          coord_flip()+ theme_minimal() 
          p        
              });

      
      
#Grafico na aba
      output$plot_g1 <- renderDygraph({
        
        dataset <- datasetInput();

        if(is.null(var_munic_sel)|is.null(dataset)){
          return()
        }
        
      
        
        precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,
                                                as.character(c(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12")))]), 
                         start = c(as.numeric(input$cod_ano),01), end = c(as.numeric(input$cod_ano),12), frequency = 12);#warning(call. = T);
        
        
        
        Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel],
                    start = c(as.numeric(input$cod_ano),01), end = c(as.numeric(input$cod_ano),12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
        
        dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&substr(tab6$dt_portaria,1,4)==as.character(input$cod_ano)&!is.na(tab6$dt_portaria)];

        lungDeaths <- cbind(precipitação, Saúde);
        
        if(length(dt.decretos)==0){    
        #  print(paste("if 0"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4));warning();
          #print(lungDeaths);
        }

        if(length(dt.decretos)==1){    
          #print(paste("if 1"))
          g1<- dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos,"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
        }

        if(length(dt.decretos)==2){    
          #print(paste("if 2"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
        }

        if(length(dt.decretos)==3){    
          #print(paste("if 3"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
        }
        
        if(length(dt.decretos)==4){    
          #print(paste("if 3"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom");
          
        }

        if(length(dt.decretos)==5){    
          #print(paste("if 3"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom");
          
        }

        if(length(dt.decretos)==6){
          #print(paste("if 3"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom");
          
        }
        
        if(length(dt.decretos)==7){
          #print(paste("if 3"))
          g1<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "Precipitação") %>%
            dyAxis("y2", label = "Taxa") %>%
            dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
            dyLimit(as.numeric(54), color = "red") %>%
            dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
            dyOptions(stackedGraph = F) %>%
            dyRangeSelector(height = 30)%>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom");
          
        }
        
        return(g1);
      });
      
      output$plot_g2<-renderDygraph({
    
        dataset<-get(levels(desc_tab$tab[desc_tab$nome==input$dataset]));
        
      #  var_munic_sel<-261110
      #  dataset<-tab3[tab3$cod_munic==var_munic_sel&tab3$fx_etaria==4,]
        
        if(is.null(var_munic_sel)|is.null(dataset)){
          return()
        }
        options(digits=10)
        
        precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,-1]), 
                         start = c(2001,01), end = c(2015,12), frequency = 12);#warning(call. = T);
        
        ndvi<-ts(data = as.numeric(tab7$valor[tab7$cod6==var_munic_sel]), 
                 start = c(2003,01), end = c(2015,12), frequency = 12)
        
        Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel],
                    start = c(2001,01), end = c(2015,12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
        
        dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
        
        lungDeaths <- cbind(precipitação, Saúde, ndvi);
        
        if(length(dt.decretos)==0){    
          g2<-dygraph(lungDeaths) %>%
          #  dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyLimit(as.numeric(54), color = "red")
        }
        
        if(length(dt.decretos)==1){    
          g2<-dygraph(lungDeaths) %>%
     #       dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
        }
        
        if(length(dt.decretos)==2){    
          g2<-dygraph(lungDeaths) %>%
       #     dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
        }
        
        if(length(dt.decretos)==3){    
          g2<-dygraph(lungDeaths) %>%
      #      dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
        }
        
        if(length(dt.decretos)==4){    
          g2<-dygraph(lungDeaths) %>%
         #   dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==5){    
          g2<-dygraph(lungDeaths) %>%
         #   dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==6){
          g2<-dygraph(lungDeaths) %>%
          #  dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==7){
          g2<-dygraph(lungDeaths) %>%
            #dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }

        if(length(dt.decretos)==8){
          #print(paste("if 3"))
          g2<-dygraph(lungDeaths) %>%
          #  dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==9){
 
           g2<-dygraph(lungDeaths) %>%
            #dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }

        if(length(dt.decretos)==10){
          g2<-dygraph(lungDeaths) %>%
            #dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==11){
          g2<-dygraph(lungDeaths) %>%
            #dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
            
        }
        
        if(length(dt.decretos)==12){
          #print(paste("if 3"))
          g2<-dygraph(lungDeaths) %>%
            #dyAxis("y", label = "Precipitação") %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==13){
          g2<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }

        if(length(dt.decretos)==14){
          g2<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "log 10", logscale = T) %>%
       #     dyAxis("y2", label = "log10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
        if(length(dt.decretos)==15){
          g2<-dygraph(lungDeaths) %>%
            dyAxis("y", label = "log 10", logscale = T) %>%
            dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyEvent(dt.decretos[15],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
            dyLimit(as.numeric(54), color = "red")
          
        }
        
#        print(length(dt.decretos))
        
        return(g2);
      });
      
      output$plot_g3<-renderPlot({
        
        dataset<-get(levels(desc_tab$tab[desc_tab$nome==input$dataset]));

        #  dataset<-tab4[tab4$cod_munic==var_munic_sel&tab4$fx_etaria==4,]
        if(is.null(var_munic_sel)|is.null(dataset)){
          return()
        }
        
        precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,-1]), 
                         start = c(2001,01), end = c(2015,12), frequency = 12);#warning(call. = T);
        
        ndvi<-ts(data = as.numeric(tab7$valor[tab7$cod6==var_munic_sel]), 
                 start = c(2003,01), end = c(2015,12), frequency = 12)
        
        Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel],
                    start = c(2001,01), end = c(2015,12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
        
        dt.decretos <- tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
        dt.decretos <- format(as.Date(dt.decretos), format = "%b %Y");
        
        
        lungDeaths <- cbind(precipitação, Saúde, ndvi);
        lungDeaths <- as.data.frame(lungDeaths);
        
        serie1<-format(seq(from = as.Date("2001-01-01"), 
                           to = as.Date("2015-12-01"), 
                           by = "month"), format("%b %Y"));
        
        lungDeaths$tempo<-serie1;
        
        lungDeaths$seca<-FALSE;
        library(caret)
        
        ##COLOCANDO A VARIAVEL DE DECRETO NA BASE
        for(i in 1:length(dt.decretos)){
          lungDeaths$seca[which(lungDeaths$tempo==dt.decretos[i])]<-TRUE
        }

        lungDeaths$seca<-as.factor(lungDeaths$seca)
        
        lungDeaths<-na.omit(lungDeaths);
        
        #transparentTheme(trans = .4);
        
        featurePlot(x = lungDeaths[,c(1,2,3)], 
                    y = lungDeaths$seca, 
                    plot = "ellipse", labels = "", cols.use = c("yellow", "red"));
        
        })
      
      # Generate a summary of the dataset ----
      output$summary <- renderPrint({
        
        dataset <- datasetInput();
        if(is.null(var_munic_sel)|is.null(dataset)){
          return()
        }

        
        chuva<-as.numeric(tab1[tab1$cod6==var_munic_sel,
        as.character(c(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12")))]);
        
        agravo<-dataset$valor[dataset$cod_munic==var_munic_sel];

        t_cor<-NA;
        try(expr = t_cor<-cor.test(x = agravo,y = chuva, method = c("spearman"), 
                        conf.level = 0.95), silent = T);
        teste_l<-as.numeric(substr(t_cor$p.value,1,5))

        if(is.na(teste_l)){
          texto_resul<-paste("...");
        }else{
        texto_resul<-paste("Pela correlação de Spearman",
              "com nível de confiança de 95%",
              "obtem-se uma correlação de",
              substr(t_cor$estimate,1,5),"\ncom p-valor de",substr(t_cor$p.value,1,4),
              if(!teste_l <= 0.05){
                ", logo, sugeri-se o descarte de uma correlação significativa." }
              else{
                paste(", logo, sugeri-se que exista uma correlação significativa."
                )});
              }
        
        cat(texto_resul);
        
      })#FIM SUMMARY
      
      
      output$view <- renderTable({
        #head(datasetInput(), n = isolate(input$obs))
        dataset <- datasetInput()
        if(is.null(var_munic_sel)|is.null(dataset)){
          return();
        }
       # var_munic_sel=230730 
        #input$cod_ano<-2009
          p<-data.frame(dataset[dataset$cod_munic==var_munic_sel,c(1:7)],
                        as.numeric(tab1[tab1$cod6==var_munic_sel, 
                              as.character(c(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12")))]));
  names(p)<-c("ano" ,"mes" ,"sigla","nome_munic","valor" ,"cod_munic","fx_etaria","precipitacao")
          print(p);
          
      })

      
      
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          dataset <- datasetInput();
      #    data<-dataset[dataset$cod_munic==var_munic_sel,];
          p<-data.frame(dataset[dataset$cod_munic==var_munic_sel,c(1:7)],
                        as.numeric(tab1[tab1$cod6==var_munic_sel, 
                                        as.character(c(paste0(2009,"01"):paste0(2009,"12")))]));
          names(p)<-c("ano" ,"mes" ,"sigla","nome_munic","valor" ,"cod_munic","fx_etaria","precipitacao");
          write.csv(p, file);
        }
      )
    })
    
  })
  
  #Entrada pela aba grafico com todos os elementos graficos
  output$munic_sel_2 <- renderPrint({
  tmp<-NULL;
  #TESTO SE A VAriAVEL DE SELECAO VEIO DA ABA DE MAPAS OU ABA DE GRAFICOS
    tmp1<-substr(as.character(input$munic_sel),1,6);
    tmp2<-substr(as.character(input$munic_sel_2),1,6);
    if(tmp1!=""&tmp2==""){
      tmp<-tmp1;
      cat(input$munic_sel);
    }
    if(tmp1==""&tmp2!=""){
      tmp<-tmp2;
      cat(input$munic_sel_2);
    }
    
    if(tmp1!=""&tmp2!=""){
      tmp<-tmp2;
      cat(input$munic_sel_2);
    }
    
    if(is.null(tmp)){
      cat("...");
    }
    
    var_munic_sel<-tmp;


    
    #print(var_munic_sel);

    
    #Atualiza a posicao para selecao do municipio
    pos_munic_sel<-which(geo$cod6 == var_munic_sel);
    #print(paste("Teste aqui ", pos_munic_sel));
    
      
      
      
    ###Entrando pelo seletor
    #Grafico na aba
    output$plot_g1 <- renderDygraph({
      
      dataset <- datasetInput();

      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      

      precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,
                                              as.character(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12"))]), 
                       start = c(as.numeric(input$cod_ano),01), end = c(as.numeric(input$cod_ano),12), frequency = 12);#warning(call. = T);
      

      Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel],
                  start = c(as.numeric(input$cod_ano),01), end = c(as.numeric(input$cod_ano),12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
      
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&substr(tab6$dt_portaria,1,4)==as.character(input$cod_ano)&!is.na(tab6$dt_portaria)];
      
      lungDeaths <- cbind(precipitação, Saúde);
      
      if(length(dt.decretos)==0){    
        #  print(paste("if 0"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4));warning();
        #print(lungDeaths);
      }
      
      if(length(dt.decretos)==1){    
        #print(paste("if 1"))
        g1<- dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos,"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
      }
      
      if(length(dt.decretos)==2){    
        #print(paste("if 2"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
      }
      
      if(length(dt.decretos)==3){    
        #print(paste("if 3"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom");warning();
      }
      
      if(length(dt.decretos)==4){    
        #print(paste("if 3"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom");
        
      }
      
      if(length(dt.decretos)==5){    
        #print(paste("if 3"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom");
        
      }
      
      if(length(dt.decretos)==6){
        #print(paste("if 3"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom");
        
      }
      
      if(length(dt.decretos)==7){
        #print(paste("if 3"))
        g1<-dygraph(lungDeaths) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom");
        
      }
      
      return(g1);
    });
    
    output$plot_g2<-renderDygraph({
      
      dataset<-get(levels(desc_tab$tab[desc_tab$nome==input$dataset]));
      
      #  var_munic_sel<-261110
      #  dataset<-tab3[tab3$cod_munic==var_munic_sel&tab3$fx_etaria==4,]
      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      options(digits=10)
      
      precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,-1]), 
                       start = c(2001,01), end = c(2015,12), frequency = 12);#warning(call. = T);
      
      ndvi<-ts(data = as.numeric(tab7$valor[tab7$cod6==var_munic_sel]), 
               start = c(2003,01), end = c(2015,12), frequency = 12)
      
      Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel&dataset$fx_etaria==input$cod_idade],
                  start = c(2001,01), end = c(2015,12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
      
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      lungDeaths <- cbind(precipitação, Saúde, ndvi);
      
      if(length(dt.decretos)==0){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==1){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #       dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==2){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #     dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==3){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #      dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==4){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==5){    
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==6){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==7){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==8){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==9){
        
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==10){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==11){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==12){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==13){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          dyAxis("y", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==14){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          dyAxis("y", label = "log 10", logscale = T) %>%
          #     dyAxis("y2", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==15){
        g2<-dygraph(lungDeaths,group = "Series&Trend") %>%
          
          dyAxis("y", label = "log 10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[15],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      #        print(length(dt.decretos))
      
      return(g2);
    });

    #Chuva
    output$plot_g2.1<-renderDygraph({
      
      #  var_munic_sel<-261110
      #  dataset<-tab3[tab3$cod_munic==var_munic_sel&tab3$fx_etaria==4,]
      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      options(digits=10)
      
      precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,-1]), 
                       start = c(2001,01), end = c(2015,12), frequency = 12);#warning(call. = T);
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      lungDeaths <- cbind(precipitação);
      
      if(length(dt.decretos)==0){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==1){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #       dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==2){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #     dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==3){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #      dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
      }
      
      if(length(dt.decretos)==4){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==5){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==6){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==7){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==8){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==9){
        
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==10){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==11){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==12){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==13){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==14){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          #     dyAxis("y2", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      if(length(dt.decretos)==15){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "mm/mês", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[15],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(54), color = "red")
        
      }
      
      #        print(length(dt.decretos))
      
      return(g2);
    });
    #NDVI
    output$plot_g2.2<-renderDygraph({
      
      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      options(digits=10)
      
    
      ndvi<-ts(data = as.numeric(tab7$valor[tab7$cod6==var_munic_sel]), 
               start = c(2003,01), end = c(2015,12), frequency = 12)
      
      
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      lungDeaths <- cbind(ndvi);
      
      if(length(dt.decretos)==0){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
      }
      
      if(length(dt.decretos)==1){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #       dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==2){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #     dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==3){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #      dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
      }
      
      if(length(dt.decretos)==4){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==5){    
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==6){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==7){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==8){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==9){
        
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==10){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==11){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==12){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==13){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==14){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          #     dyAxis("y2", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
      }
      
      if(length(dt.decretos)==15){
        g2<-dygraph(lungDeaths, group = "Series&Trend") %>%
          
          dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
          
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[15],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyLimit(as.numeric(0.33), color = "red")%>%
          dyLimit(as.numeric(0.66), color = "green")
        
        
      }
      
      #        print(length(dt.decretos))
      
      return(g2);
    });
    #saúde
    output$plot_g2.3<-renderDygraph({
      
      dataset<-get(levels(desc_tab$tab[desc_tab$nome==input$dataset]));
      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      options(digits=10)
      
      Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel&dataset$fx_etaria==input$cod_idade],
                  start = c(2001,01), end = c(2015,12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
      
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      lungDeaths <- cbind(Saúde);

      if(length(dt.decretos)==0){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F)
        
      }
      
      if(length(dt.decretos)==1){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #       dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
      }
      
      if(length(dt.decretos)==2){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #     dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
      }
      
      if(length(dt.decretos)==3){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #      dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
      }
      
      if(length(dt.decretos)==4){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyOptions(colors = rgb(100,250,10))
        
      }
      
      if(length(dt.decretos)==5){    
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #   dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==6){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==7){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==8){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #  dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==9){
        
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4)) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==10){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==11){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
      }
      
      if(length(dt.decretos)==12){
        #print(paste("if 3"))
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          #dyAxis("y", label = "Precipitação") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==13){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
        
      }
      
      if(length(dt.decretos)==14){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          #     dyAxis("y2", label = "log10", logscale = T) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom")
        
      }
      
      if(length(dt.decretos)==15){
        g2<-dygraph(lungDeaths, main = input$dataset, group = "Series&Trend") %>%
          dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
          dyEvent(dt.decretos[1],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[2],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[3],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[4],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[5],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[6],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[7],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[8],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[9],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[10],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[11],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[12],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[13],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[14],"Decreto de seca ou estiagem", labelLoc = "bottom") %>%
          dyEvent(dt.decretos[15],"Decreto de seca ou estiagem", labelLoc = "bottom")

      }
      
      #        print(length(dt.decretos))
      
      return(g2);
    });

#Grafico de dispersão           
#    output$plot_g3<-renderPlot({
      
#      dataset<-get(levels(desc_tab$tab[desc_tab$nome==input$dataset]));
      
      #  dataset<-tab4[tab4$cod_munic==var_munic_sel&tab4$fx_etaria==4,]
#      if(is.null(var_munic_sel)|is.null(dataset)){
#        return()
#      }
      
#      precipitação<-ts(data = as.numeric(tab1[tab1$cod6==var_munic_sel,-1]), 
#                       start = c(2001,01), end = c(2015,12), frequency = 12);#warning(call. = T);
      
#      ndvi<-ts(data = as.numeric(tab7$valor[tab7$cod6==var_munic_sel]), 
#               start = c(2003,01), end = c(2015,12), frequency = 12)
      
 #     Saúde <- ts(data = dataset$valor[dataset$cod_munic==var_munic_sel],
  #                start = c(2001,01), end = c(2015,12), frequency = 12);#warning("Não existe dado para faixa etária selecionada...");
      
#      dt.decretos <- tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
#      dt.decretos <- format(as.Date(dt.decretos), format = "%b %Y");
      
      
##      lungDeaths <- cbind(precipitação, Saúde, ndvi);
#      lungDeaths <- as.data.frame(lungDeaths);
      
#      serie1<-format(seq(from = as.Date("2001-01-01"), 
#                         to = as.Date("2015-12-01"), 
#                         by = "month"), format("%b %Y"));
      
#      lungDeaths$tempo<-serie1;
      
 #     lungDeaths$seca<-FALSE;
#      library(caret)
      
      ##COLOCANDO A VARIAVEL DE DECRETO NA BASE
#      for(i in 1:length(dt.decretos)){
 #       lungDeaths$seca[which(lungDeaths$tempo==dt.decretos[i])]<-TRUE
#      }
      
 #     lungDeaths$seca<-as.factor(lungDeaths$seca)
      
#      lungDeaths<-na.omit(lungDeaths);
      
      #transparentTheme(trans = .4);
      
 #     featurePlot(x = lungDeaths[,c(1,2,3)], 
#                  y = lungDeaths$seca, 
 #                 plot = "ellipse", labels = "", cols.use = c("yellow", "red"));
      
  #  })
    
    # Generate a summary of the dataset ----
    
    dataset <- datasetInput();
    if(is.null(var_munic_sel)|is.null(dataset)){
      return()
    }
    
    
    chuva<-as.numeric(tab1[tab1$cod6==var_munic_sel,
                           as.character(c(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12")))]);
    
    agravo<-dataset$valor[dataset$cod_munic==var_munic_sel];
    
    t_cor<-NA;
    try(expr = t_cor<-cor.test(x = agravo,y = chuva, method = c("spearman"), 
                               conf.level = 0.95), silent = T);
    teste_l<-as.numeric(substr(t_cor$p.value,1,5))
    
    if(is.na(teste_l)){
      texto_resul<-paste("...");
    }else{
      texto_resul<-paste("Pela correlação de Spearman",
                         "com nível de confiança de 95%",
                         "obtem-se uma correlação de",
                         substr(t_cor$estimate,1,5),"\ncom p-valor de",substr(t_cor$p.value,1,4),
                         if(!teste_l <= 0.05){
                           ", logo, sugeri-se o descarte de uma correlação significativa." }
                         else{
                           paste(", logo, sugeri-se que exista uma correlação significativa."
                           )});
    }
    
    output$summary <- renderPrint({
      
      cat(texto_resul);
      
    })#FIM SUMMARY
    
    ## Audio summary
    observeEvent(req(datasetInput()), {
      
      message(texto_resul)
      
      audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
      get_audio_description(text = texto_resul, dest_file = audio_summary_file, pcdas_token = pcdas_token)
      
      output$summary_audio <- renderUI(
        tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA)
      )
    })
    
    
    output$view <- renderTable({
      #head(datasetInput(), n = isolate(input$obs))
      dataset <- datasetInput()
      if(is.null(var_munic_sel)|is.null(dataset)){
        return();
      }
      # var_munic_sel=230730 
      #input$cod_ano<-2009
      p<-data.frame(dataset[dataset$cod_munic==var_munic_sel,c(1:7)],
                    as.numeric(tab1[tab1$cod6==var_munic_sel, 
                                    as.character(c(paste0(input$cod_ano,"01"):paste0(input$cod_ano,"12")))]));
      names(p)<-c("ano" ,"mes" ,"sigla","nome_munic","valor" ,"cod_munic","fx_etaria","precipitacao")
      print(p);
      
    })
    
    
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        dataset <- datasetInput();
        #    data<-dataset[dataset$cod_munic==var_munic_sel,];
        p<-data.frame(dataset[dataset$cod_munic==var_munic_sel,c(1:7)],
                      as.numeric(tab1[tab1$cod6==var_munic_sel, 
                                      as.character(c(paste0(as.character(input$cod_ano),"01"):paste0(as.character(input$cod_ano),"12")))]));
        names(p)<-c("ano" ,"mes" ,"sigla","nome_munic","valor" ,"cod_munic","fx_etaria","precipitacao");
        write.csv(p, file);
      }
    )
  
  
    
  });#//FIM 
  
  
  
  output$histCentile <- renderPlot({
  
  #this_time<-paste0(as.character(input$cod_ano),mes$cod[which(mes$desc==as.character(input$cod_mes))]);
  this_time<-paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes));
  
  centileBreaks <- hist(plot = F, df_map[,c(this_time)], breaks = 20)$breaks;
  
  hist(df_map[,c(this_time)],
       breaks = centileBreaks,
       main = paste("Quantidade de municípios\n segundo precipitação em",mes$desc[as.numeric(substr(this_time,5,6))],"/",input$cod_ano),
       xlab = "mm/mês",
       ylab = "n",
       xlim = range(df_map[,this_time]),
       col = '#00DD00',
       border = 'white');
  abline(v = 54, col = "red")
})
  
  ##GRAFICO PIZZA
  output$scatterCollegeIncome <- renderPlot({
    #this_time<-paste0(as.character(input$cod_ano),mes$cod[which(mes$desc==as.character(input$cod_mes))]);
    this_time<-paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes));
 
    
    tmp<-c(1:1139);
    tmp<-ifelse(df_map[,this_time]>53,F,T);
    tmp<-c(table(tmp)[[1]], table(tmp)[[2]]);
    tmp<-(tmp/1139*100);
    tmp<-as.data.frame(tmp)
    tmp$Classe<-c("Em situação de normalidade","Em déficit hídrico")
    names(tmp)[1]<-c("Freq")
    
    
    bp<- ggplot(tmp, aes(x="", y=Freq, fill = Classe))+
              ggtitle(paste0("Proporção de municípios","\ncom precipitação inferior 54 mm/mês"))+
      scale_fill_manual(values=c("#D7191C", "#ABD9E9"))+
      geom_bar(width = 1, stat = "identity")
    
    
    pie <- bp + coord_polar("y", start=0)+ theme_minimal()
    pie
    
#    p10 <- ggplot(df_map, aes(x = SIGLA, y = df_map[,this_time])) + labs(x = "", y = "mm/mês")+
#      geom_boxplot(aes(x = SIGLA, y = df_map[,this_time]))+geom_abline(intercept = 54,color= "red")
    
      
#    p10 + theme_minimal()
    
  })


  datasetInput <- eventReactive(input$update, {
    
  
    switch(input$dataset,
           "Internação Diarréia e Gastroenterite Origem Infecção Presumível" = tab5[tab5$ano==as.character(input$cod_ano)&
                                                                                    tab5$fx_etaria==as.character(input$cod_idade),],
           "Taxa de internação por asma" = tab2[tab2$ano==as.character(input$cod_ano)&
                                                  tab2$fx_etaria==as.character(input$cod_idade),],
           "Taxa de internação por dengue" = tab4[tab4$ano==as.character(input$cod_ano)&
                                                    tab4$fx_etaria==as.character(input$cod_idade),],
           "Incidência de dengue clássico" = tab3[tab3$ano==as.character(input$cod_ano)&
                                                    tab3$fx_etaria==as.character(input$cod_idade),])
  }, ignoreNULL = FALSE);

}
