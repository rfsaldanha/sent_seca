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
      
      # Paleta original
      # leg <- qpal(df_map[,this_time]);
      
      # Paleta considerando todos os valores possíveis
      # prec_pal <- colorNumeric(palette = "BrBG", domain = unique(as.vector(as.matrix(tab1[,2:ncol(tab1)]))))
      # leg <- prec_pal(df_map[,this_time])
      
      # Paleta considerando a variaçào do ano selecionado
      prec_pal <- colorQuantile(palette = "PiYG", domain = unique(as.vector(as.matrix(tab1[,grepl(input$cod_ano, names(tab1))]))), n = 10)
      leg <- prec_pal(df_map[,this_time])
      
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
	      setView(lng = -35, lat = -10.33601, zoom = 6) %>%
        addLegend(
          position = 'topleft',
          # Cores paleta original
          # colors = c("#2C7BB6","#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C"),
          # Cores paleta PiYG
          colors = c("#1C8832","#96D557", "#FFFFB2", "#FA9E4F", "#CA0017"),
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
  
  ###----- pcdas ai - map -----
  
  # Map data
  map_data <- reactive({
    this_time <- paste0(input$cod_ano,ifelse(nchar(input$cod_mes)==1,paste0("0",input$cod_mes),input$cod_mes))
    df_map <- as.data.frame(this_map)
    res <- this_map[,c("cod6", this_time)]
    res <- sf::st_drop_geometry(res)
  })
  
  # Map description by AI
  map_descr_text <- reactive({
    
    data_to_ai <- merge(map_data(), subset(df_map, select = c("cod6", "NOME_MUNIC", "SIGLA")))
    data_to_ai[,2] <- round(data_to_ai[,2], 2)
    data_to_ai <- format(data_to_ai, decimal.mark = ",", nsmall = 2)
    
    
    tryCatch({
      res <- get_text_description(
        df = data_to_ai, 
        prompt = "Este arquivo json contêm dados de precipitação na região semiárida brasileiral. A variável `NOME_MUNIC` contêm os nomes dos municípios. A variável `SIGLA` contêm os nomes dos estados dos municípios. Escreva um parágrafo técnico em português sobre os dados, incluindo valores e coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.",
        pcdas_token = pcdas_token
      )
    }, error = function(e){
      res <- ""
    })
    
    res
  })
  
  observeEvent(map_descr_text(), {
    output$map_descr_ia <- renderUI({
      tagList(
        tags$html(markdown(map_descr_text())),
        actionButton("map_descr_audio", label = "Ouvir áudio transcrição"),
        tags$img(src = "image_IA_PCDaS.png", width = "170px")
      )
    })
  })
  
  observeEvent(input$map_descr_audio, {
    show_modal_spinner()
    
    tryCatch({
      
      audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
      get_audio_description(text = map_descr_text(), dest_file = audio_summary_file, pcdas_token = pcdas_token)
      
      remove_modal_spinner()
      
      showModal(modalDialog(
        title = "PCDaS IA",
        markdown(map_descr_text()),
        tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autoplay = NA, controls = NA),
        # tags$img(src = "logo-principal-hires.png"),
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
  
  ###----- static plots - map -----
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
  })
  
  
  #Renderização dos graficos pelo mapa
  observe({
    leafletProxy("map", data = this_map[,c("cod6", this_time)]) %>% clearPopups();
    event <- input$map_shape_click
    
    if (is.null(event)){return()}
      
    isolate({
      var_munic_sel <- event$id;
      
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
        
        dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&substr(tab6$dt_portaria,1,4)==as.character(input$cod_ano)&!is.na(tab6$dt_portaria)];
        
        plot_data <- plot_g1_data();
        
        g1<-dygraph(plot_data) %>%
          dyAxis("y", label = "Precipitação") %>%
          dyAxis("y2", label = "Taxa") %>%
          dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
          dyLimit(as.numeric(54), color = "red") %>%
          dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
          dyOptions(stackedGraph = F) %>%
          dyRangeSelector(height = 30)%>%
          dyHighlight(highlightSeriesOpts = list(strokeWidth = 4))
        
        if(length(dt.decretos)>0)
          for (i in 1:length(dt.decretos)){
            g1 <- g1 %>% dyEvent(dt.decretos[i],"Decreto de seca ou estiagem", labelLoc = "bottom")
          }
        
        return(g1);
      });
      
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

      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&substr(tab6$dt_portaria,1,4)==as.character(input$cod_ano)&!is.na(tab6$dt_portaria)];
      
      plot_data <- plot_g1_data();
      
      g1<-dygraph(plot_data) %>%
        dyAxis("y", label = "Precipitação") %>%
        dyAxis("y2", label = "Taxa") %>%
        dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)", axis = 'y') %>%
        dyLimit(as.numeric(54), color = "red") %>%
        dySeries("Saúde", label = paste(input$dataset), color = "rgb(213, 119, 86)", axis = 'y2') %>%
        dyOptions(stackedGraph = F) %>%
        dyRangeSelector(height = 30)%>%
        dyHighlight(highlightSeriesOpts = list(strokeWidth = 4))
      
      if(length(dt.decretos)>0)
        for (i in 1:length(dt.decretos)){
          g1 <- g1 %>% dyEvent(dt.decretos[i],"Decreto de seca ou estiagem", labelLoc = "bottom")
        }
      
      return(g1);
    });
    
    #Chuva
    output$plot_g2.1<-renderDygraph({
      
      #  var_munic_sel<-261110
      #  dataset<-tab3[tab3$cod_munic==var_munic_sel&tab3$fx_etaria==4,]
      
      if(is.null(var_munic_sel)){
        return()
      }
      options(digits=10)
      #browser()
      data <- tab1[tab1$cod6==var_munic_sel,-1]
      plot_data <- plot_g2.1_data()
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      g2<-dygraph(plot_data, group = "Series&Trend") %>%
        dyAxis("y", label = "mm/mês", logscale = F) %>%
        dyOptions(fillAlpha = 0.3) %>%
        dyLimit(as.numeric(54), color = "red") %>%
        dySeries("precipitação", label = "Precipitação",color = "rgb(35, 34, 131)",fillGraph = TRUE) %>%
        dySeries("trend", label = "Tendência (STL)",color = "blue", strokePattern = "dotted",strokeWidth = 3) %>%
        #dySeries("p$fitted", label = "SARIMA",color = "darkgreen") %>%
        dySeries(c("p$lower", "p$mean", "p$upper"), label = "Previsão (SARIMA)",color = "purple") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.4,
                    hideOnMouseOut = TRUE)

      for (i in 1:length(dt.decretos)){
        g2 <- g2 %>% dyEvent(dt.decretos[i],"Decreto de seca ou estiagem", labelLoc = "bottom")
      }
      
      out_values <- boxplot(t(data), plot=FALSE)$out
      out_i <- which(t(data)%in%out_values)
      out <- as.Date(paste0(colnames(data[,out_i]), "01"), format = "%Y%m%d")
      if(length(out)>0)
        for (i in 1:length(out)){
          g2 <- g2 %>% dyAnnotation(out[i], 
                                    text=as.character(round(out_values[i],0)),
                                    tooltip = "atípico", width = 30, series="Precipitação")
        }
      
      return(g2);
    });
    
    #NDVI
    output$plot_g2.2<-renderDygraph({
      
      
      if(is.null(var_munic_sel)){
        return()
      }
      options(digits=10)
      
      data <- tab7[tab7$cod6==var_munic_sel,-1]
      plot_data <- plot_g2.2_data()
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      #browser()
      
      g2<-dygraph(plot_data, group = "Series&Trend") %>%
        dyAxis("y", label = "Condição da vegetação (NDVI)", logscale = F) %>%
        dyOptions(fillAlpha = 0.3) %>%
        dyLimit(as.numeric(0.33), color = "red")%>%
        dyLimit(as.numeric(0.66), color = "green") %>%
        dySeries("ndvi", label = "NDVI",color = "forestgreen",fillGraph = TRUE) %>%
        dySeries("trend", label = "Tendência (STL)",color = "green", strokePattern = "dotted",strokeWidth = 3) %>%
        #dySeries("p$fitted", label = "SARIMA",color = "darkgreen") %>%
        dySeries(c("p$lower", "p$mean", "p$upper"), label = "Previsão (SARIMA)",color = "seagreen") %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.4,
                    hideOnMouseOut = TRUE)
      
      for (i in 1:length(dt.decretos)){
        g2 <- g2 %>% dyEvent(dt.decretos[i],"Decreto de seca ou estiagem", labelLoc = "bottom")
      }
      #browser()
      out_values <- boxplot(data$valor, plot=FALSE)$out
      out_i <- which(data$valor%in%out_values)
      out <- as.Date(paste0(data$tempo[out_i], "01"), format = "%Y%m%d")
      if(length(out)>0)
        for (i in 1:length(out)){
          g2 <- g2 %>% dyAnnotation(out[i], 
                                    text=as.character(round(out_values[i],0)),
                                    tooltip = "atípico", width = 30, series="NDVI")
        }
      
      return(g2);
    });
    #saúde
    output$plot_g2.3<-renderDygraph({
      dataset_str <- as.character(desc_tab$tab[desc_tab$nome==input$dataset])
      if(is.null(var_munic_sel)|length(dataset_str)==0){
        return()
      }
      options(digits=10)
      
      dataset <- get(dataset_str);
      
      data <- dataset[dataset$cod_munic==var_munic_sel&dataset$fx_etaria==input$cod_idade,]
      plot_data <- plot_g2.3_data()
      dt.decretos<-tab6$dt_portaria[tab6$cod6==var_munic_sel&!is.na(tab6$dt_portaria)];
      
      #browser()
      
      g2<-dygraph(plot_data, group = "Series&Trend") %>%
        dyAxis("y", label = "Taxa por 100 Mil", logscale = F) %>%
        dyAxis("x", valueRange = c(start, "2017-12-01")) %>%
        dyOptions(fillAlpha = 0.3) %>%
        dySeries("Saúde", label = "Saúde",color = "rgb(213, 119, 86)",fillGraph = TRUE) %>%
        dySeries("trend", label = "Tendência (STL)",color = "orange", strokePattern = "dotted",strokeWidth = 3) %>%
        dyHighlight(highlightCircleSize = 5, 
                    highlightSeriesBackgroundAlpha = 0.4,
                    hideOnMouseOut = TRUE)
      
      for (i in 1:length(dt.decretos)){
        g2 <- g2 %>% dyEvent(dt.decretos[i],"Decreto de seca ou estiagem", labelLoc = "bottom")
      }
      #browser()
      out_values <- boxplot(data$valor, plot=FALSE)$out
      out_i <- which(data$valor%in%out_values)
      out <- as.Date(paste0(data$anomes[out_i], "01"), format = "%Y%m%d")
      if(length(out)>0)
        for (i in 1:length(out)){
          g2 <- g2 %>% dyAnnotation(out[i], 
                                    text=as.character(round(out_values[i],0)),
                                    tooltip = "atípico", width = 30, series="Saúde")
        }
      
      return(g2);
    });
    
    plot_g1_data <- reactive({
      
      dataset <- datasetInput();
      
      if(is.null(var_munic_sel)|is.null(dataset)){
        return()
      }
      
      mes_i <- paste0(input$cod_ano,"01")
      mes_f <- paste0(input$cod_ano,"12")
      start <- c(paste(stringr::str_split(mes_i,"",simplify=TRUE)[1:4], collapse = ''),
                 paste(stringr::str_split(mes_i,"",simplify=TRUE)[5:6], collapse = ''))
      end <- c(paste(stringr::str_split(mes_f,"",simplify=TRUE)[1:4], collapse = ''),
               paste(stringr::str_split(mes_f,"",simplify=TRUE)[5:6], collapse = ''))
      
      data <- tab1[tab1$cod6==var_munic_sel,as.character(c(mes_i:mes_f))]
      precipitação<-ts(data = as.numeric(data), start = start, end = end, frequency = 12);
      
      data <- dataset[dataset$cod_munic==var_munic_sel,]
      Saúde <- ts(data = as.numeric(data$valor), start = start, end = end, frequency = 12);
      
      plot_data <- cbind(precipitação, Saúde);
      plot_data
    })
    
    plot_g2.1_data <- reactive({
      if(is.null(var_munic_sel)){
        return()
      }
      options(digits=10)
      
      mes_i <- colnames(tab1[2])
      mes_f <- colnames(tab1[length(tab1)])
      start <- c(paste(stringr::str_split(mes_i,"",simplify=TRUE)[1:4], collapse = ''),
                 paste(stringr::str_split(mes_i,"",simplify=TRUE)[5:6], collapse = ''))
      end <- c(paste(stringr::str_split(mes_f,"",simplify=TRUE)[1:4], collapse = ''),
               paste(stringr::str_split(mes_f,"",simplify=TRUE)[5:6], collapse = ''))
      
      data <- tab1[tab1$cod6==var_munic_sel,-1]
      
      precipitação<-ts(data = as.numeric(data), start = start, end = end, frequency = 12);
      
      plot_data <- cbind(precipitação);
      
      decomp <- stl(precipitação, s.window = "periodic")
      #plot(decomp)
      trend <- decomp$time.series[, "trend"]
      a <- forecast::auto.arima(precipitação, seasonal = T)
      p <- forecast::forecast(a, h = 24, level=95)
      p$lower[p$lower<0] <- 0
      plot_data <- cbind(precipitação, trend,p$mean,p$lower,p$upper)#,p$fitted
      plot_data
    })
    
    plot_g2.2_data <- reactive({
      if(is.null(var_munic_sel)){
        return()
      }
      options(digits=10)
      
      data <- tab7[tab7$cod6==var_munic_sel,-1]
      
      mes_i <- min(data$tempo)
      mes_f <- max(data$tempo)
      start <- c(paste(stringr::str_split(mes_i,"",simplify=TRUE)[1:4], collapse = ''),
                 paste(stringr::str_split(mes_i,"",simplify=TRUE)[5:6], collapse = ''))
      end <- c(paste(stringr::str_split(mes_f,"",simplify=TRUE)[1:4], collapse = ''),
               paste(stringr::str_split(mes_f,"",simplify=TRUE)[5:6], collapse = ''))
      
      
      ndvi <-ts(data = as.numeric(data$valor), start = start, end = end, frequency = 12);
      
      plot_data <- cbind(ndvi);
      
      decomp <- stl(ndvi, s.window = "periodic")
      #plot(decomp)
      trend <- decomp$time.series[, "trend"]
      a <- forecast::auto.arima(ndvi, seasonal = T)
      p <- forecast::forecast(a, h = 24, level=95)
      p$lower[p$lower<(-0.1)] <- -0.1
      plot_data <- cbind(ndvi, trend,p$mean,p$lower,p$upper)#,p$fitted
      plot_data
    })
    
    plot_g2.3_data <- reactive({
      
      dataset_str <- as.character(desc_tab$tab[desc_tab$nome==input$dataset])
      if(is.null(var_munic_sel)|length(dataset_str)==0){
        return(NULL)
      }
      options(digits=10)
      
      dataset <- get(dataset_str);
      
      data <- dataset[dataset$cod_munic==var_munic_sel&dataset$fx_etaria==input$cod_idade,]
      
      mes_i <- min(data$anomes)
      mes_f <- max(data$anomes)
      start <- c(paste(stringr::str_split(mes_i,"",simplify=TRUE)[1:4], collapse = ''),
                 paste(stringr::str_split(mes_i,"",simplify=TRUE)[5:6], collapse = ''))
      end <- c(paste(stringr::str_split(mes_f,"",simplify=TRUE)[1:4], collapse = ''),
               paste(stringr::str_split(mes_f,"",simplify=TRUE)[5:6], collapse = ''))
      
      Saúde <- ts(data = as.numeric(data$valor), start = start, end = end, frequency = 12);      
      
      plot_data <- cbind(Saúde);
      
      decomp <- stl(Saúde, s.window = "periodic")
      #plot(decomp)
      trend <- decomp$time.series[, "trend"]
      plot_data <- cbind(Saúde, trend)#,p$fitted
      plot_data
    })
    
    plot_g1_descr_text <- reactive({
      plot_data <- plot_g1_data()
      
      if(is.null(plot_data)) return()
      
      #ts to data.frame
      df_ts <- data.frame(plot_data)
      df_ts$date <- as.numeric(time(plot_data))
      df_ts$date <- as.Date(paste0(floor(df_ts$date), "-", 
                                   sprintf("%02d", 1+round((df_ts$date-floor(df_ts$date))*12)), "-01"))
      #browser()
      data_to_ai <- cbind(df_ts,
                          as.data.frame(geo)[geo$cod6==var_munic_sel,c("cod6", "NOME_MUNIC", "SIGLA")])
      data_to_ai[,1] <- round(data_to_ai[,1], 2)
      data_to_ai[,2] <- round(data_to_ai[,2], 2)
      data_to_ai <- format(data_to_ai, decimal.mark = ",", nsmall = 2)
      #write.csv(data_to_ai,"data_to_ai.csv")
      
      #browser()
      prompt<- paste0("Este arquivo csv contém dados de precipitação e de indicador de saúde relacionado a ",input$dataset)
      prompt<- paste0(prompt," de um município na região semiárida brasileira em um determinado ano. A variável `NOME_MUNIC` contêm o nome do município. A variável `SIGLA` contém o nome do estado do município. A variável `precipitação` contém os valores de precipitação para o município ao longo dos meses contidos na variável `date`. A variável `Saúde` contém os valores do indicador de saúde para o município ao longo dos meses contidos na variável `date`. Escreva um parágrafo técnico em português do Brasil sobre os dados, incluindo informações sobre máximo, mínimo e média dos valores. Coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.")
      tryCatch({
        res <- get_text_description(
          df = data_to_ai, 
          prompt = prompt,
          pcdas_token = pcdas_token
        )
      }, error = function(e){
        res <- ""
      })
      
      t_cor<-NA;
      try(expr = t_cor<-cor.test(x = df_ts$Saúde,y = df_ts$precipitação, method = c("spearman"), 
                                 conf.level = 0.95), silent = T);
      teste_l<-as.numeric(substr(t_cor$p.value,1,5))
      
      if(is.na(teste_l)){
        texto_resul<-paste("");
      }else{
        texto_resul<-paste(" Pela correlação de Spearman",
                           "com nível de confiança de 95%",
                           "obtem-se uma correlação entre os dois indicadores de",
                           substr(t_cor$estimate,1,5),"\ncom p-valor de",substr(t_cor$p.value,1,4),
                           if(!teste_l <= 0.05){
                             ", logo, sugere-se o descarte de uma correlação significativa." }
                           else{
                             paste(", logo, sugere-se que exista uma correlação significativa."
                             )})
      }
      res <- paste0(res,texto_resul)
      res
    })
    
    observeEvent(plot_g1_descr_text(), {
      #browser()
      texto_resul <- plot_g1_descr_text()
      output$plot_g1_descr_ia <- renderUI({
        audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
        get_audio_description(text = texto_resul, dest_file = audio_summary_file, pcdas_token = pcdas_token)
        
        tagList(
          tags$html(markdown(texto_resul)),
          output$summary_audio <- renderUI(
            tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA)
          ),
          helpText("PCDaS IA")
        )
      })
    })
    
    # Plot description by AI
    plot_g2.1_descr_text <- reactive({
      
      plot_data <- plot_g2.1_data()
      if(is.null(plot_data)) return()
      
      #ts to data.frame
      df_ts <- data.frame(plot_data)
      df_ts$date <- as.numeric(time(plot_data))
      df_ts$date <- as.Date(paste0(floor(df_ts$date), "-", 
                                  sprintf("%02d", 1+round((df_ts$date-floor(df_ts$date))*12)), "-01"))
      
      data_to_ai <- cbind(df_ts,
                          as.data.frame(geo)[geo$cod6==var_munic_sel,c("cod6", "NOME_MUNIC", "SIGLA")])
      data_to_ai[,1] <- round(data_to_ai[,1], 2)
      data_to_ai[,2] <- round(data_to_ai[,2], 2)
      data_to_ai <- format(data_to_ai, decimal.mark = ",", nsmall = 2)
      #write.csv(data_to_ai,"data_to_ai.csv")
      
      tryCatch({
        res <- get_text_description(
          df = data_to_ai, 
          prompt = "Este arquivo csv contém dados de precipitação de um município na região semiárida brasileira. A variável `NOME_MUNIC` contêm o nome do município. A variável `SIGLA` contém o nome do estado do município. A variável `precipitação` contém os valores de precipitação para o município ao longo dos meses contidos na variável `date`. A variável `trend` contém os valores de tendência obtidos pelo método de STL. As variáveis `p$mean` contém os valores de precipitação previstos para os próximos dois anos obtidos pelo modelo SARIMA. Escreva um parágrafo técnico em português do Brasil sobre os dados, incluindo informações sobre a sazonalidade, valores atípicos em determinados meses, máximo, mínimo e média dos valores de precipitação, tendência de longo-prazo crescente ou decrescente, assim como a previsão para os próximos anos, incluindo máximos. Mencione os métodos e modelos utilizados. Coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.",
          pcdas_token = pcdas_token
        )
      }, error = function(e){
        res <- ""
      })
      
      res
    })
    
    observeEvent(plot_g2.1_descr_text(), {
      #browser()
      texto_resul <- plot_g2.1_descr_text()
      output$plot_g2.1_descr_ia <- renderUI({
        audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
        get_audio_description(text = texto_resul, dest_file = audio_summary_file, pcdas_token = pcdas_token)
        
        tagList(
          tags$html(markdown(texto_resul)),
          output$summary_audio <- renderUI(
            tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA)
          ),
          helpText("PCDaS IA")
        )
      })
    })
    
    # Plot description by AI
    plot_g2.2_descr_text <- reactive({
      
      plot_data <- plot_g2.2_data()
      if(is.null(plot_data)) return()
      #browser()
      #ts to data.frame
      df_ts <- data.frame(plot_data)
      df_ts$date <- as.numeric(time(plot_data))
      df_ts$date <- as.Date(paste0(floor(df_ts$date), "-", 
                                   sprintf("%02d", 1+round((df_ts$date-floor(df_ts$date))*12)), "-01"))
      
      data_to_ai <- cbind(df_ts,
                          as.data.frame(geo)[geo$cod6==var_munic_sel,c("cod6", "NOME_MUNIC", "SIGLA")])
      data_to_ai[,1] <- round(data_to_ai[,1], 2)
      data_to_ai[,2] <- round(data_to_ai[,2], 2)
      data_to_ai <- format(data_to_ai, decimal.mark = ",", nsmall = 2)
      #write.csv(data_to_ai,"data_to_ai.csv")
      
      tryCatch({
        res <- get_text_description(
          df = data_to_ai, 
          prompt = "Este arquivo csv contém dados de NDVI de um município na região semiárida brasileira. O indicador de NDVI é um índice de estado da vegetação e indica a produção primária (produção de clorofila) e umidade local por meio de um indicador numérico obtido por sensoriamento remoto. A variável `NOME_MUNIC` contêm o nome do município. A variável `SIGLA` contém o nome do estado do município. A variável `ndvi` contém os valores de NDVI para o município ao longo dos meses contidos na variável `date`. A variável `trend` contém os valores de tendência obtidos pelo método de STL. As variáveis `p$mean` contém os valores de NDVI previstos para os próximos dois anos obtidos pelo modelo SARIMA. Escreva um parágrafo técnico em português do Brasil sobre os dados, incluindo informações sobre a sazonalidade, valores atípicos em determinados meses, máximo, mínimo e média dos valores de NDVI, tendência de longo-prazo crescente ou decrescente, assim como a previsão para os próximos anos, incluindo máximos. Mencione os métodos e modelos utilizados. Coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.",
          pcdas_token = pcdas_token
        )
      }, error = function(e){
        res <- ""
      })
      
      res
    })
    
    observeEvent(plot_g2.2_descr_text(), {
      #browser()
      texto_resul <- plot_g2.2_descr_text()
      output$plot_g2.2_descr_ia <- renderUI({
        audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
        get_audio_description(text = texto_resul, dest_file = audio_summary_file, pcdas_token = pcdas_token)
        
        tagList(
          tags$html(markdown(texto_resul)),
          output$summary_audio <- renderUI(
            tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA)
          ),
          helpText("PCDaS IA")
        )
      })
    })
    
    plot_g2.3_descr_text <- reactive({
      plot_data <- plot_g2.3_data()
      
      if(is.null(plot_data)) return()
      
      #ts to data.frame
      df_ts <- data.frame(plot_data)
      df_ts$date <- as.numeric(time(plot_data))
      df_ts$date <- as.Date(paste0(floor(df_ts$date), "-", 
                                   sprintf("%02d", 1+round((df_ts$date-floor(df_ts$date))*12)), "-01"))
      
      data_to_ai <- cbind(df_ts,
                          as.data.frame(geo)[geo$cod6==var_munic_sel,c("cod6", "NOME_MUNIC", "SIGLA")])
      data_to_ai[,1] <- round(data_to_ai[,1], 2)
      data_to_ai[,2] <- round(data_to_ai[,2], 2)
      data_to_ai <- format(data_to_ai, decimal.mark = ",", nsmall = 2)
      #write.csv(data_to_ai,"data_to_ai.csv")
      
      #browser()
      prompt<- paste0("Este arquivo csv contém dados de indicador de saúde relacionado a ",input$dataset)
      prompt<- paste0(prompt," de um município na região semiárida brasileira. A variável `NOME_MUNIC` contêm o nome do município. A variável `SIGLA` contém o nome do estado do município. A variável `Saúde` contém os valores do indicador de saúde para o município ao longo dos meses contidos na variável `date`. A variável `trend` contém os valores de tendência obtidos pelo método de STL. Escreva um parágrafo técnico em português do Brasil sobre os dados, incluindo informações sobre a sazonalidade, valores atípicos em determinados meses, máximo, mínimo e média dos valores do indicador de saúde, assim como tendência de longo-prazo crescente ou decrescente. Mencione os métodos utilizados. Coloque em negrito os nomes dos municípios citados. Não mencione o nome do arquivo. Evite adjetivos como alarmante e preocupante.")
      tryCatch({
        res <- get_text_description(
          df = data_to_ai, 
          prompt = prompt,
          pcdas_token = pcdas_token
        )
      }, error = function(e){
        res <- ""
      })
      
      res
    })
    
    observeEvent(plot_g2.3_descr_text(), {
      #browser()
      texto_resul <- plot_g2.3_descr_text()
      output$plot_g2.3_descr_ia <- renderUI({
        audio_summary_file <- tempfile(tmpdir = "www", fileext = ".mp3")
        get_audio_description(text = texto_resul, dest_file = audio_summary_file, pcdas_token = pcdas_token)
        
        tagList(
          tags$html(markdown(texto_resul)),
          output$summary_audio <- renderUI(
            tags$audio(src = basename(audio_summary_file), type = "audio/mp3", autostart = "0", controls = NA)
          ),
          helpText("PCDaS IA")
        )
      })
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
