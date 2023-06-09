library(shiny)
library(ggplot2)
library(grid)
library(plyr)
library(dplyr)
library(magrittr)
library(feather)


all_cum_ind <- as.data.frame(
  feather::read_feather(path="ls_risk_data.feather"))
all_cum_ind_previous_cancer <- as.data.frame(
  feather::read_feather(path="ls_risk_data_subsequent.feather"))
organ_table <- as.data.frame(
  feather::read_feather(path="ls_organ.feather"))
ls_organ_precomputed <- as.data.frame(
  feather::read_feather(path="ls_organ_precomputed_2019_v2.feather"))

shinyServer(function(input, output, session){

  values <- reactiveValues()

  observe({
    values$lastLinkValue <- input$seeAbout[1] - 1
    if (input$seeAbout[1] != values$lastLinkValue && values$lastLinkValue >= 0){
      updateTabsetPanel(session, "mainNavbar", selected = "About")
    }
  })

  output$UI.Date <- renderText({
    Commentary.Date <- paste0(
      "<p style=\"font-size:80%\"><i>Last data update: December 3rd 2015</i></p>")
    return(Commentary.Date)
  })
  output$ctype <- renderPrint({ input$ctype })
  output$Age <- renderPrint({ input$Age })
  output$Age3 <- renderPrint({ input$Age3 })
  output$Sex3 <- renderPrint({ input$Sex3 })
  output$Sex4 <- renderPrint({ input$Sex4 })
  output$ctype3 <- renderPrint({ input$ctype3 })

  output$plot_incidence_gene <- renderPlot({

    df <- dplyr::filter(
      all_cum_ind,
      Cancer == input$ctype &
        Sex == input$Sex &
        Age >= input$Age &
        Gene == input$genecarrier)

    if(nrow(df) > 1){
      df$tmp <- rep(1,nrow(df))
      for (i in 2:nrow(df)) {
        df$tmp[i] <-
          df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
      }
      df$cum_risk_percent_dynamic <- (1 - df$tmp) * 100
    }
    else{
      df$cum_risk_percent_dynamic <- df$Annual_incidence_rate * 100
    }
    df$SEXGENE <- paste(df$Sex,df$Gene, sep=" - ")
    cancer_type <- paste('Any cancer type',tolower(input$Sex),sep = ' - ')
    if(input$ctype == 'CRC'){
      cancer_type <- paste('Colorectal cancer',tolower(input$Sex),sep = ' - ')
    }
    if(input$ctype == 'OVARIAN'){
      cancer_type <- paste('Ovarian cancer',tolower(input$Sex),sep = ' - ')

    }
    if(input$ctype == 'UGI'){
      cancer_type <- paste(
        'Gastric/small intestine/biliary tract/pancreas cancer',
        tolower(input$Sex),sep = ' - ')
    }
    if(input$ctype == 'URO'){
      cancer_type <- paste(
        'Urine bladder/kidney/ureter cancer',
        tolower(input$Sex),sep = ' - ')
    }
    if(input$ctype == 'END'){
      cancer_type <- paste(
        'Endometrial cancer',
        tolower(input$Sex),sep = ' - ')
    }

    pathogenic_variant <- paste0('path_',input$genecarrier)

    p <- ggplot(df, aes(x=Age,y=cum_risk_percent_dynamic,group=SEXGENE,colour=SEXGENE)) +
      geom_line(size=1) + geom_point(size=3) +
      theme_bw() +
      ggtitle(bquote(atop(bold(.(cancer_type)), italic(.(pathogenic_variant))))) +
      scale_x_continuous(breaks=seq(25,70,5),limits=c(25,70)) +
      scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
      scale_color_brewer(palette='Dark2') +
      theme(legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_line(
              colour = 'black', linewidth = 0.3, linetype = 'dashed'),
            panel.border = element_rect(size=1, colour = "black"),
            axis.text.x=element_text(family="Helvetica",size=14),
            axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5),
            axis.text.y=element_text(family="Helvetica",size=14),
            axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5),
            plot.margin = (unit(c(2, 0, 2, 2), "cm")),
            plot.title = element_text(family="Helvetica",size=14,vjust=2,hjust=0.5),
            legend.text = element_blank()) +
      ylab('Cumulative risk of first cancer (%)')

    print(p)
  }, height="auto",width="auto")


  output$plot_incidence_gene_organ_cancer <- renderPlot({

    df <- NULL
    if(input$ctype3 == '182' ||
       input$ctype3 == '174' ||
       input$ctype3 == '183'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex3)
    }
    if(input$ctype3 == 'PANCANCER' ||
       input$ctype3 == '153_154' ||
       input$ctype3 == '153' ||
       input$ctype3 == '154' ||
       input$ctype3 == '188' ||
       input$ctype3 == '189' ||
       input$ctype3 == '191' |
       input$ctype3 == '151_152_156_157'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex5)
    }
    if(input$ctype3 == '185'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex4)
    }
    if(input$genecarrier3 == 'PMS2'
       & !(input$ctype3 == '185' |
           input$ctype3 == '174' |
           input$ctype3 == '182' |
           input$ctype3 == '183')){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex6)
    }


    df$SEXGENE <- paste(df$Sex,df$Gene, sep=" - ")
    df$Age <- as.integer(df$Age_stop)
    cancer_type <- paste(unique(df$Organ),tolower(unique(df$Sex)),sep=' - ')
    if(unique(df$Sex) == 'BOTH'){
      cancer_type <- paste(
        unique(df$Organ),
        paste(tolower(unique(df$Sex)),"genders","combined",sep=" "),sep=' - ')
    }

    pathogenic_variant <- paste0('path_',input$genecarrier3)
    df_errorbars <- as.data.frame(
      dplyr::filter(df, Age == 25 |
                      Age == 30 |
                      Age == 35 |
                      Age == 40 |
                      Age == 45 |
                      Age == 50 |
                      Age == 55 |
                      Age == 60 |
                      Age == 65 |
                      Age == 70 |
                      Age == 75))


    p <- ggplot(df, aes(x=Age,y=cum_estimate,group=SEXGENE, colour=SEXGENE)) +
      geom_line(linewidth=1) + geom_point(size=3) +
      theme_bw() +
      ggtitle(bquote(atop(bold(.(cancer_type)), italic(.(pathogenic_variant))))) +
      geom_errorbar(
        aes(ymin = lower_limit, ymax = upper_limit),
        data = df_errorbars, width = 1, linewidth = 1.1) +
      scale_x_continuous(breaks=seq(25,75,5),limits=c(25,77)) +
      scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
      scale_color_brewer(palette='Dark2') +
      theme(legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_line(
              colour = 'black', linewidth = 0.3, linetype = 'dashed'),
            panel.border = element_rect(linewidth = 1, colour = "black"),
            axis.text.x=element_text(family="Helvetica",size=14),
            axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5),
            axis.text.y=element_text(family="Helvetica",size=14),
            axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5),
            plot.margin = (unit(c(2, 0, 2, 2), "cm")),
            plot.title = element_text(family="Helvetica",size=14,vjust=2,hjust=0.5),
            legend.text = element_blank()) +
      #legend.text=element_text(family="Helvetica",size=14)) +
      ylab('Cumulative risk of cancer (%)')

    print(p)
  }, height="auto",width="auto")


  output$plot_incidence_gene_subsequent_cancer <- renderPlot({

    df <- dplyr::filter(
      all_cum_ind_previous_cancer,
      Cancer == input$ctype2 &
        Age >= input$Age2 &
        Gene == input$genecarrier2)
    if(nrow(df) > 1){
      df$tmp <- rep(1,nrow(df))
      for (i in 2:nrow(df)) {
        df$tmp[i] <-
          df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
      }
      df$cum_risk_percent_dynamic <- (1 - df$tmp) * 100
    }
    else{
      df$cum_risk_percent_dynamic <- df$Annual_incidence_rate * 100
    }
    df$SEXGENE <- paste(df$Sex,df$Gene, sep=" - ")
    cancer_type <- paste('Any cancer type')
    if(input$ctype2 == 'CRC'){
      cancer_type <- paste('Colorectal cancer')
    }

    pathogenic_variant <- paste0('path_',input$genecarrier2)

    p <- ggplot(df, aes(x=Age,y=cum_risk_percent_dynamic,group=SEXGENE,colour=SEXGENE)) +
      geom_line(size=1) + geom_point(size=3) +
      theme_bw() +
      ggtitle(bquote(atop(bold(.(cancer_type)), italic(.(pathogenic_variant))))) +
      scale_x_continuous(breaks=seq(25,70,5),limits=c(25,70)) +
      scale_y_continuous(breaks=seq(0,100,10),limits=c(0,100)) +
      scale_color_brewer(palette='Dark2') +
      theme(legend.title = element_blank(),
            legend.position = "none",
            panel.grid.major = element_line(
              colour = 'black', linewidth = 0.3, linetype = 'dashed'),
            panel.border = element_rect(size=1, colour = "black"),
            axis.text.x=element_text(family="Helvetica",size=14),
            axis.title.x=element_text(family="Helvetica",size=14,vjust=-0.5),
            axis.text.y=element_text(family="Helvetica",size=14),
            axis.title.y=element_text(family="Helvetica",size=14,vjust=1.5),
            plot.margin = (unit(c(2, 0, 2, 2), "cm")),
            plot.title = element_text(family="Helvetica",size=14,vjust=2,hjust=0.5),
            legend.text = element_blank()) +
      #legend.text=element_text(family="Helvetica",size=14)) +
      ylab('Cumulative risk of subsequent cancer (%)')

    print(p)
  }, height="auto",width="auto")


  output$table_ALL <- renderTable({
    df <- as.data.frame(
      dplyr::filter(
        all_cum_ind,
        Cancer == input$ctype &
          Sex == input$Sex &
          Gene == input$genecarrier &
          Age >= input$Age))

    if(nrow(df) > 1){
      df$tmp <- rep(1,nrow(df))
      for (i in 2:nrow(df)) {
        df$tmp[i] <-
          df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
      }
      df$cum_risk_percent_dynamic <- as.integer(round((1 - df$tmp) * 100))
    }
    else{
      df$cum_risk_percent_dynamic <-
        as.integer(round(df$Annual_incidence_rate * 100))
    }

    df <- as.data.frame(
      dplyr::filter(df,
                    Age == 25 |
                      Age == 40 |
                      Age == 50 |
                      Age == 60 |
                      Age == 70))

    df <- df %>% dplyr::select(Age,Sex,Gene,cum_risk_percent_dynamic)
    df$Gene <- paste0('path_',df$Gene)
    df <- df %>% dplyr::select(Age,cum_risk_percent_dynamic)
    colnames(df) <- c('Age','Risk (%)')
    df

  },include.rownames=F)

  output$table_ALL2 <- renderTable({
    df <- as.data.frame(
      dplyr::filter(
        all_cum_ind_previous_cancer,
        Cancer == input$ctype2 &
          Gene == input$genecarrier2 &
          Age >= input$Age2))

    if(nrow(df) > 1){
      df$tmp <- rep(1,nrow(df))
      for (i in 2:nrow(df)) {
        df$tmp[i] <-
          df$tmp[i - 1] - (df$tmp[i - 1] * df$Annual_incidence_rate[i - 1])
      }
      df$cum_risk_percent_dynamic <-
        as.integer(round((1 - df$tmp) * 100))
    }
    else{
      df$cum_risk_percent_dynamic <-
        as.integer(round(df$Annual_incidence_rate * 100))
    }

    df <- as.data.frame(
      dplyr::filter(df, Age == 25 |
                      Age == 40 |
                      Age == 50 |
                      Age == 60 |
                      Age == 70))

    df <- df %>% dplyr::select(Age,Sex,Gene,cum_risk_percent_dynamic)
    df$Gene <- paste0('path_',df$Gene)
    df <- df %>% dplyr::select(Age,cum_risk_percent_dynamic)

    colnames(df) <- c('Age','Risk (%)')
    df

  },include.rownames=F)

  output$table_ALL3 <- renderTable({

    df <- NULL
    if(input$ctype3 == '182' ||
       input$ctype3 == '174' ||
       input$ctype3 == '183' ||
       input$ctype3 == '182_183'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex3)
    }
    if(input$ctype3 == '153_154' |
       input$ctype3 == 'PANCANCER' ||
       input$ctype3 == '191' ||
       input$ctype3 == '188' ||
       input$ctype3 == '189' ||
       input$ctype3 == '151_152_156_157'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex5)
      if(input$genecarrier3 == 'PMS2'){
        df <- dplyr::filter(
          ls_organ_precomputed,
          Cancer == input$ctype3 &
            Age_start == input$Age3 &
            Gene == input$genecarrier3 &
            Sex == input$Sex6)
      }
    }
    if(input$ctype3 == '185'){
      df <- dplyr::filter(
        ls_organ_precomputed,
        Cancer == input$ctype3 &
          Age_start == input$Age3 &
          Gene == input$genecarrier3 &
          Sex == input$Sex4)
    }

    df <- as.data.frame(
      dplyr::filter(df,
                    Age_stop == 25 |
                      Age_stop == 40 |
                      Age_stop == 50 |
                      Age_stop == 60 |
                      Age_stop == 70 |
                      Age_stop == 75))
    df$Age <- as.integer(df$Age_stop)
    df <- df %>% dplyr::select(Age,Sex,Gene,cum_estimate,lower_limit, upper_limit)
    df$cum_estimate <- round(df$cum_estimate, digits = 1)
    df$lower_limit <- round(df$lower_limit, digits = 1)
    df$upper_limit <- round(df$upper_limit, digits = 1)
    df$Gene <- paste0('path_',df$Gene)
    df$CI <- paste0("[",df$lower_limit," - ",df$upper_limit,"]")
    df <- df %>% dplyr::select(Age,cum_estimate,CI)

    colnames(df) <- c('Age','Risk (%)','95% Confidence interval')
    df

  })
})








