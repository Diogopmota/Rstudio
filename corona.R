#ufvjm
#alunos: Eduardo Nogueira / Diogo Passos 
#----------------------------------------
#verifique se os pacotes abaixo estão devidamente instalados
library(shiny)
library(shinythemes)
library (tidyverse)
library (lubridate)
library (DT)
library(plotly)

#cbamando o arquivo com os dados
file_data = "corona_data.csv"

#configurações globais
tb_data =read_delim(file_data , delim = ";"  , col_types = cols())  %>% 
    mutate ( last_updated_hour =  floor_date (last_updated , unit = "hour") )


tb_countries = tibble(Country = "Brasil")%>% 
                union_all (tb_data %>% 
                            arrange(desc (TotalCases))%>% 
                            distinct(Country))


last_updated = tb_data %>% 
               summarise(last_updated = max (last_updated_hour)) %>% 
               pull ()

interval_choices = c(Dia ="1 day" , Semana ="1 week" , Mes ="1 month")



# Definição do UI 
ui <- fluidPage(

    theme = shinytheme("sandstone") ,
    
    tags$head(
      tags$style(".well , .dataTables_wrapper {background-color: white; 
                        font-family: \"Trebuchet MS\",  Helvetica, sans-serif;
                        font-size: 14px}") 
      #,tags$style(".svg-container{background-color: black; border: 1px solid #777;
      #                                    font-family: Arial, Helvetica, sans-serif;}")  
      
    ) ,
    
    
    titlePanel("COVID-19 CORONAVIRUS NO BRASIL"),
    tags$p ("fonte: "
                , tags$a(href="https://covid.saude.gov.br/", "https://covid.saude.gov.br/" , target="_blank") ) ,
    

    
    textOutput("Atualização" ),

    sidebarLayout(
        sidebarPanel  (

          width = 2 ,
            selectInput("interval", label = "Intervalo" , choices = interval_choices  ) ,
            selectInput("countries", label = "Região", choices = tb_countries) ,
            tags$strong( tags$p ("Pior situação") ) ,
            plotOutput("gg_bar_current" ,click = "plot_click" ,  dblclick = "plot_dblclick" )
        ) ,
        mainPanel( width = 9 ,
                   fluidRow( 
                             plotlyOutput("gg_evolution") ,
                             #verbatimTextOutput("debug") ,                   
                             dataTableOutput("result")
                            )
                  )
    )   
)

# Definição do server
server <- function(input, output, session) {
    
    
    
    initial_date_range = reactive({ 
            tb_data %>%
            distinct(last_updated_hour) %>%
            mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
            distinct(datetime_range) %>%
            pull () %>%
            range ()
    } )
    
    
    
    
    
    tb_result =reactive(  {
          
                        req(input$interval)
                        
                        if (input$countries == "Brasil") {
                                
                                tb_data %>% 
                                arrange(desc (last_updated_hour))%>%
                                mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                select (-c(TotalRecovered,last_updated,last_updated_hour,ActiveCases ))%>% 
                                group_by( datetime_range , Country)%>% 
                                filter(row_number() == 1) %>%
                                group_by( datetime_range) %>%
                                summarise(  TotalCases = sum(TotalCases)
                                            , TotalDeaths = sum(TotalDeaths ,na.rm = T) )%>% 
                                ungroup()%>%
                                arrange (datetime_range)%>%
                                mutate (NewCases = TotalCases - lag(TotalCases) 
                                        , NewDeaths= TotalDeaths - lag(TotalDeaths) )}
                            
                        else {
                                  tb_data %>% 
                                  arrange(desc (last_updated_hour))%>%
                                  mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                  select (-c(TotalRecovered,last_updated,last_updated_hour,ActiveCases ))%>% 
                                  filter (Country %in% c(input$countries)) %>% 
                                  # filtter on barplot
                                  #filter ( Country %in% global$selected_country | global$selected_country == "*" ) %>% 
                                  group_by( datetime_range) %>%
                                  filter(row_number() == 1) %>%
                                  ungroup()%>%
                                  arrange (datetime_range)%>%
                                  mutate( NewCases = TotalCases - lag(TotalCases) 
                                          , NewDeaths= TotalDeaths - lag(TotalDeaths) 
                                  ) 
                        }
        
            }
    )
    
    tb_result_current = reactive (  {
        
        
                                    tb_data %>% 
                                    mutate (datetime_range = floor_date(last_updated_hour, unit = input$interval)) %>% 
                                    arrange(desc (last_updated_hour)) %>%
                                    group_by( Country ) %>%
                                    filter (row_number()==1)%>%
                                    arrange(desc(TotalCases))%>%
                                    head(20) }
                                    )
                                


    
    plotly_selected_date_range =    reactive( {
        
        tb=    event_data("plotly_relayout" , source = "plotly1" ) %>%
                as_tibble()
        
        if (!is_empty(tb) && str_detect( str_c( names(tb) , collapse = "/"), "xaxis.range") ) {
            
                tb %>%
                select (matches( "xaxis.range") )%>%
                pivot_longer(everything() , values_to = "date_range")%>%
                select (date_range) %>%
                pull ()%>%
                range ()
        }
        else {
            
            initial_date_range ()
        }  
    }
    )
    
    
    output$lastupdated = renderText( str_c("Ultima Atualização: " ,last_updated, collapse = T))
    
#painel do shiny------------------------------->
    output$result <- renderDataTable ({
        datatable (tb_result () %>%
                     rename("Casos" = "TotalCases")%>%
                     rename("Mortes" = "TotalDeaths")%>%
                     rename("Novas Mortes" = "NewDeaths")%>%
                     rename("Novos Casos" = "NewCases")%>%
                       filter( datetime_range >=  plotly_selected_date_range () [1] & datetime_range <=plotly_selected_date_range () [2] )%>%
                        rename(Data = datetime_range,  ),
                   
                    options = list(searching = FALSE ,pageLength = 5) )
      
    })
#Fim do painel shiny-------------------------->
#Delete-o para desaparecer ( falta traduzir dos botões do shiny)
    output$gg_evolution = renderPlotly (
        {
        

                
            interval = input$interval
            
            tick_format = case_when(
                interval == "1 day" ~ "%b %d" ,
                interval == "6 hours" ~ "%d" ,
                interval %in% c ("1 week" , "1 month") ~ "%b %d" ,
                
                TRUE ~ "%H" 
            )
            
            dtick = case_when(
                interval == "1 day" ~ 2* 86400000.0 , #  1 day = 86400000.0
                interval == "6 hours" ~ 86400000.0  , # 1 hr = 3600000 miliseconds
                interval == "1 week" ~ 7* 86400000.0  , # 1 hr = 3600000 miliseconds
                interval == "1 month" ~ 15* 86400000.0  , 
                TRUE ~ 2* 86400000.0
            )
# acho que o plots só interpreta em milessimos          
            
            x_title = case_when(
                interval == "1 day" ~ "Date" ,
                interval == "6 hours" ~ "Hour" ,
                TRUE ~ "Date" 
            )

            
            curr_totals =tb_result () %>% 
                            arrange(desc (datetime_range)) %>% 
                            head(1)%>% 
                            select (TotalCases, TotalDeaths)
  
            title = str_c(input$countries , 
                          " Total De Casos: " , curr_totals$TotalCases, 
                          " Total De Mortes: " ,curr_totals$TotalDeaths, collapse = T)
            
            tb_result () %>% 
                plot_ly( x= ~datetime_range , mode = "lines" , source = "plotly1" )  %>% 
                add_lines(y= ~TotalCases , name = "Total Casos" ,color = I ("#044289") )%>% 
                add_lines(y= ~TotalDeaths  , name = "Total Mortes",  yaxis = "y2" , color = I ("#d73a49") )%>% 
                add_text(text = ~NewCases ,y= ~TotalCases ,textposition='top center' , showlegend =T 
                         ,hovertemplate  = str_c ("Total Cases: %{y}" , "New Cases: %{text}" , "%{x}", sep = "<br>" )
                         ,color = I ("#044289") , name ="Novos Casos"  )%>% 
                add_text(text = ~NewDeaths , y= ~TotalDeaths , yaxis = "y2",textposition='bottom center' 
                         ,  showlegend = T, name ="Novas Mortes" 
                         , hovertemplate  = str_c ("Total Deaths: %{y}" , "New Deaths: %{text}", "%{x}" , sep = "<br>" )
                         , color = I ("#d73a49")
                )%>% 
                layout(yaxis2 = list(
                    overlaying = "y",
                    side = "right",
                    title = "Total Mortes" ,
                    anchor = "free" ,
                    position = 0.95
                    
                ),
                xaxis = list(title = "Data" 
                             , type = "date"
                             , tick0 = ~ min (datetime_range)  #tick0
                             , tickformat = tick_format
                             , dtick =dtick ), # dtick in miliseconds
                yaxis = list(title = "Total Casos" , zeroline = F),
                margin = list(b = 100) ,
                legend = list(x = 0.1, y = 0.9) ,
                title = title ,
                titlefont = list (family = "Trebuchet MS, sans-serif" 
                                  , size = 18
                                  #, color = "#e1e4e8"
                                  )
                ) %>% 
                config(  displaylogo = FALSE ,  modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d" , "zoom3d", "zoomInGeo" , "zoomOutGeo"
                                                                           ,"toImage" , "zoomInMapbox" , "zoomOutMapbox"
                                                                           , "sendDataToCloud" , "pan2d" , "lasso2d" , "select2d"
                                                                           , "hoverClosestCartesian", "hoverCompareCartesian"
                                                                           , "toggleHover" , "toggleSpikelines"))
            
        
        }
    )
    
    output$debug = renderPrint(
                                {  plotly_selected_date_range ()
                                })
    
    output$gg_bar_current = renderPlot( {
        
                                           curr_total = max (tb_result_current()$TotalCases)
                                        
                                            tb_result_current ()  %>% 
                                            #mutate(selected = ifelse(Country %in% global$selected_country | global$selected_country == "*", "yes" , "no" ) )%>%
                                            # ggplot(aes ( reorder (Country , TotalCases) ,  TotalCases , fill=selected)) +
                                            ggplot(aes ( reorder (Country , TotalCases) ,  TotalCases) )+
                                            geom_bar(stat = "Identity") +
                                            scale_y_continuous(breaks  = NULL) +
                                            #scale_fill_manual(values = c("yes" = "#3e3f3a", "no" = "grey" ), guide = FALSE )+
                                            labs (x=NULL , y=NULL) +
                                            coord_flip()+
                                            theme_minimal() +
                                            theme(panel.grid.major = element_blank() ,
                                                  panel.grid.minor = element_blank()) 
                                            
        
                                        }
                                      )
    
    
}

#App
shinyApp(ui, server)
