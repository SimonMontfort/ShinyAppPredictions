library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
library(tidymodels)
# library(tidyverse)
# library(stringr)
# library(plotly)
# library(DALEXtra)
# library(DALEX)
# library(ranger)
# library(caret)
# - for logs check:  rsconnect::showLogs("/Volumes/Transcend/Uni/doktorat/Predicting Core Beliefs/ShinyApp/PredictingVote/new_app_1.R")


model_rate <- readRDS("model_forest_rate.rds")
model_rate_activism <- readRDS("model_forest_rate_activism.rds")
h <- "10em"

# function to predict the probability 
predict_probability <- function(model, dat){
    stats::predict(model, dat, type = "prob") %>%
        tidyr::gather() %>%
        dplyr::mutate(value = as.numeric(value)) 
}

# # Define UI for application that draws a histogram
# # Define UI for app that draws a histogram ----
# ui <- fluidPage(
#     
#     # App title ----
#     titlePanel("Auswertung der Umfrageergebnisse zur Abstimmung des CO2-Gesetz vom 13.06.21"),
#     
#     # Sidebar layout with input and output definitions ----
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
# 
#             selectInput("pol_party", label = "Welche Partei entspricht in den Zielen und Forderungen am ehesten Ihren eigenen 
#                             Ansichten und Wünschen?",
#                         choices = c("SVP (Schweizerische Volkspartei)", 
#                                     "SP (Sozialdemokratische Partei)", 
#                                     "FDP.Die Liberalen (Freisinnig Demokratische Partei)", 
#                                     "CVP (Christlichdemokratische Volkspartei)",
#                                     "GPS (Grüne Partei Schweiz)",
#                                     "GLP (Grünliberale Partei)",
#                                     "BDP (Bürgerlich Demokratische Partei)",
#                                     "EVP (Evangelische Volkspartei der Schweiz)",
#                                     "Lega dei Ticinesi",
#                                     "PdA (Partei der Arbeit Schweiz)",
#                                     "MCG (Mouvement Citoyens Genevois)",
#                                     "CSP (Christlichsoziale Partei Schweiz)",
#                                     "EDU (Eidgenössisch-Demokratische Union)",
#                                     "Sol. (SolidaritéS)",
#                                     "Andere:",
#                                     "Keine",
#                                     "Weiss nicht / keine Antwort")
#             ),
#             
#         ),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             plotOutput(outputId = "Plot1", height = "200px")
#             # ,plotOutput(outputId = "Plot2", height = "200px")
#         )
#     )
# )

ui <- fluidPage(
  tags$head(
    tags$style("
               #my_container {
                 display: table;
                 width: 100%;
               }

               .well {
                 background-color: #FFFFFF;
                 border-color: #FFFFFF;
               }

               #col_left {
                 background-color: #FFFFFF;
               }

               #col_right {
                 background-color: #faf;
               }
               "
    ), 
  ),
  sidebarPanel(
    # conditionalPanel(
    #   'input.navbar == "panel1"',
    #   HTML("Dieses interaktive Tool dient zur Veranschaulichung der Resultate einer repräsentativen Umfrage zur Schweizer 
    #         Klimapolitk, welche im Rahmen der Abstimmung zum CO2-Gesetz vom 13. Juni 2021 durchgeführt wurde. An Hand verschiedener 
    #         persönlicher Attribute lässt sich vorhersagen, was die Zustimmung zu ambitionierter Klimapolitik und die politische Mobilisierung an Wahlen 
    #         teilzunehmen am damals zur Abstimmung stehenden CO2-Gesetz ist. 
    #         
    #         \n\n 
    #         
    #         Die Resultate zeigen, dass Vorstellungen und Überzeugungen, sowie Präferenzen die wichtigsten Faktoren sind, um sowohl Zustimmung als auch politische 
    #         Mobilisierung vorherzusagen. Andere etablierte erklärungsansätze, wie etwa die Salienz (wahrgenommene Wichtigkeit des Klimawandels gegenüber anderer Themen) oder 
    #         die Ausgestaltung der politischen Massnahmen haben einen weniger grossen Effekt.
    #         
    #         \n\n
    #         
    #         Mittels interaktiv wählbarer persönlicher Attribute macht das Modell eine Vorhersage zur Zustimmung und der politischen Mobilisierung, welche
    #         in den Graphiken veranschaulicht werden. Diese Resultate sind von politischer Relevanz, da sie beispielsweise als Grundlage zur Ausarbeitung von 
    #         neuen politischen Vorschlägen oder der Ausrichtung von Strategien der Öffentlichkeit, Interessengruppen, Parteien, Verwaltung und Regierung 
    #         dienen können.  
    #         ")
    # ),
    conditionalPanel(
      # 'input.navbar == "panel1" || input.navbar == "panel2"',
      'input.navbar == "Politische Faktoren"',
      selectInput("pol_party", label = "Welche Partei entspricht in den Zielen und Forderungen am ehesten Ihren eigenen 
                            Ansichten und Wünschen?",
                  choices = c("SVP (Schweizerische Volkspartei)", 
                              "SP (Sozialdemokratische Partei)", 
                              "FDP.Die Liberalen (Freisinnig Demokratische Partei)", 
                              "CVP (Christlichdemokratische Volkspartei)",
                              "GPS (Grüne Partei Schweiz)",
                              "GLP (Grünliberale Partei)",
                              "BDP (Bürgerlich Demokratische Partei)",
                              "EVP (Evangelische Volkspartei der Schweiz)",
                              "Lega dei Ticinesi",
                              "PdA (Partei der Arbeit Schweiz)",
                              "MCG (Mouvement Citoyens Genevois)",
                              "CSP (Christlichsoziale Partei Schweiz)",
                              "EDU (Eidgenössisch-Demokratische Union)",
                              "Sol. (SolidaritéS)",
                              "Andere:",
                              "Keine",
                              "Weiss nicht / keine Antwort")
      ),
      sliderInput("left_right", label = "Links, Mitte und Rechts sind drei Begriffe, die häufig gebraucht werden, um
                            politische Ansichten zu charakterisieren.
                            Wo sehen Sie sich selber auf einer Skala von 0 (ganz links) bis 10 (ganz rechts)?",
                  min = 0, max = 10, value = 5
      )
    ),
    conditionalPanel(
      ' input.navbar == "Beliefs"',
      title = "Politische Klimaschutzmassnahmen wie das CO2-Gesetz können auf Basis unterschiedlicher Kriterien beurteilt werden. Bitte lesen Sie sich folgende
                   Kriterien durch und geben Sie bitte an, wie sehr das zur Abstimmung stehende CO2-Gesetzes aus Ihrer Sicht das jeweilige Kriterium erfüllt oder nicht erfüllt.",
          radioButtons("effectiveness",
                       "Ökologische Wirksamkeit
                                     Definition: Das CO2-Gesetz trägt wirksam dazu bei, die CO2-Emissionen zu reduzieren und die Pariser Klimaschutzziele zu erreichen.",
                       choices = c("Ganz und gar nicht", "Eher nicht", "Weder noch", "Eher", "Voll und ganz"),
                       selected = "Weder noch",
                       inline=T
          ),
          radioButtons("efficiency",
                       "Ökonomische Effizienz
                                     Definition: Das CO2-Gesetz erreicht gesetzte Emissionsziele mit möglichst geringem Aufwand.",
                       choices = c("Ganz und gar nicht", "Eher nicht", "Weder noch", "Eher", "Voll und ganz"),
                       selected = "Weder noch",
                       inline=T
          ),
          radioButtons("competitiveness",
                       "Wettbewerbsfähigkeit der Schweizer Wirtschaft
                                     Definition: Das CO2-Gesetz fördert die Wettbewerbsfä;higkeit der Schweizer Wirtschaft und bietet Anreize zur Entwicklung neuer Technologien.",
                       choices = c("Ganz und gar nicht", "Eher nicht", "Weder noch", "Eher", "Voll und ganz"),
                       selected = "Weder noch",
                       inline=T
          ),
          radioButtons("justice",
                       "Soziale Gerechtigkeit
                                     Definition: Das CO2-Gesetz ist ein faires politisches Instrument und verteilt die Kosten sozial gerecht.",
                       choices = c("Ganz und gar nicht", "Eher nicht", "Weder noch", "Eher", "Voll und ganz"),
                       selected = "Weder noch",
                       inline=T
          ),
          radioButtons("transformation",
                       "Transformationspotenzial
                                     Definition: Das CO2-Gesetz hat die F&auml;higkeit, die Wirtschaft und Gesellschaft grundlegend umzugestalten.",
                       choices = c("Ganz und gar nicht", "Eher nicht", "Weder noch", "Eher", "Voll und ganz"),
                       selected = "Weder noch",
                       inline=T
          ),
      ),
    
  ),
  mainPanel(
    navbarPage("choose page",
               navbarMenu("page1", 
                          tabPanel("panel1", mainPanel(textOutput("zielsetzung"))), 
                          tabPanel("Politische Faktoren", 
                                   mainPanel(plotOutput("Plot1", width = "150%", height = "200px"), plotOutput("Plot2", width = "150%", height = "200px"))), 
                          tabPanel("Beliefs", 
                                   mainPanel(plotOutput("Plot3", width = "150%", height = "200px"), plotOutput("Plot4", width = "150%", height = "200px"))), 
               ),
               navbarMenu("page2", tabPanel("panel3", plotOutput("plot3")), tabPanel("panel4", plotOutput("plot4"))),
               id = "navbar"
    )
  )
)

# ui <- fluidPage(sidebarLayout(
#   sidebarPanel(navlistPanel(
#     widths = c(12, 12), "SidebarMenu",
#     selectInput("pol_party", label = "Welche Partei entspricht in den Zielen und Forderungen am ehesten Ihren eigenen 
#                             Ansichten und Wünschen?",
#                 choices = c("SVP (Schweizerische Volkspartei)", 
#                             "SP (Sozialdemokratische Partei)", 
#                             "FDP.Die Liberalen (Freisinnig Demokratische Partei)", 
#                             "CVP (Christlichdemokratische Volkspartei)",
#                             "GPS (Grüne Partei Schweiz)",
#                             "GLP (Grünliberale Partei)",
#                             "BDP (Bürgerlich Demokratische Partei)",
#                             "EVP (Evangelische Volkspartei der Schweiz)",
#                             "Lega dei Ticinesi",
#                             "PdA (Partei der Arbeit Schweiz)",
#                             "MCG (Mouvement Citoyens Genevois)",
#                             "CSP (Christlichsoziale Partei Schweiz)",
#                             "EDU (Eidgenössisch-Demokratische Union)",
#                             "Sol. (SolidaritéS)",
#                             "Andere:",
#                             "Keine",
#                             "Weiss nicht / keine Antwort")
#     ),
#     # tabPanel(numericInput('num', 'Number', min = 1, max = 10, value = 1, step = 1))
#   )),
#   mainPanel(navbarPage(title = "nav w/ sidebarMenu",
#                        
#                        tabPanel(h4("Perspective 1"),
#                                 tabsetPanel(
#                                   tabPanel("Subtab 1.1",
#                                            plotOutput("Plot1")),
#                                   tabPanel("Subtab 1.2")
#                                 )),
#                        tabPanel(h4("Perspective 2"),
#                                 tabsetPanel(
#                                   tabPanel("Subtab 2.1"),
#                                   tabPanel("Subtab 2.2")
#                                 )))
#             
#   )
# ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  ds <- reactive({
    
    transform4 <- function(x){
      unlist(lapply(x, function(x)
        if (is.na(x)) {NA} 
        else if (x == "Voll und ganz") {5}
        else if (x == "Eher") {4}
        else if (x == "Weder noch") {3}
        else if (x == "Eher nicht") {2}
        else if (x == "Ganz und gar nicht") {1}
      )
      )
    }
    
    efficiency <- transform4(input$efficiency)
    effectiveness <- transform4(input$effectiveness)
    competitiveness <- transform4(input$competitiveness)
    justice <- transform4(input$justice)
    transformation <- transform4(input$transformation)
    
    dat <- tibble(
      "civi_stat" = 1,
      "fin_cond" = 1,
      "pol_party" = if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
      else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
      else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
      else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
      else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
      else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
      else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
      else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
      else if (input$pol_party == "Lega dei Ticinesi") {9}
      else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
      else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
      else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
      else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
      else if (input$pol_party == "Sol. (SolidaritéS)") {14}
      else if (input$pol_party == "Andere:") {15}
      else if (input$pol_party == "Keine") {16}
      else if (input$pol_party == "Weiss nicht / keine Antwort") {17},
      "renew_heating" = 1,
      "left_right" = input$left_right,
      "prior_benefit" = 1,
      "ren_driver" = 1,
      "home_owner" = 1,
      "educ" = 1,
      "empl_sect" = 1,
      "empl_stat" = 2,
      "gender" = 1,
      "region" = 1,
      "know_targ" = 1,
      "know_build" = 1,
      "know_trans" = 1,
      "know_food" = 1,
      "know_avia" = 1,
      "know_wast" = 1,
      "efficiency" = efficiency,
      "effectiveness" = effectiveness,
      "competitiveness" = competitiveness,
      "justice" = justice,
      "transformation" = transformation
      
    )
  })
   

    output$Plot1 <- renderPlot({
        
        predict_probability(model_rate, ds()) %>%
          dplyr::mutate(
            value = ifelse(key == ".pred_2", value*(-1), value),
            value = ifelse(key == ".pred_1", value*(-1), value),
            value = ifelse(key == ".pred_3", value/2, value),
            dv = ""
          ) %>%
          dplyr::bind_rows(.[.$key == ".pred_3",] %>% dplyr::mutate(value = value *(-1))) %>%
          dplyr::mutate(key = factor(key, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
          ggplot2::ggplot(.) +
          ggplot2::geom_bar(aes(x = dv, y = value, fill = key), stat = "identity", position = position_stack(reverse = TRUE)) +
          ggplot2::theme_minimal() +
          ggplot2::coord_flip() +
          ggplot2::ylim(-1,1) +
          ggplot2::labs(
            title = "Public Support",
            x = "",
            y = "Probability"
          ) +
          ggplot2::scale_fill_manual(name = "", labels =c("Viel Aufwand zur Unterstützung", "Etwas Aufwand zur Unterstützung", "Viel Aufwand zur Verhinderung", "Etwas Aufwand zur Verhinderung", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
          ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
          guides(fill=guide_legend(nrow=2,byrow=TRUE))
       
    })
    
    output$Plot2 <- renderPlot({
      
      predict_probability(model_rate_activism, ds()) %>%
        dplyr::mutate(
          value = ifelse(key == ".pred_2", value*(-1), value),
          value = ifelse(key == ".pred_1", value*(-1), value),
          value = ifelse(key == ".pred_3", value/2, value),
          dv = ""
        ) %>%
        dplyr::bind_rows(.[.$key == ".pred_3",] %>% dplyr::mutate(value = value *(-1))) %>%
        dplyr::mutate(key = factor(key, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
        ggplot2::ggplot(.) +
        ggplot2::geom_bar(aes(x = dv, y = value, fill = key), stat = "identity", position = position_stack(reverse = TRUE)) +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip() +
        ggplot2::ylim(-1,1) +
        ggplot2::labs(
          title = "Political Mobilisation",
          x = "",
          y = "Probability"
        ) +
        ggplot2::scale_fill_manual(name = "", labels =c("Unterstütze voll und ganz", "Unterstütze", "Lehne voll und ganz ab", "Lehne ab", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
        ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
    
    output$Plot3 <- renderPlot({
      
      predict_probability(model_rate, ds()) %>%
        dplyr::mutate(
          value = ifelse(key == ".pred_2", value*(-1), value),
          value = ifelse(key == ".pred_1", value*(-1), value),
          value = ifelse(key == ".pred_3", value/2, value),
          dv = ""
        ) %>%
        dplyr::bind_rows(.[.$key == ".pred_3",] %>% dplyr::mutate(value = value *(-1))) %>%
        dplyr::mutate(key = factor(key, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
        ggplot2::ggplot(.) +
        ggplot2::geom_bar(aes(x = dv, y = value, fill = key), stat = "identity", position = position_stack(reverse = TRUE)) +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip() +
        ggplot2::ylim(-1,1) +
        ggplot2::labs(
          title = "Public Support",
          x = "",
          y = "Probability"
        ) +
        ggplot2::scale_fill_manual(name = "", labels =c("Viel Aufwand zur Unterstützung", "Etwas Aufwand zur Unterstützung", "Viel Aufwand zur Verhinderung", "Etwas Aufwand zur Verhinderung", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
        ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
    
    output$Plot4 <- renderPlot({
      
      predict_probability(model_rate_activism, ds()) %>%
        dplyr::mutate(
          value = ifelse(key == ".pred_2", value*(-1), value),
          value = ifelse(key == ".pred_1", value*(-1), value),
          value = ifelse(key == ".pred_3", value/2, value),
          dv = ""
        ) %>%
        dplyr::bind_rows(.[.$key == ".pred_3",] %>% dplyr::mutate(value = value *(-1))) %>%
        dplyr::mutate(key = factor(key, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
        ggplot2::ggplot(.) +
        ggplot2::geom_bar(aes(x = dv, y = value, fill = key), stat = "identity", position = position_stack(reverse = TRUE)) +
        ggplot2::theme_minimal() +
        ggplot2::coord_flip() +
        ggplot2::ylim(-1,1) +
        ggplot2::labs(
          title = "Political Mobilisation",
          x = "",
          y = "Probability"
        ) +
        ggplot2::scale_fill_manual(name = "", labels =c("Unterstütze voll und ganz", "Unterstütze", "Lehne voll und ganz ab", "Lehne ab", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
        ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
        guides(fill=guide_legend(nrow=2,byrow=TRUE))
      
    })
    
    output$zielsetzung <- renderText(HTML("Dieses interaktive Tool dient zur Veranschaulichung der Resultate einer repräsentativen Umfrage zur Schweizer 
            Klimapolitk, welche im Rahmen der Abstimmung zum CO2-Gesetz vom 13. Juni 2021 durchgeführt wurde. An Hand verschiedener 
            persönlicher Attribute lässt sich vorhersagen, was die Zustimmung zu ambitionierter Klimapolitik und die politische Mobilisierung an Wahlen 
            teilzunehmen am damals zur Abstimmung stehenden CO2-Gesetz ist. 
            
            \n\n 
            
            Die Resultate zeigen, dass Vorstellungen und Überzeugungen, sowie Präferenzen die wichtigsten Faktoren sind, um sowohl Zustimmung als auch politische 
            Mobilisierung vorherzusagen. Andere etablierte erklärungsansätze, wie etwa die Salienz (wahrgenommene Wichtigkeit des Klimawandels gegenüber anderer Themen) oder 
            die Ausgestaltung der politischen Massnahmen haben einen weniger grossen Effekt.
            
            \n\n
            
            Mittels interaktiv wählbarer persönlicher Attribute macht das Modell eine Vorhersage zur Zustimmung und der politischen Mobilisierung, welche
            in den Graphiken veranschaulicht werden. Diese Resultate sind von politischer Relevanz, da sie beispielsweise als Grundlage zur Ausarbeitung von 
            neuen politischen Vorschlägen oder der Ausrichtung von Strategien der Öffentlichkeit, Interessengruppen, Parteien, Verwaltung und Regierung 
            dienen können.  
            "))
   
}

# Run the application 
shinyApp(ui = ui, server = server)
