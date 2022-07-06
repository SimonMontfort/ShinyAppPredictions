library(shiny)
# library(shinydashboard)
# library(shinyWidgets)
library(tidymodels)
library(tidyverse)
# library(dplyr)
# library(stringr)
# library(ggplot2)
# library(plotly)
# library(shinythemes)
# library(xgboost)
# library(DALEXtra)
# library(DALEX)
# library(scales)
# library(readxl)
# library(ranger)
# library(ranger)
# library(caret)
# - for logs check:  rsconnect::showLogs("/Volumes/Transcend/Uni/doktorat/Predicting Core Beliefs/ShinyApp/PredictingVote/new_app_1.R")


model_rate <- readRDS("model_forest_rate.rds")
model_rate_activism <- readRDS("model_forest_rate_activism.rds")


# function to predict the probability 
predict_probability <- function(model, dat){
  stats::predict(model, dat, type = "prob") %>%
    tidyr::gather() %>%
    dplyr::mutate(value = as.numeric(value)) 
}

# Define UI for application that draws a histogram
# Define UI for app that draws a histogram ----
ui <- navbarPage("Demo", selected = "Service Use", collapsible = TRUE, inverse = TRUE, 
                 # theme = shinytheme("paper"),
                 tabPanel("Participation"),
                 # App title ----
                 # titlePanel("Auswertung der Umfrageergebnisse zur Abstimmung des CO2-Gesetz vom 13.06.21"),
                 
                 # Sidebar panel for inputs ----
                 tabPanel("Political Variables",
                          fluidPage(
                            tabsetPanel(
                              tabPanel(
                                sidebarLayout(
                                  sidebarPanel(
                                    sliderInput("left_right", label = "Links, Mitte und Rechts sind drei Begriffe, die häufig gebraucht werden, um
                            politische Ansichten zu charakterisieren.
                            Wo sehen Sie sich selber auf einer Skala von 0 (ganz links) bis 10 (ganz rechts)?",
                                                min = 0, max = 10, value = 5
                                    ),
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
                                                            "Weiss nicht / keine Antwort")),
                                  ),
                                  mainPanel(plotOutput(outputId = "Plot1", height = "200px"))
                                ),
                              ),
                            )
                          )
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # # wraps the reactive inputs into a function
  # ds <- reactive({
  #     
  #     # educ <- if(input$educ == "Grundausbildung (inklusive nicht abgeschlossen)") {1}
  #     # else if (input$educ == "Erstausbildung") {2}
  #     # else if (input$educ == "Obligatorische Schule") {3}
  #     # else if (input$educ == "Zweitausbildung") {4}
  #     # 
  #     # civi_stat <- if(input$civi_stat == "Ledig") {1}
  #     # else if (input$civi_stat == "Verheiratet") {2}
  #     # else if (input$civi_stat == "In eingetragener Partnerschaft") {3}
  #     # else if (input$civi_stat == "Verwitwet") {4}
  #     # else if (input$civi_stat == "Geschieden") {5}
  #     # else if (input$civi_stat == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # empl_sect <- if(input$empl_sect == "Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft") {1}
  #     # else if(input$empl_sect == "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk") {2}
  #     # else if(input$empl_sect == "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen") {3}
  #     # else if(input$empl_sect == "weiss nicht / keine Antwort oder arbeitet nicht") {0}
  #     # 
  #     # empl_stat <- if (input$empl_stat == "Vollzeit beschäftigt") {3}
  #     # else if (input$empl_stat == "Teilzeit beschäftigt") {2}
  #     # else if (input$empl_stat == "Ausbildung") {1}
  #     # else if (input$empl_stat == "Andere") {0}
  #     # 
  #     # fin_cond <- if(input$fin_cond == "Ja") {3}
  #     # else if(input$fin_cond == "Es geht so") {2}
  #     # else if(input$fin_cond == "Nein") {1}
  #     # else if(input$fin_cond == "Weiss nicht / keine Antwort") {0}
  #     # 
  #     # pol_party <- if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
  #     # else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
  #     # else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
  #     # else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
  #     # else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
  #     # else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
  #     # else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
  #     # else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
  #     # else if (input$pol_party == "Lega dei Ticinesi") {9}
  #     # else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
  #     # else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
  #     # else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
  #     # else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
  #     # else if (input$pol_party == "Sol. (SolidaritéS)") {14}
  #     # else if (input$pol_party == "Andere:") {15}
  #     # else if (input$pol_party == "Keine") {16}
  #     # else if (input$pol_party == "Weiss nicht / keine Antwort") {17}
  #     # 
  #     # # add qid22 (place of residence)
  #     # 
  #     # home_owner <- if(input$home_owner == "Ja") {1}
  #     # else if (input$home_owner == "Nein") {0}
  #     # 
  #     # renew_heating <- if(input$renew_heating == "Wärmepumpe, Fernwärme, Erdsonde, Strom, Solarenergie oder Holz, Holzschnitzel, Holzpellets") {1}
  #     # else if (input$renew_heating == "Ölheizung, Gasheizung oder Weiss nicht / keine Antwort") {2}
  #     # 
  #     # # renew_heating <- if(input$renew_heating == "Ölheizung oder Gasheizung") {1}
  #     # # else if (input$renew_heating == "Gasheizung") {2}
  #     # # else if (input$renew_heating == "Wärmepumpe") {3}
  #     # # else if (input$renew_heating == "Fernwärme") {4}
  #     # # else if (input$renew_heating == "Fernwärme") {5}
  #     # # else if (input$renew_heating == "Erdsonde") {6}
  #     # # else if (input$renew_heating == "Strom") {7}
  #     # # else if (input$renew_heating == "Solarenergie") {8}
  #     # # else if (input$renew_heating == "Holz, Holzschnitzel, Holzpellets") {9}
  #     # # else if (input$renew_heating == "Weiss nicht / keine Antwort") {10}
  #     # 
  #     # prior_benefit <- if(input$prior_benefit == "Stark beeinflusst") {5}
  #     # else if (input$prior_benefit == "Etwas beeinflusst") {4}
  #     # else if (input$prior_benefit == "Eher nicht beeinflusst") {3}
  #     # else if (input$prior_benefit == "Überhaupt nicht beeinflusst") {2}
  #     # else if (input$prior_benefit == "Weiss nicht / keine Antwort") {1}
  #     # 
  #     # ren_driver <- if(input$ren_driver == "Ja") {1}
  #     # else if (input$ren_driver == "Nein") {2}
  #     # 
  #     # gender <- if(input$gender == "Frau") {1}
  #     # else if (input$gender == "Mann") {2}
  #     # 
  #     # region <- if(input$region == "Genferseeregion") {1}
  #     # else if(input$region == "Mittelland") {2}
  #     # else if(input$region == "Nordwestschweiz") {3}
  #     # else if(input$region == "Zürich") {4}
  #     # else if(input$region == "Ostschweiz") {5}
  #     # else if(input$region == "Zentralschweiz") {6}
  #     # else if(input$region == "Tessin") {7}
  #     # 
  #     # # transForm <- function(x){
  #     # #     if(x == "Unterstütze voll und ganz") {5}
  #     # #     else if(x == "Unterstütze eher") {4}
  #     # #     else if(x == "Weder noch") {3}
  #     # #     else if(x == "Lehne eher ab") {2}
  #     # #     else if(x == "Lehne voll und ganz ab") {1}
  #     # # }
  #     # # 
  #     # # swiss_pol_comp <- transForm(input$swiss_pol_comp)
  #     # # swiss_pol_effect <- transForm(input$swiss_pol_effect)
  #     # # swiss_pol_effic <- transForm(input$swiss_pol_effic)
  #     # # swiss_pol_just <- transForm(input$swiss_pol_just)
  #     # # swiss_pol_lead <- transForm(input$swiss_pol_lead)
  #     # # swiss_pol_vol <- transForm(input$swiss_pol_vol)
  #     # # swiss_pol_subs <- transForm(input$swiss_pol_subs)
  #     # # swiss_pol_tax <- transForm(input$swiss_pol_tax)
  #     # # swiss_pol_regu <- transForm(input$swiss_pol_regu)
  #     # # swiss_pol_tech <- transForm(input$swiss_pol_tech)
  #     # # swiss_pol_beha <- transForm(input$swiss_pol_beha)
  #     # # swiss_pol_comb <- transForm(input$swiss_pol_comb)
  #     # # swiss_pol_fed <- transForm(input$swiss_pol_fed)
  #     # 
  #     # sal_ahv <- ifelse(input$salience == "Altersvorsorge/AHV", 1, 0)
  #     # sal_unemp <- ifelse(input$salience == "Arbeitslosigkeit", 1, 0)
  #     # sal_refug <- ifelse(input$salience == "Flüchtlinge", 1, 0)
  #     # sal_eu <- ifelse(input$salience == "Verhältnis der Schweiz zur Europäischen Union", 1, 0)
  #     # sal_health <- ifelse(input$salience == "Gesundheitswesen / Krankenversicherung", 1, 0)
  #     # sal_energy <- ifelse(input$salience == "Energieversorgung", 1, 0)
  #     # sal_traff <- ifelse(input$salience == "Verkehr", 1, 0)
  #     # sal_glob <- ifelse(input$salience == "Globalisierung der Wirtschaft / Freihandel", 1, 0)
  #     # sal_env <- ifelse(input$salience == "Umweltschutz / Klimawandel", 1, 0)
  #     # sal_crim <- ifelse(input$salience == "Kriminalität", 1, 0)
  #     # sal_uneq <- ifelse(input$salience == "Ungleichheit bei Einkommen und Vermögen", 1, 0)
  #     # sal_cult <- ifelse(input$salience == "Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen", 1, 0)
  #     # sal_for <- ifelse(input$salience == "Ausländische Arbeitskräfte in der Schweiz", 1, 0)
  #     # sal_pop <- ifelse(input$salience == "Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung", 1, 0)
  #     # 
  #     # transform2 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Unterstütze voll und ganz") {5}
  #     #         else if (x == "Unterstütze") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Lehne ab") {2}
  #     #         else if (x == "Lehne voll und ganz ab") {1}
  #     #     )
  #     #     )
  #     # }
  #     
  #     # sect_build <- transform2(input$sect_build)
  #     # sect_trans <- transform2(input$sect_trans)
  #     # sect_food <- transform2(input$sect_food)
  #     # sect_avia <- transform2(input$sect_avia)
  #     # sect_indu <- transform2(input$sect_indu)
  #     # sect_wast <- transform2(input$sect_wast)
  #     # 
  #     # transform3 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Im Gesetz enthalten") {3}
  #     #         else if (x == "Nicht im Gesetz enthalten") {2}
  #     #         else if (x == "Weiss nicht") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # know_targ <- transform3(input$know_targ)
  #     # know_build <- transform3(input$know_build)
  #     # know_trans <- transform3(input$know_trans)
  #     # know_food <- transform3(input$know_food)
  #     # know_avia <- transform3(input$know_avia)
  #     # know_wast <- transform3(input$know_wast)
  #     # 
  #     # switch <- if(input$switch == "Immer schon") {1}
  #     # else if(input$switch == "Seit Kürzerem (weniger als zwei Jahre) oder seit Längerem (mehr als zwei Jahre)") {2}
  #     # 
  #     # freq_user_driver <- if(input$freq_user_driver == "0-10'000 km") {0}
  #     # else if(input$freq_user_driver == "10'001-20'000 km") {1}
  #     # else if(input$freq_user_driver == "20'001-30'000 km") {2}
  #     # else if(input$freq_user_driver == "30'001-40'000 km") {3}
  #     # else if(input$freq_user_driver == "mehr als 50'000 km") {4}
  #     # else if(input$freq_user_driver == "Weiss nicht / keine Antwort") {5}
  #     # freq_user_fly <- if(input$freq_user_fly == "0-1 Flüge") {0}
  #     # else if(input$freq_user_fly == "2-3 Flüge") {1}
  #     # else if(input$freq_user_fly == "4-5 Flüge") {2}
  #     # else if(input$freq_user_fly == "6-7 Flüge") {3}
  #     # else if(input$freq_user_fly == "8-9 Flüge") {4}
  #     # else if(input$freq_user_fly == "mehr als 9 Flüge") {5}
  #     # freq_user_fish <- if(input$freq_user_fish == "An weniger als einem Tag") {0}
  #     # else if(input$freq_user_fish == "1") {1}
  #     # else if(input$freq_user_fish == "2") {2}
  #     # else if(input$freq_user_fish == "3") {3}
  #     # else if(input$freq_user_fish == "4") {4}
  #     # else if(input$freq_user_fish == "5") {5}
  #     # else if(input$freq_user_fish == "6") {6}
  #     # else if(input$freq_user_fish == "7") {7}
  #     # freq_user_meat <- if(input$freq_user_meat == "An weniger als einem Tag") {0}
  #     # else if(input$freq_user_meat == "1") {1}
  #     # else if(input$freq_user_meat == "2") {2}
  #     # else if(input$freq_user_meat == "3") {3}
  #     # else if(input$freq_user_meat == "4") {4}
  #     # else if(input$freq_user_meat == "5") {5}
  #     # else if(input$freq_user_meat == "6") {6}
  #     # else if(input$freq_user_meat == "7") {7}
  #     # freq_user_meat_subs <- if(input$freq_user_meat_subs == "Nie") {0}
  #     # else if(input$freq_user_meat_subs == "Selten") {1}
  #     # else if(input$freq_user_meat_subs == "Einmal pro Monat") {2}
  #     # else if(input$freq_user_meat_subs == "Mehrmals pro Monat") {3}
  #     # else if(input$freq_user_meat_subs == "Ungefähr ein mal pro Woche") {4}
  #     # else if(input$freq_user_meat_subs == "Mehrmals pro Woche") {5}
  #     # else if(input$freq_user_meat_subs == "Jeden Tag") {6}
  #     # 
  #     # transform4 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Voll und ganz") {5}
  #     #         else if (x == "Eher") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Eher nicht") {2}
  #     #         else if (x == "Ganz und gar nicht") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # efficiency <- transform4(input$efficiency)
  #     # effectiveness <- transform4(input$effectiveness)
  #     # competitiveness <- transform4(input$competitiveness)
  #     # justice <- transform4(input$justice)
  #     # transformation <- transform4(input$transformation)
  #     # 
  #     # Attrib1 <- if(input$Attrib1 == "40%") {1}
  #     # else if(input$Attrib1 == "50%") {2}
  #     # else if(input$Attrib1 == "60%") {3}
  #     # else if(input$Attrib1 == "70%") {4}
  #     # else if(input$Attrib1 == "80%") {5}
  #     # Attrib2 <- if(input$Attrib2 == "No tax on petrol") {1}  
  #     # else if(input$Attrib2 == "0.14 Fr./l petrol") {2}
  #     # else if(input$Attrib2 == "0.28 Fr./l petrol") {3}
  #     # else if(input$Attrib2 == "0.42 Fr./l petrol") {4}
  #     # else if(input$Attrib2 == "0.56 Fr./l petrol") {5}
  #     # Attrib3 <- if(input$Attrib3 == "No tax on heating oil") {1} 
  #     # else if(input$Attrib3 ==  "0.16 Fr./l heating oil") {2}
  #     # else if(input$Attrib3 == "0.31 Fr./l heating oil") {3}
  #     # else if(input$Attrib3 == "0.47 Fr./l heating oil") {4}
  #     # else if(input$Attrib3 == "0.63 Fr./l heating oil") {5}
  #     # Attrib4 <- if(input$Attrib4 == "No tax on meat") {1}  
  #     # else if(input$Attrib4 == "0.77 Fr./kg meat") {2}
  #     # else if(input$Attrib4 ==  "1.53 Fr./kg meat") {3}
  #     # else if(input$Attrib4 == "2.30 Fr./kg meat") {4}
  #     # else if(input$Attrib4 == "3.07 Fr./kg meat") {5}
  #     # Attrib5 <- if(input$Attrib5 == "No tax") {1}
  #     # else if(input$Attrib5 == "10 Fr. for short- and 30 Fr. for long-distance") {2} 
  #     # else if(input$Attrib5 == "25 Fr. for short- and 75 Fr. for long-distance") {3}
  #     # else if(input$Attrib5 == "40 Fr. for short- and 120 Fr. for long-distance") {4}
  #     # else if(input$Attrib5 == "55 Fr. for short- and 165 Fr. for long-distance") {5}
  #     # Attrib6 <- if(input$Attrib6 == "Exclusively lump sum reimbursement") {1} 
  #     # else if(input$Attrib6 == "Mostly lump sum reimbursement") {2}
  #     # else if(input$Attrib6 == "Lump sum reimbursement und investment into climate protection") {3}
  #     # else if(input$Attrib6 == "Mostly investment into climate protection") {4}
  #     # else if(input$Attrib6 == "Exclusively investment into climate protection") {5}
  #     # 
  #     # cov_fin <- if(input$cov_fin == "klar verschlechtert.") {1} 
  #     # else if(input$cov_fin == "eher verschlechtert.") {2}
  #     # else if(input$cov_fin == "nicht verändert.") {3}
  #     # else if(input$cov_fin == "eher verbessert.") {4}
  #     # else if(input$cov_fin == "klar verbessert.") {5}
  #     # else if(input$cov_fin == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # cov_job <- if(input$cov_job == "Keine Änderung") {0} 
  #     # else if(input$cov_job == "Ich arbeitete in Kurzarbeit.") {1}
  #     # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {2}
  #     # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {3}
  #     # else if(input$cov_job == "Ich arbeitete (teilweise) im Homeoffice.") {4}
  #     # else if(input$cov_job == "Ich musste Überzeit kompensieren oder Ferien nehmen, weil es zu wenig Arbeit gab.") {5}
  #     # else if(input$cov_job == "Ich arbeitete mehr als üblich.") {6}
  #     # else if(input$cov_job == "Ich habe meinen Job verloren.") {7}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Agentur musste Veranstaltungen absagen.") {8}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Branche profitierte von der Covid-19-Krise (z.B. viel mehr Arbeit/Nachfrage).") {9}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Branche litt unter der Covid-19-Krise (z.B. weniger Arbeit/Nachfrage, Teilschliessung, Kapazitätsbeschränkungen).") {10}
  #     # else if(input$cov_job == "Anderes (Bitte angeben)") {11}
  #     # else if(input$cov_job == "Weiss nicht / keine Antwort") {12}
  #     # 
  #     # 
  #     # cov_health <- if(input$cov_health == "Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.") {1} 
  #     # else if(input$cov_health == "Ich musste aufgrund einer behördlichen Anordnung in Quarantäne.") {2}
  #     # else if(input$cov_health == "Ich bin selber an Covid-19 erkrankt.") {3}
  #     # else if(input$cov_health == "Personen aus meinem engen Familienkreis sind erkrankt oder mussten in Quarantäne.") {4}
  #     # else if(input$cov_health == "Ich bin in meinem Alltag einem erhöhten Risiko einer Ansteckung ausgesetzt (z.B. Beruf mit Personenkontakt).") {5}
  #     # else if(input$cov_health == "Ich bin ein Risikopatient.") {6}
  #     # else if(input$cov_health == "Anderes (Bitte angeben)") {7}
  #     # else if(input$cov_health == "Weiss nicht / Keine Antwort") {8}
  #     # 
  #     # cov_info <- if(input$cov_info == "gut informiert.") {1} 
  #     # else if(input$cov_info == "eher informiert.") {2}
  #     # else if(input$cov_info == "eher nicht informiert.") {3}
  #     # else if(input$cov_info == "gar nicht informiert.") {4}
  #     # else if(input$cov_info == "klar verbessert.") {5}
  #     # else if(input$cov_info == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # transform5 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Weiss nicht / keine Antwort") {3}
  #     #         else if (x == "Trifft zu") {2}
  #     #         else if (x == "Trifft nicht zu") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_know_fin <- transform5(input$cov_know_fin)
  #     # cov_know_fed <- transform5(input$cov_know_fed)
  #     # cov_know_pub <- transform5(input$cov_know_pub)
  #     # cov_know_10i <- transform5(input$cov_know_10i)
  #     # cov_know_ref <- transform5(input$cov_know_ref)
  #     # 
  #     # cov_prof <- if(input$cov_prof == "Stimme nicht zu") {0} 
  #     # else if(input$cov_prof == "Stimme eher nicht zu") {1}
  #     # else if(input$cov_prof == "Weder noch") {2}
  #     # else if(input$cov_prof == "Stimme eher zu") {3}
  #     # else if(input$cov_prof == "Stimme zu") {4}
  #     # 
  #     # transform6 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Hohes Vertrauen") {5}
  #     #         else if (x == "Etwas Vertrauen") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Wenig Vertrauen") {2}
  #     #         else if (x == "Gar kein Vertrauen") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_trust_fede <- transform6(input$cov_trust_fede)
  #     # cov_trust_parl <- transform6(input$cov_trust_parl)
  #     # cov_trust_part <- transform6(input$cov_trust_part)
  #     # cov_trust_agen <- transform6(input$cov_trust_agen)
  #     # cov_trust_cant <- transform6(input$cov_trust_cant)
  #     # cov_trust_bag <- transform6(input$cov_trust_bag)
  #     # cov_trust_heal <- transform6(input$cov_trust_heal)
  #     # 
  #     # 
  #     # transform7 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Weiss nicht / keine Antwort") {6}
  #     #         else if (x == "Zufrieden") {5}
  #     #         else if (x == "Eher zufrieden") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Eher nicht zufrieden") {2}
  #     #         else if (x == "Nicht zufrieden") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_perf_fed <- transform7(input$cov_perf_fed)
  #     # cov_perf_cant <- transform7(input$cov_perf_cant)
  #     
  #     dat <- tibble(
  #         # "civi_stat" = 1,
  #         # "fin_cond" = 1,
  #         # "pol_party" = pol_party,
  #         # "renew_heating" = 1,
  #         # "left_right" = input$left_right,
  #         # "prior_benefit" = prior_benefit,
  #         # "ren_driver" = ren_driver,
  #         # "home_owner" = 1,
  #         # "educ" = educ,
  #         # "empl_sect" = empl_sect,
  #         # "empl_stat" = 2,
  #         # "gender" = gender,
  #         # "region" = region,
  #         
  #         "civi_stat" = 1,
  #         "fin_cond" = 1,
  #         "pol_party" =  if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
  #         else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
  #         else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
  #         else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
  #         else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
  #         else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
  #         else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
  #         else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
  #         else if (input$pol_party == "Lega dei Ticinesi") {9}
  #         else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
  #         else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
  #         else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
  #         else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
  #         else if (input$pol_party == "Sol. (SolidaritéS)") {14}
  #         else if (input$pol_party == "Andere:") {15}
  #         else if (input$pol_party == "Keine") {16}
  #         else if (input$pol_party == "Weiss nicht / keine Antwort") {17},
  #         "renew_heating" = 1,
  #         "left_right" = 1,
  #         "prior_benefit" = 1,
  #         "ren_driver" = 1,
  #         "home_owner" = 1,
  #         "educ" = 1,
  #         "empl_sect" = 1,
  #         "empl_stat" = 2,
  #         "gender" = 1,
  #         "region" = 1,
  #         
  #         # "sal_ahv" = sal_ahv,
  #         # "sal_unemp" = sal_unemp,
  #         # "sal_refug" = sal_refug,
  #         # "sal_eu" = sal_eu,
  #         # "sal_health" = sal_health,
  #         # "sal_energy" = sal_energy,
  #         # "sal_traff" = sal_traff,
  #         # "sal_glob" = sal_glob,
  #         # "sal_env" = sal_env,
  #         # "sal_crim" = sal_crim,
  #         # "sal_uneq" = sal_uneq,
  #         # "sal_cult" = sal_cult,
  #         # "sal_for" = sal_for,
  #         # "sal_pop" = sal_pop,
  #         # "swiss_pol_comp" = swiss_pol_comp,
  #         # "swiss_pol_effect" = swiss_pol_effect,
  #         # "swiss_pol_effic" = swiss_pol_effic,
  #         # "swiss_pol_just" = swiss_pol_just,
  #         # "swiss_pol_lead" = swiss_pol_lead,
  #         # "swiss_pol_vol" = swiss_pol_vol,
  #         # "swiss_pol_subs" = swiss_pol_subs,
  #         # "swiss_pol_tax" = swiss_pol_tax,
  #         # "swiss_pol_regu" = swiss_pol_regu,
  #         # "swiss_pol_tech" = swiss_pol_tech,
  #         # "swiss_pol_beha" = swiss_pol_beha,
  #         # "swiss_pol_comb" = swiss_pol_comb,
  #         # "swiss_pol_fed" = swiss_pol_fed,
  #         # "sect_build" = sect_build,
  #         # "sect_trans" = sect_trans,
  #         # "sect_food" = sect_food,
  #         # "sect_avia" = sect_avia,
  #         # "sect_indu" = sect_indu,
  #         # "sect_wast" = sect_wast,
  #         "know_targ" = 1,
  #         "know_build" = 1,
  #         "know_trans" = 1,
  #         "know_food" = 1,
  #         "know_avia" = 1,
  #         "know_wast" = 1,
  #         "efficiency" = 3,
  #         "effectiveness" = 3,
  #         "competitiveness" = 3,
  #         "justice" = 3,
  #         "transformation" = 3
  #         # "switch" = switch,
  #         # "freq_user_driver" = freq_user_driver,
  #         # "freq_user_fly" = freq_user_fly,
  #         # "freq_user_fish" = freq_user_fish,
  #         # "freq_user_meat" = freq_user_meat,
  #         # "freq_user_meat_subs" = freq_user_meat_subs,
  #         
  #         # "Attrib1" = Attrib1,
  #         # "Attrib2" = Attrib2,
  #         # "Attrib3" = Attrib3,
  #         # "Attrib4" = Attrib4,
  #         # "Attrib5" = Attrib5,
  #         # "Attrib6" = Attrib6,
  #         # "cov_imp" = input$cov_imp,
  #         # "cov_fin" = cov_fin,
  #         # "cov_job" = cov_job,
  #         # "cov_health" = cov_health,
  #         # "cov_info" = cov_info,
  #         # "cov_know_fin" = cov_know_fin,
  #         # "cov_know_fed" =  cov_know_fed,
  #         # "cov_know_pub" = cov_know_pub,
  #         # "cov_know_10i" = cov_know_10i,
  #         # "cov_know_ref" = cov_know_ref,
  #         # "cov_prof" = cov_prof,
  #         # "cov_trust_fede" = cov_trust_fede,
  #         # "cov_trust_parl" = cov_trust_parl,
  #         # "cov_trust_part" = cov_trust_part,
  #         # "cov_trust_agen" = cov_trust_agen,
  #         # "cov_trust_cant" = cov_trust_cant,
  #         # "cov_trust_bag" = cov_trust_bag,
  #         # "cov_trust_heal" = cov_trust_heal,
  #         # "cov_perf_fed" = cov_perf_fed,
  #         # "cov_perf_cant" = cov_perf_cant
  #     )
  # }) 
  
  output$Plot1 <- renderPlot({
    # educ <- if(input$educ == "Grundausbildung (inklusive nicht abgeschlossen)") {1}
    # else if (input$educ == "Erstausbildung") {2}
    # else if (input$educ == "Obligatorische Schule") {3}
    # else if (input$educ == "Zweitausbildung") {4}
    # 
    # civi_stat <- if(input$civi_stat == "Ledig") {1}
    # else if (input$civi_stat == "Verheiratet") {2}
    # else if (input$civi_stat == "In eingetragener Partnerschaft") {3}
    # else if (input$civi_stat == "Verwitwet") {4}
    # else if (input$civi_stat == "Geschieden") {5}
    # else if (input$civi_stat == "Weiss nicht / keine Antwort") {6}
    # 
    # empl_sect <- if(input$empl_sect == "Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft") {1}
    # else if(input$empl_sect == "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk") {2}
    # else if(input$empl_sect == "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen") {3}
    # else if(input$empl_sect == "weiss nicht / keine Antwort oder arbeitet nicht") {0}
    # 
    # empl_stat <- if (input$empl_stat == "Vollzeit beschäftigt") {3}
    # else if (input$empl_stat == "Teilzeit beschäftigt") {2}
    # else if (input$empl_stat == "Ausbildung") {1}
    # else if (input$empl_stat == "Andere") {0}
    # 
    # fin_cond <- if(input$fin_cond == "Ja") {3}
    # else if(input$fin_cond == "Es geht so") {2}
    # else if(input$fin_cond == "Nein") {1}
    # else if(input$fin_cond == "Weiss nicht / keine Antwort") {0}
    # 
    # pol_party <- if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
    # else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
    # else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
    # else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
    # else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
    # else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
    # else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
    # else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
    # else if (input$pol_party == "Lega dei Ticinesi") {9}
    # else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
    # else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
    # else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
    # else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
    # else if (input$pol_party == "Sol. (SolidaritéS)") {14}
    # else if (input$pol_party == "Andere:") {15}
    # else if (input$pol_party == "Keine") {16}
    # else if (input$pol_party == "Weiss nicht / keine Antwort") {17}
    # 
    # # add qid22 (place of residence)
    # 
    # home_owner <- if(input$home_owner == "Ja") {1}
    # else if (input$home_owner == "Nein") {0}
    # 
    # renew_heating <- if(input$renew_heating == "Wärmepumpe, Fernwärme, Erdsonde, Strom, Solarenergie oder Holz, Holzschnitzel, Holzpellets") {1}
    # else if (input$renew_heating == "Ölheizung, Gasheizung oder Weiss nicht / keine Antwort") {2}
    # 
    # # renew_heating <- if(input$renew_heating == "Ölheizung oder Gasheizung") {1}
    # # else if (input$renew_heating == "Gasheizung") {2}
    # # else if (input$renew_heating == "Wärmepumpe") {3}
    # # else if (input$renew_heating == "Fernwärme") {4}
    # # else if (input$renew_heating == "Fernwärme") {5}
    # # else if (input$renew_heating == "Erdsonde") {6}
    # # else if (input$renew_heating == "Strom") {7}
    # # else if (input$renew_heating == "Solarenergie") {8}
    # # else if (input$renew_heating == "Holz, Holzschnitzel, Holzpellets") {9}
    # # else if (input$renew_heating == "Weiss nicht / keine Antwort") {10}
    # 
    # prior_benefit <- if(input$prior_benefit == "Stark beeinflusst") {5}
    # else if (input$prior_benefit == "Etwas beeinflusst") {4}
    # else if (input$prior_benefit == "Eher nicht beeinflusst") {3}
    # else if (input$prior_benefit == "Überhaupt nicht beeinflusst") {2}
    # else if (input$prior_benefit == "Weiss nicht / keine Antwort") {1}
    # 
    # ren_driver <- if(input$ren_driver == "Ja") {1}
    # else if (input$ren_driver == "Nein") {2}
    # 
    # gender <- if(input$gender == "Frau") {1}
    # else if (input$gender == "Mann") {2}
    # 
    # region <- if(input$region == "Genferseeregion") {1}
    # else if(input$region == "Mittelland") {2}
    # else if(input$region == "Nordwestschweiz") {3}
    # else if(input$region == "Zürich") {4}
    # else if(input$region == "Ostschweiz") {5}
    # else if(input$region == "Zentralschweiz") {6}
    # else if(input$region == "Tessin") {7}
    # 
    # # transForm <- function(x){
    # #     if(x == "Unterstütze voll und ganz") {5}
    # #     else if(x == "Unterstütze eher") {4}
    # #     else if(x == "Weder noch") {3}
    # #     else if(x == "Lehne eher ab") {2}
    # #     else if(x == "Lehne voll und ganz ab") {1}
    # # }
    # # 
    # # swiss_pol_comp <- transForm(input$swiss_pol_comp)
    # # swiss_pol_effect <- transForm(input$swiss_pol_effect)
    # # swiss_pol_effic <- transForm(input$swiss_pol_effic)
    # # swiss_pol_just <- transForm(input$swiss_pol_just)
    # # swiss_pol_lead <- transForm(input$swiss_pol_lead)
    # # swiss_pol_vol <- transForm(input$swiss_pol_vol)
    # # swiss_pol_subs <- transForm(input$swiss_pol_subs)
    # # swiss_pol_tax <- transForm(input$swiss_pol_tax)
    # # swiss_pol_regu <- transForm(input$swiss_pol_regu)
    # # swiss_pol_tech <- transForm(input$swiss_pol_tech)
    # # swiss_pol_beha <- transForm(input$swiss_pol_beha)
    # # swiss_pol_comb <- transForm(input$swiss_pol_comb)
    # # swiss_pol_fed <- transForm(input$swiss_pol_fed)
    # 
    # sal_ahv <- ifelse(input$salience == "Altersvorsorge/AHV", 1, 0)
    # sal_unemp <- ifelse(input$salience == "Arbeitslosigkeit", 1, 0)
    # sal_refug <- ifelse(input$salience == "Flüchtlinge", 1, 0)
    # sal_eu <- ifelse(input$salience == "Verhältnis der Schweiz zur Europäischen Union", 1, 0)
    # sal_health <- ifelse(input$salience == "Gesundheitswesen / Krankenversicherung", 1, 0)
    # sal_energy <- ifelse(input$salience == "Energieversorgung", 1, 0)
    # sal_traff <- ifelse(input$salience == "Verkehr", 1, 0)
    # sal_glob <- ifelse(input$salience == "Globalisierung der Wirtschaft / Freihandel", 1, 0)
    # sal_env <- ifelse(input$salience == "Umweltschutz / Klimawandel", 1, 0)
    # sal_crim <- ifelse(input$salience == "Kriminalität", 1, 0)
    # sal_uneq <- ifelse(input$salience == "Ungleichheit bei Einkommen und Vermögen", 1, 0)
    # sal_cult <- ifelse(input$salience == "Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen", 1, 0)
    # sal_for <- ifelse(input$salience == "Ausländische Arbeitskräfte in der Schweiz", 1, 0)
    # sal_pop <- ifelse(input$salience == "Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung", 1, 0)
    # 
    # transform2 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA} 
    #         else if (x == "Unterstütze voll und ganz") {5}
    #         else if (x == "Unterstütze") {4}
    #         else if (x == "Weder noch") {3}
    #         else if (x == "Lehne ab") {2}
    #         else if (x == "Lehne voll und ganz ab") {1}
    #     )
    #     )
    # }
    
    # sect_build <- transform2(input$sect_build)
    # sect_trans <- transform2(input$sect_trans)
    # sect_food <- transform2(input$sect_food)
    # sect_avia <- transform2(input$sect_avia)
    # sect_indu <- transform2(input$sect_indu)
    # sect_wast <- transform2(input$sect_wast)
    # 
    # transform3 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA} 
    #         else if (x == "Im Gesetz enthalten") {3}
    #         else if (x == "Nicht im Gesetz enthalten") {2}
    #         else if (x == "Weiss nicht") {1}
    #     )
    #     )
    # }
    # 
    # know_targ <- transform3(input$know_targ)
    # know_build <- transform3(input$know_build)
    # know_trans <- transform3(input$know_trans)
    # know_food <- transform3(input$know_food)
    # know_avia <- transform3(input$know_avia)
    # know_wast <- transform3(input$know_wast)
    # 
    # switch <- if(input$switch == "Immer schon") {1}
    # else if(input$switch == "Seit Kürzerem (weniger als zwei Jahre) oder seit Längerem (mehr als zwei Jahre)") {2}
    # 
    # freq_user_driver <- if(input$freq_user_driver == "0-10'000 km") {0}
    # else if(input$freq_user_driver == "10'001-20'000 km") {1}
    # else if(input$freq_user_driver == "20'001-30'000 km") {2}
    # else if(input$freq_user_driver == "30'001-40'000 km") {3}
    # else if(input$freq_user_driver == "mehr als 50'000 km") {4}
    # else if(input$freq_user_driver == "Weiss nicht / keine Antwort") {5}
    # freq_user_fly <- if(input$freq_user_fly == "0-1 Flüge") {0}
    # else if(input$freq_user_fly == "2-3 Flüge") {1}
    # else if(input$freq_user_fly == "4-5 Flüge") {2}
    # else if(input$freq_user_fly == "6-7 Flüge") {3}
    # else if(input$freq_user_fly == "8-9 Flüge") {4}
    # else if(input$freq_user_fly == "mehr als 9 Flüge") {5}
    # freq_user_fish <- if(input$freq_user_fish == "An weniger als einem Tag") {0}
    # else if(input$freq_user_fish == "1") {1}
    # else if(input$freq_user_fish == "2") {2}
    # else if(input$freq_user_fish == "3") {3}
    # else if(input$freq_user_fish == "4") {4}
    # else if(input$freq_user_fish == "5") {5}
    # else if(input$freq_user_fish == "6") {6}
    # else if(input$freq_user_fish == "7") {7}
    # freq_user_meat <- if(input$freq_user_meat == "An weniger als einem Tag") {0}
    # else if(input$freq_user_meat == "1") {1}
    # else if(input$freq_user_meat == "2") {2}
    # else if(input$freq_user_meat == "3") {3}
    # else if(input$freq_user_meat == "4") {4}
    # else if(input$freq_user_meat == "5") {5}
    # else if(input$freq_user_meat == "6") {6}
    # else if(input$freq_user_meat == "7") {7}
    # freq_user_meat_subs <- if(input$freq_user_meat_subs == "Nie") {0}
    # else if(input$freq_user_meat_subs == "Selten") {1}
    # else if(input$freq_user_meat_subs == "Einmal pro Monat") {2}
    # else if(input$freq_user_meat_subs == "Mehrmals pro Monat") {3}
    # else if(input$freq_user_meat_subs == "Ungefähr ein mal pro Woche") {4}
    # else if(input$freq_user_meat_subs == "Mehrmals pro Woche") {5}
    # else if(input$freq_user_meat_subs == "Jeden Tag") {6}
    # 
    # transform4 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA} 
    #         else if (x == "Voll und ganz") {5}
    #         else if (x == "Eher") {4}
    #         else if (x == "Weder noch") {3}
    #         else if (x == "Eher nicht") {2}
    #         else if (x == "Ganz und gar nicht") {1}
    #     )
    #     )
    # }
    # 
    # efficiency <- transform4(input$efficiency)
    # effectiveness <- transform4(input$effectiveness)
    # competitiveness <- transform4(input$competitiveness)
    # justice <- transform4(input$justice)
    # transformation <- transform4(input$transformation)
    # 
    # Attrib1 <- if(input$Attrib1 == "40%") {1}
    # else if(input$Attrib1 == "50%") {2}
    # else if(input$Attrib1 == "60%") {3}
    # else if(input$Attrib1 == "70%") {4}
    # else if(input$Attrib1 == "80%") {5}
    # Attrib2 <- if(input$Attrib2 == "No tax on petrol") {1}  
    # else if(input$Attrib2 == "0.14 Fr./l petrol") {2}
    # else if(input$Attrib2 == "0.28 Fr./l petrol") {3}
    # else if(input$Attrib2 == "0.42 Fr./l petrol") {4}
    # else if(input$Attrib2 == "0.56 Fr./l petrol") {5}
    # Attrib3 <- if(input$Attrib3 == "No tax on heating oil") {1} 
    # else if(input$Attrib3 ==  "0.16 Fr./l heating oil") {2}
    # else if(input$Attrib3 == "0.31 Fr./l heating oil") {3}
    # else if(input$Attrib3 == "0.47 Fr./l heating oil") {4}
    # else if(input$Attrib3 == "0.63 Fr./l heating oil") {5}
    # Attrib4 <- if(input$Attrib4 == "No tax on meat") {1}  
    # else if(input$Attrib4 == "0.77 Fr./kg meat") {2}
    # else if(input$Attrib4 ==  "1.53 Fr./kg meat") {3}
    # else if(input$Attrib4 == "2.30 Fr./kg meat") {4}
    # else if(input$Attrib4 == "3.07 Fr./kg meat") {5}
    # Attrib5 <- if(input$Attrib5 == "No tax") {1}
    # else if(input$Attrib5 == "10 Fr. for short- and 30 Fr. for long-distance") {2} 
    # else if(input$Attrib5 == "25 Fr. for short- and 75 Fr. for long-distance") {3}
    # else if(input$Attrib5 == "40 Fr. for short- and 120 Fr. for long-distance") {4}
    # else if(input$Attrib5 == "55 Fr. for short- and 165 Fr. for long-distance") {5}
    # Attrib6 <- if(input$Attrib6 == "Exclusively lump sum reimbursement") {1} 
    # else if(input$Attrib6 == "Mostly lump sum reimbursement") {2}
    # else if(input$Attrib6 == "Lump sum reimbursement und investment into climate protection") {3}
    # else if(input$Attrib6 == "Mostly investment into climate protection") {4}
    # else if(input$Attrib6 == "Exclusively investment into climate protection") {5}
    # 
    # cov_fin <- if(input$cov_fin == "klar verschlechtert.") {1} 
    # else if(input$cov_fin == "eher verschlechtert.") {2}
    # else if(input$cov_fin == "nicht verändert.") {3}
    # else if(input$cov_fin == "eher verbessert.") {4}
    # else if(input$cov_fin == "klar verbessert.") {5}
    # else if(input$cov_fin == "Weiss nicht / keine Antwort") {6}
    # 
    # cov_job <- if(input$cov_job == "Keine Änderung") {0} 
    # else if(input$cov_job == "Ich arbeitete in Kurzarbeit.") {1}
    # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {2}
    # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {3}
    # else if(input$cov_job == "Ich arbeitete (teilweise) im Homeoffice.") {4}
    # else if(input$cov_job == "Ich musste Überzeit kompensieren oder Ferien nehmen, weil es zu wenig Arbeit gab.") {5}
    # else if(input$cov_job == "Ich arbeitete mehr als üblich.") {6}
    # else if(input$cov_job == "Ich habe meinen Job verloren.") {7}
    # else if(input$cov_job == "Mein Unternehmen/ meine Agentur musste Veranstaltungen absagen.") {8}
    # else if(input$cov_job == "Mein Unternehmen/ meine Branche profitierte von der Covid-19-Krise (z.B. viel mehr Arbeit/Nachfrage).") {9}
    # else if(input$cov_job == "Mein Unternehmen/ meine Branche litt unter der Covid-19-Krise (z.B. weniger Arbeit/Nachfrage, Teilschliessung, Kapazitätsbeschränkungen).") {10}
    # else if(input$cov_job == "Anderes (Bitte angeben)") {11}
    # else if(input$cov_job == "Weiss nicht / keine Antwort") {12}
    # 
    # 
    # cov_health <- if(input$cov_health == "Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.") {1} 
    # else if(input$cov_health == "Ich musste aufgrund einer behördlichen Anordnung in Quarantäne.") {2}
    # else if(input$cov_health == "Ich bin selber an Covid-19 erkrankt.") {3}
    # else if(input$cov_health == "Personen aus meinem engen Familienkreis sind erkrankt oder mussten in Quarantäne.") {4}
    # else if(input$cov_health == "Ich bin in meinem Alltag einem erhöhten Risiko einer Ansteckung ausgesetzt (z.B. Beruf mit Personenkontakt).") {5}
    # else if(input$cov_health == "Ich bin ein Risikopatient.") {6}
    # else if(input$cov_health == "Anderes (Bitte angeben)") {7}
    # else if(input$cov_health == "Weiss nicht / Keine Antwort") {8}
    # 
    # cov_info <- if(input$cov_info == "gut informiert.") {1} 
    # else if(input$cov_info == "eher informiert.") {2}
    # else if(input$cov_info == "eher nicht informiert.") {3}
    # else if(input$cov_info == "gar nicht informiert.") {4}
    # else if(input$cov_info == "klar verbessert.") {5}
    # else if(input$cov_info == "Weiss nicht / keine Antwort") {6}
    # 
    # transform5 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA}
    #         else if (x == "Weiss nicht / keine Antwort") {3}
    #         else if (x == "Trifft zu") {2}
    #         else if (x == "Trifft nicht zu") {1}
    #     )
    #     )
    # }
    # 
    # cov_know_fin <- transform5(input$cov_know_fin)
    # cov_know_fed <- transform5(input$cov_know_fed)
    # cov_know_pub <- transform5(input$cov_know_pub)
    # cov_know_10i <- transform5(input$cov_know_10i)
    # cov_know_ref <- transform5(input$cov_know_ref)
    # 
    # cov_prof <- if(input$cov_prof == "Stimme nicht zu") {0} 
    # else if(input$cov_prof == "Stimme eher nicht zu") {1}
    # else if(input$cov_prof == "Weder noch") {2}
    # else if(input$cov_prof == "Stimme eher zu") {3}
    # else if(input$cov_prof == "Stimme zu") {4}
    # 
    # transform6 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA}
    #         else if (x == "Hohes Vertrauen") {5}
    #         else if (x == "Etwas Vertrauen") {4}
    #         else if (x == "Weder noch") {3}
    #         else if (x == "Wenig Vertrauen") {2}
    #         else if (x == "Gar kein Vertrauen") {1}
    #     )
    #     )
    # }
    # 
    # cov_trust_fede <- transform6(input$cov_trust_fede)
    # cov_trust_parl <- transform6(input$cov_trust_parl)
    # cov_trust_part <- transform6(input$cov_trust_part)
    # cov_trust_agen <- transform6(input$cov_trust_agen)
    # cov_trust_cant <- transform6(input$cov_trust_cant)
    # cov_trust_bag <- transform6(input$cov_trust_bag)
    # cov_trust_heal <- transform6(input$cov_trust_heal)
    # 
    # 
    # transform7 <- function(x){
    #     unlist(lapply(x, function(x)
    #         if (is.na(x)) {NA}
    #         else if (x == "Weiss nicht / keine Antwort") {6}
    #         else if (x == "Zufrieden") {5}
    #         else if (x == "Eher zufrieden") {4}
    #         else if (x == "Weder noch") {3}
    #         else if (x == "Eher nicht zufrieden") {2}
    #         else if (x == "Nicht zufrieden") {1}
    #     )
    #     )
    # }
    # 
    # cov_perf_fed <- transform7(input$cov_perf_fed)
    # cov_perf_cant <- transform7(input$cov_perf_cant)
    
    dat <- tibble(
      # "civi_stat" = 1,
      # "fin_cond" = 1,
      # "pol_party" = pol_party,
      # "renew_heating" = 1,
      # "left_right" = input$left_right,
      # "prior_benefit" = prior_benefit,
      # "ren_driver" = ren_driver,
      # "home_owner" = 1,
      # "educ" = educ,
      # "empl_sect" = empl_sect,
      # "empl_stat" = 2,
      # "gender" = gender,
      # "region" = region,
      
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
      
      # "sal_ahv" = sal_ahv,
      # "sal_unemp" = sal_unemp,
      # "sal_refug" = sal_refug,
      # "sal_eu" = sal_eu,
      # "sal_health" = sal_health,
      # "sal_energy" = sal_energy,
      # "sal_traff" = sal_traff,
      # "sal_glob" = sal_glob,
      # "sal_env" = sal_env,
      # "sal_crim" = sal_crim,
      # "sal_uneq" = sal_uneq,
      # "sal_cult" = sal_cult,
      # "sal_for" = sal_for,
      # "sal_pop" = sal_pop,
      # "swiss_pol_comp" = swiss_pol_comp,
      # "swiss_pol_effect" = swiss_pol_effect,
      # "swiss_pol_effic" = swiss_pol_effic,
      # "swiss_pol_just" = swiss_pol_just,
      # "swiss_pol_lead" = swiss_pol_lead,
      # "swiss_pol_vol" = swiss_pol_vol,
      # "swiss_pol_subs" = swiss_pol_subs,
      # "swiss_pol_tax" = swiss_pol_tax,
      # "swiss_pol_regu" = swiss_pol_regu,
      # "swiss_pol_tech" = swiss_pol_tech,
      # "swiss_pol_beha" = swiss_pol_beha,
      # "swiss_pol_comb" = swiss_pol_comb,
      # "swiss_pol_fed" = swiss_pol_fed,
      # "sect_build" = sect_build,
      # "sect_trans" = sect_trans,
      # "sect_food" = sect_food,
      # "sect_avia" = sect_avia,
      # "sect_indu" = sect_indu,
      # "sect_wast" = sect_wast,
      "know_targ" = 1,
      "know_build" = 1,
      "know_trans" = 1,
      "know_food" = 1,
      "know_avia" = 1,
      "know_wast" = 1,
      "efficiency" = 3,
      "effectiveness" = 3,
      "competitiveness" = 3,
      "justice" = 3,
      "transformation" = 3
      # "switch" = switch,
      # "freq_user_driver" = freq_user_driver,
      # "freq_user_fly" = freq_user_fly,
      # "freq_user_fish" = freq_user_fish,
      # "freq_user_meat" = freq_user_meat,
      # "freq_user_meat_subs" = freq_user_meat_subs,
      
      # "Attrib1" = Attrib1,
      # "Attrib2" = Attrib2,
      # "Attrib3" = Attrib3,
      # "Attrib4" = Attrib4,
      # "Attrib5" = Attrib5,
      # "Attrib6" = Attrib6,
      # "cov_imp" = input$cov_imp,
      # "cov_fin" = cov_fin,
      # "cov_job" = cov_job,
      # "cov_health" = cov_health,
      # "cov_info" = cov_info,
      # "cov_know_fin" = cov_know_fin,
      # "cov_know_fed" =  cov_know_fed,
      # "cov_know_pub" = cov_know_pub,
      # "cov_know_10i" = cov_know_10i,
      # "cov_know_ref" = cov_know_ref,
      # "cov_prof" = cov_prof,
      # "cov_trust_fede" = cov_trust_fede,
      # "cov_trust_parl" = cov_trust_parl,
      # "cov_trust_part" = cov_trust_part,
      # "cov_trust_agen" = cov_trust_agen,
      # "cov_trust_cant" = cov_trust_cant,
      # "cov_trust_bag" = cov_trust_bag,
      # "cov_trust_heal" = cov_trust_heal,
      # "cov_perf_fed" = cov_perf_fed,
      # "cov_perf_cant" = cov_perf_cant
    )
    
    predict_probability(model_rate, dat) %>%
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
  
  # output$Plot2 <- renderPlot({
  #     # educ <- if(input$educ == "Grundausbildung (inklusive nicht abgeschlossen)") {1}
  #     # else if (input$educ == "Erstausbildung") {2}
  #     # else if (input$educ == "Obligatorische Schule") {3}
  #     # else if (input$educ == "Zweitausbildung") {4}
  #     # 
  #     # civi_stat <- if(input$civi_stat == "Ledig") {1}
  #     # else if (input$civi_stat == "Verheiratet") {2}
  #     # else if (input$civi_stat == "In eingetragener Partnerschaft") {3}
  #     # else if (input$civi_stat == "Verwitwet") {4}
  #     # else if (input$civi_stat == "Geschieden") {5}
  #     # else if (input$civi_stat == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # empl_sect <- if(input$empl_sect == "Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft") {1}
  #     # else if(input$empl_sect == "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk") {2}
  #     # else if(input$empl_sect == "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen") {3}
  #     # else if(input$empl_sect == "weiss nicht / keine Antwort oder arbeitet nicht") {0}
  #     # 
  #     # empl_stat <- if (input$empl_stat == "Vollzeit beschäftigt") {3}
  #     # else if (input$empl_stat == "Teilzeit beschäftigt") {2}
  #     # else if (input$empl_stat == "Ausbildung") {1}
  #     # else if (input$empl_stat == "Andere") {0}
  #     # 
  #     # fin_cond <- if(input$fin_cond == "Ja") {3}
  #     # else if(input$fin_cond == "Es geht so") {2}
  #     # else if(input$fin_cond == "Nein") {1}
  #     # else if(input$fin_cond == "Weiss nicht / keine Antwort") {0}
  #     # 
  #     # pol_party <- if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
  #     # else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
  #     # else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
  #     # else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
  #     # else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
  #     # else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
  #     # else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
  #     # else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
  #     # else if (input$pol_party == "Lega dei Ticinesi") {9}
  #     # else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
  #     # else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
  #     # else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
  #     # else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
  #     # else if (input$pol_party == "Sol. (SolidaritéS)") {14}
  #     # else if (input$pol_party == "Andere:") {15}
  #     # else if (input$pol_party == "Keine") {16}
  #     # else if (input$pol_party == "Weiss nicht / keine Antwort") {17}
  #     # 
  #     # # add qid22 (place of residence)
  #     # 
  #     # home_owner <- if(input$home_owner == "Ja") {1}
  #     # else if (input$home_owner == "Nein") {0}
  #     # 
  #     # renew_heating <- if(input$renew_heating == "Wärmepumpe, Fernwärme, Erdsonde, Strom, Solarenergie oder Holz, Holzschnitzel, Holzpellets") {1}
  #     # else if (input$renew_heating == "Ölheizung, Gasheizung oder Weiss nicht / keine Antwort") {2}
  #     # 
  #     # # renew_heating <- if(input$renew_heating == "Ölheizung oder Gasheizung") {1}
  #     # # else if (input$renew_heating == "Gasheizung") {2}
  #     # # else if (input$renew_heating == "Wärmepumpe") {3}
  #     # # else if (input$renew_heating == "Fernwärme") {4}
  #     # # else if (input$renew_heating == "Fernwärme") {5}
  #     # # else if (input$renew_heating == "Erdsonde") {6}
  #     # # else if (input$renew_heating == "Strom") {7}
  #     # # else if (input$renew_heating == "Solarenergie") {8}
  #     # # else if (input$renew_heating == "Holz, Holzschnitzel, Holzpellets") {9}
  #     # # else if (input$renew_heating == "Weiss nicht / keine Antwort") {10}
  #     # 
  #     # prior_benefit <- if(input$prior_benefit == "Stark beeinflusst") {5}
  #     # else if (input$prior_benefit == "Etwas beeinflusst") {4}
  #     # else if (input$prior_benefit == "Eher nicht beeinflusst") {3}
  #     # else if (input$prior_benefit == "Überhaupt nicht beeinflusst") {2}
  #     # else if (input$prior_benefit == "Weiss nicht / keine Antwort") {1}
  #     # 
  #     # ren_driver <- if(input$ren_driver == "Ja") {1}
  #     # else if (input$ren_driver == "Nein") {2}
  #     # 
  #     # gender <- if(input$gender == "Frau") {1}
  #     # else if (input$gender == "Mann") {2}
  #     # 
  #     # region <- if(input$region == "Genferseeregion") {1}
  #     # else if(input$region == "Mittelland") {2}
  #     # else if(input$region == "Nordwestschweiz") {3}
  #     # else if(input$region == "Zürich") {4}
  #     # else if(input$region == "Ostschweiz") {5}
  #     # else if(input$region == "Zentralschweiz") {6}
  #     # else if(input$region == "Tessin") {7}
  #     # 
  #     # # transForm <- function(x){
  #     # #     if(x == "Unterstütze voll und ganz") {5}
  #     # #     else if(x == "Unterstütze eher") {4}
  #     # #     else if(x == "Weder noch") {3}
  #     # #     else if(x == "Lehne eher ab") {2}
  #     # #     else if(x == "Lehne voll und ganz ab") {1}
  #     # # }
  #     # # 
  #     # # swiss_pol_comp <- transForm(input$swiss_pol_comp)
  #     # # swiss_pol_effect <- transForm(input$swiss_pol_effect)
  #     # # swiss_pol_effic <- transForm(input$swiss_pol_effic)
  #     # # swiss_pol_just <- transForm(input$swiss_pol_just)
  #     # # swiss_pol_lead <- transForm(input$swiss_pol_lead)
  #     # # swiss_pol_vol <- transForm(input$swiss_pol_vol)
  #     # # swiss_pol_subs <- transForm(input$swiss_pol_subs)
  #     # # swiss_pol_tax <- transForm(input$swiss_pol_tax)
  #     # # swiss_pol_regu <- transForm(input$swiss_pol_regu)
  #     # # swiss_pol_tech <- transForm(input$swiss_pol_tech)
  #     # # swiss_pol_beha <- transForm(input$swiss_pol_beha)
  #     # # swiss_pol_comb <- transForm(input$swiss_pol_comb)
  #     # # swiss_pol_fed <- transForm(input$swiss_pol_fed)
  #     # 
  #     # sal_ahv <- ifelse(input$salience == "Altersvorsorge/AHV", 1, 0)
  #     # sal_unemp <- ifelse(input$salience == "Arbeitslosigkeit", 1, 0)
  #     # sal_refug <- ifelse(input$salience == "Flüchtlinge", 1, 0)
  #     # sal_eu <- ifelse(input$salience == "Verhältnis der Schweiz zur Europäischen Union", 1, 0)
  #     # sal_health <- ifelse(input$salience == "Gesundheitswesen / Krankenversicherung", 1, 0)
  #     # sal_energy <- ifelse(input$salience == "Energieversorgung", 1, 0)
  #     # sal_traff <- ifelse(input$salience == "Verkehr", 1, 0)
  #     # sal_glob <- ifelse(input$salience == "Globalisierung der Wirtschaft / Freihandel", 1, 0)
  #     # sal_env <- ifelse(input$salience == "Umweltschutz / Klimawandel", 1, 0)
  #     # sal_crim <- ifelse(input$salience == "Kriminalität", 1, 0)
  #     # sal_uneq <- ifelse(input$salience == "Ungleichheit bei Einkommen und Vermögen", 1, 0)
  #     # sal_cult <- ifelse(input$salience == "Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen", 1, 0)
  #     # sal_for <- ifelse(input$salience == "Ausländische Arbeitskräfte in der Schweiz", 1, 0)
  #     # sal_pop <- ifelse(input$salience == "Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung", 1, 0)
  #     # 
  #     # transform2 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Unterstütze voll und ganz") {5}
  #     #         else if (x == "Unterstütze") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Lehne ab") {2}
  #     #         else if (x == "Lehne voll und ganz ab") {1}
  #     #     )
  #     #     )
  #     # }
  #     
  #     # sect_build <- transform2(input$sect_build)
  #     # sect_trans <- transform2(input$sect_trans)
  #     # sect_food <- transform2(input$sect_food)
  #     # sect_avia <- transform2(input$sect_avia)
  #     # sect_indu <- transform2(input$sect_indu)
  #     # sect_wast <- transform2(input$sect_wast)
  #     # 
  #     # transform3 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Im Gesetz enthalten") {3}
  #     #         else if (x == "Nicht im Gesetz enthalten") {2}
  #     #         else if (x == "Weiss nicht") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # know_targ <- transform3(input$know_targ)
  #     # know_build <- transform3(input$know_build)
  #     # know_trans <- transform3(input$know_trans)
  #     # know_food <- transform3(input$know_food)
  #     # know_avia <- transform3(input$know_avia)
  #     # know_wast <- transform3(input$know_wast)
  #     # 
  #     # switch <- if(input$switch == "Immer schon") {1}
  #     # else if(input$switch == "Seit Kürzerem (weniger als zwei Jahre) oder seit Längerem (mehr als zwei Jahre)") {2}
  #     # 
  #     # freq_user_driver <- if(input$freq_user_driver == "0-10'000 km") {0}
  #     # else if(input$freq_user_driver == "10'001-20'000 km") {1}
  #     # else if(input$freq_user_driver == "20'001-30'000 km") {2}
  #     # else if(input$freq_user_driver == "30'001-40'000 km") {3}
  #     # else if(input$freq_user_driver == "mehr als 50'000 km") {4}
  #     # else if(input$freq_user_driver == "Weiss nicht / keine Antwort") {5}
  #     # freq_user_fly <- if(input$freq_user_fly == "0-1 Flüge") {0}
  #     # else if(input$freq_user_fly == "2-3 Flüge") {1}
  #     # else if(input$freq_user_fly == "4-5 Flüge") {2}
  #     # else if(input$freq_user_fly == "6-7 Flüge") {3}
  #     # else if(input$freq_user_fly == "8-9 Flüge") {4}
  #     # else if(input$freq_user_fly == "mehr als 9 Flüge") {5}
  #     # freq_user_fish <- if(input$freq_user_fish == "An weniger als einem Tag") {0}
  #     # else if(input$freq_user_fish == "1") {1}
  #     # else if(input$freq_user_fish == "2") {2}
  #     # else if(input$freq_user_fish == "3") {3}
  #     # else if(input$freq_user_fish == "4") {4}
  #     # else if(input$freq_user_fish == "5") {5}
  #     # else if(input$freq_user_fish == "6") {6}
  #     # else if(input$freq_user_fish == "7") {7}
  #     # freq_user_meat <- if(input$freq_user_meat == "An weniger als einem Tag") {0}
  #     # else if(input$freq_user_meat == "1") {1}
  #     # else if(input$freq_user_meat == "2") {2}
  #     # else if(input$freq_user_meat == "3") {3}
  #     # else if(input$freq_user_meat == "4") {4}
  #     # else if(input$freq_user_meat == "5") {5}
  #     # else if(input$freq_user_meat == "6") {6}
  #     # else if(input$freq_user_meat == "7") {7}
  #     # freq_user_meat_subs <- if(input$freq_user_meat_subs == "Nie") {0}
  #     # else if(input$freq_user_meat_subs == "Selten") {1}
  #     # else if(input$freq_user_meat_subs == "Einmal pro Monat") {2}
  #     # else if(input$freq_user_meat_subs == "Mehrmals pro Monat") {3}
  #     # else if(input$freq_user_meat_subs == "Ungefähr ein mal pro Woche") {4}
  #     # else if(input$freq_user_meat_subs == "Mehrmals pro Woche") {5}
  #     # else if(input$freq_user_meat_subs == "Jeden Tag") {6}
  #     # 
  #     # transform4 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA} 
  #     #         else if (x == "Voll und ganz") {5}
  #     #         else if (x == "Eher") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Eher nicht") {2}
  #     #         else if (x == "Ganz und gar nicht") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # efficiency <- transform4(input$efficiency)
  #     # effectiveness <- transform4(input$effectiveness)
  #     # competitiveness <- transform4(input$competitiveness)
  #     # justice <- transform4(input$justice)
  #     # transformation <- transform4(input$transformation)
  #     # 
  #     # Attrib1 <- if(input$Attrib1 == "40%") {1}
  #     # else if(input$Attrib1 == "50%") {2}
  #     # else if(input$Attrib1 == "60%") {3}
  #     # else if(input$Attrib1 == "70%") {4}
  #     # else if(input$Attrib1 == "80%") {5}
  #     # Attrib2 <- if(input$Attrib2 == "No tax on petrol") {1}  
  #     # else if(input$Attrib2 == "0.14 Fr./l petrol") {2}
  #     # else if(input$Attrib2 == "0.28 Fr./l petrol") {3}
  #     # else if(input$Attrib2 == "0.42 Fr./l petrol") {4}
  #     # else if(input$Attrib2 == "0.56 Fr./l petrol") {5}
  #     # Attrib3 <- if(input$Attrib3 == "No tax on heating oil") {1} 
  #     # else if(input$Attrib3 ==  "0.16 Fr./l heating oil") {2}
  #     # else if(input$Attrib3 == "0.31 Fr./l heating oil") {3}
  #     # else if(input$Attrib3 == "0.47 Fr./l heating oil") {4}
  #     # else if(input$Attrib3 == "0.63 Fr./l heating oil") {5}
  #     # Attrib4 <- if(input$Attrib4 == "No tax on meat") {1}  
  #     # else if(input$Attrib4 == "0.77 Fr./kg meat") {2}
  #     # else if(input$Attrib4 ==  "1.53 Fr./kg meat") {3}
  #     # else if(input$Attrib4 == "2.30 Fr./kg meat") {4}
  #     # else if(input$Attrib4 == "3.07 Fr./kg meat") {5}
  #     # Attrib5 <- if(input$Attrib5 == "No tax") {1}
  #     # else if(input$Attrib5 == "10 Fr. for short- and 30 Fr. for long-distance") {2} 
  #     # else if(input$Attrib5 == "25 Fr. for short- and 75 Fr. for long-distance") {3}
  #     # else if(input$Attrib5 == "40 Fr. for short- and 120 Fr. for long-distance") {4}
  #     # else if(input$Attrib5 == "55 Fr. for short- and 165 Fr. for long-distance") {5}
  #     # Attrib6 <- if(input$Attrib6 == "Exclusively lump sum reimbursement") {1} 
  #     # else if(input$Attrib6 == "Mostly lump sum reimbursement") {2}
  #     # else if(input$Attrib6 == "Lump sum reimbursement und investment into climate protection") {3}
  #     # else if(input$Attrib6 == "Mostly investment into climate protection") {4}
  #     # else if(input$Attrib6 == "Exclusively investment into climate protection") {5}
  #     # 
  #     # cov_fin <- if(input$cov_fin == "klar verschlechtert.") {1} 
  #     # else if(input$cov_fin == "eher verschlechtert.") {2}
  #     # else if(input$cov_fin == "nicht verändert.") {3}
  #     # else if(input$cov_fin == "eher verbessert.") {4}
  #     # else if(input$cov_fin == "klar verbessert.") {5}
  #     # else if(input$cov_fin == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # cov_job <- if(input$cov_job == "Keine Änderung") {0} 
  #     # else if(input$cov_job == "Ich arbeitete in Kurzarbeit.") {1}
  #     # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {2}
  #     # else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {3}
  #     # else if(input$cov_job == "Ich arbeitete (teilweise) im Homeoffice.") {4}
  #     # else if(input$cov_job == "Ich musste Überzeit kompensieren oder Ferien nehmen, weil es zu wenig Arbeit gab.") {5}
  #     # else if(input$cov_job == "Ich arbeitete mehr als üblich.") {6}
  #     # else if(input$cov_job == "Ich habe meinen Job verloren.") {7}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Agentur musste Veranstaltungen absagen.") {8}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Branche profitierte von der Covid-19-Krise (z.B. viel mehr Arbeit/Nachfrage).") {9}
  #     # else if(input$cov_job == "Mein Unternehmen/ meine Branche litt unter der Covid-19-Krise (z.B. weniger Arbeit/Nachfrage, Teilschliessung, Kapazitätsbeschränkungen).") {10}
  #     # else if(input$cov_job == "Anderes (Bitte angeben)") {11}
  #     # else if(input$cov_job == "Weiss nicht / keine Antwort") {12}
  #     # 
  #     # 
  #     # cov_health <- if(input$cov_health == "Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.") {1} 
  #     # else if(input$cov_health == "Ich musste aufgrund einer behördlichen Anordnung in Quarantäne.") {2}
  #     # else if(input$cov_health == "Ich bin selber an Covid-19 erkrankt.") {3}
  #     # else if(input$cov_health == "Personen aus meinem engen Familienkreis sind erkrankt oder mussten in Quarantäne.") {4}
  #     # else if(input$cov_health == "Ich bin in meinem Alltag einem erhöhten Risiko einer Ansteckung ausgesetzt (z.B. Beruf mit Personenkontakt).") {5}
  #     # else if(input$cov_health == "Ich bin ein Risikopatient.") {6}
  #     # else if(input$cov_health == "Anderes (Bitte angeben)") {7}
  #     # else if(input$cov_health == "Weiss nicht / Keine Antwort") {8}
  #     # 
  #     # cov_info <- if(input$cov_info == "gut informiert.") {1} 
  #     # else if(input$cov_info == "eher informiert.") {2}
  #     # else if(input$cov_info == "eher nicht informiert.") {3}
  #     # else if(input$cov_info == "gar nicht informiert.") {4}
  #     # else if(input$cov_info == "klar verbessert.") {5}
  #     # else if(input$cov_info == "Weiss nicht / keine Antwort") {6}
  #     # 
  #     # transform5 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Weiss nicht / keine Antwort") {3}
  #     #         else if (x == "Trifft zu") {2}
  #     #         else if (x == "Trifft nicht zu") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_know_fin <- transform5(input$cov_know_fin)
  #     # cov_know_fed <- transform5(input$cov_know_fed)
  #     # cov_know_pub <- transform5(input$cov_know_pub)
  #     # cov_know_10i <- transform5(input$cov_know_10i)
  #     # cov_know_ref <- transform5(input$cov_know_ref)
  #     # 
  #     # cov_prof <- if(input$cov_prof == "Stimme nicht zu") {0} 
  #     # else if(input$cov_prof == "Stimme eher nicht zu") {1}
  #     # else if(input$cov_prof == "Weder noch") {2}
  #     # else if(input$cov_prof == "Stimme eher zu") {3}
  #     # else if(input$cov_prof == "Stimme zu") {4}
  #     # 
  #     # transform6 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Hohes Vertrauen") {5}
  #     #         else if (x == "Etwas Vertrauen") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Wenig Vertrauen") {2}
  #     #         else if (x == "Gar kein Vertrauen") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_trust_fede <- transform6(input$cov_trust_fede)
  #     # cov_trust_parl <- transform6(input$cov_trust_parl)
  #     # cov_trust_part <- transform6(input$cov_trust_part)
  #     # cov_trust_agen <- transform6(input$cov_trust_agen)
  #     # cov_trust_cant <- transform6(input$cov_trust_cant)
  #     # cov_trust_bag <- transform6(input$cov_trust_bag)
  #     # cov_trust_heal <- transform6(input$cov_trust_heal)
  #     # 
  #     # 
  #     # transform7 <- function(x){
  #     #     unlist(lapply(x, function(x)
  #     #         if (is.na(x)) {NA}
  #     #         else if (x == "Weiss nicht / keine Antwort") {6}
  #     #         else if (x == "Zufrieden") {5}
  #     #         else if (x == "Eher zufrieden") {4}
  #     #         else if (x == "Weder noch") {3}
  #     #         else if (x == "Eher nicht zufrieden") {2}
  #     #         else if (x == "Nicht zufrieden") {1}
  #     #     )
  #     #     )
  #     # }
  #     # 
  #     # cov_perf_fed <- transform7(input$cov_perf_fed)
  #     # cov_perf_cant <- transform7(input$cov_perf_cant)
  #     
  #     # dat <- tibble(
  #     #     # "civi_stat" = 1,
  #     #     # "fin_cond" = 1,
  #     #     # "pol_party" = pol_party,
  #     #     # "renew_heating" = 1,
  #     #     # "left_right" = input$left_right,
  #     #     # "prior_benefit" = prior_benefit,
  #     #     # "ren_driver" = ren_driver,
  #     #     # "home_owner" = 1,
  #     #     # "educ" = educ,
  #     #     # "empl_sect" = empl_sect,
  #     #     # "empl_stat" = 2,
  #     #     # "gender" = gender,
  #     #     # "region" = region,
  #     #     
  #     #     "civi_stat" = 1,
  #     #     "fin_cond" = 1,
  #     #     "pol_party" = if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
  #     #     else if (input$pol_party == "SP (Sozialdemokratische Partei)") {2}
  #     #     else if (input$pol_party == "FDP.Die Liberalen (Freisinnig Demokratische Partei)") {3}
  #     #     else if (input$pol_party == "CVP (Christlichdemokratische Volkspartei)") {4}
  #     #     else if (input$pol_party == "GPS (Grüne Partei Schweiz)") {5}
  #     #     else if (input$pol_party == "GLP (Grünliberale Partei)") {6}
  #     #     else if (input$pol_party == "BDP (Bürgerlich Demokratische Partei)") {7}
  #     #     else if (input$pol_party == "EVP (Evangelische Volkspartei der Schweiz)") {8}
  #     #     else if (input$pol_party == "Lega dei Ticinesi") {9}
  #     #     else if (input$pol_party == "PdA (Partei der Arbeit Schweiz)") {10}
  #     #     else if (input$pol_party == "MCG (Mouvement Citoyens Genevois)") {11}
  #     #     else if (input$pol_party == "CSP (Christlichsoziale Partei Schweiz)") {12}
  #     #     else if (input$pol_party == "EDU (Eidgenössisch-Demokratische Union)") {13}
  #     #     else if (input$pol_party == "Sol. (SolidaritéS)") {14}
  #     #     else if (input$pol_party == "Andere:") {15}
  #     #     else if (input$pol_party == "Keine") {16}
  #     #     else if (input$pol_party == "Weiss nicht / keine Antwort") {17},
  #     #     "renew_heating" = 1,
  #     #     "left_right" = 1,
  #     #     "prior_benefit" = 1,
  #     #     "ren_driver" = 1,
  #     #     "home_owner" = 1,
  #     #     "educ" = 1,
  #     #     "empl_sect" = 1,
  #     #     "empl_stat" = 2,
  #     #     "gender" = 1,
  #     #     "region" = 1,
  #     #     
  #     #     # "sal_ahv" = sal_ahv,
  #     #     # "sal_unemp" = sal_unemp,
  #     #     # "sal_refug" = sal_refug,
  #     #     # "sal_eu" = sal_eu,
  #     #     # "sal_health" = sal_health,
  #     #     # "sal_energy" = sal_energy,
  #     #     # "sal_traff" = sal_traff,
  #     #     # "sal_glob" = sal_glob,
  #     #     # "sal_env" = sal_env,
  #     #     # "sal_crim" = sal_crim,
  #     #     # "sal_uneq" = sal_uneq,
  #     #     # "sal_cult" = sal_cult,
  #     #     # "sal_for" = sal_for,
  #     #     # "sal_pop" = sal_pop,
  #     #     # "swiss_pol_comp" = swiss_pol_comp,
  #     #     # "swiss_pol_effect" = swiss_pol_effect,
  #     #     # "swiss_pol_effic" = swiss_pol_effic,
  #     #     # "swiss_pol_just" = swiss_pol_just,
  #     #     # "swiss_pol_lead" = swiss_pol_lead,
  #     #     # "swiss_pol_vol" = swiss_pol_vol,
  #     #     # "swiss_pol_subs" = swiss_pol_subs,
  #     #     # "swiss_pol_tax" = swiss_pol_tax,
  #     #     # "swiss_pol_regu" = swiss_pol_regu,
  #     #     # "swiss_pol_tech" = swiss_pol_tech,
  #     #     # "swiss_pol_beha" = swiss_pol_beha,
  #     #     # "swiss_pol_comb" = swiss_pol_comb,
  #     #     # "swiss_pol_fed" = swiss_pol_fed,
  #     #     # "sect_build" = sect_build,
  #     #     # "sect_trans" = sect_trans,
  #     #     # "sect_food" = sect_food,
  #     #     # "sect_avia" = sect_avia,
  #     #     # "sect_indu" = sect_indu,
  #     #     # "sect_wast" = sect_wast,
  #     #     "know_targ" = 1,
  #     #     "know_build" = 1,
  #     #     "know_trans" = 1,
  #     #     "know_food" = 1,
  #     #     "know_avia" = 1,
  #     #     "know_wast" = 1,
  #     #     "efficiency" = 3,
  #     #     "effectiveness" = 3,
  #     #     "competitiveness" = 3,
  #     #     "justice" = 3,
  #     #     "transformation" = 3
  #     #     # "switch" = switch,
  #     #     # "freq_user_driver" = freq_user_driver,
  #     #     # "freq_user_fly" = freq_user_fly,
  #     #     # "freq_user_fish" = freq_user_fish,
  #     #     # "freq_user_meat" = freq_user_meat,
  #     #     # "freq_user_meat_subs" = freq_user_meat_subs,
  #     #     
  #     #     # "Attrib1" = Attrib1,
  #     #     # "Attrib2" = Attrib2,
  #     #     # "Attrib3" = Attrib3,
  #     #     # "Attrib4" = Attrib4,
  #     #     # "Attrib5" = Attrib5,
  #     #     # "Attrib6" = Attrib6,
  #     #     # "cov_imp" = input$cov_imp,
  #     #     # "cov_fin" = cov_fin,
  #     #     # "cov_job" = cov_job,
  #     #     # "cov_health" = cov_health,
  #     #     # "cov_info" = cov_info,
  #     #     # "cov_know_fin" = cov_know_fin,
  #     #     # "cov_know_fed" =  cov_know_fed,
  #     #     # "cov_know_pub" = cov_know_pub,
  #     #     # "cov_know_10i" = cov_know_10i,
  #     #     # "cov_know_ref" = cov_know_ref,
  #     #     # "cov_prof" = cov_prof,
  #     #     # "cov_trust_fede" = cov_trust_fede,
  #     #     # "cov_trust_parl" = cov_trust_parl,
  #     #     # "cov_trust_part" = cov_trust_part,
  #     #     # "cov_trust_agen" = cov_trust_agen,
  #     #     # "cov_trust_cant" = cov_trust_cant,
  #     #     # "cov_trust_bag" = cov_trust_bag,
  #     #     # "cov_trust_heal" = cov_trust_heal,
  #     #     # "cov_perf_fed" = cov_perf_fed,
  #     #     # "cov_perf_cant" = cov_perf_cant
  #     # )
  #     
  #     # dplyr::bind_cols(bind_rows(predict_probability(model_rate_activism, dat)),
  #     #                  rep(c(""), each = nrow(predict_probability(model_rate_activism, dat)))) %>%
  #     #     `colnames<-`(c("Zustimmmung", "Wert", "dv")) %>%
  #     #     dplyr::mutate(
  #     #         Wert = ifelse(Zustimmmung == ".pred_2", Wert*(-1), Wert),
  #     #         Wert = ifelse(Zustimmmung == ".pred_1", Wert*(-1), Wert),
  #     #         Wert = ifelse(Zustimmmung == ".pred_3", Wert/2, Wert),
  #     #     ) %>%
  #     #     dplyr::bind_rows(.[.$Zustimmmung == ".pred_3",] %>% dplyr::mutate(Wert = Wert *(-1))) %>%
  #     #     dplyr::mutate(Zustimmmung = factor(Zustimmmung, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
  #     #     ggplot2::ggplot(.) +
  #     #     ggplot2::geom_bar(aes(x = dv, y = Wert, fill = Zustimmmung), stat = "identity", position = position_stack(reverse = TRUE)) +
  #     #     ggplot2::theme_minimal() +
  #     #     ggplot2::coord_flip() +
  #     #     ggplot2::ylim(-1,1) +
  #     #     ggplot2::labs(
  #     #         title = "Political Mobilisation",
  #     #         x = "",
  #     #         y = "Probability"
  #     #     ) +
  #     #     ggplot2::scale_fill_manual(name = "", labels =c( "Viel Aufwand zur Unterstützung", "Etwas Aufwand zur Unterstützung", "Viel Aufwand zur Verhinderung", "Etwas Aufwand zur Verhinderung", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
  #     #     ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)), legend.position = "bottom") +
  #     #     guides(fill=guide_legend(nrow=2,byrow=TRUE))
  #     
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)
