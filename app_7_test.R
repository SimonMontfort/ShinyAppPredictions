# map https://stackoverflow.com/questions/48432061/turn-states-on-a-map-into-clickable-objects-in-shiny 
### What to do for publishing?
# - upload all data and all models
# - comment working directory when publishing
# - for logs check:  rsconnect::showLogs()
# setwd("/Volumes/Transcend/Uni/doktorat/Predicting Core Beliefs/ShinyApp/PredictingVote")

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidymodels)
library(tidyverse)
# library(dplyr)
library(stringr)
# library(ggplot2)
library(plotly)
# library(xgboost)
library(DALEXtra)
library(DALEX)
# library(scales)
# library(readxl)
# library(ranger)

data <- bind_cols(c(".pred_1", ".pred_2", ".pred_3", ".pred_4", ".pred_5"),
      c(-0.561, -0.0227, 0.192, 0.0162, 0.0166),
      c("Mobilisation", "Mobilisation", "Mobilisation", "Mobilisation", "Mobilisation"))
colnames(data) <-   c("Zustimmmung", "Wert", "dv") 

model_rate <- readRDS("model_forest_rate.rds")
model_rate_activism <- readRDS("model_forest_rate_activism.rds")

ggplot_pdp <- function(obj, x, title, x_labs) {

    p <-
        as_tibble(obj$agr_profiles) %>%
        mutate(`_label_` = stringr::str_remove(`_label_`, "Tidy-Boosting.")) %>%
        ggplot(aes(`_x_`, `_yhat_`)) +
        geom_line(
            data = as_tibble(obj$cp_profiles) %>%
                mutate(`_label_` = stringr::str_remove(`_label_`, "Tidy-Boosting.")),
            aes(x = {{ x }}, group = `_ids_`, color = `_label_`),
            size = 0.5, alpha = 0.01) +
        scale_x_continuous(labels = x_labs, n.breaks = length(x_labs)) +
        facet_grid(.~`_label_`, labeller = as_labeller(c("1" = "Oppose fully", "2" = "Oppose", "3" = "Neither nor", "4" = "Support", "5" = "Support fully"))) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom") +
        labs(
            title = title,
            x = "Prior Benefit",
            y = "Predicted Support"
        ) +
        guides(color=guide_legend(title = "Legend"))

    num_colors <- n_distinct(obj$agr_profiles$`_label_`)

    if (num_colors > 1) {
        p <- p + geom_line(aes(color = `_label_`), size = 1.2, alpha = 0.8)
    } else {
        p <- p + geom_line(color = "midnightblue", size = 1.2, alpha = 0.8)
    }

    p
}


# function to predict the probability 
predict_probability <- function(model, dat){
    stats::predict(model, dat, type = "prob") %>%
        tidyr::gather() %>%
        dplyr::mutate(value = as.numeric(value)) 
}

prediction_category <- function(model, dat){
    predict(model, dat) %>%
    mutate_all(., as.numeric)
}

prediction_main_category <- function(prediction_prob){
    prediction_prob_main <- prediction_prob %>% 
        arrange(desc(value)) %>%
        dplyr::slice(1) %>%
        select(value) %>% 
        mutate(value = as.numeric(as.character(value)))
}

h <- "10em"

choice_swiss_clim_pol <- c("Lehne voll und ganz ab",
                           "Lehne eher ab",
                           "Weder noch",
                           "Unterstütze eher",
                           "Unterstütze voll und ganz")

ui <- dashboardPage(skin = "blue",
    dashboardHeader(title = "Zustimmung"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Interakive Model Vorhersagen"
                ,tabName = "interact_tab"
                ,selected=TRUE
                # icon = icon("snowflake")
            ),
            menuItem(
                "Resultate: Effekte",
                tabName = "results_tab_effects"
            )
        )
        ,collapsed = T
    ),
    dashboardBody(
        fluidRow(
   
        ########## CSS Style ########## 
        tags$head(tags$style(HTML('
            .main-header .logo {
                font-family: "Helvetica", Helvetica, "Helvetica";
                font-weight: bold;
                font-size: 24px;
            }
            
            .skin-red .main-sidebar {background-color:  yellow !important;}
            
            .box.box-solid.box-primary>.box-header {
                color:#fff;
                background:#001f3f!important
            }
            .skin-blue .main-header .logo {
                background-color: #001f3f!important;
            }
            .skin-blue .main-header .navbar {
                background-color: #001f3f!important;
            }
        
            .irs--shiny .irs-bar { 
                border-top: #001f3f;
                background: #001f3ft;
                border-bottom: #001f3f;  
            }
            
            .box.box-solid.box-primary {
                border: #001f3f;
            }
            
            .box-body {
                # height: 300px;
            }
            
            .small-box h3 {
                white-space: none; 
            }
                                  '))),
        
        ########## Items ##########     
    tabItems(
        tabItem(
            tabName = "interact_tab",
            box(title = "Zielsetzung: Auswertung der Umfrageergebnisse zur Abstimmung des CO2-Gesetz vom 13.06.21"
                ,textOutput("zielsetzung")
                ,status = "primary"
                ,solidHeader = TRUE
                ,width = 12
            ),
            box(title = "Vorhergesagte Wahrscheinlichkeit: Zustimmung zu ambitionierter Klimapolitik"
                ,footer = "Frage: Gehen Ihnen die Massnahmen im zur Abstimmung stehenden CO2-Gesetz zu wenig weit oder zu weit? Die Ziele und Massnahmen gehen mir... 
                [Klar zu wenig weit, Eher zu wenig weit, Unentschieden, Eher zu weit, Klar zu weit]"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("plot2", height = "120px")
                ,width = 12
            ),
            box(title = "Vorhergesagte Wahrscheinlichkeit: Politische Mobilisierung"
                ,footer = "Frage: Wieviel Aufwand würden Sie persönlich betreiben, um den jeweiligen Vorschlag politisch zu unterstützen oder zu verhindern (z.B. Petition unterschreiben, Petition starten, Demonstrieren, Spenden etc.)? Die Ziele und Massnahmen gehen mir... 
                [Viel Aufwand zur Unterstützung, Etwas Aufwand zur Unterstützung, Weder noch, Etwas Aufwand zur Verhinderung, Viel Aufwand zur Verhinderung]"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput("plot3", height = "120px")
                ,width = 12
            ),
            box(selectInput("home_owner", label = "Sind sie Hausbesitzer*in",
                            choices = c("Ja",
                                        "Nein")
            ), height = h),
            box(selectInput("renew_heating", label = "Wie wird das Gebäude geheizt, in dem Sie wohnen? (Mehrfachantworten möglich)",
                            choices = c("Wärmepumpe, Fernwärme, Erdsonde, Strom, Solarenergie oder Holz, Holzschnitzel, Holzpellets",
                                        "Ölheizung, Gasheizung oder Weiss nicht / keine Antwort")
            ), height = h),
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("educ", label = "Welches ist die höchste Ausbildung, die Sie mit einem Zeugnis oder Diplom abgeschlossen haben?",
                                    choices = c("Grundausbildung (inklusive nicht abgeschlossen)",
                                                "Erstausbildung",
                                                "Zweitausbildung")
                    ), height = h),
                    box(selectInput("civi_stat", label = "Welches ist Ihr Zivilstand?",
                                    choices = c("Ledig",
                                                "Verheiratet",
                                                "In eingetragener Partnerschaft",
                                                "Verwitwet",
                                                "Geschieden",
                                                "Weiss nicht / keine Antwort")
                    ), height = h),
                ),
                column(
                    width = 12,
                    box(selectInput("empl_stat", label = "Welches ist Ihre momentane Beschäftigungssituation?",
                                    choices = c("Vollzeit beschäftigt",
                                                "Teilzeit beschäftigt",
                                                "Ausbildung",
                                                "Andere")
                    ), height = h),
                    box(selectInput("empl_sect", label = "In welchem Berufssektor sind Sie aktuell tätig?",
                                    choices = c("Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft",
                                                "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk",
                                                "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen",
                                                "weiss nicht / keine Antwort oder arbeitet nicht")
                    ), height = h)
                )
                ),
            
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("fin_cond", label = "Vom Einkommen muss man einen Teil gleich wieder ausgeben, z.B. für Miete und
                            Versicherungen. Mit dem, was Ihnen dann noch bleibt, würden Sie sagen, dass Sie da gut auskommen?",
                                    choices = c("Ja",
                                                "Es geht so",
                                                "Nein",
                                                "Weiss nicht / keine Antwort")
                    ), height = h),
                    # box(sliderInput("QID23", label = "Seit welchem Jahr wohnen Sie ununterbrochen an Ihrem jetzigen Wohnort?",
                    #                 min = 1, max = 20, value = 2
                    # )),
                ),
                column(
                    width = 12,
                    box(sliderInput("left_right", label = "Links, Mitte und Rechts sind drei Begriffe, die häufig gebraucht werden, um
                            politische Ansichten zu charakterisieren.
                            Wo sehen Sie sich selber auf einer Skala von 0 (ganz links) bis 10 (ganz rechts)?",
                                    min = 0, max = 10, value = 5
                    ), height = h),
                    box(selectInput("pol_party", label = "Welche Partei entspricht in den Zielen und Forderungen am ehesten Ihren eigenen 
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
                    ), height = h),
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("ren_driver", label = "Fahren sie ein Auto mit erneuerbarem Treibstoff?",
                                    choices = c("Ja",
                                                "Nein")
                    ), height = h),
                    box(selectInput("prior_benefit", label = "Es gibt bereits Massnahmen in der Schweiz, die klimafreundliches Verhalten belohnen.
                                    Wie ist es bei Ihnen persönlich, würden Sie sagen, dass diese Massnahmen Sie bereits beeinflusst haben?
                                    Bisherige politische Massnahmen haben mein Verhalten...",
                                    choices = c("Stark beeinflusst",
                                                "Etwas beeinflusst",
                                                "Eher nicht beeinflusst",
                                                "Überhaupt nicht beeinflusst",
                                                "Weiss nicht / keine Antwort")
                    ), height = h),
                ),
            ), 
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("salience", label = "Unten sehen Sie einige Themen aufgelistet, über die in der letzten Zeit viel diskutiert und geschrieben wurde.
                    Welche dieser Themen sehen Sie als [das] wichtigste Problem der Schweiz an?",
                                    choices = c("Altersvorsorge/AHV",
                                                "Arbeitslosigkeit",
                                                "Flüchtlinge",
                                                "Verhältnis der Schweiz zur Europäischen Union",
                                                "Gesundheitswesen / Krankenversicherung",
                                                "Energieversorgung",
                                                "Verkehr",
                                                "Globalisierung der Wirtschaft / Freihandel",
                                                "Umweltschutz / Klimawandel",
                                                "Kriminalität",
                                                "Ungleichheit bei Einkommen und Vermögen",
                                                "Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen",
                                                "Ausländische Arbeitskräfte in der Schweiz",
                                                "Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung")
                    ), height = h),
                    box(selectInput("gender", label = "Was ist Ihr Geschlecht?",
                                    choices = c("Frau",
                                                "Mann")
                    ), height = h),
                ),
            ), 
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("region", label = "In welcher Region wohnen Sie? (Hauptwohnsitz)",
                                    choices = c("Genferseeregion",
                                                "Mittelland",
                                                "Nordwestschweiz",
                                                "Zürich",
                                                "Ostschweiz",
                                                "Zentralschweiz",
                                                "Tessin")
                    ), height = h),
                    box(selectInput("switch", label = "Seit ungefähr wann [haben Sie Ihr Verhalten verändert (erneuerbare Treibstoffe/ Fleisch)]?",
                                    choices = c("Immer schon",
                                                "Seit Kürzerem (weniger als zwei Jahre) oder seit Längerem (mehr als zwei Jahre)")
                    ), height = h),
                ),
            ), 
            # fluidRow(
            #     column(
            #         width = 12,
            #         box(selectInput("switch_driver_bio", label = "Seit ungefähr wann nutzen Sie Biotreibstoffe als Antrieb für Ihr Auto?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #         box(selectInput("switch_driver_ev", label = "Seit ungefähr wann nutzen Sie ein elektrisches Auto?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #     ),
            #     
            # ), 
            # fluidRow(
            #     column(
            #         width = 12,
            #         box(selectInput("switch_driver_gas", label = "Seit ungefähr wann nutzen Sie Biotreibstoffe als Antrieb für Ihr Auto?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #         box(selectInput("switch_driver_meat_1d", label = "Seit ungefähr wann essen Sie an maximal einem Tag pro Woche Fleisch?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #     ),
            #     
            # ), 
            # fluidRow(
            #     column(
            #         width = 12,
            #         box(selectInput("switch_driver_meat_2d", label = "Seit ungefähr wann essen Sie an maximal zwei Tagen pro Woche Fleisch?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #         box(selectInput("switch_driver_vege", label = "Seit ungefähr wann sind sie Vegetarier?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #     ),
            #     
            # ),
            # fluidRow(
            #     column(
            #         width = 12,
            #         box(selectInput("switch_driver_vega", label = "Seit ungefähr wann sind sie Veganer?",
            #                         choices = c("immer schon",
            #                                     "Seit Kürzerem (weniger als zwei Jahre)",
            #                                     "Seit Längerem (mehr als zwei Jahre)")
            #         ), height = h),
            #     ),
            #     # column(
            #     #     width = 12,
            #     #     box(selectInput("switch_driver_vega", label = "Seit ungefähr wann nutzen Sie ein elektrisches Auto?",
            #     #                     choices = c("immer schon",
            #     #                                 "Seit Kürzerem (weniger als zwei Jahre)",
            #     #                                 "Seit Längerem (mehr als zwei Jahre)")
            #     #     ), height = h),
            #     # ),
            #     
            # ),
            
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("freq_user_driver", label = "Ungefähr wie viele Kilometer Auto fahren Sie pro Jahr?",
                                    choices = c("0-10'000 km",
                                                "10'001-20'000 km",
                                                "20'001-30'000 km",
                                                "30'001-40'000 km",
                                                "40'0001-50'000 km",
                                                "mehr als 50'000 km",
                                                "Weiss nicht / keine Antwort"
                                                )
                    ), height = h),
                    box(selectInput("freq_user_fly", label = "Ungefähr wie viele Flüge haben Sie in den letzten zwei Jahren vor Beginn der Covid Krise 
                                    (im Zeitraum 2018-2020) gemacht? Zählen Sie Hin- und Rückflug als einen Flug.",
                                    choices = c("0-1 Flüge",
                                                "1-2 Flüge",
                                                "2-3 Flüge",
                                                "4-5 Flüge",
                                                "6-7 Flüge",
                                                "8-9 Flüge",
                                                "mehr als 9 Flüge")
                    ), height = h),
                ),
                
            ),
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("freq_user_fish", label = "An ungefähr wie vielen Tagen in einer normalen Woche essen Sie Fisch?",
                                    choices = c("An weniger als einem Tag",
                                                "1",
                                                "2",
                                                "3",
                                                "4",
                                                "5",
                                                "6",
                                                "7"
                                    )
                    ), height = h),
                    box(selectInput("freq_user_meat", label = "An ungefähr wie vielen Tagen in einer normalen Woche essen Sie Fleisch?",
                                    choices = c("An weniger als einem Tag",
                                                "1",
                                                "2",
                                                "3",
                                                "4",
                                                "5",
                                                "6",
                                                "7"
                                    )
                    ), height = h),
                ),
            ),
            fluidRow(
                column(
                    width = 12,
                    box(selectInput("freq_user_meat_subs", label = "Wie oft haben Sie im letzten Jahr Fleischersatzprodukte gegessen?
                                    Fleischersatzprodukte sind Nahrungsmittel, welche die Textur, den Geschmack, die Nährwerte von Fleischprodukten nachahmen und z.B. als Burger, 
                                    Würste, in Hackfleisch-Form oder Chicken Nuggets konsumiert werden. Pflanzliche Fleischersatzprodukte werden üblicherweise aus Zutaten wie 
                                    Bohnen, Weizen, vegetarischen Ölen oder anderen pflanzlichen Produkten hergestellt.",
                                    choices = c("Nie",
                                                "Selten",
                                                "Einmal pro Monat",
                                                "Mehrmals pro Monat",
                                                "Ungefähr ein mal pro Woche",
                                                "Mehrmals pro Woche",
                                                "Jeden Tag"
                                    )
                    ), height = h),
                ),
                # column(
                #     width = 12,
                #     box(selectInput("freq_user_meat", label = "Seit ungefähr wann nutzen Sie ein elektrisches Auto?",
                #                     choices = c("An weniger als einem Tag",
                #                                 "1",
                #                                 "2",
                #                                 "3",
                #                                 "4",
                #                                 "5",
                #                                 "6",
                #                                 "7"
                #                     )
                #     ), height = h),
                # ),
            ),
            
            fluidRow(
                column(
                    width = 12,
                    box(title = "Politische Klimaschutzmassnahmen wie das CO2-Gesetz können auf Basis unterschiedlicher Kriterien beurteilt werden. Bitte lesen Sie sich folgende 
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
                                ,width = 12
                            ),
                    box(title = "Geben Sie bitte an, wie sehr Sie die folgenden Aussagen ablehnen oder unterstützten. Bei der Ausgestaltung der Schweizer Klimapolitik 
                                 sollte vor allem…",
                        radioButtons("swiss_pol_comp",
                                     "…die Wettbewerbsfähigkeit der Schweizer Wirtschaft berücksichtigt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_effect",
                                     "…die ökologische Wirksamkeit (Erreichung der Pariser Klimaziele) berücksichtigt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_effic",
                                     "…die ökonomische Effizienz (möglichst geringer Aufwand für die Zielerreichung) berücksichtigt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_just",
                                     "…die soziale Gerechtigkeit berücksichtigt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_lead",
                                     "…die Vorreiterrolle der Schweiz in der internationalen Klimapolitik berücksichtigt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_vol",
                                     "…auf freiwillige Massnahmen (z.B. Informationskampagnen und Selbstverpflichtungen) gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_subs",
                                     "…auf Subventionen gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_tax",
                                     "…auf CO2-Abgaben gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_regu",
                                     "…auf regulatorische Massnahmen (z.B. Emissionsstandards und Verbote) gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_tech",
                                     "…auf Massnahmen zur Förderung technologischer Innovationen gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_beha",
                                     "…auf Massnahmen zur Förderung von Verhaltensänderungen gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_comb",
                                     "…auf eine Kombination verschiedener Massnahmen gesetzt werden.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("swiss_pol_fed",
                                     "…die Kompetenz beim Bund (nicht den Kantonen) liegen.",
                                     choices = choice_swiss_clim_pol,
                                     selected = "Weder noch",
                                     inline=T
                        )
                    ,width = 12
                    ),
                    # box(title = "Wie sehr unterstützen oder lehnen Sie Emissionsreduktionen in den folgenden Bereichen ab?",
                    #     radioButtons("sect_build",
                    #                  "Im Gebäudebereich",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     ),
                    #     radioButtons("sect_trans",
                    #                  "Im Strassenverkehr",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     ),
                    #     radioButtons("sect_food",
                    #                  "Im Ernährungs- und Agrarbereich",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     ),
                    #     radioButtons("sect_avia",
                    #                  "Im Flugverkehr",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     ),
                    #     radioButtons("sect_indu",
                    #                  "Im Industriebereich",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     ),
                    #     radioButtons("sect_wast",
                    #                  "Im Abfallbereich",
                    #                  choices = c("Lehne voll und ganz ab", "Lehne ab", "Weder noch", "Unterstütze", "Unterstütze voll und ganz"),
                    #                  selected = "Weder noch",
                    #                  inline=T
                    #     )
                    #     ,width = 12
                    # ),
                    box(title = "Wie sehr unterstützen oder lehnen Sie Emissionsreduktionen in den folgenden Bereichen ab?",
                        radioButtons("know_targ",
                                     HTML("Emissionsreduktionsziel (1990-2030):<br/>
                                     Das Reduktionsziel betrifft die im In- und Ausland durchgeführte Verminderung der in der Schweiz ausgestossenen Treibhausgase (z.B. CO2 oder Methan)."),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("know_build",
                                     HTML("Abgabe auf Brennstoffe:<br/>
                                     Der Bund erhebt eine Abgabe auf der Herstellung, Erzeugung, Gewinnung und Einfuhr von fossilen Brennstoffen (CO2-Abgabe)."),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("know_trans",
                                     HTML("Abgabe auf Treibstoffe:<br/>
                                     Wer Benzin und Diesel importiert, muss verstärkt in den Klimaschutz investieren, was zu höheren Kosten führt als bisher. Für diese Investitionen darf an der Tankstelle ein Zuschlag erhoben werden."),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("know_food",
                                     HTML("Abgabe auf Fleisch:<br/>
                                     Die zur Abstimmung stehende Vorlage sieht eine CO2-Abgabe im Ernährungsbereich vor."),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("know_avia",
                                     HTML("Flugticketabgabe:<br/>
                                     Der Bund erhebt eine Abgabe auf Flugtickets von Luftverkehrsunternehmen."),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("know_wast",
                                     HTML("Verwendung der Abgabe:<br/>
                                     Die Einnahmen aus der CO2-Abgabe werden r&uuml;ckerstattet sowie zur Erreichung der Emissionsreduktionsziele eingesetzt"),
                                     choices = c("Im Gesetz enthalten", "Nicht im Gesetz enthalten", "Weiss nicht"),
                                     # selected = "Weder noch",
                                     inline=T
                        )
                        ,width = 12
                    ),
                    box(title = "Wie sehr unterstützen oder lehnen Sie Emissionsreduktionen in den folgenden Bereichen ab?",
                        radioButtons("Attrib1",
                                     HTML("Emissionsreduktionsziel (1990-2030):<br/>
                                     Das Reduktionsziel betrifft die im In- und Ausland durchgeführte Verminderung der in der Schweiz ausgestossenen Treibhausgase (z.B. CO2 oder Methan)."),
                                     choices = c("40%", "50%", "60%", "70%", "80%"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("Attrib2",
                                     HTML("Abgabe auf Brennstoffe:<br/>
                                     Der Bund erhebt eine Abgabe auf der Herstellung, Erzeugung, Gewinnung und Einfuhr von fossilen Brennstoffen (CO2-Abgabe)."),
                                     choices = c("No tax on petrol", "0.14 Fr./l petrol", "0.28 Fr./l petrol", "0.42 Fr./l petrol", "0.56 Fr./l petrol"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("Attrib3",
                                     HTML("Abgabe auf Treibstoffe:<br/>
                                     Wer Benzin und Diesel importiert, muss verstärkt in den Klimaschutz investieren, was zu höheren Kosten führt als bisher. Für diese Investitionen darf an der Tankstelle ein Zuschlag erhoben werden."),
                                     choices =  c("No tax on heating oil", "0.16 Fr./l heating oil", "0.31 Fr./l heating oil", "0.47 Fr./l heating oil", "0.63 Fr./l heating oil"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("Attrib4",
                                     HTML("Abgabe auf Fleisch:<br/>
                                     Die zur Abstimmung stehende Vorlage sieht eine CO2-Abgabe im Ernährungsbereich vor."),
                                     choices = c("No tax on meat", "0.77 Fr./kg meat", "1.53 Fr./kg meat", "2.30 Fr./kg meat", "3.07 Fr./kg meat"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("Attrib5",
                                     HTML("Flugticketabgabe:<br/>
                                     Der Bund erhebt eine Abgabe auf Flugtickets von Luftverkehrsunternehmen."),
                                     choices = c("No tax", "10 Fr. for short- and 30 Fr. for long-distance", "25 Fr. for short- and 75 Fr. for long-distance", "40 Fr. for short- and 120 Fr. for long-distance", "55 Fr. for short- and 165 Fr. for long-distance"),
                                     # selected = "Weder noch",
                                     inline=T
                        ),
                        radioButtons("Attrib6",
                                     HTML("Verwendung der Abgabe:<br/>
                                     Die Einnahmen aus der CO2-Abgabe werden r&uuml;ckerstattet sowie zur Erreichung der Emissionsreduktionsziele eingesetzt"),
                                     choices = c("Exclusively lump sum reimbursement", "Mostly lump sum reimbursement", "Lump sum reimbursement und investment into climate protection", "Mostly investment into climate protection", "Exclusively investment into climate protection"), 
                                     # selected = "Weder noch",
                                     inline=T
                        )
                        ,width = 12
                    ),
                ),
                
            ), 
            
                      
            
            
            
            
            box(sliderInput("cov_imp", label = HTML("Wenn Sie an das letzte halbe Jahr der Covid-19-Pandemie denken, wie schätzen Sie Ihre persönliche Betroffenheit von der Covid-19-Krise 
                            insgesamt ein?"),
                            min = 1, max = 10, value = 5
            )),
            box(radioButtons("cov_fin", label = "Wie hat sich Ihre finanzielle Situation in der Covid-19-Pandemie verändert? Meine finanzielle Situation hat sich ...",
                              choices = c("klar verschlechtert.", 
                                          "eher verschlechtert.", 
                                          "nicht verändert.", 
                                          "eher verbessert.", 
                                          "klar verbessert.",
                                          "Weiss nicht / keine Antwort"),
                             selected = "nicht verändert.",

            )),
            
            box(radioButtons("cov_job", label = "Wie hat sich Ihre Jobsituation im letzten halben Jahr aufgrund der Covid-19-Pandemie verändert? 
                             Bitte wählen Sie alle Antworten aus, die auf Sie zutreffen.",
                             choices = c("Keine Änderung", 
                                         "Ich arbeitete in Kurzarbeit.", 
                                         "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.",
                                         "Ich arbeitete (teilweise) im Homeoffice.",
                                         "Ich musste Überzeit kompensieren oder Ferien nehmen, weil es zu wenig Arbeit gab.",
                                         "Ich arbeitete mehr als üblich.",
                                         "Ich habe meinen Job verloren.",
                                         "Ich musste meinen Betrieb schliessen.",
                                         "Mein Unternehmen/ meine Agentur musste Veranstaltungen absagen.",
                                         "Mein Unternehmen/ meine Branche profitierte von der Covid-19-Krise (z.B. viel mehr Arbeit/Nachfrage).",
                                         "Mein Unternehmen/ meine Branche litt unter der Covid-19-Krise (z.B. weniger Arbeit/Nachfrage, Teilschliessung, Kapazitätsbeschränkungen).",
                                         "Anderes (Bitte angeben)",
                                         "Weiss nicht / keine Antwort"),
                             selected = "Keine Änderung",
                             
            )),
            
            box(radioButtons("cov_health", label = "Waren Sie direkt oder indirekt betroffen von den gesundheitlichen Auswirkungen der Covid-19-Pandemie? 
                             Bitte wählen Sie alle Antworten aus, die auf Sie zutreffen.",
                             choices = c("Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.", 
                                         "Ich musste aufgrund einer behördlichen Anordnung in Quarantäne.", 
                                         "Ich bin selber an Covid-19 erkrankt.",
                                         "Personen aus meinem engen Familienkreis sind erkrankt oder mussten in Quarantäne.",
                                         "Ich bin in meinem Alltag einem erhöhten Risiko einer Ansteckung ausgesetzt (z.B. Beruf mit Personenkontakt).",
                                         "Ich bin ein Risikopatient.",
                                         "Anderes (Bitte angeben)",
                                         "Weiss nicht / Keine Antwort"
                                         ),
                             selected = "Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.",
                             
            )),

            box(radioButtons("cov_info", label = "Als wie gut informiert über das Covid-19-Gesetz schätzen Sie sich selbst ein? Ich fühle mich...",
                             choices = c("gut informiert.", 
                                         "eher informiert.", 
                                         "eher nicht informiert.",
                                         "gar nicht informiert.",
                                         "Weiss nicht / keine Antwort"
                             ),
                             selected = "eher informiert.",
            )),
            
            box(radioButtons("cov_prof", label = "Im Covid-19-Gesetz werden verschiedene Leistungen geregelt. Wie stark stimmen Sie der folgenden Aussage zu: Alle Personen, 
                             die von diesen Massnahmen profitieren, profitieren zu Recht davon.",
                             choices = c("Stimme nicht zu",
                                         "Stimme eher nicht zu",
                                         "Weder noch",
                                         "Stimme eher zu",
                                         "Stimme zu"
                             ),
                             selected = "Weder noch",
            )),
            
            box(title = "Im Folgenden lesen Sie einige Aussagen zum Covid-19-Gesetz. Geben Sie an, ob die Aussagen aus Ihrer Sicht zutreffen oder nicht.",
                radioButtons("cov_know_fin",
                             HTML("Das Gesetz regelt, dass Personen, die ihre Arbeit aufgrund einer behördlich verordneten Quarantäne unterbrechen müssen, keine Entschädigung erhalten."),
                             choices = c("Trifft nicht zu", "Trifft zu", "Weiss nicht / keine Antwort"),
                             selected = "Weiss nicht / keine Antwort",
                             inline=T
                ),
                radioButtons("cov_know_fed",
                             HTML("Das Gesetz regelt, dass der Bund sich an kantonalen Härtefallhilfen (für Restaurants, Hotels, Eventbranche) beteiligt."),
                             choices = c("Trifft nicht zu", "Trifft zu", "Weiss nicht / keine Antwort"),
                             selected = "Weiss nicht / keine Antwort",
                             inline=T
                ),
                radioButtons("cov_know_pub",
                             HTML("Das Gesetz schreibt vor, dass nur geimpfte Personen, genesene oder getestete Personen  an Veranstaltungen teilnehmen dürfen."),
                             choices = c("Trifft nicht zu", "Trifft zu", "Weiss nicht / keine Antwort"),
                             selected = "Weiss nicht / keine Antwort",
                             inline=T
                ),
                radioButtons("cov_know_10i",
                             HTML("Das Gesetz schreibt vor, dass sich bis auf Weiteres maximal 10 Personen in Innenräumen treffen dürfen."),
                             choices = c("Trifft nicht zu", "Trifft zu", "Weiss nicht / keine Antwort"),
                             selected = "Weiss nicht / keine Antwort",
                             inline=T
                ),
                radioButtons("cov_know_ref",
                             HTML("Gegen das Gesetz wurde das Referendum ergriffen."),
                             choices = c("Trifft nicht zu", "Trifft zu", "Weiss nicht / keine Antwort"),
                             selected = "Weiss nicht / keine Antwort",
                             inline=T
                )
                ,width = 12
            ),



            box(title = "Wie stark vertrauen Sie den folgenden Institutionen, die Covid-19-Krise bestmöglich zu bewältigen?",
                radioButtons("cov_trust_fede",
                             HTML("Dem Bundesrat"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_parl",
                             HTML("Dem Schweizer Parlament (Nationalrat und Ständerat)"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_part",
                             HTML("Den politischen Parteien auf eidgenössischer Ebene"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_agen",
                             HTML("Der Bundesverwaltung"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_cant",
                             HTML("Der kantonalen Verwaltung"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_bag",
                             HTML("Bundesamt für Gesundheit (BAG)"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_trust_heal",
                             HTML("Dem Gesundheitssystem"),
                             choices = c("Hohes Vertrauen", "Etwas Vertrauen", "Weder noch", "Wenig Vertrauen", "Gar kein Vertrauen"),
                             selected = "Weder noch",
                             inline=T
                )
                ,width = 12
            ),
    
            box(title = "Wie bewerten Sie die Leistung folgender Akteure bei der Bewältigung der Covid-19-Pandemie im Allgemeinen?",
                radioButtons("cov_perf_fed",
                             HTML("Dem Bundesrat"),
                             choices = c("Nicht zufrieden", "Eher nicht zufrieden", "Weder noch", "Eher zufrieden", "Zufrieden", "Weiss nicht / keine Antwort"),
                             selected = "Weder noch",
                             inline=T
                ),
                radioButtons("cov_perf_cant",
                             HTML("Dem Schweizer Parlament (Nationalrat und Ständerat)"),
                             choices = c("Nicht zufrieden", "Eher nicht zufrieden", "Weder noch", "Eher zufrieden", "Zufrieden", "Weiss nicht / keine Antwort"),
                             selected = "Weder noch",
                             inline=T
                )
                ,width = 12
            ),
            
            # box(sliderInput("QID19", label = "Bill Length (mm)",
            #                 choices = c("Teilzeit arbeitstätig (21-34.9 Std. pro Woche)",                                                                  
            #                             "Vollzeit arbeitstätig (35 Std. und mehr pro Woche)",                                                              
            #                             "in Ausbildung",                                                                                                   
            #                             "Teilzeit arbeitstätig (9-20.9 Std. pro Woche),Rentner/in (AHV, IV)",                                              
            #                             "Teilzeit arbeitstätig (9-20.9 Std. pro Woche)",                                                                   
            #                             "Rentner/in (AHV, IV)",                                                                                            
            #                             "arbeitslos",                                                                                                      
            #                             "Teilzeit arbeitstätig (21-34.9 Std. pro Woche),Teilzeit arbeitstätig (1-8.9 Std. pro Woche)",                    
            #                             "Rentner/in (AHV, IV),anderes",                                                                              
            #                             "Teilzeit arbeitstätig (1-8.9 Std. pro Woche)",                                                                   
            #                             "Teilzeit arbeitstätig (1-8.9 Std. pro Woche),Haus- und Familienarbeit")
            #                 ))
            # box(sliderInput("QID20", label = "Bill Depth (mm)",
            #                 # min = 10, max = 25, value = 17
            #                 )),
            # box(sliderInput("QID21", label = "Flipper Length (mm)",
            #                 # min = 170, max = 235, value = 200
            #                 )),
            # box(sliderInput("QID22", label = "Body Mass (g)",
            #                 # min = 2700, max = 6300, value = 4000
            #                 ))
        ),
        tabItem(
            tabName = "results_tab_effects",
            box(title = "Partial Dependence Plot"
                ,footer = "This page presents the effect for selected variables on outcome probabilities for the two dependent variables in this study: support for ambitous climate policy 
                and political mobilisatoin."
                ,status = "primary"
                ,solidHeader = TRUE
                ,collapsible = TRUE
                ,plotlyOutput("plot5", height = "1000px")
                ,width = 12
            ),
            box(selectInput("show_plot", label = "Partial Dependence Plot für folgende Variable",
                            choices = c("Ziviler Status",
                                        "Ausbildung",
                                        "Effektivität",
                                        "Effizienz",
                                        "Gerechtigkeit",
                                        "Transformationspotential")
            ), height = h                ,width = 12),
    
            # box(title = "blib"
            #     ,footer = "blub"
            #     ,status = "primary"
            #     ,solidHeader = TRUE 
            #     ,collapsible = TRUE 
            #     ,plotlyOutput("plot4", height = "1000px")
            #     ,width = 12
            # ),
        )
    )
        ),
    ) 
    # Dynamic valueBoxes
    # valueBoxOutput("progressBox", width = 12)
)

server <- function(input, output) { 
    
    # wraps the reactive inputs into a function
    ds <- reactive({
        
        educ <- if(input$educ == "Grundausbildung (inklusive nicht abgeschlossen)") {1}
        else if (input$educ == "Erstausbildung") {2}
        else if (input$educ == "Obligatorische Schule") {3}
        else if (input$educ == "Zweitausbildung") {4}
 
        civi_stat <- if(input$civi_stat == "Ledig") {1}
        else if (input$civi_stat == "Verheiratet") {2}
        else if (input$civi_stat == "In eingetragener Partnerschaft") {3}
        else if (input$civi_stat == "Verwitwet") {4}
        else if (input$civi_stat == "Geschieden") {5}
        else if (input$civi_stat == "Weiss nicht / keine Antwort") {6}

        empl_sect <- if(input$empl_sect == "Primärer Sektor, u.a. Landwirtschaft, Forstwirtschaft") {1}
        else if(input$empl_sect == "Sekundärer Sektor, u.a. Industrie, Gewerbe, Handwerk") {2}
        else if(input$empl_sect == "Tertiärer Sektor, u.a. Dienstleistungen, Verwaltungen") {3}
        else if(input$empl_sect == "weiss nicht / keine Antwort oder arbeitet nicht") {0}

        empl_stat <- if (input$empl_stat == "Vollzeit beschäftigt") {3}
        else if (input$empl_stat == "Teilzeit beschäftigt") {2}
        else if (input$empl_stat == "Ausbildung") {1}
        else if (input$empl_stat == "Andere") {0}
        
        fin_cond <- if(input$fin_cond == "Ja") {3}
        else if(input$fin_cond == "Es geht so") {2}
        else if(input$fin_cond == "Nein") {1}
        else if(input$fin_cond == "Weiss nicht / keine Antwort") {0}
        
        pol_party <- if(input$pol_party == "SVP (Schweizerische Volkspartei)") {1}
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
        else if (input$pol_party == "Weiss nicht / keine Antwort") {17}
        
        # add qid22 (place of residence)
        
        home_owner <- if(input$home_owner == "Ja") {1}
        else if (input$home_owner == "Nein") {0}
        
        renew_heating <- if(input$renew_heating == "Wärmepumpe, Fernwärme, Erdsonde, Strom, Solarenergie oder Holz, Holzschnitzel, Holzpellets") {1}
        else if (input$renew_heating == "Ölheizung, Gasheizung oder Weiss nicht / keine Antwort") {2}
        
        # renew_heating <- if(input$renew_heating == "Ölheizung oder Gasheizung") {1}
        # else if (input$renew_heating == "Gasheizung") {2}
        # else if (input$renew_heating == "Wärmepumpe") {3}
        # else if (input$renew_heating == "Fernwärme") {4}
        # else if (input$renew_heating == "Fernwärme") {5}
        # else if (input$renew_heating == "Erdsonde") {6}
        # else if (input$renew_heating == "Strom") {7}
        # else if (input$renew_heating == "Solarenergie") {8}
        # else if (input$renew_heating == "Holz, Holzschnitzel, Holzpellets") {9}
        # else if (input$renew_heating == "Weiss nicht / keine Antwort") {10}

        prior_benefit <- if(input$prior_benefit == "Stark beeinflusst") {5}
        else if (input$prior_benefit == "Etwas beeinflusst") {4}
        else if (input$prior_benefit == "Eher nicht beeinflusst") {3}
        else if (input$prior_benefit == "Überhaupt nicht beeinflusst") {2}
        else if (input$prior_benefit == "Weiss nicht / keine Antwort") {1}
        
        ren_driver <- if(input$ren_driver == "Ja") {1}
        else if (input$ren_driver == "Nein") {2}
        
        gender <- if(input$gender == "Frau") {1}
        else if (input$gender == "Mann") {2}
        
        region <- if(input$region == "Genferseeregion") {1}
        else if(input$region == "Mittelland") {2}
        else if(input$region == "Nordwestschweiz") {3}
        else if(input$region == "Zürich") {4}
        else if(input$region == "Ostschweiz") {5}
        else if(input$region == "Zentralschweiz") {6}
        else if(input$region == "Tessin") {7}
        
        transForm <- function(x){
            if(x == "Unterstütze voll und ganz") {5}
            else if(x == "Unterstütze eher") {4}
            else if(x == "Weder noch") {3}
            else if(x == "Lehne eher ab") {2}
            else if(x == "Lehne voll und ganz ab") {1}
        }
        
        swiss_pol_comp <- transForm(input$swiss_pol_comp)
        swiss_pol_effect <- transForm(input$swiss_pol_effect)
        swiss_pol_effic <- transForm(input$swiss_pol_effic)
        swiss_pol_just <- transForm(input$swiss_pol_just)
        swiss_pol_lead <- transForm(input$swiss_pol_lead)
        swiss_pol_vol <- transForm(input$swiss_pol_vol)
        swiss_pol_subs <- transForm(input$swiss_pol_subs)
        swiss_pol_tax <- transForm(input$swiss_pol_tax)
        swiss_pol_regu <- transForm(input$swiss_pol_regu)
        swiss_pol_tech <- transForm(input$swiss_pol_tech)
        swiss_pol_beha <- transForm(input$swiss_pol_beha)
        swiss_pol_comb <- transForm(input$swiss_pol_comb)
        swiss_pol_fed <- transForm(input$swiss_pol_fed)
        
        sal_ahv <- ifelse(input$salience == "Altersvorsorge/AHV", 1, 0)
        sal_unemp <- ifelse(input$salience == "Arbeitslosigkeit", 1, 0)
        sal_refug <- ifelse(input$salience == "Flüchtlinge", 1, 0)
        sal_eu <- ifelse(input$salience == "Verhältnis der Schweiz zur Europäischen Union", 1, 0)
        sal_health <- ifelse(input$salience == "Gesundheitswesen / Krankenversicherung", 1, 0)
        sal_energy <- ifelse(input$salience == "Energieversorgung", 1, 0)
        sal_traff <- ifelse(input$salience == "Verkehr", 1, 0)
        sal_glob <- ifelse(input$salience == "Globalisierung der Wirtschaft / Freihandel", 1, 0)
        sal_env <- ifelse(input$salience == "Umweltschutz / Klimawandel", 1, 0)
        sal_crim <- ifelse(input$salience == "Kriminalität", 1, 0)
        sal_uneq <- ifelse(input$salience == "Ungleichheit bei Einkommen und Vermögen", 1, 0)
        sal_cult <- ifelse(input$salience == "Zusammenleben von Menschen unterschiedlicher Kulturen und Religionen", 1, 0)
        sal_for <- ifelse(input$salience == "Ausländische Arbeitskräfte in der Schweiz", 1, 0)
        sal_pop <- ifelse(input$salience == "Zunahme der Schweizer Wohnbevölkerung / Zersiedelung / Verstädterung", 1, 0)
        
        transform2 <- function(x){
            unlist(lapply(x, function(x)
                if (is.na(x)) {NA} 
                else if (x == "Unterstütze voll und ganz") {5}
                else if (x == "Unterstütze") {4}
                else if (x == "Weder noch") {3}
                else if (x == "Lehne ab") {2}
                else if (x == "Lehne voll und ganz ab") {1}
            )
            )
        }
        
        sect_build <- transform2(input$sect_build)
        sect_trans <- transform2(input$sect_trans)
        sect_food <- transform2(input$sect_food)
        sect_avia <- transform2(input$sect_avia)
        sect_indu <- transform2(input$sect_indu)
        sect_wast <- transform2(input$sect_wast)
        
        transform3 <- function(x){
            unlist(lapply(x, function(x)
                if (is.na(x)) {NA} 
                else if (x == "Im Gesetz enthalten") {3}
                else if (x == "Nicht im Gesetz enthalten") {2}
                else if (x == "Weiss nicht") {1}
            )
            )
        }
        
        know_targ <- transform3(input$know_targ)
        know_build <- transform3(input$know_build)
        know_trans <- transform3(input$know_trans)
        know_food <- transform3(input$know_food)
        know_avia <- transform3(input$know_avia)
        know_wast <- transform3(input$know_wast)
    
        switch <- if(input$switch == "Immer schon") {1}
        else if(input$switch == "Seit Kürzerem (weniger als zwei Jahre) oder seit Längerem (mehr als zwei Jahre)") {2}
        
        freq_user_driver <- if(input$freq_user_driver == "0-10'000 km") {0}
        else if(input$freq_user_driver == "10'001-20'000 km") {1}
        else if(input$freq_user_driver == "20'001-30'000 km") {2}
        else if(input$freq_user_driver == "30'001-40'000 km") {3}
        else if(input$freq_user_driver == "mehr als 50'000 km") {4}
        else if(input$freq_user_driver == "Weiss nicht / keine Antwort") {5}
        freq_user_fly <- if(input$freq_user_fly == "0-1 Flüge") {0}
        else if(input$freq_user_fly == "2-3 Flüge") {1}
        else if(input$freq_user_fly == "4-5 Flüge") {2}
        else if(input$freq_user_fly == "6-7 Flüge") {3}
        else if(input$freq_user_fly == "8-9 Flüge") {4}
        else if(input$freq_user_fly == "mehr als 9 Flüge") {5}
        freq_user_fish <- if(input$freq_user_fish == "An weniger als einem Tag") {0}
        else if(input$freq_user_fish == "1") {1}
        else if(input$freq_user_fish == "2") {2}
        else if(input$freq_user_fish == "3") {3}
        else if(input$freq_user_fish == "4") {4}
        else if(input$freq_user_fish == "5") {5}
        else if(input$freq_user_fish == "6") {6}
        else if(input$freq_user_fish == "7") {7}
        freq_user_meat <- if(input$freq_user_meat == "An weniger als einem Tag") {0}
        else if(input$freq_user_meat == "1") {1}
        else if(input$freq_user_meat == "2") {2}
        else if(input$freq_user_meat == "3") {3}
        else if(input$freq_user_meat == "4") {4}
        else if(input$freq_user_meat == "5") {5}
        else if(input$freq_user_meat == "6") {6}
        else if(input$freq_user_meat == "7") {7}
        freq_user_meat_subs <- if(input$freq_user_meat_subs == "Nie") {0}
        else if(input$freq_user_meat_subs == "Selten") {1}
        else if(input$freq_user_meat_subs == "Einmal pro Monat") {2}
        else if(input$freq_user_meat_subs == "Mehrmals pro Monat") {3}
        else if(input$freq_user_meat_subs == "Ungefähr ein mal pro Woche") {4}
        else if(input$freq_user_meat_subs == "Mehrmals pro Woche") {5}
        else if(input$freq_user_meat_subs == "Jeden Tag") {6}
        
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
        
        Attrib1 <- if(input$Attrib1 == "40%") {1}
        else if(input$Attrib1 == "50%") {2}
        else if(input$Attrib1 == "60%") {3}
        else if(input$Attrib1 == "70%") {4}
        else if(input$Attrib1 == "80%") {5}
        Attrib2 <- if(input$Attrib2 == "No tax on petrol") {1}  
        else if(input$Attrib2 == "0.14 Fr./l petrol") {2}
        else if(input$Attrib2 == "0.28 Fr./l petrol") {3}
        else if(input$Attrib2 == "0.42 Fr./l petrol") {4}
        else if(input$Attrib2 == "0.56 Fr./l petrol") {5}
        Attrib3 <- if(input$Attrib3 == "No tax on heating oil") {1} 
        else if(input$Attrib3 ==  "0.16 Fr./l heating oil") {2}
        else if(input$Attrib3 == "0.31 Fr./l heating oil") {3}
        else if(input$Attrib3 == "0.47 Fr./l heating oil") {4}
        else if(input$Attrib3 == "0.63 Fr./l heating oil") {5}
        Attrib4 <- if(input$Attrib4 == "No tax on meat") {1}  
        else if(input$Attrib4 == "0.77 Fr./kg meat") {2}
        else if(input$Attrib4 ==  "1.53 Fr./kg meat") {3}
        else if(input$Attrib4 == "2.30 Fr./kg meat") {4}
        else if(input$Attrib4 == "3.07 Fr./kg meat") {5}
        Attrib5 <- if(input$Attrib5 == "No tax") {1}
        else if(input$Attrib5 == "10 Fr. for short- and 30 Fr. for long-distance") {2} 
        else if(input$Attrib5 == "25 Fr. for short- and 75 Fr. for long-distance") {3}
        else if(input$Attrib5 == "40 Fr. for short- and 120 Fr. for long-distance") {4}
        else if(input$Attrib5 == "55 Fr. for short- and 165 Fr. for long-distance") {5}
        Attrib6 <- if(input$Attrib6 == "Exclusively lump sum reimbursement") {1} 
        else if(input$Attrib6 == "Mostly lump sum reimbursement") {2}
        else if(input$Attrib6 == "Lump sum reimbursement und investment into climate protection") {3}
        else if(input$Attrib6 == "Mostly investment into climate protection") {4}
        else if(input$Attrib6 == "Exclusively investment into climate protection") {5}
        
        cov_fin <- if(input$cov_fin == "klar verschlechtert.") {1} 
        else if(input$cov_fin == "eher verschlechtert.") {2}
        else if(input$cov_fin == "nicht verändert.") {3}
        else if(input$cov_fin == "eher verbessert.") {4}
        else if(input$cov_fin == "klar verbessert.") {5}
        else if(input$cov_fin == "Weiss nicht / keine Antwort") {6}
        
        cov_job <- if(input$cov_job == "Keine Änderung") {0} 
        else if(input$cov_job == "Ich arbeitete in Kurzarbeit.") {1}
        else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {2}
        else if(input$cov_job == "Ich habe mein Pensum verringert, um für meine Kinder oder andere Angehörige zu sorgen.") {3}
        else if(input$cov_job == "Ich arbeitete (teilweise) im Homeoffice.") {4}
        else if(input$cov_job == "Ich musste Überzeit kompensieren oder Ferien nehmen, weil es zu wenig Arbeit gab.") {5}
        else if(input$cov_job == "Ich arbeitete mehr als üblich.") {6}
        else if(input$cov_job == "Ich habe meinen Job verloren.") {7}
        else if(input$cov_job == "Mein Unternehmen/ meine Agentur musste Veranstaltungen absagen.") {8}
        else if(input$cov_job == "Mein Unternehmen/ meine Branche profitierte von der Covid-19-Krise (z.B. viel mehr Arbeit/Nachfrage).") {9}
        else if(input$cov_job == "Mein Unternehmen/ meine Branche litt unter der Covid-19-Krise (z.B. weniger Arbeit/Nachfrage, Teilschliessung, Kapazitätsbeschränkungen).") {10}
        else if(input$cov_job == "Anderes (Bitte angeben)") {11}
        else if(input$cov_job == "Weiss nicht / keine Antwort") {12}
        
        
        cov_health <- if(input$cov_health == "Ich war nicht direkt oder nur indirekt von den gesundheitlichen Auswirkungen der Covid-19-Pandemie betroffen.") {1} 
        else if(input$cov_health == "Ich musste aufgrund einer behördlichen Anordnung in Quarantäne.") {2}
        else if(input$cov_health == "Ich bin selber an Covid-19 erkrankt.") {3}
        else if(input$cov_health == "Personen aus meinem engen Familienkreis sind erkrankt oder mussten in Quarantäne.") {4}
        else if(input$cov_health == "Ich bin in meinem Alltag einem erhöhten Risiko einer Ansteckung ausgesetzt (z.B. Beruf mit Personenkontakt).") {5}
        else if(input$cov_health == "Ich bin ein Risikopatient.") {6}
        else if(input$cov_health == "Anderes (Bitte angeben)") {7}
        else if(input$cov_health == "Weiss nicht / Keine Antwort") {8}
        
        cov_info <- if(input$cov_info == "gut informiert.") {1} 
        else if(input$cov_info == "eher informiert.") {2}
        else if(input$cov_info == "eher nicht informiert.") {3}
        else if(input$cov_info == "gar nicht informiert.") {4}
        else if(input$cov_info == "klar verbessert.") {5}
        else if(input$cov_info == "Weiss nicht / keine Antwort") {6}
        
        transform5 <- function(x){
            unlist(lapply(x, function(x)
                if (is.na(x)) {NA}
                else if (x == "Weiss nicht / keine Antwort") {3}
                else if (x == "Trifft zu") {2}
                else if (x == "Trifft nicht zu") {1}
            )
            )
        }
        
        cov_know_fin <- transform5(input$cov_know_fin)
        cov_know_fed <- transform5(input$cov_know_fed)
        cov_know_pub <- transform5(input$cov_know_pub)
        cov_know_10i <- transform5(input$cov_know_10i)
        cov_know_ref <- transform5(input$cov_know_ref)
    
        cov_prof <- if(input$cov_prof == "Stimme nicht zu") {0} 
        else if(input$cov_prof == "Stimme eher nicht zu") {1}
        else if(input$cov_prof == "Weder noch") {2}
        else if(input$cov_prof == "Stimme eher zu") {3}
        else if(input$cov_prof == "Stimme zu") {4}
        
        transform6 <- function(x){
          unlist(lapply(x, function(x)
            if (is.na(x)) {NA}
            else if (x == "Hohes Vertrauen") {5}
            else if (x == "Etwas Vertrauen") {4}
            else if (x == "Weder noch") {3}
            else if (x == "Wenig Vertrauen") {2}
            else if (x == "Gar kein Vertrauen") {1}
          )
          )
        }
        
        cov_trust_fede <- transform6(input$cov_trust_fede)
        cov_trust_parl <- transform6(input$cov_trust_parl)
        cov_trust_part <- transform6(input$cov_trust_part)
        cov_trust_agen <- transform6(input$cov_trust_agen)
        cov_trust_cant <- transform6(input$cov_trust_cant)
        cov_trust_bag <- transform6(input$cov_trust_bag)
        cov_trust_heal <- transform6(input$cov_trust_heal)

        
        transform7 <- function(x){
            unlist(lapply(x, function(x)
                if (is.na(x)) {NA}
                else if (x == "Weiss nicht / keine Antwort") {6}
                else if (x == "Zufrieden") {5}
                else if (x == "Eher zufrieden") {4}
                else if (x == "Weder noch") {3}
                else if (x == "Eher nicht zufrieden") {2}
                else if (x == "Nicht zufrieden") {1}
            )
            )
        }
        
        cov_perf_fed <- transform7(input$cov_perf_fed)
        cov_perf_cant <- transform7(input$cov_perf_cant)
        
        dat <- tibble(
            "educ" = educ,
            "civi_stat" = civi_stat,
            "empl_sect" = empl_sect,
            "empl_stat" = empl_stat,
            "fin_cond" = fin_cond,
            # "plz" = input$plz,
            "left_right" = input$left_right,
            "pol_party" = pol_party,
            "home_owner" = home_owner,
            "renew_heating" = renew_heating,
            "prior_benefit" = prior_benefit,
            "ren_driver" = ren_driver,
            "gender" = gender,
            "region" = region,
            "sal_ahv" = sal_ahv, 
            "sal_unemp" = sal_unemp, 
            "sal_refug" = sal_refug, 
            "sal_eu" = sal_eu, 
            "sal_health" = sal_health, 
            "sal_energy" = sal_energy, 
            "sal_traff" = sal_traff, 
            "sal_glob" = sal_glob, 
            "sal_env" = sal_env, 
            "sal_crim" = sal_crim, 
            "sal_uneq" = sal_uneq, 
            "sal_cult" = sal_cult, 
            "sal_for" = sal_for, 
            "sal_pop" = sal_pop,
            "swiss_pol_comp" = swiss_pol_comp,
            "swiss_pol_effect" = swiss_pol_effect,
            "swiss_pol_effic" = swiss_pol_effic,
            "swiss_pol_just" = swiss_pol_just,
            "swiss_pol_lead" = swiss_pol_lead,
            "swiss_pol_vol" = swiss_pol_vol,
            "swiss_pol_subs" = swiss_pol_subs,
            "swiss_pol_tax" = swiss_pol_tax,
            "swiss_pol_regu" = swiss_pol_regu,
            "swiss_pol_tech" = swiss_pol_tech,
            "swiss_pol_beha" = swiss_pol_beha,
            "swiss_pol_comb" = swiss_pol_comb,
            "swiss_pol_fed" = swiss_pol_fed,
            "sect_build" = sect_build, 
            "sect_trans" = sect_trans, 
            "sect_food" = sect_food, 
            "sect_avia" = sect_avia, 
            "sect_indu" = sect_indu, 
            "sect_wast" = sect_wast,
            "know_targ" = know_targ,
            "know_build" = know_build, 
            "know_trans" = know_trans,
            "know_food" = know_food, 
            "know_avia" = know_avia, 
            "know_wast" = know_wast,
            "switch" = switch,
            "freq_user_driver" = freq_user_driver, 
            "freq_user_fly" = freq_user_fly, 
            "freq_user_fish" = freq_user_fish, 
            "freq_user_meat" = freq_user_meat, 
            "freq_user_meat_subs" = freq_user_meat_subs,
            "efficiency" = efficiency,
            "effectiveness" = effectiveness,
            "competitiveness" = competitiveness,
            "justice" = justice,
            "transformation" = transformation,
            "Attrib1" = Attrib1,
            "Attrib2" = Attrib2,
            "Attrib3" = Attrib3,
            "Attrib4" = Attrib4,
            "Attrib5" = Attrib5,
            "Attrib6" = Attrib6,
            "cov_imp" = input$cov_imp,
            "cov_fin" = cov_fin,
            "cov_job" = cov_job,
            "cov_health" = cov_health,
            "cov_info" = cov_info,
            "cov_know_fin" = cov_know_fin,
            "cov_know_fed" =  cov_know_fed,
            "cov_know_pub" = cov_know_pub,
            "cov_know_10i" = cov_know_10i,
            "cov_know_ref" = cov_know_ref,
            "cov_prof" = cov_prof,
            "cov_trust_fede" = cov_trust_fede, 
            "cov_trust_parl" = cov_trust_parl, 
            "cov_trust_part" = cov_trust_part, 
            "cov_trust_agen" = cov_trust_agen, 
            "cov_trust_cant" = cov_trust_cant, 
            "cov_trust_bag" = cov_trust_bag, 
            "cov_trust_heal" = cov_trust_heal,
            "cov_perf_fed" = cov_perf_fed, 
            "cov_perf_cant" = cov_perf_cant
        )
    }) 
    
    
    # wraps the reactive inputs into a function
    ds2 <- reactive({

        show_plot <- if(input$show_plot == "Ziviler Status") {"civi_stat"}
        else if (input$show_plot == "Ausbildung") {"educ"}
        else if (input$show_plot == "Effektivität") {"effectiveness"}
        else if (input$show_plot == "Effizienz") {"efficiency"}
        else if (input$show_plot == "Gerechtigkeit") {"justice"}
        else if (input$show_plot == "Transformationspotential") {"transformation"}


        dat2 <- tibble(
            "show_plot" = show_plot,
        )
    })

    
    
    # output$belief_prediction <- renderValueBox({
    # 
    #     
    #     prediction_prob <- bind_cols(bind_rows(predict_probability(model_efficiency, ds()), 
    #                                            predict_probability(model_effectiveness, ds()), 
    #                                            predict_probability(model_competitiveness, ds()),
    #                                            predict_probability(model_justice, ds()),
    #                                            predict_probability(model_transformation, ds())),
    #                                  rep(c("Efficiency", "Effectiveness", "Competitiveness", "Justice", "Transformation"), each = nrow(predict_probability(model_efficiency, ds()))))
    #     colnames(prediction_prob) <- c("key", "value", "dv")
    #     
    #     prediction_prob$key <- str_replace(prediction_prob$key, ".pred_", "")
    #     
    #         valueBox(
    #             value = HTML(paste0("Vorhergesagte Antwortkategorien <br> mit der höchsten Wahrscheinlichkeit: " #,  round(100*max(prediction_prob$value[prediction_prob$dv == "Efficiency"]), 0), "%"
    #                                 )),
    #             subtitle = HTML(paste0("Ökonomischer Effizienz: ", prediction_prob$key[prediction_prob$value == max(prediction_prob$value[prediction_prob$dv == "Efficiency"])], "<br>", 
    #                             "Ökologische Effektivität: ", prediction_prob$key[prediction_prob$value == max(prediction_prob$value[prediction_prob$dv == "Effectiveness"]) ], "<br>",
    #                             "Wettbewerbsfähigkeit: ", prediction_prob$key[prediction_prob$value == max(prediction_prob$value[prediction_prob$dv == "Competitiveness"])], "<br>",
    #                             "Gerechtigkeit: ", prediction_prob$key[prediction_prob$value == max(prediction_prob$value[prediction_prob$dv == "Justice"])], "<br>",
    #                             "Transformationspotential: ", prediction_prob$key[prediction_prob$value == max(prediction_prob$value[prediction_prob$dv == "Transformation"])], "<br>" )),
    #             # width = NULL,
    #             width = 12,
    #             color = "navy",
    #             # # color = prediction_color,
    #             # icon = icon("snowflake")
    #         )
    #         
    # })
    
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
            in den interaktiven Graphiken veranschaulicht werden. Diese Resultate sind von politischer Relevanz, da sie beispielsweise als Grundlage zur Ausarbeitung von 
            neuen politischen Vorschlägen oder der Ausrichtung von Strategien der Öffentlichkeit, Interessengruppen, Parteien, Verwaltung und Regierung 
            dienen können.  
            "))
    
    # output$plot <- renderPlotly({
    #     
    #     prediction_prob_core <- bind_cols(bind_rows(predict_probability(model_efficiency, ds()), 
    #                         predict_probability(model_effectiveness, ds()), 
    #                         predict_probability(model_competitiveness, ds()),
    #                         predict_probability(model_justice, ds()),
    #                         predict_probability(model_transformation, ds())),
    #               rep(c("Effizienz", "Effectivität", "Wettbewerbsfähigkeit", "Gerechtigkeit", "Transformation"), each = nrow(predict_probability(model_efficiency, ds()))))
    #     colnames(prediction_prob_core) <- c("Zustimmmung", "Wert", "dv")
    #     
    #     # prediction_prob_core$Zustimmmung <- str_replace(prediction_prob_core$Zustimmmung, "pred", "")
    #     
    #     ggplotly(
    #         prediction_prob_core %>% 
    #             dplyr::mutate(
    #                 Wert = ifelse(Zustimmmung == ".pred_Eher nicht", Wert*(-1), Wert),
    #                 Wert = ifelse(Zustimmmung == ".pred_Ganz und gar nicht", Wert*(-1), Wert),
    #                 Wert = ifelse(Zustimmmung == ".pred_Weder noch", Wert/2, Wert),
    #             ) %>% 
    #             bind_rows(.[.$Zustimmmung == ".pred_Weder noch",] %>% mutate(Wert = Wert *(-1))) %>% 
    #             mutate(Zustimmmung = factor(Zustimmmung, levels = c(".pred_Weder noch", ".pred_Ganz und gar nicht", ".pred_Eher nicht",  ".pred_Eher", ".pred_Voll und ganz"))) %>% 
    #             ggplot(.) + 
    #             geom_bar(aes(x = dv, y = Wert, fill = Zustimmmung), stat = "identity", position = position_stack(reverse = TRUE)) + 
    #             theme_light() + 
    #             coord_flip() + 
    #             # geom_text(mapping = aes(
    #             #     label = ifelse(Wert != "Weder noch", paste0(round(abs(Wert)*100, 0), "%"), "")
    #             #     , x = dv, y = Wert,
    #             #     ),
    #             #     # color = "black", cex = 5,
    #             #     position=position_stack(vjust=0.5)
    #             # ) +
    #             ylim(-1,1) +
    #             # scale_x_discrete(labels = c("Efficiency", "Effectiveness", "Competitiveness", "Justice", "Transformation")) +
    #             # ggtitle("<b>Core Beliefs:</b> Politische Klimaschutzmassnahmen wie das CO2-Gesetz können auf Basis unterschiedlicher
    #             #         Kriterien beurteilt werden. Bitte lesen Sie sich folgende Kriterien durch und geben Sie bitte an, wie sehr das
    #             #         zur Abstimmung stehende CO2-Gesetzes aus Ihrer Sicht das jeweilige Kriterium erfüllt oder nicht erfüllt.") +
    #             scale_fill_manual(name = "Core Beliefs", labels =c( "Voll und ganz", "Eher", "Ganz und gar nicht", "Eher nicht", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
    #             # xlab("Ordered Category") + 
    #             ylab("Probability") +
    #             theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)))
    #     ) 
    #         # layout(legend = list(orientation = "h", x = 0.4, y = -0.2)) 
    #     
    # })
    
    output$plot2 <- renderPlotly({
        
        # prediction_prob_core <- bind_cols(bind_rows(predict_probability(model_rate, ds())),
        #                                   rep(c("Ambition"), each = nrow(predict_probability(model_rate, ds()))))
        # colnames(prediction_prob_core) <- c("Zustimmmung", "Wert", "dv")
        # 
        # ggplotly(
        #     prediction_prob_core %>% 
        #         dplyr::mutate(
        #             Wert = ifelse(Zustimmmung == ".pred_2", Wert*(-1), Wert),
        #             Wert = ifelse(Zustimmmung == ".pred_1", Wert*(-1), Wert),
        #             Wert = ifelse(Zustimmmung == ".pred_3", Wert/2, Wert),
        #         ) %>% 
        #         dplyr::bind_rows(.[.$Zustimmmung == ".pred_3",] %>% mutate(Wert = Wert *(-1))) %>% 
        #         dplyr::mutate(Zustimmmung = factor(Zustimmmung, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>% 
        #         ggplot(.) + 
        #         geom_bar(aes(x = dv, y = Wert, fill = Zustimmmung), stat = "identity", position = position_stack(reverse = TRUE)) + 
        #         theme_light() + 
        #         coord_flip() + 
        #         # geom_text(mapping = aes(
        #         #     label = ifelse(Wert != "Weder noch", paste0(round(abs(Wert)*100, 0), "%"), "")
        #         #     , x = dv, y = Wert,
        #         #     ),
        #         #     # color = "black", cex = 5,
        #         #     position=position_stack(vjust=0.5)
        #         # ) +
        #         ylim(-1,1) +
        #         # scale_x_discrete(labels = c("Efficiency", "Effectiveness", "Competitiveness", "Justice", "Transformation")) +
        #         # ggtitle("<b>Core Beliefs:</b> Politische Klimaschutzmassnahmen wie das CO2-Gesetz können auf Basis unterschiedlicher
        #         #         Kriterien beurteilt werden. Bitte lesen Sie sich folgende Kriterien durch und geben Sie bitte an, wie sehr das
        #         #         zur Abstimmung stehende CO2-Gesetzes aus Ihrer Sicht das jeweilige Kriterium erfüllt oder nicht erfüllt.") +
        #         scale_fill_manual(name = "Core Beliefs", labels =c( "Unterstütze voll und ganz", "Unterstütze", "Lehne voll und ganz ab", "Lehne ab", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
        #         # xlab("Ordered Category") + 
        #         ylab("Probability") +
        #         theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)))
        #     # height = 100
        # ) 
            # layout(legend = list(orientation = "h", x = 0.4, y = -0.2)) 
    })
    
    output$plot3 <- renderPlotly({
        
        ggplotly(
            # dplyr::bind_cols(bind_rows(predict_probability(model_rate_activism, ds())),
            #                                           rep(c("Mobilisation"), each = nrow(predict_probability(model_rate_activism, ds())))) %>% 
            #     `colnames<-`(c("Zustimmmung", "Wert", "dv")) %>% 
            #     dplyr::mutate(
            #         Wert = ifelse(Zustimmmung == ".pred_2", Wert*(-1), Wert),
            #         Wert = ifelse(Zustimmmung == ".pred_1", Wert*(-1), Wert),
            #         Wert = ifelse(Zustimmmung == ".pred_3", Wert/2, Wert),
            #     ) 
            data %>%
                dplyr::bind_rows(.[.$Zustimmmung == ".pred_3",] %>% dplyr::mutate(Wert = Wert *(-1))) %>%
                dplyr::mutate(Zustimmmung = factor(Zustimmmung, levels = c(".pred_3", ".pred_2", ".pred_1", ".pred_4", ".pred_5"))) %>%
                ggplot2::ggplot(.) +
                ggplot2::geom_bar(aes(x = dv, y = Wert, fill = Zustimmmung), stat = "identity", position = position_stack(reverse = TRUE)) +
                ggplot2::theme_light() +
                ggplot2::coord_flip() +
                ggplot2::ylim(-1,1) +
                ggplot2::scale_fill_manual(name = "Core Beliefs", labels =c( "Viel Aufwand zur Unterstützung", "Etwas Aufwand zur Unterstützung", "Viel Aufwand zur Verhinderung", "Etwas Aufwand zur Verhinderung", "Weder noch"), limits = rev, values = c("darkgreen", "lightgreen", "red4", "red3",  "grey")) +
                ggplot2::ylab("Probability") +
                ggplot2::theme(plot.title = element_text(margin = ggplot2::margin(30,30,30,30)))
            
            # ggplot(as.data.frame(sample(1:50, 100, T)), aes(x = `sample(1:50, 100, T)`)) +
            #     geom_bar()
        ) 
        # layout(legend = list(orientation = "h", x = 0.4, y = -0.2)) 
    })
    output$plot5 <- renderPlotly({

        p <- readRDS(paste0("partial_dependence_forest_rate__exp_", ds2(), ".rds"))

        ggplotly(p)
    })
    
    # output$plot4 <- renderPlotly({
    #     
    #     # ## TODO: put these into the main script
    #     vote_small <- vote %>%
    #         select(ambition,
    #                "civi_stat",
    #                "fin_cond",
    #                "pol_party",
    #                "renew_heating", "prior_benefit", "left_right",
    #                "ren_driver",
    #                "home_owner",
    #                "educ",
    #                "empl_stat",
    #                "empl_sect",
    #                "sal_ahv", "sal_unemp", "sal_refug", "sal_eu", "sal_health", "sal_energy", "sal_traff", "sal_glob", "sal_env", "sal_crim", "sal_uneq", "sal_cult", "sal_for", "sal_pop"
    #         )
    #     
    #     # explainer_xgb <- DALEXtra::explain_tidymodels(final_boosted_ambition, 
    #     #                                               data = vote_small[,-1], 
    #     #                                               y = vote_small$ambition,
    #     #                                               label = "Tidy-Boosting") 
    #     
    #     # prof <- model_profile(explainer_xgb)$agr_profiles
    #     example <- vote_small[3, ]
    #     parts <- predict_parts(explainer_xgb, example)
    #     
    #     ggplotly(
    #         # model_performance(explainer_xgb) %>% plot(geom = "histogram"),
    #         # model_performance(explainer_xgb) %>% plot(geom = "boxplot"),
    #         # model_parts(explainer_xgb) %>% plot()
    #         as_tibble(prof) %>%
    #             mutate(`_label_` = stringr::str_remove(`_label_`, "Tidy-Boosting."),
    #                    `_x_` = round(`_x_`, 0)) %>%
    #             ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
    #             geom_line(size = 1.2, alpha = 0.8) +
    #             facet_wrap(~`_vname_`, scales = "free_x", 
    #                        # labeller = labeller(`_vname_` = c("civi_stat" = "Zivilstand"))
    #             ) + 
    #             # scale_x_continuous(breaks = function(`_x_`) round(`_x_`, 2)) +
    #             labs(x = "Variable Value", 
    #                  y = "Average Prediction", 
    #                  color = NULL,
    #                  title = "Partial dependence profile for support of more ambitious climate policy",
    #                  subtitle = "Predictions from an XGBoost Model") + 
    #             scale_color_manual(name = "Support", labels =c( "Klar zu wenig weit", "Eher zu wenig weit", "Eher zu weit", "Klar zu weit", "Unentschieden"), values = c("darkgreen", "lightgreen", "red3", "red4", "grey")) +
    #             theme_light()
    #         # model_diagnostics(explainer_xgb) %>% plot(variable = "y", yvariable = "y_hat")
    #         # 
    #         # lewandowski <- vote_small[3, ]
    #         # predict_parts(explainer_xgb, lewandowski) %>% plot()
    #         # predict_profile(explainer_xgb, lewandowski) %>% plot() + theme(legend.position = "bottom")
    #         # predict_diagnostics(explainer_xgb, lewandowski) %>% plot()
    #         # fifa_small_pca <- my_rec(turnout, vars) %>% prep() %>% bake(vote_small) %>% as.data.frame()
    #     ) 
    # })
    
}


shinyApp(ui, server)
