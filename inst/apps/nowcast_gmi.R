# library(shiny)
# library(tidyverse)
# library(lubridate)
# library(vars)
# library(expm)
# library(ggthemes)
# library(gridExtra)
# library(reshape2)
# library(eurostat)
# library(timeDate)
# library(RJDemetra)
#
# source_directory <- list.files("../EMDFM_pkg/", pattern = ".R$", full.names = T)
# myenv <- new.env()
# sapply(source_directory, source, local = myenv)
# attach(myenv, name = "EM-DFM")

mobility_indexes_all <- c("mobility_workplaces", "mobility_transit_stations", "mobility_workplaces")

ui <- fluidPage(

    titlePanel("Nowcasting industrial production in the EU"),

    sidebarLayout(
        sidebarPanel(
          helpText("Get real-time forecasts of industrial production in European countries,
                   using data from Eurostat and Google."),

          selectInput("geo_code", "EU country:",
                      c("Italy" = "IT",
                        "France" = "FR",
                        "Germany" = "DE",
                        "Spain" = "ES",
                        "Greece" = "GR")),


          checkboxGroupInput("mobility_indexes", "Mobility indexes:",
                             c("Retail and recreation" = "mobility_retail_and_recreation",
                               "Transit stations" = "mobility_transit_stations",
                               "Workplaces" = "mobility_workplaces")),

          checkboxInput("gmi_daily", "Use high-frequency indexes", FALSE),

          selectInput("estimator", "Algorithm for estimation:",
                      c("PCA" = "PCA",
                        "Kalman filter" = "KF",
                        "Kalman smoother" = "KS",
                        "Expectation Maximization" = "EM",
                        "dynfactoR library" = "package_test"
                        )),

          selectInput("thresh_imputation", "Threshold for imputation of missing values (preliminary step for PCA)",
                      c("Precise but slow" = "0.015",
                        "Balanced" = "0.05",
                        "Fast" = "0.5"),
                      selected = "0.05"),

          sliderInput("r",
                       "Number of factors:",
                       min = 2,
                       max = 10,
                       value = 4),

          sliderInput("p",
                       "Number of lags in the state equation:",
                       min = 1,
                       max = 12,
                       value = 5,
                      round = T),

          numericInput("horizon",
                    label = "Forecast horizon:",
                    value = 4,
                    min = 1,
                    max = 12,
                    width = "100%"),

          checkboxInput("s_adj", "Plot seasonal-adjusted forecasts", FALSE),

          checkboxInput("forecast_evaluation", "Perform rolling-window forecast evaluation", FALSE),

          conditionalPanel(
            condition = "input.forecast_evaluation == true",
            numericInput("window_size",
                      label = "Number of observations in the sliding window:",
                      value = 376,
                      width = "100%")

          ),
          # submitButton("Apply", icon("refresh")),
          actionButton("do", "Apply")
        ),

        mainPanel(
           plotOutput("forecast_plot"),
           verbatimTextOutput("strfile")
        )
    )
)


server <- function(input, output, session) {

  get_data <- reactive({
    emDFM::download_clean_data(input$geo_code)
  }) %>%
    bindCache(input$geo_code) # Rendering allows not to download data relative to the same country multiple times

  get_seasonal_attributes <- reactive({
      eurostat_indpro_NSA <- eurostat::get_eurostat("sts_inpr_m",
                                          filters = list(geo = input$geo_code,
                                                         s_adj = "NSA",
                                                         unit = "I15",
                                                         nace_r2 = "C")) %>%
      dplyr::select(date = time, indpro_NSA = values)
      seasonal_attributes_indpro <- emDFM::get_seasonal_attributes_indpro(eurostat_indpro_NSA)
      seasonal_attributes_indpro
  }) %>%
    bindCache(input$geo_code)


  # data_stationary_scaled <- reactive(download_clean_data(input$geo_code))
  plot_model_fit <- eventReactive(input$do, {
    gmi_daily <- input$gmi_daily
    mobility_indexes <- input$mobility_indexes
    estimator <- input$estimator
    horizon <- input$horizon
    window_size <- input$window_size
    r <- input$r
    p <- input$p
    thresh_imputation <- as.numeric(input$thresh_imputation)
    thresh <- 0.001

    withProgress(message = "Data fetching and model fitting",{

      incProgress(0, detail = "Download data from Eurostat (takes ~1m)")
      data_list <- get_data()
      if(gmi_daily){
        dataset <- data_list$data_high_freq
      } else {
        dataset <- data_list$data_low_freq
      }
      #dataset <- read_csv("../dataset_clean_LowFrequency_08_08_2022.csv")
      # dataset <- dataset %>%
      #   dplyr::select(-gdp_SCA, -consumption_SCA, -exports_SCA, -imports_SCA, -gross_capital_formation_SCA)
      #
      incProgress(1/3, detail = "Setting up the model")

      mobility_indexes_to_delete <- mobility_indexes_all[!(mobility_indexes_all %in% mobility_indexes)]
      if(!is.null(mobility_indexes_to_delete)){
        dataset <- dataset %>%
          dplyr::select(-starts_with(mobility_indexes_to_delete))
      }

      if(input$forecast_evaluation){
        evaluation_input <- list(d = dataset,
                                 horizon = horizon,
                                 window_size = window_size,
                                 r = r,
                                 p = p,
                                 thresh_imputation = thresh_imputation,
                                 thresh = thresh                 # L1 convergence of parameters in EM
        )

        incProgress(2/3, detail = "Sliding-window forecast evaluation")
        dfm_eval_fit <- emDFM::forecast_evaluation(estimator, evaluation_input)

        incProgress(3/3, detail = "Plotting the results")
        plot_title <- paste0(estimator,
                             "-DFM ",
                             ifelse(is.null(mobility_indexes), "without", "with"),
                             " mobility index")

        forecast_plot <- emDFM::plot_forecast_evaluation(dfm_eval_fit, title = plot_title, annotate = T)

        } else {

          incProgress(2/3, detail = "Setting up the model")

          if(!input$s_adj){
            seasonal_attributes_indpro <- get_seasonal_attributes()
          } else {
            seasonal_attributes_indpro = NULL
          }

          forecast_input <- list(
            estimator = estimator,
            variable = "indpro_SCA",
            horizon = horizon,
            r = r,
            p = p,
            thresh = thresh,
            thresh_imputation = thresh_imputation,
            seasonal_attributes_indpro = seasonal_attributes_indpro
          )


          incProgress(3/3, detail = "Forecast")
          forecast_plot <- emDFM::forecast_pipeline(dataset, forecast_input)
        }

      list(plot = forecast_plot,
           dataset = dataset)
    })
  })

    # Make output

  # observeEvent(input$do, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
    output$forecast_plot <- renderPlot({
      plot_model_fit()$plot
    })

    output$strfile <- renderPrint({str(plot_model_fit()$dataset %>% dplyr::select(-starts_with("mobility_")))})
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
