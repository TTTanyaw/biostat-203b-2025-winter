library(DBI)
library(bigrquery)
library(ggplot2)
library(dplyr)
library(lubridate)
library(gt)
library(gtsummary)
library(tidyverse)

get_patient_demo <- function(subject_id, con) {
  tbl(con, "patients") |>
    filter(subject_id == !!subject_id) |>
    left_join(tbl(con, "admissions") |> 
                select(subject_id, race), by = "subject_id") |>
    select(subject_id, gender, anchor_age, race) |>
    collect()
}


get_adt_data <- function(subject_id, con) {
  tbl(con, "transfers") |>
    filter(subject_id == !!subject_id) |>
    filter(!is.na(careunit), 
           !is.na(intime), 
           !is.na(outtime), 
           careunit != "UNKNOWN") |>
    mutate(ICU = grepl('CU', careunit)) |>
    select(subject_id, careunit, intime, outtime, ICU) |>
    arrange(intime) |>
    collect() 
}

get_lab_events <- function(subject_id, con) {
  tbl(con, "labevents") |>
    filter(subject_id == !!subject_id) |>
    select(subject_id, charttime) |>
    distinct() |>
    collect()
}

get_procedure_data <- function(subject_id, con) {
  tbl(con, "procedures_icd") |>
    filter(subject_id == !!subject_id) |>
    left_join(tbl(con, "d_icd_procedures"), by = "icd_code") |>
    mutate(charttime = as_datetime(chartdate), type_of_event = "Procedure") |>
    select(subject_id, charttime, long_title) |>
    collect()
}

get_top_diagnoses <- function(subject_id, con) {
  tbl(con, "diagnoses_icd") |>
    filter(subject_id == !!subject_id) |>
    left_join(tbl(con, "d_icd_diagnoses"), 
              by = c("icd_code", "icd_version")) |>
    group_by(long_title) |>
    summarise(freq = n(), .groups = "drop") |>
    arrange(desc(freq)) |>
    slice_min(order_by = -freq, n = 3, with_ties = FALSE) |>
    collect()
}

generate_adt_plot <- function(subject_id, con) {
  patient_demo <- get_patient_demo(subject_id, con)
  adt_data <- get_adt_data(subject_id, con)
  lab_events <- get_lab_events(subject_id, con)
  procedure_data <- get_procedure_data(subject_id, con)
  diagnoses <- get_top_diagnoses(subject_id, con)
  
  ggplot() +
    scale_x_datetime(name = "Calendar Time", 
                     limits = c(min(adt_data$intime) - days(1), 
                                max(adt_data$outtime))) +
    scale_y_discrete(name = NULL, limits = c("Procedure", "Lab", "ADT")) +
    
    geom_point(data = procedure_data, 
               aes(x = charttime, y = "Procedure", shape = long_title), 
               size = 3, color = "black") +
    
    scale_shape_manual(values = c(1:n_distinct(procedure_data$long_title))) +
    
    geom_point(data = lab_events, 
               aes(x = charttime, y = "Lab"), 
               shape = 3, size = 2, color = "black") +
    
    geom_segment(data = adt_data, 
                 aes(x = intime, xend = outtime, y = "ADT", 
                     yend = "ADT", color = careunit, 
                     linewidth = as.factor(ICU))) +
    
    theme_bw() +
    
    theme(legend.position = "bottom", 
          legend.box = "vertical", 
          legend.key.size = unit(0, "pt"), 
          legend.text = element_text(size = 7)) +
    
    guides(linewidth = "none", 
           color = guide_legend(title = "Care Unit", ncol = 3, keywidth = 1),
           shape = guide_legend(title = "Procedure", ncol = 2),
           size = "none") +
    
    labs(title = paste("Patient", patient_demo$subject_id[1], ",", 
                       patient_demo$gender[1], ",", 
                       patient_demo$anchor_age[1], "years old,", 
                       patient_demo$race[1]),
         subtitle = paste(diagnoses$long_title[1],
                          diagnoses$long_title[2],
                          diagnoses$long_title[3],
                          sep = "\n"))
}



get_icu_vitals <- function(subject_id, con) {
  vitals_itemids <- c(220045, 220180, 220179, 223761, 220210)
  
  tbl(con, "chartevents") |>
    filter(subject_id == !!subject_id, itemid %in% vitals_itemids) |>
    select(subject_id, stay_id, itemid, charttime, valuenum) |>
    collect()
}

get_item_labels <- function(con) {
  tbl(con, "d_items") |>
    filter(abbreviation %in% c("HR", "RR", "NBPs", "NBPd", "Temperature F")) |>
    select(itemid, label, abbreviation) |>
    collect()
}

generate_icu_vitals_plot <- function(subject_id, con) {
  vitals <- get_icu_vitals(subject_id, con)
  item_labels <- get_item_labels(con)
  
  vitals <- vitals |>
    left_join(item_labels, by = "itemid")
  
  # Check if the patient has any data
  if (nrow(vitals) == 0) {
    return(ggplot() + ggtitle("No ICU vitals data available for this patient"))
  }
  
  # Generate plot
  ggplot(vitals, aes(x = charttime, y = valuenum, group = abbreviation, 
                     color = abbreviation)) +
    geom_line() +
    geom_point(size = 1.5) +
    facet_grid(abbreviation ~ stay_id, scales = "free") +   
    labs(
      title = paste("Patient", vitals$subject_id[1], "ICU Stays - Vitals"),
      x = "Calendar Time",
      y = "Vital Value",
      color = "Vital Type"
    ) +
    theme_light() +
    theme(legend.position = "none") +
    guides(x = guide_axis(n.dodge = 2))
}





library(shiny)
library(shinydashboard)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "ICU Cohort Explorer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
      menuItem("Patient Details", 
               tabName = "patient_details", icon = icon("user-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      # Summary Tab
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Select Category", status = "primary", 
                    solidHeader = TRUE, collapsible = TRUE,
                    selectInput("category", "Category:", 
                                choices = c("Demographics", 
                                            "Lab Measurements", "Vitals")),
                    selectInput("variable", "Variable:", choices = NULL)
                ),
                box(title = "Graphical Summary", status = "warning", 
                    plotOutput("variable_plot")),
                box(title = "Numerical Summary", status = "info", 
                    DTOutput("variable_summary"))
              )
      ),
      # Patient Details Tab
      tabItem(tabName = "patient_details",
              h1("Patient Details"),
              textInput("input_patient_id", "Enter Patient ID:", value = ""),
              textOutput("details_text"),
              selectInput("plot_type", "Select Plot Type:", 
                          choices = c("ICU Stays Plot", "ADT Plot")),
              actionButton("load_data", "Load Patient Data"),
              plotOutput("patient_plot")
      )
    )
  )
)


server <- function(input, output, session) {
  mimic_icu_cohort <- readRDS("mimic_icu_cohort.rds")
  
  observe({
    vars <- switch(input$category,
                   "Demographics" = c("age_intime", 
                                      "gender", "race", 
                                      "insurance", 
                                      "marital_status"),
                   "Lab Measurements" = c("bicarbonate", 
                                          "chloride", 
                                          "creatinine", 
                                          "glucose", 
                                          "potassium",
                                          "sodium", 
                                          "hematocrit", 
                                          "wbc"),
                   "Vitals" = c("heart_rate", 
                                "non_invasive_blood_pressure_diastolic", 
                                "non_invasive_blood_pressure_systolic", 
                                "respiratory_rate", 
                                "temperature_fahrenheit"))
    updateSelectInput(session, "variable", choices = vars)
  })
  
  # Graphical summary
  output$variable_plot <- renderPlot({
    req(input$variable)
    if (input$variable %in% c("gender", "race", 
                              "insurance", "marital_status")) {
      ggplot(mimic_icu_cohort, 
             aes_string(x = input$variable, fill = input$variable)) +
        geom_bar() +
        theme_minimal() +
        labs(title = paste("Distribution of", input$variable), x = NULL)
    } else {
      ggplot(mimic_icu_cohort, aes_string(x = input$variable)) +
        geom_histogram(bins = 30) +
        theme_minimal() +
        labs(title = paste("Distribution of", input$variable), x = NULL)
    }
  })
  
  output$variable_summary <- renderDT({
    req(input$variable)
    summary_data <- mimic_icu_cohort %>%
      select(input$variable) %>%
      summary()
    
    summary_df <- data.frame(Value = unlist(summary_data))
    datatable(summary_df, 
              options = list(pageLength = 6, 
                             searching = FALSE, paging = FALSE))
  })
  
  observeEvent(input$load_data, {
    req(input$input_patient_id)
    patient_id <- as.numeric(input$input_patient_id)
    
    print(paste("Fetching data for patient:", patient_id))
    
    patient_demo <- get_patient_demo(patient_id, con_bq)
    output$details_text <- renderText({
      if (nrow(patient_demo) > 0) {
        paste("Patient", patient_demo$subject_id[1], "-", 
              patient_demo$gender[1], ",", 
              patient_demo$anchor_age[1], "years old,", 
              patient_demo$race[1])
      } else {
        "No patient data found."
      }
    })
    
    output$patient_plot <- renderPlot({
      tryCatch({
        if (input$plot_type == "ADT Plot") {
          generate_adt_plot(patient_id, con_bq)
        } else if (input$plot_type == "ICU Stays Plot") {
          generate_icu_vitals_plot(patient_id, con_bq)
        }
      }, error = function(e) {
        print(e)
        ggplot() + ggtitle("Error loading plot")
      })
    })
  })
}

shinyApp(ui = ui, server = server)
