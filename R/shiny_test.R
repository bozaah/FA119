library(shiny)
library(dplyr)
library(highcharter)
library(ggplot2)
library(plotly)

source(here::here("R/utils.R"))

# Load data
merged_data <- 
  arrow::read_parquet(
    here::here("data/processed/batch2_model_df_noNAs.parquet")
  ) |>
  data.table::setDT()

# Manually set batch and make `day_night` a factor
merged_data[, day_night := factor(day_night)]
merged_data[, day_night := ifelse(day_night == 0, "night", "day")]
merged_data[, batch := 2]
merged_data[, sex := ifelse(sex == 0, "male", "female")]
merged_data[, dead := ifelse(dead == 0, "Healthy", "Compromised")]


# App
ui <- fluidPage(
  titlePanel("3Ps Batch 2 data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tag_id", "Select Piglet:", choices = unique(merged_data$tag_id), selectize = TRUE),
      h4(textOutput("pen_info")),
      h4(textOutput("batch_info")),
      hr(),
      h4("Tag IDs of Compromised Pigs:"),
      h5(textOutput("n_dead")),
      textOutput("dead_pig_ids"),
      hr(),
      h4("Statistics for Selected Pig:"),
      tableOutput("selected_pig_stats"),
      hr(),
      h4("Comparison for health status:"),
      tableOutput("comparison_dead"),
      hr(),
      h4("Comparison of for period:"),
      tableOutput("comparison_day_night"),
      hr(),
      h4("Comparison for sex:"),
      tableOutput("comparison_sex")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Line Plots", 
                 plotlyOutput("tempPlot"),
                 plotlyOutput("behaviorPlot")
        ),
        tabPanel("Density Plots", 
                 highchartOutput("tempDensityPlot"),
                 highchartOutput("behaviorDensityPlot")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  updateSelectizeInput(session, 'tag_id', choices = unique(merged_data$tag_id), server = TRUE)
  
  
  # Prepare datasets ----
  ## Pig data ----
  filtered_data <- reactive({
    data <- subset(merged_data, tag_id == input$tag_id)
    # print("Filtered Data:")
    # print(head(data))  # Debugging: Print filtered data
    # print(nrow(data)) 
    data
  })
  
  ## Pen data ----
  pen_data <- reactive({
    pen <- filtered_data()$pen_number[1]
    data <- merged_data |> filter(pen_number == !!pen)
    # print("Pen Data:")
    # print(head(data))  # Debugging: Print pen data
    # print(nrow(data)) 
    data
  })
  
  ## Batch data ----
  batch_data <- reactive({
    batch <- filtered_data()$batch[1]
    data <- merged_data |> filter(batch == !!batch)
    # print("Batch Data:")
    # print(head(data))  # Debugging: Print batch data
    # print(nrow(data)) 
    data
  })
  
  # Pen details for selected XioT tag ----
  output$pen_info <- renderText({
    pen <- filtered_data()$pen_number[1]
    paste("Pen:", pen)
  })
  
  # Trial batch details for selected XioT tag ----
  output$batch_info <- renderText({
    batch <- filtered_data()$batch[1]
    paste("Batch:", batch)
  })
  
  # Calculate stats for each "group" ----
  ## Stats for a given XioT tag ----
  output$selected_pig_stats <- renderTable({
    pig_data <- filtered_data()
    calculate_stats(pig_data, "tag_temp", "tag_id")
  })
  
  ## Stats per period (day/night) ----
  output$comparison_day_night <- renderTable({
    data <- calculate_stats(merged_data, "tag_temp", "day_night")
    day_stats <- subset(data, day_night == "day")
    night_stats <- subset(data, day_night == "night")
    
    comparison <- rbind(Day = day_stats, Night = night_stats)
    comparison
  })
  
  ## Stats per sex ----
  output$comparison_sex <- renderTable({
    data <- calculate_stats(merged_data, "tag_temp", "sex")
    male_stats <- subset(data, sex == "male")
    female_stats <- subset(data, sex == "female")
    
    comparison <- rbind(Female = female_stats, Male = male_stats)
    comparison
  })
  
  ## Stats per health ----
  output$comparison_dead <- renderTable({
    data <- calculate_stats(merged_data, "tag_temp", "dead")
    dead_stats <- subset(data, dead == "Compromised")
    healthy_stats <- subset(data, dead == "Healthy")
    
    comparison <- rbind(Healthy = healthy_stats, Dead = dead_stats)
    comparison
  })
  
  ## XioT ids of compromised pigs (ie dead pigs) ----
  output$dead_pig_ids <- renderText({
    dead_pigs <- merged_data[dead == "Compromised", unique(tag_id)]
    paste(paste(dead_pigs, collapse = ", "))
  })
  
  ## Number of compromised pigs(ie dead pigs) ----
  output$n_dead <- renderText({
    dead_pigs <- merged_data[dead == "Compromised", unique(tag_id)]
    paste("Number of compromised pigs:", length(dead_pigs))
  })
  
  # Plot outputs ----
  ## temporal plot with XioT temperature ----
  output$tempPlot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = date_time, y = tag_temp)) +
      geom_line(linewidth = 1.2, alpha = .8) +
      scale_x_datetime(breaks = scales::date_breaks("2 days"),
                       labels = scales::date_format(" %d-%b-%y (%a)")) +
      labs(title = "Temperature Over Time", x = "Time", y = "XioT Temperature\n") +
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle = 330))
    ggplotly(p)
  })
  
  ## temporal plot with behaviour index ----
  output$behaviorPlot <- renderPlotly({
    data <- filtered_data()
    p <- ggplot(data, aes(x = date_time, y = activity)) +
      geom_line(linewidth = 1.2, alpha = .8) +
      scale_x_datetime(breaks = scales::date_breaks("2 days"),
                       labels = scales::date_format("%d-%b-%y (%a)")) +
      labs(title = "Behavior Index Over Time", x = "Time", y = "Behavior Index\n") +
      theme_light(base_size = 14) +
      theme(axis.text.x = element_text(angle = 330))
    ggplotly(p)
  })
  
  ## Density plots temperature ----
  output$tempDensityPlot <- renderHighchart({
    
    # Calculate densities for each "group"
    pen_density <- density(pen_data()$tag_temp)
    batch_density <- density(batch_data()$tag_temp)
    selected_density <- density(filtered_data()$tag_temp)
    
    # HC Plot
    highchart() |>
      hc_add_series(name = "Pen", data = list_parse2(data.frame(x = pen_density$x, y = pen_density$y)), type = "area", color = "royalblue", fillOpacity = 0.3) |>
      hc_add_series(name = "Batch", data = list_parse2(data.frame(x = batch_density$x, y = batch_density$y)), type = "area", color = "grey", fillOpacity = 0.3) |>
      hc_add_series(name = "Selected XioT Tag", data = list_parse2(data.frame(x = selected_density$x, y = selected_density$y)), type = "area", color = "firebrick", fillOpacity = 0.25) |>
      hc_title(text = "Temperature Density Comparison") |>
      hc_xAxis(title = list(text = "Average Temperature")) |>
      hc_yAxis(title = list(text = "Density"))
  })
  
  ## Density plots behaviour index ----
  # Calculate densities for each "group"
  output$behaviorDensityPlot <- renderHighchart({
    pen_density <- density(pen_data()$activity)
    batch_density <- density(batch_data()$activity)
    selected_density <- density(filtered_data()$activity)
    
    # HC Plot
    highchart() |>
      hc_add_series(name = "Pen", data = list_parse2(data.frame(x = pen_density$x, y = pen_density$y)), type = "area", color = "royalblue", fillOpacity = 0.5) |>
      hc_add_series(name = "Batch", data = list_parse2(data.frame(x = batch_density$x, y = batch_density$y)), type = "area", color = "grey", fillOpacity = 0.5) |>
      hc_add_series(name = "Selected XioT Tag", data = list_parse2(data.frame(x = selected_density$x, y = selected_density$y)), type = "area", color = "firebrick", fillOpacity = 0.5) |>
      hc_title(text = "Behavior Index Density Comparison") |>
      hc_xAxis(title = list(text = "Behavior Index")) |>
      hc_yAxis(title = list(text = "Density"))
  })
}

shinyApp(ui = ui, server = server)
