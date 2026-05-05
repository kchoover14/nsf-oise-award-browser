# Load clean data
awards <- read.csv('oiseAwards.csv')

# Convert dates to date
awards$endDate <- as.Date(awards$endDate, format="%m/%d/%Y")
awards$lastAmendmentDate <- as.Date(awards$lastAmendmentDate, format="%m/%d/%Y")
awards$endDate <- as.Date(awards$endDate, format="%m/%d/%Y")

# Remove dollar symbol from column
awards$awardedAmountToDate <- as.numeric(gsub("[\\$,]", "", awards$awardedAmountToDate))

# Function to generate the Plotly jitter map
generatePlotlyJitterMap <- function(data) {
  jittered_data <- data %>%
    mutate(
      jitter_lon = long + runif(n(), -0.6, 0.6),
      jitter_lat = lat + runif(n(), -0.6, 0.6),
      marker_size = sqrt(awardedAmountToDate) / 100  # Adjust size scaling as necessary
    )
  
  plot_geo(jittered_data, lat = ~jitter_lat, lon = ~jitter_lon) %>%
    add_markers(
      text = ~popupContent,
      color = ~program,
      colors = c('#005699', '#6C628C', '#3390A3', '#DCC276'),
      marker = list(
        size = ~marker_size,
        opacity = 0.8,
        line = list(
          color = ~program,
          width = 2
        ),
        symbol = 'circle-open'
      ),
      hoverinfo = "text"
    ) %>%
    layout(
      geo = list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = 'rgb(255, 255, 255)',
        subunitwidth = 1,
        countrywidth = 1,
        subunitcolor = 'rgb(0, 0, 0)',
        countrycolor = 'rgb(0, 0, 0)',
        bgcolor = 'rgba(0,0,0,0)'
      )
    )
}

# Function to generate the plot
generatePlot <- function(data) {
  p <- ggplot(data, aes(x = endDate, y = awardedAmountToDate, 
    color = program, size = awardedAmountToDate,
    text = paste("Award ID: ", awardID, "<br>",
      "Program: ", program, "<br>", 
      "Track: ", as.factor(track), "<br>", 
      "Amount Awarded as of May 2024: ", scales::dollar(awardedAmountToDate), "<br>",
      "Awardee: ", organization))) +
    geom_point() +
    scale_color_viridis_d(end = 0.8, alpha = 0.4) +
    labs(title = "OISE Awards, US Dollars Awarded to Date",
      x = '',
      y = '',
      color = "Program",
      size="") +
    theme_classic() +
    scale_y_continuous(labels = function(x) paste0(format(round(x / 1e6, 2), nsmall = 2), "M"))
  
  ggplotly(p, tooltip = "text")
}

# Function to generate the line chart
generateLineChart <- function(data) {
  # Summarize the data by year and program
  summarized_data <- data %>%
    mutate(year = format(endDate, "%Y")) %>%
    group_by(year, program) %>%
    summarize(total_awarded = sum(awardedAmountToDate))
  
  # Create the ggplot object
  p <- ggplot(summarized_data, aes(x = year, y = total_awarded, color = program, group = program)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    labs(title = "Total Awards by Program and Year",
      x = 'Year',
      y = 'Total Awarded Amount',
      color = "Program") +
    theme_minimal() +
    scale_y_continuous(labels = scales::dollar)
  
  p
}

# Function to generate the table with specific columns
generateTable <- function(data) {
  # Select specific columns and change their names within datatable call using 'colnames' argument
  datatable(data[, c("awardID", "program", "endDate", "awardedAmountToDate", "PI", "organization", "EPSCoR")],
    options = list(dom = 'ftp', paging = TRUE, autoWidth = TRUE),
    colnames = c("Award ID", "Program", "Start Date", "Amount Awarded", "Principal Investigator", "Organization", "EPSCoR"))
}
