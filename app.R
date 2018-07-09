library(tidyverse)
library(lubridate)
library(shiny)

# Global variables go here

setwd('/Users/aru2/github/doerodney/presentation')
df <- read.csv('recent_data.csv', header = TRUE)
# glimpse(df)



# Define the UI:
ui <- bootstrapPage(
    selectInput('vmname', 'VM Name:', unique(sort(df$dns_name))),
    selectInput('metric', 'Metric:', c('avg_cpu_usage', 'avg_disk_usage', 'avg_mem_usage')),
    plotOutput('plot')
)


# Define the server code:
server <- function(input, output) {
    output$plot <- renderPlot({
        df_filtered <- df %>% filter(dns_name == input$vmname)
        
        if (input$metric == 'avg_cpu_usage') {
            ggplot(df_filtered, aes(x=ymd_hms(point_time), y=avg_cpu_usage)) +
                geom_line() +
                scale_x_datetime(date_labels = "%b %d") +
                ggtitle('avg cpu usage vs time') +
                xlab('time [utc]') +
                ylab('avg cpu usage [?]')
        }
        else if (input$metric == 'avg_mem_usage') {
            ggplot(df_filtered, aes(x=ymd_hms(point_time), y=avg_mem_usage)) +
                geom_line() +
                scale_x_datetime(date_labels = "%b %d") +
                ggtitle('avg mem usage vs time') +
                xlab('time [utc]') +
                ylab('avg mem usage [gb]')
        }
        else if (input$metric == 'avg_disk_usage') {
            ggplot(df_filtered, aes(x=ymd_hms(point_time), y=avg_disk_usage)) +
                geom_line() +
                scale_x_datetime(date_labels = "%b %d") +
                ggtitle('avg disk usage vs time') +
                xlab('time [utc]') +
                ylab('avg disk usage [gb]')
        }
    })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)
