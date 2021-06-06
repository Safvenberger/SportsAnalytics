library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)

# Read data
seasons <- c("0708", "0809", "0910", "1011", "1112", "1213", "1314")
assists <- lapply(seasons, function(season) cbind(
    read.csv(paste(season, "/assist.csv", sep = "")), season) %>% 
        arrange(-Assists) %>% 
        mutate(Rank_trad = seq(1:nrow(.))) %>% # Create rank for traditional metric
        arrange(-weighted_assist) %>% 
        mutate(Rank_w = seq(1:nrow(.)), # Create rank for weighted metric
               Rank_diff = Rank_trad - Rank_w) # Rank difference
        ) %>% 
    rbindlist() %>% 
    rename(PlayerName = Playername,
           Metric_trad = Assists, 
           Metric_weight = weighted_assist) %>% 
    relocate(starts_with("Rank"), .before = PlayerId) # Change column order

goals <- lapply(seasons, function(season) cbind(
    read.csv(paste(season, "/goal.csv", sep = "")),
    season)) %>% 
    rbindlist() %>% 
    rename(Metric_trad = Goals, 
           Metric_weight = Goals_w)

plusminus <- lapply(seasons, function(season) cbind(
    read.csv(paste(season, "/plusminus.csv", sep = "")),
    season)) %>% 
    rbindlist() %>% 
    rename(Metric_trad = PlusMinus, 
           Metric_weight = PlusMinus_w)

points <- lapply(seasons, function(season) cbind(
    read.csv(paste(season, "/point.csv", sep = "")),
    season)) %>% 
    rbindlist() %>% 
    rename(Metric_trad = Points, 
           Metric_weight = Points_w)

# Create a vector of the datasets/metrics
metrics <- c("Assists", "Goals", "PlusMinus", "Points")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("HOCKEY"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput("dataset", "Metric", 
                        choices = metrics),
            
            uiOutput("PlayerName"),
            
            checkboxInput("Facet",
                        "Facet or nah:",
                        value = FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
        #   plotOutput("hockey")
        #)
        tabsetPanel(type = "tabs",
                    tabPanel("Plot", plotOutput("hockey")),
                    tabPanel("Table", plotOutput("overall")),
                    tabPanel("Corr", 
                             fluidRow(
                                 column(width = 6, tableOutput("table")),
                                 column(width = 6, tableOutput("table2"))
                                 )
        )
        )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # Interactive way to select the dataset of interest
    dataset <- reactive({
        switch(input$dataset,
               "Assists" = assists,
               "Goals" = goals,
               "PlusMinus" = plusminus,
               "Points" = points
               )
    })
    
    output$PlayerName <- renderUI({
        metric <- dataset()
        selectInput("PlayerName",
                    "Select players",
                    choices = metric %>% 
                        group_by(PlayerName) %>% 
                        summarise(sum = sum(Metric_weight)) %>% 
                        arrange(-sum) %>% 
                        select(PlayerName) %>% 
                        pull(),
                    multiple = TRUE)
        })
    
    output$hockey <- renderPlot({
        # Get the currently selected metric
        metric <- dataset()
        metric_name <- input$dataset
        labels <- c(metric_name, 
                    paste("Weighted", metric_name)
                    )
        
        # Ensure at least one player is selected
        validate(
            need(!is.null(input$PlayerName), 
                 message = "At least one player needs to be selected "
            )
        )
        
        if(input$Facet) {
            metric %>% 
                filter(PlayerName %in% input$PlayerName) %>% 
                ggplot(.) + 
                geom_point(aes(season, Metric_trad, 
                               group = PlayerId, color = "Traditional")) + 
                geom_path(aes(season, Metric_trad, 
                              group = PlayerId, color = "Traditional")) + 
                geom_point(aes(season, Metric_weight, 
                               group = PlayerId, color = "Weighted"))+ 
                geom_path(aes(season, Metric_weight, 
                               group = PlayerId, color = "Weighted")) + 
                facet_wrap(~PlayerName) + 
                scale_color_manual(labels = labels, 
                                   values = c("Firebrick", "Midnightblue")) + 
                ylab(metric_name) + theme_bw()
            
        }
        else {
            metric %>% 
                filter(PlayerName %in% input$PlayerName) %>% 
                ggplot(.) + 
                geom_point(aes(season, Metric_trad, 
                               group = PlayerId, color = PlayerName)) + 
                geom_path(aes(season, Metric_trad,
                              group = PlayerId, color = PlayerName)) + 
                geom_point(aes(season, Metric_weight, 
                               group = PlayerId, color = PlayerName)) + 
                geom_path(aes(season, Metric_weight, 
                              group = PlayerId, color = PlayerName)) + 
                ylab(metric_name) + theme_bw()
        }
    })
    
    output$overall <- renderPlot({
        # Get the currently selected metric
        metric <- dataset()
        metric_name <- input$dataset
        labels <- c(metric_name, 
                    paste("Weighted", metric_name)
        )
        
        bind_rows(
            assists %>% 
                mutate(metric = "Assists"),
            
            goals %>% 
                mutate(metric = "Goals"),
            
            plusminus %>% 
                mutate(metric = "PlusMinus"),
            
            points %>% 
                mutate(metric = "Points"),
        ) %>% 
            ggplot(., aes(Metric_trad, Metric_weight, group = season,
                          color = season)) + 
            geom_point(alpha = 0.4) + 
            geom_smooth(se = FALSE, alpha = 0.7) + 
            facet_wrap(~metric, scales = "free") + 
            #scale_color_manual(labels = labels, 
            #                   values = c("Firebrick", "Midnightblue")) + 
            xlab("Traditional") + ylab("Weighted") + theme_bw()
     
    })
    
    output$table <- renderTable({
        bind_rows(
            assists %>% 
                group_by(season) %>% 
                summarise(., Pearson = cor(Metric_trad, Metric_weight, method = "pearson"),
                          Kendall = cor(Metric_trad, Metric_weight, method = "kendall"),
                          Spearman = cor(Metric_trad, Metric_weight, method = "spearman")) %>% 
                mutate(metric = "Assists"),
            
            goals %>% 
                group_by(season) %>% 
                summarise(., Pearson = cor(Metric_trad, Metric_weight, method = "pearson"),
                          Kendall = cor(Metric_trad, Metric_weight, method = "kendall"),
                          Spearman = cor(Metric_trad, Metric_weight, method = "spearman")) %>% 
                mutate(metric = "Goals")
        ) 
    })
    
    output$table2 <- renderTable({
        bind_rows(
            plusminus %>% 
                group_by(season) %>% 
                summarise(., Pearson = cor(Metric_trad, Metric_weight, method = "pearson"),
                          Kendall = cor(Metric_trad, Metric_weight, method = "kendall"),
                          Spearman = cor(Metric_trad, Metric_weight, method = "spearman")) %>% 
                mutate(metric = "PlusMinus"),
            
            points %>% 
                group_by(season) %>% 
                summarise(., Pearson = cor(Metric_trad, Metric_weight, method = "pearson"),
                          Kendall = cor(Metric_trad, Metric_weight, method = "kendall"),
                          Spearman = cor(Metric_trad, Metric_weight, method = "spearman")) %>% 
                mutate(metric = "Points")
        ) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
