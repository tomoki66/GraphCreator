library(shiny)
library(tidyverse)
# CONSTANTS
COLOURS <- list(
  "Dark Gray - #363636",
  "Black - #000000",
  "Blue - #1d1094",
  "Red - #992614",
  "Yellow - #c2ad0e",
  "Orange - #bd7511",
  "Purple - #740091",
  "Pink - #ab6c73",
  "Green - #177322"
)
COL_THEMES <- list(
  "Set1",
  "Set2",
  "Set3",
  "Pastel1",
  "Pastel2",
  "Paired",
  "Dark1",
  "Dark2",
  "Spectral"
)
GGPLOT_THEMES <- list(
  "Gray ",
  "Black and White",
  "Light",
  "Dark",
  "Minimal",
  "Classic",
  "Void",
  "Linedraw"
)

ui <- fluidPage(
  
  #init
  
  
  titlePanel("Graph Creator 3000"),
  sidebarLayout(
    sidebarPanel(
      fileInput('data_upload', label = "Upload Data", accept = c(".csv", ".xlsx")),
      textInput(
        "title",
        "Graph Title"
      ),
      selectInput(
        'complex',
        "Customization Complexity",
        choices = list("Basic", "Advanced"),
        selected = "Basic"
      ),
      radioButtons(
        "graphtype",
        "Choose Graph Type",
        choiceNames = list(
          "Scatter Plot",
          "Histogram",
          "Bar Graph",
          "Pie Chart"
        ),
        choiceValues = list("scatter", "hist", "bar", "pie")
      ),
      selectInput(
        inputId = "var.x",
        "X-axis Variable",
        choices = ""
      ),
      conditionalPanel(
        condition = "input.graphtype == 'hist'",
        numericInput(
          inputId = "binwidth",
          "Bin Width",
          value = 10,
          min = 0.1
        )
      ),
      conditionalPanel(
        condition = "input.graphtype == 'pie'",
        radioButtons(
          "perclabs",
          "Label Percentages",
          choices = list("Yes", "No"),
          selected = "No"
        ),
        conditionalPanel(
          condition = "input.perclabs == 'Yes'",
          sliderInput(
            'dec',
            'Decimal Digits',
            min = 0,
            max = 3,
            value = 0
          )
        )
      ),
      
      conditionalPanel(
        condition = "input.graphtype != 'hist' & input.graphtype != 'pie'",
        selectInput(
          inputId = "var.y",
          "Y-axis variable",
          choices = ""
        ),
        radioButtons(
          "filter",
          "Add third variable?",
          choices = list("Yes", "No"),
          selected = "No"
        ),
        conditionalPanel(
          condition = "input.filter == 'Yes'",
          selectInput(
            inputId = "var.filter",
            "Third Variable",
            choices = ""
          ),
          radioButtons(
            "facet",
            "Separate Graphs by Third Variable?",
            choices = list("Yes", "No"),
            selected = "No"
          ),
          conditionalPanel(
            condition = "facet == 'Yes'",
            sliderInput(
              'var.ncol',
              "Columns for Facet Grid",
              min = 1,
              max = 1,
              value = 1
            )
          )
        )
      ),
      conditionalPanel(
        condition = 'input.complex == "Advanced"',
        conditionalPanel(
          condition = 'input.graphtype == "scatter"',
          radioButtons(
            'lm',
            "Add Line of Best Fit?",
            choices = list("Yes", "No"),
            selected = "No"
          ),
          conditionalPanel(
            condition = "input.lm == 'Yes'",
            selectInput(
              inputId = "line_colour",
              label = "Line of Best Fit Colour",
              choices = COLOURS,
              selected = "Red - #992614"
            )
          )
        ),
        conditionalPanel(
          condition = "input.graphtype == 'bar'",
          radioButtons(
            'flipcoord',
            "Flip Bar Orientation?",
            choices = list("Yes", "No"),
            selected = "No"
          ),
          selectInput(
            inputId = "bar_order",
            label = "Order of Bars",
            choices = list(
              "X-Ascending",
              "X-Descending",
              "Y-Ascending",
              "Y-Descending"
            ),
            selected = "X-Ascending"
          )
        ),
        conditionalPanel(
          condition = "input.filter == 'No' & input.graphtype != 'pie'",
          selectInput(
            inputId = "colour",
            label = "Data Point Colour",
            choices = COLOURS,
            selected = "Dark Gray - #363636"
          )
        ),
        conditionalPanel(
          condition = "input.filter == 'Yes' | input.graphtype == 'pie'",
          selectInput(
            inputId = "theme",
            label = "Set Colour Theme",
            choices = COL_THEMES
          )
        ),
        conditionalPanel(
          condition = "input.graphtype != 'pie'",
          selectInput(
            inputId = "ggplot_theme",
            label = "Set Graph Theme",
            choices = GGPLOT_THEMES,
            selected = "Classic"
          ),
        )
      )
    ),
    mainPanel(
      plotOutput("graph"),
      downloadButton(
        'download'
      )
    )
  )
)


  

server <- function(input, output, session) {
  # back end logic
  data <- reactive({
    req(input$data_upload)
    
    
    ext <- tools::file_ext(input$data_upload$name)
    
    dt <- switch(
      ext,
      "csv" = read.csv(input$data_upload$datapath),
      "xlsx" = readxl::read_xlsx(input$data_upload$datapath),
      validate("Invalid file; Please upload a .csv or .xlsx file")
    )
    
    updateSelectInput(inputId = "var.x", choices = colnames(dt))
    updateSelectInput(inputId = "var.y", choices = colnames(dt))
    updateSelectInput(inputId = "var.filter", choices = colnames(dt))
    
    return(dt)
  })
  
  
  plotInput <- reactive({
    req(input$data_upload)
    
    #HISTOGRAM
    if (input$graphtype == "hist"){
      g1 <- ggplot(data = data(), aes(x = data()[,input$var.x]))+
        geom_histogram(binwidth = input$binwidth, fill = substrRight(input$colour, 7))+
        labs(
          x = input$var.x
        )
      
      
      
    # PIE CHART
    } else if (input$graphtype == "pie") {
      g1 <- data()%>%
        count(data()[,input$var.x])%>%
        rename(
          col1 = 1
        )%>%
        filter(
          !is.na(col1)
        )%>%
        mutate(
          col1 = as.character(col1)
        )%>%
        arrange(desc(col1)) %>%
        mutate(
          perc = n / sum(n),
          prop = n / sum(n) *100,
          ypos = cumsum(prop) - 0.5*prop
        )%>%
        ggplot(aes(x="", y= prop, fill = col1)) +
        geom_bar(stat="identity", width=1, color="white") +
        coord_polar("y", start=0) +
        theme_void()+
        guides(fill=guide_legend(title=input$var.x))+
        scale_fill_brewer(palette = input$theme)
      
      if (input$perclabs == "Yes"){
        label.round <- paste("%1.", input$dec, "f%%", sep = "")
        g1 <- g1 + geom_label(aes(y = ypos, label = sprintf(label.round, perc*100)), fill = "white", color = "black", size=6)
      }
      
    # BAR GRAPH
    } else if (input$graphtype == "bar") {
      if (input$bar_order == "X-Ascending") {
        g1 <- ggplot(data(), aes(x = data()[,input$var.x], y = data()[, input$var.y]))
      } else if (input$bar_order == "X-Descending"){
        g1 <- ggplot(data(), aes(x = reorder(data()[,input$var.x], -data()[,input$var.x]), y = data()[,input$var.y]))
      } else if (input$bar_order== "Y-Ascending"){
        g1 <- ggplot(data(), aes(x = reorder(data()[,input$var.x], data()[,input$var.y]), y = data()[,input$var.y]))
      } else {
        g1 <- ggplot(data(), aes(x = reorder(data()[,input$var.x], -data()[,input$var.y]), y = data()[,input$var.y]))
      }
      
      if (input$filter == "Yes"){
        g1 <- g1 + 
          geom_bar(aes(fill = data()[,input$var.filter]), stat = "identity")
        if(input$facet == "Yes"){
          g1 <- g1+
            facet_grid(~data()[, input$var.filter])
        }
      } else {
        g1 <- g1 + geom_bar(fill = substrRight(input$colour, 7), stat = "identity")
      }
      
      g1 <- g1 +
        labs(
          x = input$var.x,
          y = input$var.y
        )
      
      if (input$flipcoord == "Yes"){
        g1 <- g1 + coord_flip()
      }
      
    # SCATTER PLOT
    } else if (input$graphtype == "scatter"){
      g1 <- ggplot(data(), aes(x = data()[,input$var.x], y = data()[, input$var.y]))
      if (input$filter == "Yes"){
        g1 <- g1 +
          geom_point(aes(colour = data()[,input$var.filter]))
        if(input$facet == "Yes"){
          g1 <- g1 +
            facet_grid(~data()[,input$var.filter])
        }
      } else {
        g1 <- g1 +
          geom_point(colour = substrRight(input$colour, 7))
      }
      
      
      if(input$lm == "Yes"){
        g1 <- g1 +
          geom_smooth(method=lm , colour = substrRight(input$line_colour, 7), se=TRUE) 
      }
      g1 <- g1 +
        labs(
          x = input$var.x,
          y = input$var.y
        )
    } 
    
    # SET GRAPH THEME
    if(input$graphtype != "pie"){
      if (input$ggplot_theme == "Gray "){
        g1 <- g1 + theme_gray()
      } else if (input$ggplot_theme == "Black and White"){
        g1 <- g1 + theme_bw()
      } else if (input$ggplot_theme == "Light"){
        g1 <- g1 + theme_light()
      } else if (input$ggplot_theme == "Dark"){
        g1 <- g1 + theme_dark()
      } else if (input$ggplot_theme == "Minimal"){
        g1 <- g1 + theme_classic()
      } else if (input$ggplot_theme == "Classic"){
        g1 <- g1 + theme_classic()
      } else if (input$ggplot_theme == "Void"){
        g1 <- g1 + theme_void()
      } else if (input$ggplot_theme == "Linedraw"){
        g1 <- g1 + theme_linedraw()
      }
    }
    
    g1 <- g1 + labs(
      title = input$title
    )
    
    return(g1)
  })
  
  output$graph <- renderPlot({
    print(plotInput())
  })
  
  
  output$download <- downloadHandler(
    filename = function() { paste(input$title, '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png")
    }
  )
}

#Function

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

shinyApp(ui, server)



