library(shiny)
source("functions.R")

example_query<-read.delim("example_query")
example_reference<-read.delim("example_reference")
example_background<-read.delim("example_background")

u <- shinyUI(
    fluidPage(
        sidebarPanel(
            textAreaInput('vec1', 'Query values (newline delimited)'),
            textAreaInput('vec2', 'Target values (newline delimited)'),
            textAreaInput('vec3', 'Background values (newline delimited)'),
            actionButton("run", "Run test"),
            actionButton("reset", "Reset"),
            actionButton("example", "Example"),
        ),
        
        mainPanel(
            h3('Fisher test for enrichment'),
            br(),
            p("Test for enrichment of one list in another list, given a third, background list"),
            p("Enter newline delimited values in the relevant boxes to the left"),
            h3('Test result:'),
            verbatimTextOutput("show_results")
            
        )
    ))

s <- shinyServer(function(input, output, session) {
    
    results<-eventReactive(input$run, {
        
        
        query <- unlist(strsplit(input$vec1,"\n"))
        ref <- unlist(strsplit(input$vec2,"\n"))
        back<- unlist(strsplit(input$vec3,"\n"))
        contingency<-contingency_table(query,ref,back)
        result<-fisher_test(contingency)
        
        result<-paste(result, collapse="\n")
        
        cat(result)
    })
    
    output$show_results<-renderPrint({
        results()
    })
    
    observeEvent(input$reset, {
        output$show_results <- renderText({
        })
        updateTextInput(session, "vec1", value="")
        updateTextInput(session, "vec2", value="")
        updateTextInput(session, "vec3", value="")
    })
    
    observeEvent(input$example, {
        output$show_results <- renderText({
        })
        updateTextInput(session, "vec1", value=example_query)
        updateTextInput(session, "vec2", value=example_reference)
        updateTextInput(session, "vec3", value=example_background)
        output$show_results<-renderPrint({
            results()
        })
        
    })
}
)



shinyApp(ui = u, server = s)
