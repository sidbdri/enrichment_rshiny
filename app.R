library(shiny)
library(VennDiagram)
library(stringr)
library(eulerr)
source("functions.R")


example_query<-scan_in("example_query")
example_reference<-scan_in("example_reference")
example_background<-scan_in("example_background")
query <- unlist(strsplit(input$vec1,"\n"))
ref <- unlist(strsplit(input$vec2,"\n"))
back<- unlist(strsplit(input$vec3,"\n"))



u <- shinyUI(
    fluidPage(
        sidebarPanel(
            textAreaInput('vec1', 'Query values (newline delimited)'),
            textAreaInput('vec2', 'Target values (newline delimited)'),
            textAreaInput('vec3', 'Background values (newline delimited)'),
            actionButton("run", "Run test"),
            actionButton("reset", "Reset"),
            actionButton("example", "Example")
        )
        
        mainPanel(
            h3('Fisher test for enrichment'),
            br(),
            p("Test for enrichment of one list in another list, given a third, background list"),
            p("Enter newline delimited values in the relevant boxes to the left"),
            h3('Test result:'),
            verbatimTextOutput("show_results"),
            br(),
            imageOutput("venn_diagram")
        )
    ))

s <- shinyServer(function(input, output, session) {
    results<-eventReactive(input$run, {
        query <- unlist(strsplit(input$vec1,"\n"))
        ref <- unlist(strsplit(input$vec2,"\n"))
        back<- unlist(strsplit(input$vec3,"\n"))
        contingency<-contingency_table(query,ref,back)
        print(contingency)
        result<-fisher_test(contingency)
        result<-paste(result, collapse="\n")
        cat(result)
    })
    
    # set up venn diagram
    venn<-eventReactive(input$run, {
        outfile <- tempfile(fileext='.png')
        png(outfile, width=400, height=300)
        par(mfrow=c(1,2))
        draw_venn(input$vec1, input$vec2, input$vec3)
        dev.off()
        list(src = outfile,
             contentType = 'image/png',
             width = 400,
             height = 300,
             alt = "")
    })

    # shownresults and venn diagram on clicking run
    observeEvent(input$run, {
        output$show_results<-renderPrint({
            results()
        })
        output$venn_diagram <- renderImage({
            venn()
        })
    })
    
    # render empty text and plot blank on clicking reset
    observeEvent(input$reset, {
        output$show_results <- renderText({
        })
        updateTextInput(session, "vec1", value="")
        updateTextInput(session, "vec2", value="")
        updateTextInput(session, "vec3", value="")
        output$venn_diagram <- plot.new()
        dev.off()
            
    })
    
    # fill in inputs with example data on clicking example
    observeEvent(input$example, {
        output$show_results <- renderText({
        })
        updateTextInput(session, "vec1", value=example_query)
        updateTextInput(session, "vec2", value=example_reference)
        updateTextInput(session, "vec3", value=example_background)
    })
    
}
)


shinyApp(ui = u, server = s)
