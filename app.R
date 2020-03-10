library(shiny)

example_query<-c("1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at", "1786_at", "1112_g_at", "1331_s_at", "1355_g_at", "1372_at")
example_reference<-c("1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at", "1786_at", "1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at")
example_background<-c("1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at", "1786_at", "1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at", "1786_at", "1112_g_at", "1331_s_at", "1355_g_at", "1372_at", "1391_s_at", "1403_s_at", "1419_g_at", "1575_at", "1645_at", "1786_at")

example_query<-paste(example_query, collapse="\n")
example_reference<-paste(example_reference, collapse="\n")
example_background<-paste(example_background, collapse="\n")


calculate_intersect<-function(x, y){
    i<-intersect(x, y)
    return(i)
}

fisher_test<-function(contingency_tbl){
    test_result<-fisher.test(contingency_tbl)
    return(test_result)
}

contingency_table<-function(query, reference, background){
    #lengths of each list
    qlength<-length(query)
    rlength<-length(reference)
    blength<-length(background)
    
    # how many values are shared between query and reference
    intersect_query_ref<-length(intersect(query, reference))
    
    # how many values are shared between query and background (not counting values intersecting with query)
    intersect_query_back<-length(intersect(query, background)) - intersect_query_ref
    
    # how many values are shared between reference and background (not counting values intersecting with query)
    intersect_ref_back<- length(intersect(reference, background)) - intersect_query_ref
    
    # how many values are in background and not in the query or reference
    background_only<-blength - ((qlength + rlength) - intersect_query_ref)
    
    # make a contingency table of calculated values
    contingency_table<-matrix(c(intersect_query_ref, intersect_query_back, intersect_ref_back, background_only), nrow=2)
    return(contingency_table)
}

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
