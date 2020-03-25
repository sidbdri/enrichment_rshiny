




######################

# Run this piece of code to show the plot that *should* render when using euler() below
# numbers reflect the numbers produced using the example dataset
# plot code only differs in using "one" rather than "length(one)" as in the draw_venn function below, for simplicity of setup

one<-11
two<-31
three<-41
overlap12<-11
overlap13<-11
overlap23<-21
overlap123<-11

venn<-euler(c(one = one, two = two, three = three,
           "one&two" = overlap12, "one&three" = overlap13, "two&three" = overlap23,
           "one&two&three" = overlap123), input = "union")
plot(venn)

######################



draw_venn<-function(one, two, three){
    one<-unlist(str_split(one, "\n"))
    two<-unlist(str_split(two, "\n"))
    three<-unlist(str_split(three, "\n"))
    one<-one[one != ""]
    two<-two[two != ""]
    three<-three[three != ""]
    overlap12 <- length(calculate.overlap(x=list(one, two))$a3)
    overlap13 <- length(calculate.overlap(x=list(one, three))$a3)
    overlap23 <- length(calculate.overlap(x=list(two, three))$a3)
    overlap123 <- length(calculate.overlap(x=list(one, two, three))$a5)
    
    
    # BUG: switch between two calls to create venn diagram, bottom one works, top one does not
    
    #venn<-euler(c(one = length(one), two = length(two), three = length(three),
    #           "one&two" = overlap12, "one&three" = overlap13, "two&three" = overlap23,
    #           "one&two&three" = overlap123), input = "union")
    
    venn<-draw.triple.venn(length(one), length(two), length(three), overlap12, overlap23, overlap13, overlap123, col = c("lightpink", "lightblue", "lightgreen"), fill = c("lightpink", "lightblue", "lightgreen"))
    
    
    return(venn)
}

scan_in<-function(filename){
  test_data<-scan(filename, what=character())
  test_data<-paste(test_data, collapse="\n")
  return(test_data)
}

calculate_intersect<-function(x, y){
  i<-intersect(x, y)
  return(i)
}

fisher_test<-function(contingency_tbl){
  #check that the contingency table does not contain negative values
  #if none, return result, if negative values, return error message
  if (all(contingency_tbl >= 0)) {
    test_result<-fisher.test(contingency_tbl)
  } else {
    test_result<-"There are negative values in your contingency table. \nHave you entered your input lists correctly?"
  }
  return(test_result)
}

contingency_table<-function(query, reference, background){
  
  # restrict query and references to values also present in background list
  query <- query[query %in% background]
  reference <- reference[reference %in% background]
  
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