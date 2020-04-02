
draw_venn<-function(query, ref, back){
    query<-unlist(str_split(query, "\n"))
    ref<-unlist(str_split(ref, "\n"))
    back<-unlist(str_split(back, "\n"))
    query<-query[query != ""]
    ref<-ref[ref != ""]
    back<-back[back != ""]
    overlap12 <- length(calculate.overlap(x=list(query, ref))$a3)
    overlap13 <- length(calculate.overlap(x=list(query, back))$a3)
    overlap23 <- length(calculate.overlap(x=list(ref, back))$a3)
    overlap123 <- length(calculate.overlap(x=list(query, ref, back))$a5)
    venn<-draw.triple.venn(length(query), length(ref), length(back), overlap12, overlap23, overlap13, overlap123, col = c("lightpink", "lightblue", "lightgreen"), fill = c("lightpink", "lightblue", "lightgreen"), category=c("Query", "Reference", "Background"))
    
    
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
    test_result<-NULL
      #
  }
  return(test_result)
}

print_results<-function(r){
  # fisher test function returns NULL if negative vaklues present in contigency table
  # check first if parameter passed in is fisher test result or NULL, otherwise extract relevant values
  if (!is.null(r)){
    ci_lower<-r$conf.int[1]
    ci_upper<-r$conf.int[2]
    ci<-paste(ci_lower, ci_upper, sep=", ")
    p<-r$p.value
    e<-r$estimate
    results<-paste("confidence interval: ", ci, "\n", "odds ratio: ", e, "\n", "pvalue: ", p, "\n")
  } else {
    results<-"There are negative values in the contingency table. Did you enter the lists in the correct order?"
  }
  return(results)
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

  rownames(contingency_table)<-c("in.query", "not.in.query")
  colnames(contingency_table)<-c("in.ref", "not.in.ref")
  return(contingency_table)
}