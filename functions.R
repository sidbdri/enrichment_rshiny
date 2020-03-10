
scan_in<-function(filename){
  test_data<-scan(filename, what=character())
  test_data<-paste(test_data, collapse="\n")
  return(test_data)
}

check_table<-function(input_table){
  ifall(m < 0)
}

calculate_intersect<-function(x, y){
  i<-intersect(x, y)
  return(i)
}

fisher_test<-function(contingency_tbl){
  # check that the contingency table does not contain negative values
  # if none, return result, if negative values, return error message
  if (all(contingency_tbl < 0)) {
    test_result<-fisher.test(contingency_tbl)
  } else {
    test_result<-"There are negative values in your contingency table. \nHave you entered your input lists correctly?"
  }
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