RDF_columns_test = function(colnames_data1,text_out){
  test_col = grep("levels|CUTS_COND", colnames_data1)
  if(length(test_col)==0){
    print(text_out)
  }

}
