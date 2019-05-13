RDF_columns_test = function(colnames_data1,text_out){
  test_col = grep("DISC_CLASSES|CUTS_COND", colnames_data1)
  if(length(test_col)==0){
    print(text_out)
  }

}
