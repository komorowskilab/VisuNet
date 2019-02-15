data_input = function(data1, type){
  #R.Rosetta output
  if(type == 'RDF'){
    df = data1
  }else if(type == 'L'){
    df = data1
  }else if(type == 'RF'){
    #Rosetta output format
  }else{
    print('Invalid data type!')
  }
  return(df)
}
