Viewrules_type_L <- function(rules, node_id){
  n_elem <- lapply(as.character(rules$features[node_id]), function(x) str_count(x, pattern = "=")) %>%  unlist %>% max
  rules_RDF <- as.character(rules$features[node_id]) %>% as_tibble %>%
    separate(value,into= paste0('y', 1:(n_elem*2)), sep = ",|\\=" , extra = 'drop' ) %>%
    unite("features", paste0('y', seq(from=1, to=(n_elem*2), by=2)), na.rm = TRUE, remove = TRUE, sep = ',') %>%
    unite("levels", paste0('y', seq(from=2, to=(n_elem*2), by=2)), na.rm = TRUE, remove = TRUE, sep = ',')
  rules_RDF_fin <- cbind(rules_RDF,rules[node_id,-1] )
  return(rules_RDF_fin)
}

