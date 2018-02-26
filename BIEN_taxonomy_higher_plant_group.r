BIEN_taxonomy_higher_plant_group <- function (higher_plant_group, print.query = FALSE, ...) 
{
  is_char(higher_plant_group)
  is_log(print.query)
  sql_select <- paste("SELECT DISTINCT higher_plant_group, \"class\", superorder, \"order\", scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author,scrubbed_taxonomic_status")
  sql_from <- paste(" FROM bien_taxonomy")
  sql_where <- paste(" WHERE higher_plant_group in (", paste(shQuote(higher_plant_group, 
                                                                  type = "sh"), collapse = ", "), ") AND scrubbed_species_binomial IS NOT NULL")
  sql_order_by <- paste(" ORDER BY higher_plant_group,scrubbed_family,scrubbed_genus,scrubbed_species_binomial,scrubbed_author ")
  query <- paste(sql_select, sql_from, sql_where, sql_order_by, 
                 ";")
  if (print.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
                  perl = TRUE)
    print(query)
  }
  return(BIEN_sql(query, ...))
}



