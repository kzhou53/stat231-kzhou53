justice_url <- "https://en.wikipedia.org/wiki/List_of_justices_of_the_Supreme_Court_of_the_United_States"

justice_table <- justice_url %>%
  read_html() %>% 
  html_nodes("table")
justice_table <- html_table(justice_table[[2]], fill = TRUE)

write.csv(justice_table, 
          "/Users/kimberlyzhou/git/stat231-kzhou53/homeworks/justices.csv", 
          row.names = FALSE)
