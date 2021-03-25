alone_quotes <- read_html("https://www.brainyquote.com/topics/alone-quotes")

quote <- alone_quotes %>%
  html_nodes(".oncl_q") %>%
  html_text()

person <- quotes_html %>%
  html_nodes(".oncl_a") %>%
  html_text()

quotes_dat <- data.frame(quote = quotes, stringsAsFactors = FALSE) %>%
  filter(quote != "\n") %>%
  mutate(person = person
         , together = paste('"', as.character(quote), '" --'
                            , as.character(person), sep=""))

write.csv(quotes_dat, 
          "/Users/kimberlyzhou/git/stat231-kzhou53/homeworks/quotes.csv", 
          row.names = FALSE)