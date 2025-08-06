library(rvest)

url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"

requests <- read_html(url)
table_nodes <- html_nodes(requests, "table")
table <- html_table(table_nodes[[1]], fill = TRUE)
table

summary(table)
write.csv(table, "bicycle_sharing_systems.csv", row.names = FALSE)
