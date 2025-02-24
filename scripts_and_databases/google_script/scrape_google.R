


rm(list=ls())
choose.dir()
setwd("C:\\Users\\joaov\\Downloads")

library(tidyverse)
library(rvest)

tab <- read.csv("coleta20142.csv")

for (i in 1:26) {
  

#Definir o termo de pesquisa
#query <- 'site:globo.com enchente "2018"'
#query <- 'site:noticias.uol.com.br enchente "2018"'
#query <- 'site:terra.com.br enchente "2018"'
#query <- 'site:metropoles.com enchente "2018"'

query <- tab$query[i]

#Tradutor do URL
encode_special_characters <- function(text) {
  encoded_text <- NULL
  special_characters <- list('&' = '%26', '=' = '%3D', '+' = '%2B', ' ' = '%20')  # Add more special characters as needed
  for (char in strsplit(text, '')[[1]]) {
    encoded_text <- paste0(encoded_text, ifelse(is.null(special_characters[[char]]), char, special_characters[[char]]))
  }
  return(tolower(encoded_text))
}

#Pesquisar pelo URL
query2 <- encode_special_characters(query)
html_dat <- read_html(paste0("https://news.google.com/search?q=",query2,"&hl=pt-BR&gl=BR&ceid=BR%3Aen"))
dat <- data.frame(Link = html_dat %>%
                    html_nodes("article") %>% 
                    html_node("a") %>% 
                    html_attr('href')) %>% 
  mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))

#Guardar os artigos pegos pelo URL
news_text <- html_dat %>%
  html_nodes("article") %>% 
  html_text2()

x <- strsplit(news_text, "\n")
news_df <- data.frame(
  Title = sapply(x, function(item) item[3]),
  Source = sapply(x, function(item) item[1]),
  Time = sapply(x, function(item) item[4]),
  Link = dat$Link
)

#view(news_df)

#Filtrar pelo ano
filtered_df <- news_df %>%
  filter(str_detect(Time, "2014"))

#view(filtered_df)

write_csv(filtered_df, tab$csv[i])
}

view(read.csv("globoheatwave2020.csv"))
view(read.csv("uolenchente2018.csv"))
view(read.csv("terraenchente2018.csv"))
view(read.csv("metropolesenchente2018.csv"))

