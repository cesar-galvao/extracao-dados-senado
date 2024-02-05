# Load required libraries
pacman::p_load(httr, jsonlite, dplyr, purrr)

source("funcoes.R")


# API call ----

#obtem codigo de parlamentares
parlamentares <- readRDS("tabelas/parlamentares.rds")
codigos <- parlamentares$codigo

# caminhos de download
paths <- codigos |>
  sort() |>
map_vec(function(x){
  glue::glue("https://legis.senado.leg.br/dadosabertos/senador/{x}/autorias")
})



# # monta tabela de autorias
 if(!file.exists("tabelas/autorias.rds")){

   # monta a lista de autorias
   autorias_list <- list()
   errors <-c()
   
   for (i in 1:length(paths)) {
     tryCatch({
       autorias_list[i] <- pega_autoria(paths[i])
     }, error = function(e) {
       errors <<- c(errors, i)
       # Code to handle errors
       cat("Error in iteration", i, ":", conditionMessage(e), "\n")
     })
   }
   
   # elimina length == 0
   autorias_list_filtered <- autorias_list[!(sapply(autorias_list, is.null))]
   
   # trata os erros
   
   
} else {legislaturas <- readRDS("tabelas/autorias.rds")}