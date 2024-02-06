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
   errors <-data.frame(
     iteration = integer(),
     code = character()
   )
   
   #conf <- integer()
   
   for (i in 1:length(paths)) {
     #conf <<- c(conf, i)
     tryCatch({
       autorias_list[[i]] <- pega_autoria(paths[i])
     }, error = function(e) {
       errors[i,] <<- c(i,codigos[i])
       # Code to handle errors
     })
   } #lista total tem 971, numero de parlamentares com código
   errors <- errors %>% filter(!is.na(iteration))
   #remove casos com length == 0, reduzimos para 555
   autorias_list <- autorias_list[map_vec(autorias_list, length) != 0]
   
   # erros retornam apenas codigos sem retorno da API
   paths_errors <- errors$code |>
     sort() |>
     map_vec(function(x){
       glue::glue("https://legis.senado.leg.br/dadosabertos/senador/{x}/autorias")
     })
   
   #pega as que deram erro
   autorias_errors <- map(paths_errors, pega_autoria)
   #remove os null
   autorias_errors <- autorias_errors[map_vec(autorias_errors, length) != 0]
   
   #salva como tabela
   saveRDS(bind_rows(autorias_list, autorias_errors), "tabelas/autorias.rds")
   
 } else {autorias <- readRDS("tabelas/autorias.rds")}


glimpse(autorias)

autorias_sexo <- parlamentares %>%
  select(codigo, sexo, partido) %>%
  inner_join(autorias, by = c("codigo" = "cod_parlamentar")) %>%
  rename("cod_materia" = Codigo)

openxlsx::write.xlsx(autorias_sexo, "tabelas formatos usuário/autorias_sexo.xlsx", asTable = TRUE)
openxlsx::write.xlsx(parlamentares, "tabelas formatos usuário/parlamentares.xlsx", asTable = TRUE)
