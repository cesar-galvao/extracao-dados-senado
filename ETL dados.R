# Load required libraries
pacman::p_load(httr, jsonlite, dplyr, purrr)


# API REQUEST - JSON TO LIST - FUNCTION ----
pega_request <- function(api_url){
  # get request
  response = GET(api_url)
  # Check if the request was successful (status code 200)
  if (http_status(response)$category == "Success") {
    message("Success!")
    # Parse the JSON response into an R list
    api_data = fromJSON(content(response, "text"))
    # Now, 'api_data' contains the API response as an R list
    return(api_data)
  } else {
    # Print an error message if the request was not successful
    message(cat("Error:", http_status(response)$reason, "\n"))
  }
}

# Chamada API legislaturas ----

if(!file.exists("tabelas/legislaturas.rds")){
legislaturas <- pega_request(
  "https://legis.senado.leg.br/dadosabertos/senador/lista/legislatura/46/56"
  )
saveRDS(legislaturas, "tabelas/legislaturas.rds")
} else {legislaturas <- readRDS("tabelas/legislaturas.rds")}

# ETL parlamentares legislatura 46 a 56 | basic demographics ----
parlamentares <- legislaturas$ListaParlamentarLegislatura$Parlamentares$Parlamentar$IdentificacaoParlamentar %>% 
  select("codigo" = CodigoParlamentar,
         "nome" = NomeCompletoParlamentar,
         "sexo" = SexoParlamentar,
         "partido" = SiglaPartidoParlamentar,
         "cod_leg_atual" = CodigoPublicoNaLegAtual,
         "uf" = UfParlamentar)
saveRDS(parlamentares, "tabelas/parlamentares.rds")
# write.csv(parlamentares, "parlamentares.csv")



# ETL mandatos legislatura 46 a 56 | Ocupantes ----
mandatos <- legislaturas$ListaParlamentarLegislatura$Parlamentares$Parlamentar$Mandatos$Mandato |>
  as.list()
saveRDS(mandatos, "tabelas/mandatos.rds")

# selecionar apenas itens que não são dataframe

# NOT SURE IF SHOULD FILTER
# casos nao contidos no filtro não têm identificação de quem é o titular
#filtrado <- mandatos[!lapply(mandatos, is.data.frame) |> unlist()]

# RODAR APENAS EM TESTES
 # x <- mandatos[[5]]
 # monta_observacao(mandatos[[54]]) |> glimpse()
 # monta_observacao(mandatos[[5]]) |> glimpse()
 # monta_observacao(mandatos[[1]]) |> glimpse()
 # monta_observacao(mandatos[[78]]) |> glimpse()

# funcao de limpeza para mandatos ----
monta_observacao <- function(x){
    
  # codigo do mandato
    cod_mandato = if(!is.null(x$CodigoMandato)){
      x$CodigoMandato
    } else { NA } #casos absurdos
    
  # UF do parlamentar  
    uf = x$UfParlamentar
    
    # se titular nao existe, retorna dataframe (0,0)
    participacao = x$Titular |> as.data.frame()
    
    if(dim(participacao)[1] == 0){
      participacao = data.frame(
        DescricaoParticipacao = NA,
        CodigoParlamentar = NA,
        NomeParlamentar = NA
      )
    }
    
    # inserir NA em dados da suplencia do titular quando nao 
    # houve titular em qualquer das ocorrências
    suplencia = 
      if(any(is.na(participacao$DescricaoParticipacao))){
        data.frame(
          DescricaoParticipacao = 
            if(!is.null(x$DescricaoParticipacao)){
              x$DescricaoParticipacao
            } else { NA },
          CodigoParlamentar = NA,
          NomeParlamentar = NA
        )
      } else{
        data.frame(
          DescricaoParticipacao = x$DescricaoParticipacao
          ) |> 
          cbind(participacao[,-1])
      }
    

    #esse bloco contem legislaturas e suplentes 
    #casos: simples, composto cbind reciclando se necessario
    if(!is.null(x$Suplentes$Suplente[[1]])){
      # caso simples
      if((x$Suplentes$Suplente[[1]] %>% class()) == "character"){
        suplentes = x$Suplentes$Suplente |> as.data.frame() #caso simples
        primeira_legis = data.frame(
          legislatura = "primeira",
          x$PrimeiraLegislaturaDoMandato
        )
        segunda_legis = data.frame(
          legislatura = "segunda",
          x$SegundaLegislaturaDoMandato
        )
        legis_suplentes = cbind(rbind(primeira_legis, segunda_legis), suplentes)
      } else { 
        
        #caso composto
        
        #cria df pra receber legislaturas e suplentes
        legis_suplentes = data.frame(
          legislatura = character(),
          NumeroLegislatura = character(),
          DataInicio = character(),
          DataFim = character(),
          DescricaoParticipacao = character(),
          CodigoParlamentar = character()
        )
        
        for (i in 1:length(x$Suplentes$Suplente)){
          #pega primeiro par de primeira e segunda legis
          par <- data.frame(legislatura = 
                              c("primeira", "segunda")) |>
            cbind( #adicionar colunna de legislatura
              #ao dataframe de legislaturas ordenadas
              rbind(x$PrimeiraLegislaturaDoMandato[i,],
                        x$SegundaLegislaturaDoMandato[i,])
              )
            # se o elemento da lista for nulo, adicionar com NA
            if (!is.null(x$Suplentes$Suplente[[i]])){
              par <- cbind(par, x$Suplentes$Suplente[[i]])
            } else{
              par <- par |>
                cbind(data.frame(
                  DescricaoParticipacao = NA,
                  CodigoParlamentar = NA,
                  NomeParlamentar = NA
                  )
                )
            }
          
          legis_suplentes <- rbind(legis_suplentes, par)
          #cbind com primeiro elemento da lista de suplentes
          #insere na df de legis suplentes
        }
      }
      #renomear variáveis da dataframe legis suplentes
      #----#
    }  else {
      #aqui tem que juntar as legis com os NA abaixo, que é o caso
      # em que suplentes é NULL
      primeira_legis = data.frame(
        legislatura = "primeira",
        x$PrimeiraLegislaturaDoMandato
      )
      segunda_legis = data.frame(
        legislatura = "segunda",
        x$SegundaLegislaturaDoMandato
      )
      legis_suplentes = cbind(rbind(primeira_legis, segunda_legis), 
      data.frame(
        participacao_suplente = NA,
        cod_parlamentar_suplente = NA,
        nome_suplente = NA)
      )
    }
    
    #renomeia variaveis de suplente para diferenciar
    #dos titulares
    names(legis_suplentes)[5:7] = c("participacao_suplente",
                         "cod_parlamentar_suplente",
                         "nome_suplente")
    
    return(
      cbind(uf, participacao, cod_mandato, legis_suplentes)
    )
}


# FOR REAL

mandatos_df <- data.frame(
  uf = character(),
  DescricaoParticipacao = character(),
  CodigoParlamentar = character(),
  NomeParlamentar = character(),
  cod_mandato = character(),
  legislatura = character(),
  NumeroLegislatura = character(),
  DataInicio = character(),
  DataFim = character(),
  participacao_suplente = character(),
  cod_parlamentar_suplente = character(),
  nome_suplente = character(),
  item_lista = character()
)

errors <-c()

for (i in 1:length(mandatos)) {
  tryCatch({
    #print(i)
    mandatos_df = rbind(mandatos_df,
                        cbind(monta_observacao(mandatos[[i]]), item_lista = i))
      
  }, error = function(e) {
    errors <<- c(errors, i)
    # Code to handle errors
    cat("Error in iteration", i, ":", conditionMessage(e), "\n")
  })
}

saveRDS(mandatos_df, "tabelas/mandatos_df.rds")

library(openxlsx)

write.csv(mandatos_df, "mandatos_df.csv")
readRDS('parlamentares.rds') |> write.xlsx("parlamentares.xlsx")

# Contagem de NA por legislatura ----

# Limpeza da lista gigante de legislaturas ----
rm(legislaturas)
gc()
