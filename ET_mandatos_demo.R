# Load required libraries
pacman::p_load(httr, jsonlite, dplyr, purrr)

source("funcoes.R")

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
