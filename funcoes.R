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


# AS PERCENT ----
as_percent <- function(x){
  
  obj <- base::round(x*100, digits = 2) |>
    as.double()

  percent <- ifelse(obj - round(obj, 0) == 0,
         sprintf("%.2f", obj),
         as.character(obj)) %>%
    paste0(., "%")
  
  return(percent)
}
  

# MONTA OBSERVACAO E LIMPA MANDATOS ----
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

# PEGA AUTORIAS ----

pega_autoria <- function(path){ #must be iterated over every path
  request_autoria <- pega_request(path)
  
  df_autoria <- request_autoria$MateriasAutoriaParlamentar$Parlamentar$Autorias$Autoria$Materia %>%
    cbind(autor_principal = request_autoria$MateriasAutoriaParlamentar$Parlamentar$Autorias$Autoria$IndicadorAutorPrincipal,
          outros_autores = request_autoria$MateriasAutoriaParlamentar$Parlamentar$Autorias$Autoria$IndicadorOutrosAutores,
          cod_parlamentar = request_autoria$MateriasAutoriaParlamentar$Parlamentar$Codigo,
          nome_parlamentar = request_autoria$MateriasAutoriaParlamentar$Parlamentar$Nome)
  return(df_autoria)
}
