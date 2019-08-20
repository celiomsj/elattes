#' @title GetTypeProductionNoDuplicate
#'
#'
#'
#' @param publicacoesPorAno a list of Lattes CV XML files.
#' @param levenshtein field a list of Lattes CV XML files.
#' @param tipo field a list of Lattes CV XML files.
#' @param tipoDaProducao field a list of Lattes CV XML files.
#'
#'
#' @export GetTypeProductionNoDuplicate

GetTypeProductionNoDuplicate <- function(publicacoesPorAno, levenshtein, tipo, tipoDaProducao) {

	result <- NULL



	if (tipo == 'PERIODICO') {
 		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if (tipo == 'EVENTO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	}else if(tipo == 'LIVRO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	}else if(tipo == 'CAPITULO_DE_LIVRO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	}else if(tipo == 'ARTIGO_ACEITO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
        }else if(tipo == 'TEXTO_EM_JORNAIS'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_CONCLUIDA_MESTRADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	}  else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_CONCLUIDA_DOUTORADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	} else if(tipo == 'OUTRAS_ORIENTACOES_CONCLUIDAS'){
		result<-RemoveDuplicate(publicacoesPorAno,levenshtein,tipo, tipoDaProducao)
	}

	result

}


RemoveDuplicate<-function( listaPorAno, levenshtein, deQualtipo, tipoDaProducao){

  anos<-hash::keys(listaPorAno)

	publicacoesPorAnoSemDuplicatas <- hash::hash()

  for (ano in anos){
    publications <- if (tipoDaProducao == 'BIBLIOGRAFICA') {
      listaPorAno[[ano]]$publications
    } else {
      listaPorAno[[ano]]$orientacoes
    }

    pubs_idlattes <- lapply(publications, function(x) x[['IDLATTES']])
    pubs_titles <- lapply(publications, get_title_or_advisee, type = deQualtipo)

    pubs_deduped <- list()

    for (i in seq_along(publications)) {

      same_idlattes <- pubs_idlattes[[i]] == pubs_idlattes[-(1:i)]

      if (deQualtipo == "ORIENTACAO_CONCLUIDA_MESTRADO" ||
          deQualtipo == "ORIENTACAO_CONCLUIDA_DOUTORADO" ||
          deQualtipo == "OUTRAS_ORIENTACOES_CONCLUIDAS" ||
          deQualtipo == "ORIENTACAO_CONCLUIDA_POS_DOUTORADO" ||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'||
          deQualtipo == 'ORIENTACAO_EM_ANDAMENTO_ESPECIALIZACAO') {

        codigos_cursos <- lapply(publications[-(1:i)], function(x) x[['DETALHAMENTO']][['CODIGO_CURSO']])

        similar <- !same_idlattes &
          (publications[[i]][['DETALHAMENTO']][['CODIGO_CURSO']] == codigos_cursos) &
          (stringdist::stringdist(toupper(pubs_titles[[i]]), toupper(pubs_titles[-(1:i)]), method = 'lv') < levenshtein)
      } else {
        similar <- !same_idlattes &
          (stringdist::stringdist(toupper(pubs_titles[[i]]), toupper(pubs_titles[-(1:i)]), method = 'lv') < levenshtein)

      }


      if (!any(similar)) {
        pubs_deduped <- c(pubs_deduped, list(publications[[i]]))
      }
    }

    pubs_deduped_obj <- BuildObject(publications, pubs_deduped, levenshtein, deQualtipo)
    hash::.set(publicacoesPorAnoSemDuplicatas, ano, pubs_deduped_obj)

  }

	publicacoesPorAnoSemDuplicatas
}


get_title_or_advisee <- function(publication, type) {
  switch(type,
         'PERIODICO' = publication$DADOS_BASICOS_DO_ARTIGO$TITULO_DO_ARTIGO,
         'ARTIGO_ACEITO' = publication$DADOS_BASICOS_DO_ARTIGO$TITULO_DO_ARTIGO,
         'EVENTO' = publication$DADOS_BASICOS_DO_TRABALHO$TITULO_DO_TRABALHO,
         'LIVRO' = publication$DADOS_BASICOS_DO_LIVRO$TITULO_DO_LIVRO,
         'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA' = publication$DADOS_BASICOS_DE_OUTRA_PRODUCAO$TITULO,
         'CAPITULO_DE_LIVRO' = publication$DADOS_BASICOS_DO_CAPITULO$TITULO_DO_CAPITULO_DO_LIVRO,
         'TEXTO_EM_JORNAIS' = publication$DADOS_BASICOS_DO_TEXTO$TITULO_DO_TEXTO,
         'ORIENTACAO_CONCLUIDA' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_CONCLUIDA_MESTRADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_CONCLUIDA_DOUTORADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'OUTRAS_ORIENTACOES_CONCLUIDAS' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_CONCLUIDA_POS_DOUTORADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_MESTRADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_DOUTORADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_GRADUACAO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO,
         'ORIENTACAO_EM_ANDAMENTO_ESPECIALIZACAO' = publication$DETALHAMENTO$NOME_DO_ORIENTADO
  )
}


BuildObject <- function (publicacoes, publicacoesUnicas, levenshtein, tipo) {

		publications <- list()


		for (pub in publicacoesUnicas) {

			lista <- GetAuthorsEndogenous(publicacoes, pub, levenshtein, tipo)
			names <- NULL
			if(tipo != 'ORIENTACAO_CONCLUIDA_MESTRADO' ||
                           tipo != 'OUTRAS_ORIENTACOES_CONCLUIDAS' ||
                           tipo != 'ORIENTACAO_CONCLUIDA_DOUTORADO' ||
                           tipo != 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'||
		  	   tipo !='ORIENTACAO_CONCLUIDA_POS_DOUTORADO' ||
			   tipo !='ORIENTACAO_EM_ANDAMENTO_DOUTORADO' ||
			   tipo != 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO' ||
			   tipo != 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO' ||
			   tipo != 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'){
				names<-GetNameCitation(pub)
			}

			novaPublicacao <- CreateObject(pub,names, lista[[1]], lista[[2]], tipo)
			publications <- c(publications, list(novaPublicacao))
		 }



		publications

}


GetAuthorsEndogenous <- function(publicacoes, publicacao, levenshtein, tipo) {

  pubs_idlattes <- lapply(publicacoes, function(x) x[['IDLATTES']])
  pubs_titles <- lapply(publicacoes, get_title_or_advisee, type = tipo)
  orientadores <- lapply(publicacoes, function(x) x[['NOME_COMPLETO']])

  if (tipo == "ORIENTACAO_CONCLUIDA_MESTRADO" ||
      tipo == "ORIENTACAO_CONCLUIDA_DOUTORADO" ||
      tipo == "OUTRAS_ORIENTACOES_CONCLUIDAS" ||
      tipo == "ORIENTACAO_CONCLUIDA_POS_DOUTORADO" ||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO'||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO'||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO'||
      tipo == 'ORIENTACAO_EM_ANDAMENTO_ESPECIALIZACAO') {

    codigos_cursos <- lapply(publicacoes, function(x) x[['DETALHAMENTO']][['CODIGO_CURSO']])
    matches <- (publicacao[['DETALHAMENTO']][['CODIGO_CURSO']] == codigos_cursos) &
      (stringdist::stringdist(toupper(get_title_or_advisee(publicacao, tipo)),
                              toupper(pubs_titles), method = 'lv') < levenshtein)

  } else  {
    matches <- stringdist::stringdist(toupper(get_title_or_advisee(publicacao, tipo)),
                                      toupper(pubs_titles), method = 'lv') < levenshtein
  }

  listas <- list()
  listas <- c(listas, list(pubs_idlattes[matches]))
  listas <- c(listas, list(orientadores[matches]))

  return(listas)

}


GetNameCitation<-function(publicacao){


 	names<-list()


	for(item in publicacao$AUTORES){
		names<-c(names, list(item$NOME_PARA_CITACAO))
	}


        names

}

CreateObject <- function(pubB, nomesDeCitacao, endogenos, orientadores ,tipo ){

          x <- list()
	  x <- nomesDeCitacao

	  y <- list()
	  y <- endogenos

	  pub<-new.env(parent=emptyenv())

	if(tipo =='PERIODICO'){

		  pub$TITULO_DO_ARTIGO <- pubB$DADOS_BASICOS_DO_ARTIGO$TITULO_DO_ARTIGO
		  pub$ANO_DO_ARTIGO <- pubB$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO
		  pub$TITULO_DO_PERIODICO_OU_REVISTA<- pubB$DETALHAMENTO_DO_ARTIGO$TITULO_DO_PERIODICO_OU_REVISTA
		  pub$VOLUME <- pubB$DETALHAMENTO_DO_ARTIGO$VOLUME
		  pub$ISSN <- pubB$DETALHAMENTO_DO_ARTIGO$ISSN
		  pub$DOI <- pubB$DADOS_BASICOS_DO_ARTIGO$DOI
		  pub$authors <- x
		  pub$PAGINA_INICIAL <- pubB$DETALHAMENTO_DO_ARTIGO$PAGINA_INICIAL
		  pub$PAGINA_FINAL <-pubB$DETALHAMENTO_DO_ARTIGO$PAGINA_FINAL
		  pub$NATUREZA <- pubB$DADOS_BASICOS_DO_ARTIGO$NATUREZA
		  pub$authorsEndogenous <- y

	}else if ( tipo == 'EVENTO'){

		  pub$ANO_DO_TRABALHO  <- pubB$DADOS_BASICOS_DO_TRABALHO$ANO_DO_TRABALHO
		  pub$DOI  <- pubB$DADOS_BASICOS_DO_TRABALHO$DOI
		  pub$NATUREZA  <- pubB$DADOS_BASICOS_DO_TRABALHO$NATUREZA
		  pub$PAIS_DO_EVENTO   <- pubB$DADOS_BASICOS_DO_TRABALHO$PAIS_DO_EVENTO
		  pub$TITULO_DO_TRABALHO  <- pubB$DADOS_BASICOS_DO_TRABALHO$TITULO_DO_TRABALHO
		  pub$CIDADE_DO_EVENTO   <- pubB$DETALHAMENTO_DO_TRABALHO$CIDADE_DO_EVENTO
		  pub$SERIE<-pubB$DETALHAMENTO_DO_TRABALHO$SERIE
		  pub$FASCICULO<-pubB$DETALHAMENTO_DO_TRABALHO$FASCICULO
		  pub$VOLUME<-pubB$DETALHAMENTO_DO_TRABALHO$VOLUME
		  pub$authors <- x
		  pub$CLASSIFICACAO_DO_EVENTO  <- pubB$DETALHAMENTO_DO_TRABALHO$CLASSIFICACAO_DO_EVENTO
		  pub$NOME_DO_EVENTO  <-pubB$DETALHAMENTO_DO_TRABALHO$NOME_DO_EVENTO
		  pub$PAGINA_INICIAL <- pubB$DETALHAMENTO_DO_TRABALHO$PAGINA_INICIAL
                  pub$PAGINA_FINAL <- pubB$DETALHAMENTO_DO_TRABALHO$PAGINA_FINAL
		  pub$authorsEndogenous <- y


        }else if (tipo == 'LIVRO'){

		  pub$ANO  <- pubB$DADOS_BASICOS_DO_LIVRO$ANO
		  pub$DOI  <- pubB$DADOS_BASICOS_DO_LIVRO$DOI
		  pub$NATUREZA  <- pubB$DADOS_BASICOS_DO_LIVRO$NATUREZA
		  pub$PAIS_DE_PUBLICACAO   <- pubB$DADOS_BASICOS_DO_LIVRO$PAIS_DE_PUBLICACAO
		  pub$TIPO  <- pubB$DADOS_BASICOS_DO_LIVRO$TIPO
		  pub$TITULO_DO_LIVRO  <- pubB$DADOS_BASICOS_DO_LIVRO$TITULO_DO_LIVRO

		  pub$ISBN  <- pubB$DETALHAMENTO_DO_LIVRO$ISBN
		  pub$authors <- x
		  pub$NOME_DA_EDITORA <- pubB$DETALHAMENTO_DO_LIVRO$NOME_DA_EDITORA
		  pub$NUMERO_DA_EDICAO_REVISAO  <-pubB$DETALHAMENTO_DO_LIVRO$NUMERO_DA_EDICAO_REVISAO
		  pub$NUMERO_DE_PAGINAS <- pubB$DETALHAMENTO_DO_LIVRO$NUMERO_DE_PAGINAS
                  pub$NUMERO_DE_VOLUMES<- pubB$DETALHAMENTO_DO_LIVRO$NUMERO_DE_VOLUMES
		  pub$authorsEndogenous <- y



       } else if (tipo == 'DEMAIS_TIPOS_DE_PRODUCAO_BIBLIOGRAFICA'){

		  pub$ANO  <- pubB$DADOS_BASICOS_DE_OUTRA_PRODUCAO$ANO
		  pub$DOI  <- pubB$DADOS_BASICOS_DE_OUTRA_PRODUCAO$DOI
		  pub$NATUREZA  <- pubB$DADOS_BASICOS_DE_OUTRA_PRODUCAO$NATUREZA
		  pub$PAIS_DE_PUBLICACAO   <- pubB$DADOS_BASICOS_DE_OUTRA_PRODUCAO$PAIS_DE_PUBLICACAO

		  pub$TITULO  <- pubB$DADOS_BASICOS_DE_OUTRA_PRODUCAO$TITULO


		  pub$authors <- x
		  pub$EDITORA <- pubB$DETALHAMENTO_DE_OUTRA_PRODUCAO$EDITORA
		  pub$NUMERO_DE_PAGINAS <- pubB$DETALHAMENTO_DE_OUTRA_PRODUCAO$NUMERO_DE_PAGINAS
                  pub$CIDADE_DA_EDITORA<- pubB$DETALHAMENTO_DE_OUTRA_PRODUCAO$CIDADE_DA_EDITORA
		  pub$authorsEndogenous <- y


       } else if (tipo == 'CAPITULO_DE_LIVRO'){

		  pub$ANO  <- pubB$DADOS_BASICOS_DO_CAPITULO$ANO
		  pub$DOI  <- pubB$DADOS_BASICOS_DO_CAPITULO$DOI
		  pub$PAIS_DE_PUBLICACAO   <- pubB$DADOS_BASICOS_DO_CAPITULO$PAIS_DE_PUBLICACAO
		  pub$TIPO  <- pubB$DADOS_BASICOS_DO_CAPITULO$TIPO
		  pub$TITULO_DO_CAPITULO_DO_LIVRO  <- pubB$DADOS_BASICOS_DO_CAPITULO$TITULO_DO_CAPITULO_DO_LIVRO

		  pub$ISBN  <- pubB$DETALHAMENTO_DO_CAPITULO$ISBN
		  pub$authors <- x
		  pub$NOME_DA_EDITORA <- pubB$DETALHAMENTO_DO_CAPITULO$NOME_DA_EDITORA
		  pub$NUMERO_DA_EDICAO_REVISAO  <-pubB$DETALHAMENTO_DO_CAPITULO$NUMERO_DA_EDICAO_REVISAO
 	          pub$ORGANIZADORES  <-pubB$DETALHAMENTO_DO_CAPITULO$ORGANIZADORES
		  pub$PAGINA_FINAL <- pubB$DETALHAMENTO_DO_CAPITULO$PAGINA_FINAL
 		  pub$PAGINA_INICIAL <- pubB$DETALHAMENTO_DO_CAPITULO$PAGINA_INICIAL
                  pub$TITULO_DO_LIVRO<- pubB$DETALHAMENTO_DO_CAPITULO$TITULO_DO_LIVRO
		  pub$authorsEndogenous <- y



       }else if (tipo == 'ARTIGO_ACEITO'){

		  pub$TITULO_DO_ARTIGO <- pubB$DADOS_BASICOS_DO_ARTIGO$TITULO_DO_ARTIGO
		  pub$ANO_DO_ARTIGO <- pubB$DADOS_BASICOS_DO_ARTIGO$ANO_DO_ARTIGO
		  pub$TITULO_DO_PERIODICO_OU_REVISTA<- pubB$DETALHAMENTO_DO_ARTIGO$TITULO_DO_PERIODICO_OU_REVISTA
		  pub$VOLUME <- pubB$DETALHAMENTO_DO_ARTIGO$VOLUME
		  pub$ISSN <- pubB$DETALHAMENTO_DO_ARTIGO$ISSN
		  pub$DOI <- pubB$DADOS_BASICOS_DO_ARTIGO$DOI
		  pub$authors <- x
		  pub$PAGINA_INICIAL <- pubB$DETALHAMENTO_DO_ARTIGO$PAGINA_INICIAL
		  pub$PAGINA_FINAL <-pubB$DETALHAMENTO_DO_ARTIGO$PAGINA_FINAL
		  pub$NATUREZA <- pubB$DADOS_BASICOS_DO_ARTIGO$NATUREZA
		  pub$authorsEndogenous <- y



       } else if(tipo == 'TEXTO_EM_JORNAIS'){


		  pub$NATUREZA <- pubB$DADOS_BASICOS_DO_TEXTO$NATUREZA
		  pub$TITULO_DO_TEXTO <- pubB$DADOS_BASICOS_DO_TEXTO$TITULO_DO_TEXTO
		  pub$ANO_DO_TEXTO <- pubB$DADOS_BASICOS_DO_TEXTO$ANO_DO_TEXTO
      		  pub$PAIS_DE_PUBLICACAO<-pubB$DADOS_BASICOS_DO_TEXTO$PAIS_DE_PUBLICACAO
		  pub$DOI <- pubB$DADOS_BASICOS_DO_TEXTO$DOI


		  pub$TITULO_DO_JORNAL_OU_REVISTA<- pubB$DETALHAMENTO_DO_TEXTO$TITULO_DO_JORNAL_OU_REVISTA
		  pub$DATA_DE_PUBLICACAO <- pubB$DETALHAMENTO_DO_TEXTO$DATA_DE_PUBLICACAO
		  pub$ISSN <- pubB$DETALHAMENTO_DO_TEXTO$ISSN

		  pub$authors <- x
		  pub$PAGINA_INICIAL <- pubB$DETALHAMENTO_DO_TEXTO$PAGINA_INICIAL
		  pub$PAGINA_FINAL <-pubB$DETALHAMENTO_DO_TEXTO$PAGINA_FINAL
		  pub$authorsEndogenous <- y


	} else if(tipo == 'ORIENTACAO_EM_ANDAMENTO_MESTRADO' ||
                  tipo == 'ORIENTACAO_EM_ANDAMENTO_DOUTORADO' ||
		  tipo == 'ORIENTACAO_EM_ANDAMENTO_DE_POS_DOUTORADO'||
		  tipo == 'ORIENTACAO_EM_ANDAMENTO_GRADUACAO' ||
		  tipo == 'ORIENTACAO_EM_ANDAMENTO_INICIACAO_CIENTIFICA'||
		  tipo == 'ORIENTACAO_CONCLUIDA_MESTRADO' ||
		  tipo == 'ORIENTACAO_CONCLUIDA_DOUTORADO' ||
		  tipo == 'OUTRAS_ORIENTACOES_CONCLUIDAS' ||
		  tipo == 'ORIENTACAO_CONCLUIDA_POS_DOUTORADO'){



		pub$NOME_DO_ORIENTADO <-  pubB$DETALHAMENTO$NOME_DO_ORIENTADO
		pub$NOME_INSTITUICAO <- pubB$DETALHAMENTO$NOME_INSTITUICAO
		pub$FLAG_BOLSA  <- pubB$DETALHAMENTO$FLAG_BOLSA
		pub$NUMERO_ID_ORIENTADO  <- pubB$DETALHAMENTO$NUMERO_ID_ORIENTADO
		pub$NOME_CURSO  <- pubB$DETALHAMENTO$NOME_CURSO
		pub$CODIGO_AGENCIA_FINANCIADORA  <- pubB$DETALHAMENTO$CODIGO_AGENCIA_FINANCIADORA
		pub$NOME_DA_AGENCIA  <- pubB$DETALHAMENTO$NOME_DA_AGENCIA
		pub$CODIGO_CURSO  <- pubB$DETALHAMENTO$CODIGO_CURSO
 		pub$TIPO_DE_ORIENTACAO  <- pubB$DETALHAMENTO$TIPO_DE_ORIENTACAO
		pub$CODIGO_INSTITUICAO  <- pubB$DETALHAMENTO$CODIGO_INSTITUICAO

		pub$ANO  <- pubB$DADOS_BASICOS$ANO
 		pub$NATUREZA  <- pubB$DADOS_BASICOS$NATUREZA
		pub$TITULO_DO_TRABALHO  <- pubB$DADOS_BASICOS$TITULO_DO_TRABALHO
		pub$PAIS  <- pubB$DADOS_BASICOS$PAIS
		pub$ANO  <- pubB$DADOS_BASICOS$ANO

	pub$authorsEndogenous <- y
        pub$orientadores <-orientadores

	}

  pub
}



