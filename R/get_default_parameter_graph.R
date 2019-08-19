#' @title GetDefualtParameterGraph
#'
#' 
#'
#' 
#'
#' 
#'
#'
#'
#' @export GetDefualtParameterGraph
GetDefualtParameterGraph <- function(){

default <- hash::hash()

	#grafo de coautoria
 hash::.set( default,  "incluir_artigo_em_periodico", "sim" )
 hash::.set( default, "incluir_livro_publicado", "sim" )
 hash::.set( default, "incluir_capitulo_de_livro_publicado", "sim" )
 hash::.set( default, "incluir_texto_em_jornal_de_noticia", "sim" )
 hash::.set( default, "incluir_trabalho_em_congresso", "sim" )	
 hash::.set( default, "incluir_artigo_aceito_para_publicacao", "sim" )
 hash::.set( default, "incluir_outro_tipo_de_producao_bibliografica", "sim" )

return(default)
}
