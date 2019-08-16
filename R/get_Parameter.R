#' @title GetParameter
#'
#'
#'
#' @param path a list of Lattes CV XML files
#'
#'
#'
#'
#'
#' @export GetParameter


GetParameter <-function(path){

	parametrosGerais<-jsonlite::fromJSON(path, simplifyVector = FALSE)
	arquivo_de_entrada<-parametrosGerais$global$arquivo_de_entrada
	parametrosEspecificos<-jsonlite::fromJSON(arquivo_de_entrada, simplifyVector = FALSE)


 	 parameters<-new.env(parent=emptyenv())
	 parameters$gerais<-parametrosGerais
	 parameters$especificos<-parametrosEspecificos
 	 parameters
}
