#' @title csmbox

#' Display expression of a selected gene
#' @usage library(hscsm)
#' @usage data(csm)
#' @usage csmbox(csm,gene="RUNX2")
#' @examples library(hscsm)
#' @examples data(csm)
#' @examples csmbox(csm,gene="RUNX2")


csmbox<-function(csm,gene="RUNX2"){

suppressWarnings({
	## install survival needed package if necessary

	if(!require(dplyr)){
    		install.packages("dplyr")
    		library(dplyr)}
	if(!require(ggplot2)){
		install.packages("ggplot2")
    		library(ggplot2)}
	if(!require(tibble)){
		install.packages("tibble")
    		library(tibble)}

	if(gene %in% colnames(csm))
		{
	csm%>%select(pheno,group,gene,culture)->res
	res<-as_tibble(res)
	res$gene<-names(res)[3]
	colnames(res)<-c("pheno","group","count","culture","gene")

	res
	ggplot(res,mapping=aes(x=count,y=group,fill=pheno))+geom_boxplot(alpha=0.5)+
	xlab(gene)+ylab("Human Tissues")+
	geom_jitter(aes(colour = culture),size=2)+
	theme_minimal()+
	theme(text = element_text(size = 16))

		}else {
			print("This gene is absent of the dataset")
		}


	})
}


