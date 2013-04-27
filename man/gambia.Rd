\name{gambia}
\alias{gambia}
\docType{data}
\title{
  Results of GO enrichment analysis in TB
}
\description{
  A data.frame object containing the results of a GO enrichment analysis
  from the GOstats package.

}
\usage{data(gambia)}
\format{
  A data frame with 318 observations on the following 9 variables.
  \describe{
          GOBPID       Pvalue OddsRatio  ExpCount Count Size
          Term

    \item{\code{GOBPID}}{Gene Ontology (GO) biological process (BP) identifier}
    \item{\code{Pvalue}}{P value from enrichment test}
    \item{\code{OddsRatio}}{Measure of enrichment}
    \item{\code{ExpCount}}{expected number of genes in the enriched partition which map to this GO term}
    \item{\code{Count}}{number of genes in the enriched partition which map to this GO term}
    \item{\code{Size}}{number of genes within this GO Term}
    \item{\code{Term}}{Gene Ontology term description}
  }
}
\details{
  The data results from a microarray analysis of the whole blood
  transcriptome of tuberculosis (TB) patients compared to healthy
  individuals. Genes were sorted by their p-value and analysed using
  the GOstats package.

  Significantly enriched GO terms are included in this data frame.
}
\source{
  Maertzdorf J., Ota M., Repsilber D., Mollenkopf H.J., Weiner J., et al. (2011)
  Functional Correlations of Pathogenesis-Driven Gene Expression Signatures
  in Tuberculosis. PLoS ONE 6(10): e26938. doi:10.1371/journal.pone.0026938
}
\examples{
data(gambia)
tagcloud( gambia$Term, -log( gambia$Pvalue ) )
}
\keyword{datasets}
\keyword{tagcloud}
