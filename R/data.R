#' Metadata of Serratia Genomes
#'
#' A dataset containing metadata for Serratia genomes, including taxonomy levels
#' and relative abundances.
#'
#' @format A data frame with 3863 rows and 20 variables:
#' \describe{
#'   \item{id}{Genome ID}
#'   \item{query_signature}{Query signature hash}
#'   \item{best_hit_id}{Identifier of the best hit genome}
#'   \item{best_hit_signature}{Signature of the best hit}
#'   \item{similarity_score}{ANI/similarity score to best hit}
#'   \item{levels_reached}{Deepest taxonomic level resolved}
#'   \item{final_code}{GTDB code}
#'   \item{best_hit_code}{GTDB code of best hit}
#'   \item{L_0}{Taxonomic rank level 0 (e.g. Kingdom)}
#'   \item{L_1}{Taxonomic rank level 1 (e.g. Phylum)}
#'   \item{L_2}{Taxonomic rank level 2 (e.g. Class)}
#'   \item{L_3}{Taxonomic rank level 3 (e.g. Order)}
#'   \item{L_4}{Taxonomic rank level 4 (e.g. Family)}
#'   \item{L_5}{Taxonomic rank level 5 (e.g. Genus)}
#'   \item{Assigned_Species}{Assigned species name}
#'   \item{Relative_Abundance....}{Relative abundance percentage of the genome}
#'   \item{ANI....}{ANI similarity percentage}
#'   \item{gtdb_species}{GTDB assigned species name}
#'   \item{Colection}{Collection category}
#'   \item{Assigned_Species2}{Secondary species assignment}
#' }
"metadata"

#' Antimicrobial Resistance (AMR) Genes
#'
#' A dataset containing AMR genes detected in various genomes.
#'
#' @format A data frame with 62791 rows and 34 variables:
#' \describe{
#'   \item{Name}{Genome ID matching metadata id}
#'   \item{Protein.identifier}{NCBI protein accessions}
#'   \item{Contig.id}{Contig identifier where the gene is located}
#'   \item{Start}{Start position in contig}
#'   \item{Stop}{Stop position in contig}
#'   \item{Strand}{Strand direction (+ or -)}
#'   \item{Gene.symbol}{Symbol of the AMR gene (e.g. sdeA)}
#'   \item{Sequence.name}{Detailed product sequence name}
#'   \item{Scope}{Scope of resistance}
#'   \item{Element.type}{Element type (e.g. AMR, STRESS)}
#'   \item{Element.subtype}{Element subtype (e.g. BIOCIDE)}
#'   \item{Class}{Drug resistance class}
#'   \item{Subclass}{Drug resistance subclass}
#'   \item{Method}{Method of detection}
#'   \item{Target.length}{Target reference sequence length}
#'   \item{Reference.sequence.length}{Length of reference sequence}
#'   \item{X..Coverage.of.reference.sequence}{Percentage coverage}
#'   \item{X..Identity.to.reference.sequence}{Percentage identity}
#'   \item{Alignment.length}{Length of alignment}
#'   \item{Accession.of.closest.sequence}{Accession number of closest sequence}
#'   \item{Name.of.closest.sequence}{Name of closest sequence}
#'   \item{HMM.id}{HMM identifier}
#'   \item{HMM.description}{HMM model description}
#'   \item{Hierarchy.node}{AMR ontology node}
#'   \item{Protein.id}{Protein identifier}
#'   \item{Element.symbol}{Element symbol}
#'   \item{Element.name}{Element name}
#'   \item{Type}{Type classification}
#'   \item{Subtype}{Subtype classification}
#'   \item{X..Coverage.of.reference}{Percentage coverage fallback}
#'   \item{X..Identity.to.reference}{Percentage identity fallback}
#'   \item{Closest.reference.accession}{Closest reference accession}
#'   \item{Closest.reference.name}{Closest reference name}
#'   \item{HMM.accession}{HMM accession}
#' }
"amr"
