#' NCBI information for 1000 eukaryotes
#'
#' A dataset containing NCBI information of 1000 eukaryotes. The variables are as follows:
#'
#' @format A data frame with 1000 rows and 19 variables:
#' \describe{
#'   \item{X.Organism.Name}{Organism name at the species level}
#'   \item{taxid}{NCBI taxid}
#'   \item{BioProject.Accession}{BioProject Accession number (from BioProject database)}
#'   \item{BioProject.ID}{BioProject ID}
#'   \item{Group}{Commonly used organism groups:  Animals, Fungi, Plants, Protists}
#'   \item{SubGroup}{NCBI Taxonomy level below group: Mammals, Birds, Fishes, Flatworms, Insects, Amphibians, Reptiles, Roundworms, Ascomycetes, Basidiomycetes, Land Plants, Green Algae, Apicomplexans, Kinetoplasts}
#'   \item{Size..Mb.}{Total length of DNA submitted for the project}
#'   \item{GC.}{Percent of nitrogenous bases (guanine or cytosine) in DNA submitted for the project}
#'   \item{Assembly.Accession}{Name of the genome assembly (from NCBI Assembly database)}
#'   \item{Replicons}{Number of replicons in the assembly}
#'   \item{WGS}{Four-letter Accession prefix followed by version as defined in WGS division of GenBank/INSDC}
#'   \item{Scaffolds}{Number of scaffolds in the assembly}
#'   \item{Genes}{Number of Genes annotated in the assembly}
#'   \item{Proteins}{Number of Proteins annotated in the assembly}
#'   \item{Release.Date}{First public sequence release for the project}
#'   \item{Modify.Date}{Sequence modification date for the project}
#'   \item{Status}{Highest level of assembly: <br> Chromosomes: one or more chromosomes are assembled<br> Scaffolds or contigs: sequence assembled but no chromosomes}
#'   \item{Center}{Origin of the sample}
#'   \item{BioSample.Accession}{BioSample Accession number}
#' }
#' @usage data(eukaryotes_1000)
"eukaryotes_1000"

#' NCBI information for 80 eukaryotes
#'
#' A dataset containing NCBI information of 80 eukaryotes. The variables are as follows:
#'
#' @format A data frame with 80 rows and 19 variables:
#' \describe{
#'   \item{X.Organism.Name}{Organism name at the species level}
#'   \item{taxid}{NCBI taxid}
#'   \item{BioProject.Accession}{BioProject Accession number (from BioProject database)}
#'   \item{BioProject.ID}{BioProject ID}
#'   \item{Group}{Commonly used organism groups:  Animals, Fungi, Plants, Protists}
#'   \item{SubGroup}{NCBI Taxonomy level below group: Mammals, Birds, Fishes, Flatworms, Insects, Amphibians, Reptiles, Roundworms, Ascomycetes, Basidiomycetes, Land Plants, Green Algae, Apicomplexans, Kinetoplasts}
#'   \item{Size..Mb.}{Total length of DNA submitted for the project}
#'   \item{GC.}{Percent of nitrogenous bases (guanine or cytosine) in DNA submitted for the project}
#'   \item{Assembly.Accession}{Name of the genome assembly (from NCBI Assembly database)}
#'   \item{Replicons}{Number of replicons in the assembly}
#'   \item{WGS}{Four-letter Accession prefix followed by version as defined in WGS division of GenBank/INSDC}
#'   \item{Scaffolds}{Number of scaffolds in the assembly}
#'   \item{Genes}{Number of Genes annotated in the assembly}
#'   \item{Proteins}{Number of Proteins annotated in the assembly}
#'   \item{Release.Date}{First public sequence release for the project}
#'   \item{Modify.Date}{Sequence modification date for the project}
#'   \item{Status}{Highest level of assembly: <br> Chromosomes: one or more chromosomes are assembled<br> Scaffolds or contigs: sequence assembled but no chromosomes}
#'   \item{Center}{Origin of the sample}
#'   \item{BioSample.Accession}{BioSample Accession number}
#' }
#' @usage data(eukaryotes_80)
"eukaryotes_80"

#' Transformation in a LifemapR format of NCBI information for 1000 eukaryotes
#'
#' A dataset containing NCBI information of 1000 eukaryotes. The variables are as follows:
#'
#' @format A lifemap object - a list containing the basemap used to fetch data and df, a data frame with 2760 rows and 26 variables:
#' \describe{
#'   \item{X.Organism.Name}{Organism name at the species level}
#'   \item{taxid}{NCBI taxid}
#'   \item{BioProject.Accession}{BioProject Accession number (from BioProject database)}
#'   \item{BioProject.ID}{BioProject ID}
#'   \item{Group}{Commonly used organism groups:  Animals, Fungi, Plants, Protists}
#'   \item{SubGroup}{NCBI Taxonomy level below group: Mammals, Birds, Fishes, Flatworms, Insects, Amphibians, Reptiles, Roundworms, Ascomycetes, Basidiomycetes, Land Plants, Green Algae, Apicomplexans, Kinetoplasts}
#'   \item{Size..Mb.}{Total length of DNA submitted for the project}
#'   \item{GC.}{Percent of nitrogenous bases (guanine or cytosine) in DNA submitted for the project}
#'   \item{Assembly.Accession}{Name of the genome assembly (from NCBI Assembly database)}
#'   \item{Replicons}{Number of replicons in the assembly}
#'   \item{WGS}{Four-letter Accession prefix followed by version as defined in WGS division of GenBank/INSDC}
#'   \item{Scaffolds}{Number of scaffolds in the assembly}
#'   \item{Genes}{Number of Genes annotated in the assembly}
#'   \item{Proteins}{Number of Proteins annotated in the assembly}
#'   \item{Release.Date}{First public sequence release for the project}
#'   \item{Modify.Date}{Sequence modification date for the project}
#'   \item{Status}{Highest level of assembly: <br> Chromosomes: one or more chromosomes are assembled<br> Scaffolds or contigs: sequence assembled but no chromosomes}
#'   \item{Center}{Origin of the sample}
#'   \item{BioSample.Accession}{BioSample Accession number}
#'   \item{lon}{longitude of taxids on a specific basemap}
#'   \item{lat}{latitude of taxids on a specific basemap}
#'   \item{sci_name}{scientific name of taxids}
#'   \item{zoom}{zoom of taxids on a specific basemap}
#'   \item{ascend}{the list of all ancestors of taxids on a specific basemap}
#'   \item{type}{either "requested" if the taxid was given, "ancestor" if gotten from the database}
#'   \item{ancestor}{the direct ancestor oftaxids on a specific basemap}
#' }
#' @usage data(LM_eukaryotes)
"LM_eukaryotes"

#' Kraken results
#'
#' A dataset containing NCBI information of 1000 eukaryotes. The variables are as follows:
#'
#' @format A data frame with 4427 rows and 6 variables:
#' \describe{
#'   \item{coverage_percent}{Percentage of fragments covered by the clade rooted at this taxon}
#'   \item{coverage_number}{Number of fragments covered by the clade rooted at this taxon}
#'   \item{fragment_number}{Number of fragments assigned directly to this taxon}
#'   \item{rank}{A rank code, indicating (U)nclassified, (R)oot, (D)omain, (K)ingdom, (P)hylum, (C)lass, (O)rder, (F)amily, (G)enus, or (S)pecies. Taxa that are not at any of these 10 ranks have a rank code that is formed by using the rank code of the closest ancestor rank with a number indicating the distance from that rank. E.g., "G2" is a rank code indicating a taxon is between genus and species and the grandparent taxon is at the genus rank.}
#'   \item{taxid}{NCBI taxonomic ID number}
#'   \item{name}{Indented scientific name}
#' }
#' @usage data(kraken_res)
"kraken_res"


#' Genomic results
#'
#' A dataset containing information on Genome size and TE content for 808 taxids
#'
#' @format A data frame with 808 rows and 3 variables:
#' \describe{
#'   \item{taxid}{NCBI taxid}
#'   \item{Genome_size}{the Genome size in pb}
#'   \item{TEcontent_bp}{the transposable element content in pb}
#' }
#' @usage data(gen_res)
"gen_res"
