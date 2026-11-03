# Transformation in a LifemapR format of NCBI information for 1000 eukaryotes

A dataset containing NCBI information of 1000 eukaryotes. The variables
are as follows:

## Usage

``` r
data(LM_eukaryotes)
```

## Format

A lifemap object - a list containing the basemap used to fetch data and
df, a data frame with 2760 rows and 26 variables:

- X.Organism.Name:

  Organism name at the species level

- taxid:

  NCBI taxid

- BioProject.Accession:

  BioProject Accession number (from BioProject database)

- BioProject.ID:

  BioProject ID

- Group:

  Commonly used organism groups: Animals, Fungi, Plants, Protists

- SubGroup:

  NCBI Taxonomy level below group: Mammals, Birds, Fishes, Flatworms,
  Insects, Amphibians, Reptiles, Roundworms, Ascomycetes,
  Basidiomycetes, Land Plants, Green Algae, Apicomplexans, Kinetoplasts

- Size..Mb.:

  Total length of DNA submitted for the project

- GC.:

  Percent of nitrogenous bases (guanine or cytosine) in DNA submitted
  for the project

- Assembly.Accession:

  Name of the genome assembly (from NCBI Assembly database)

- Replicons:

  Number of replicons in the assembly

- WGS:

  Four-letter Accession prefix followed by version as defined in WGS
  division of GenBank/INSDC

- Scaffolds:

  Number of scaffolds in the assembly

- Genes:

  Number of Genes annotated in the assembly

- Proteins:

  Number of Proteins annotated in the assembly

- Release.Date:

  First public sequence release for the project

- Modify.Date:

  Sequence modification date for the project

- Status:

  Highest level of assembly:  
  Chromosomes: one or more chromosomes are assembled  
  Scaffolds or contigs: sequence assembled but no chromosomes

- Center:

  Origin of the sample

- BioSample.Accession:

  BioSample Accession number

- lon:

  longitude of taxids on a specific basemap

- lat:

  latitude of taxids on a specific basemap

- sci_name:

  scientific name of taxids

- zoom:

  zoom of taxids on a specific basemap

- ascend:

  the list of all ancestors of taxids on a specific basemap

- type:

  either "requested" if the taxid was given, "ancestor" if gotten from
  the database

- ancestor:

  the direct ancestor oftaxids on a specific basemap
