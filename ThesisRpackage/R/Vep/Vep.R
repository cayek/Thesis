#' @export
variant_effect_predictor <- function(vep.input,
                                     species = "homo_sapiens",
                                     vep.script = "~/BiocompSoftware/ensembl-tools-release-84/scripts/variant_effect_predictor/variant_effect_predictor.pl",
                                     vep.cache = "~/.vep/") {
  input <- tempfile()
  write.table(vep.input, file = input, row.names = FALSE, col.names = FALSE, na = "", quote = FALSE)
  output <- tempfile()
  cmd <- paste0("perl ", vep.script, " -i ", input,
                " -o ", output, " --cache ",
                " --dir ", vep.cache,
                " --species ", species)
  system(cmd)
  res.df <- readr::read_delim(output, comment = "##", na = "-", delim = "\t",
                              col_types = cols(
                                `#Uploaded_variation` = col_character(),
                                Location = col_character(),
                                Allele = col_character(),
                                Gene = col_character(),
                                Feature = col_character(),
                                Feature_type = col_character(),
                                Consequence = col_character(),
                                cDNA_position = col_integer(),
                                CDS_position = col_integer(),
                                Protein_position = col_integer(),
                                Amino_acids = col_character(),
                                Codons = col_character(),
                                Existing_variation = col_character(),
                                Extra = col_character()
                              ))

  ## adding locus name column
  aux <- vep.input %>%
    dplyr::transmute(identifier,
                     Location = paste0(chromosome,":", start))
  res <- dplyr::inner_join(res.df, aux)
  res
}
