#' Map Free-Text Condition Names to Standard Portal Terminology
#'
#' Newborn screening lab results often come back with inconsistent
#' free-text condition names (different labs/systems abbreviate things
#' differently). This function maps those messy strings to the exact,
#' standardized condition names required by the NewSTEPs case-reporting
#' portal, using pattern matching on common abbreviations and synonyms.
#'
#' This is a portfolio-demo rebuild of a real condition-mapping
#' function - the mapping logic itself is unchanged (these are public
#' RUSP - Recommended Uniform Screening Panel - disorder names, not
#' any patient or case-specific data), but the surrounding pipeline
#' (file paths, real case data) has been stripped out.
#'
#' @param condition A character vector of free-text condition/result
#'   strings (e.g., from a lab results system).
#' @param source A character vector the same length as `condition`,
#'   flagging which data source each result came from. Use "SCD" for
#'   sickle cell disease results (which route through a different
#'   phenotype-based mapping); any other value falls through to the
#'   general string-matching logic.
#' @param phenotype Optional character vector of SCD phenotype codes
#'   (e.g., "FS", "FC", "FE") used only when `source == "SCD"`.
#'
#' @return A character vector of standardized portal condition names.
#'   Any input that doesn't match a known pattern is returned prefixed
#'   with "REVIEW: " so it can be caught and manually checked before
#'   any real-world submission.
#'
#' @examples
#' map_portal_condition(
#'   condition = c("MCADD", "Maple Syrup Urine Disease", "Some Weird Result"),
#'   source = c("General Abnormal", "General Abnormal", "General Abnormal")
#' )
#'
#' @export
map_portal_condition <- function(condition, source, phenotype = NA_character_) {
  # toupper() + trimws() normalize case and stray whitespace before
  # pattern matching, so "mcadd", "MCADD ", and "MCADD" all match
  # the same way
  cu <- toupper(trimws(condition))

  dplyr::case_when(
    # -- Sickle cell disease: mapped from phenotype code, not free text --
    source == "SCD" & phenotype %in% c("FS", "FE", "FC") ~ "Presence of Hb S",
    source == "SCD" ~ "Presence of Other Hb Variant",

    # -- Congenital infection --
    stringr::str_detect(cu, "CCMV|\\bCMV\\b") ~ "Cytomegalovirus - CMV",

    # -- Endocrine disorders --
    stringr::str_detect(cu, "\\bCAH\\b|ADRENAL|SALT WASTING|SIMPLE VIRILIZING") ~
      "Congenital adrenal hyperplasia - CAH",
    stringr::str_detect(cu, "HYPOTHYROID|HYPERTHYROID|GRAVES|\\bTHOP\\b|PANHYPOPITUITAR") ~
      "Congenital hypothyroidism - CH",

    # -- Lysosomal storage disorders --
    stringr::str_detect(cu, "\\bMPS.?I\\b(?!I)|MUCOPOLYSACCHARIDOSIS I(?!I)") ~
      "Mucopolysaccharidosis I - MPS I",
    stringr::str_detect(cu, "\\bMPS.?II\\b|MUCOPOLYSACCHARIDOSIS II") ~
      "Mucopolysaccharidosis II - MPS II",
    stringr::str_detect(cu, "POMPE") ~ "Pompe",

    # -- Other RUSP-core conditions --
    stringr::str_detect(cu, "BIOTINIDASE|\\bBIOT\\b") ~ "Biotinidase deficiency - BIOT",
    stringr::str_detect(cu, "CYSTIC FIBROSIS|\\bCRMS\\b") ~ "Cystic fibrosis - CF",
    stringr::str_detect(cu, "(?<![A-Z])\\bCF\\b(?![A-Z])") ~ "Cystic fibrosis - CF",
    stringr::str_detect(cu, "GALACTOSEMIA|\\bGALT\\b") ~ "Classic galactosemia - GALT",
    stringr::str_detect(cu, "\\bSCID\\b|SEVERE COMBINED|THYMIC APLASIA|22Q") ~
      "Severe Combined Immunodeficiencies - SCID",
    stringr::str_detect(cu, "ADRENOLEUKODYSTROPHY|\\bXALD\\b|ZELLWEGER") ~
      "X-linked Adrenoleukodystrophy",
    stringr::str_detect(cu, "\\bSMA\\b|SPINAL MUSCULAR") ~ "Spinal Muscular Atrophy - SMA",
    stringr::str_detect(cu, "KRABBE") ~ "Krabbe Disease",

    # -- Amino acid disorders --
    stringr::str_detect(cu, "\\bPKU\\b|PHENYLKETONURIA|HYPERPHE") ~ "Classic PKU & Hyperphe",
    stringr::str_detect(cu, "\\bMSUD\\b|MAPLE SYRUP") ~ "Maple syrup urine disease - MSUD",
    stringr::str_detect(cu, "HOMOCYSTINURIA|\\bHCY\\b") ~ "Homocystinuria - HCY",
    stringr::str_detect(cu, "\\bOTC\\b|ORNITHINE TRANSCARBAMYLASE") ~
      "Ornithine transcarbamylase deficiency - OTC",
    stringr::str_detect(cu, "TYROSINEMIA|\\bTYR\\b") ~ "Tyrosinemia, type I - TYR I",

    # -- Organic acid disorders --
    stringr::str_detect(cu, "PROPIONIC|\\bPROP\\b") ~ "Propionic acidemia - PROP",
    stringr::str_detect(cu, "METHYLMALONIC|\\bMUT\\b|\\bMMA\\b") ~
      "Methylmalonic acidemia (methylmalonyl-CoA mutase) - MUT",
    stringr::str_detect(cu, "ISOVALERIC|\\bIVA\\b") ~ "Isovaleric acidemia - IVA",
    stringr::str_detect(cu, "GLUTARIC ACIDEMIA I(?!I)|\\bGA.?I\\b(?!I)") ~
      "Glutaric acidemia type I - GA1",

    # -- Fatty acid oxidation disorders --
    stringr::str_detect(cu, "\\bMCAD\\b|\\bMCADD\\b") ~
      "Medium-chain acyl-CoA dehydrogenase deficiency - MCAD",
    stringr::str_detect(cu, "\\bVLCAD\\b|\\bVLCADD\\b") ~
      "Very long-chain acyl-CoA dehydrogenase deficiency - VLCAD",
    stringr::str_detect(cu, "\\bLCHAD\\b") ~
      "Long-chain L-3 hydroxyacyl-CoA dehydrogenase deficiency - LCHAD",

    # -- Anything that doesn't match a known pattern gets flagged --
    # rather than silently mis-categorized, so a human reviews it
    TRUE ~ paste0("REVIEW: ", condition)
  )
}


#' Map SendSS-Style Codes to Portal Demographic Values
#'
#' Three small lookup functions that translate short demographic codes
#' (as commonly stored in state surveillance systems) into the exact
#' string values a reporting portal expects. Genericized from the same
#' real reporting pipeline as [map_portal_condition()] - contains only
#' generic demographic coding conventions, no case data.
#'
#' @param x A character vector of raw codes (e.g., "M", "F", "B", "W").
#'
#' @return A character vector of standardized portal values.
#'
#' @name map_demographics
NULL

#' @rdname map_demographics
#' @export
map_gender <- function(x) {
  dplyr::case_when(
    toupper(trimws(x)) %in% c("F", "FEMALE") ~ "FEMALE",
    toupper(trimws(x)) %in% c("M", "MALE") ~ "MALE",
    toupper(trimws(x)) %in% c("U", "UNKNOWN") ~ "UNKNOWN",
    TRUE ~ "UNSPECIFIED"
  )
}

#' @rdname map_demographics
#' @export
map_ethnicity <- function(x) {
  dplyr::case_when(
    toupper(trimws(x)) %in% c("YES", "HISPANIC") ~ "HISPANIC_LATINO_OR_SPANISH",
    toupper(trimws(x)) %in% c("NO", "NOT HISPANIC") ~ "NOT_HISPANIC_LATINO_OR_SPANISH",
    toupper(trimws(x)) %in% c("U", "UNKNOWN") ~ "UNKNOWN",
    TRUE ~ "NOT_REPORTED"
  )
}

#' @rdname map_demographics
#' @export
map_race <- function(x) {
  dplyr::case_when(
    toupper(trimws(x)) %in% c("B", "BLACK") ~ "BLACK_OR_AFRICAN_AMERICAN",
    toupper(trimws(x)) %in% c("W", "WHITE") ~ "WHITE",
    toupper(trimws(x)) %in% c("A", "ASIAN") ~ "ASIAN",
    toupper(trimws(x)) %in% c("I", "AMERICAN INDIAN", "NATIVE AMERICAN") ~ "NATIVE_AMERICAN",
    toupper(trimws(x)) %in% c("P", "PACIFIC ISLANDER", "ISLANDER") ~ "ISLANDER",
    toupper(trimws(x)) %in% c("U", "UNKNOWN") ~ "UNKNOWN",
    TRUE ~ "NOT_REPORTED"
  )
}
