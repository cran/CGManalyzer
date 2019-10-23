
setSPEC.fn = function(SPEC.name) {
  #****************************************************************************************************
  # Function to load settings for the selected SPEC parameter
  # SPEC.name: the SPEC name to load, which is a R script name.
  #            one of "SPEC.FreestyleLibre.R", "SPEC.Glutalor.R" and "SPEC.Medtronic.R".
  # Author: Xiaohua Douglas Zhang
  # Date:   2019-10-08
  #*****************************************************************************************************
  source( system.file("SPEC", SPEC.name, package = "CGManalyzer") )
}

