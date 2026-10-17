# Non-interactive CER analysis example

# EXAMPLE 1 #########################################################
design <- SetupDesign_CER(
  nArms = 3,
  nEps = 1,
  SampleSize = 300,
  EpType = list( EP1 = "Continuous" ),
  sigma = list( EP1 = c( 1, 1, 1 ) ),
  CommonStdDev = TRUE,
  prop.ctr = list( EP1 = NA ),
  allocRatio = c( 1, 1, 1 ),
  WI = c( .5, .5 ),
  G = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
  test.type = "Parametric",
  plotGraphs = TRUE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLook_CER(
  design = design,
  p_raw = c( H1 = .1, H2 = .2 )
)

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLook_CER(
  design = design,
  state = look1_analysis,
  p_raw = c( H1 = .0001, H2 = .2 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 2 #########################################################
design <- SetupDesign_CER(
  nArms = 3, nEps = 1, SampleSize = 300,
  EpType = list( EP1 = "Continuous" ),
  sigma = list( EP1 = c( 1, 1, 1 ) ), CommonStdDev = TRUE,
  prop.ctr = list( EP1 = NA ), allocRatio = c( 1, 1, 1 ),
  WI = c( 0.5, 0.5 ), G = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  test.type = "Parametric", alpha = 0.025, info_frac = c( 0.5, 1.0 ),
  typeOfDesign = "asOF", plotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLook_CER( design, p_raw = c( H1 = 0.1, H2 = 0.2 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLook_CER(
  design, state = look1_analysis, selection = "H2",
  p_raw = c( H1 = NA, H2 = 0.02 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 3 #########################################################
design <- SetupDesign_CER(
  nArms = 3, nEps = 1, SampleSize = 300,
  EpType = list( EP1 = "Continuous" ),
  sigma = list( EP1 = c( 1, 1, 1 ) ), CommonStdDev = TRUE,
  prop.ctr = list( EP1 = NA ), allocRatio = c( 1, 1, 1 ),
  WI = c( 0.5, 0.5 ), G = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  test.type = "Parametric", alpha = 0.025, info_frac = c( 0.5, 1.0 ),
  typeOfDesign = "asOF", plotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLook_CER(
  design, p_raw = c( H1 = 0.0001, H2 = 0.2 )
)

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLook_CER(
  design, state = look1_analysis,
  new_sample_size = c( 125, 125 ),
  p_raw = c( H1 = NA, H2 = 0.01 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 4 #########################################################
design <- SetupDesign_CER(
  nArms = 3, nEps = 2, SampleSize = 400,
  EpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
  sigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
  CommonStdDev = TRUE, prop.ctr = list( EP1 = NA, EP2 = NA ),
  allocRatio = c( 1, 1, 1 ), WI = c( 0.5, 0.5, 0, 0 ),
  G = matrix( c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
    4, 4, byrow = TRUE ),
  test.type = "Partly-Parametric", alpha = 0.025,
  info_frac = c( 0.5, 1.0 ), typeOfDesign = "asOF", plotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLook_CER( design,
  p_raw = c( H1 = .15, H2 = .25, H3 = .10, H4 = .20 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLook_CER( design, state = look1_analysis,
  p_raw = c( H1 = .05, H2 = .10, H3 = .03, H4 = .12 ) )
  
cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 5 #########################################################
mGraph <- matrix( c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
  4, 4, byrow = TRUE )

design <- SetupDesign_CER(
  nArms = 3, nEps = 2, SampleSize = 210,
  EpType = list( EP1 = "Continuous", EP2 = "Continuous" ),
  sigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
  CommonStdDev = FALSE, prop.ctr = list( EP1 = NA, EP2 = NA ),
  allocRatio = c( 1, 1, 1 ), WI = c( .5, .5, 0, 0 ), G = mGraph,
  test.type = "Parametric", alpha = .025, info_frac = c( .5, 1 ),
  typeOfDesign = "asOF", plotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLook_CER( design,
  p_raw = c( H1 = .00045, H2 = .0952, H3 = .0225, H4 = .1104 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLook_CER( design, state = look1_analysis,
  selection = c( "H2", "H4" ),
  new_sample_size = c( 88, 87 ),
  new_weights = c( H2 = .5, H4 = .5 ),
  new_G = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  p_raw = c( H1 = NA, H2 = .0299, H3 = NA, H4 = .0586 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################
