# Non-interactive CER analysis example

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
  plotGraphs = FALSE
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
