# Non-interactive CER analysis example

# EXAMPLE 1 #########################################################
design <- SetupAnalysisCER(
  nArms = 3,
  nEps = 1,
  nSampleSize = 300,
  lEpTypes = list( EP1 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ) ),
  bCommonStdDev = TRUE,
  lCtrlProps = list( EP1 = NA ),
  vAllocRatios = c( 1, 1, 1 ),
  vWeights = c( .5, .5 ),
  mGMatrix = matrix( c( 0, 1, 1, 0 ), nrow = 2, byrow = TRUE ),
  strTestType = "Parametric",
  bPlotGraphs = TRUE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLookCER(
  cState = design,
  vRawPVals = c( H1 = .1, H2 = .2 )
)

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLookCER(
  cState = look1_analysis,
  vRawPVals = c( H1 = .0001, H2 = .2 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 2 #########################################################
design <- SetupAnalysisCER(
  nArms = 3, nEps = 1, nSampleSize = 300,
  lEpTypes = list( EP1 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ) ), bCommonStdDev = TRUE,
  lCtrlProps = list( EP1 = NA ), vAllocRatios = c( 1, 1, 1 ),
  vWeights = c( 0.5, 0.5 ), mGMatrix = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  strTestType = "Parametric", dAlpha = 0.025, vInfoFracts = c( 0.5, 1.0 ),
  strDesignType = "asOF", bPlotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLookCER( design, vRawPVals = c( H1 = 0.1, H2 = 0.2 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLookCER(
  look1_analysis, vSelectHypo = "H2",
  vRawPVals = c( H1 = NA, H2 = 0.02 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 3 #########################################################
design <- SetupAnalysisCER(
  nArms = 3, nEps = 1, nSampleSize = 300,
  lEpTypes = list( EP1 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ) ), bCommonStdDev = TRUE,
  lCtrlProps = list( EP1 = NA ), vAllocRatios = c( 1, 1, 1 ),
  vWeights = c( 0.5, 0.5 ), mGMatrix = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  strTestType = "Parametric", dAlpha = 0.025, vInfoFracts = c( 0.5, 1.0 ),
  strDesignType = "asOF", bPlotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLookCER(
  design, vRawPVals = c( H1 = 0.0001, H2 = 0.2 )
)

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLookCER(
  look1_analysis,
  vNewSampleSizes = c( 125, 125 ),
  vRawPVals = c( H1 = NA, H2 = 0.01 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 4 #########################################################
design <- SetupAnalysisCER(
  nArms = 3, nEps = 2, nSampleSize = 400,
  lEpTypes = list( EP1 = "Continuous", EP2 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
  bCommonStdDev = TRUE, lCtrlProps = list( EP1 = NA, EP2 = NA ),
  vAllocRatios = c( 1, 1, 1 ), vWeights = c( 0.5, 0.5, 0, 0 ),
  mGMatrix = matrix( c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
    4, 4, byrow = TRUE ),
  strTestType = "Partly-Parametric", dAlpha = 0.025,
  vInfoFracts = c( 0.5, 1.0 ), strDesignType = "asOF", bPlotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLookCER( design,
  vRawPVals = c( H1 = .15, H2 = .25, H3 = .10, H4 = .20 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLookCER( look1_analysis,
  vRawPVals = c( H1 = .05, H2 = .10, H3 = .03, H4 = .12 ) )
  
cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################

# EXAMPLE 5 #########################################################
mGraph <- matrix( c( 0, .5, .5, 0, .5, 0, 0, .5, 0, 1, 0, 0, 1, 0, 0, 0 ),
  4, 4, byrow = TRUE )

design <- SetupAnalysisCER(
  nArms = 3, nEps = 2, nSampleSize = 210,
  lEpTypes = list( EP1 = "Continuous", EP2 = "Continuous" ),
  lSigma = list( EP1 = c( 1, 1, 1 ), EP2 = c( 1, 1, 1 ) ),
  bCommonStdDev = FALSE, lCtrlProps = list( EP1 = NA, EP2 = NA ),
  vAllocRatios = c( 1, 1, 1 ), vWeights = c( .5, .5, 0, 0 ), mGMatrix = mGraph,
  strTestType = "Parametric", dAlpha = .025, vInfoFracts = c( .5, 1 ),
  strDesignType = "asOF", bPlotGraphs = FALSE
)

cat("\nDesign:")
print(design)

look1_analysis <- AnalyzeLookCER( design,
  vRawPVals = c( H1 = .00045, H2 = .0952, H3 = .0225, H4 = .1104 ) )

cat("\nLook 1 output:")
print(look1_analysis)

look2_analysis <- AnalyzeLookCER( look1_analysis,
  vSelectHypo = c( "H2", "H4" ),
  vNewSampleSizes = c( 88, 87 ),
  vWeights = c( H2 = .5, H4 = .5 ),
  mGMatrix = matrix( c( 0, 1, 1, 0 ), 2, 2, byrow = TRUE ),
  vRawPVals = c( H1 = NA, H2 = .0299, H3 = NA, H4 = .0586 )
)

cat("\nLook 2 output:")
print(look2_analysis)
#####################################################################
