# Regression tests for legacy adaptGMCP_CER() outputs.

lCerScenarios <- BuildCerRegressionScenarios()

testthat::test_that( "CER-regression-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-regression-02 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-02" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-02" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-regression-03 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-regression-03" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-regression-03" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-Examp-AdaptGMCP-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-AdaptGMCP-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-AdaptGMCP-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-Examp-3arm-1ep matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-3arm-1ep" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-3arm-1ep" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-Examp-2ep matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-Examp-2ep" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-Examp-2ep" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-GSExample4-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample4-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample4-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-GSExample4-02 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample4-02" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample4-02" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-GSExample2-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-GSExample2-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-GSExample2-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )

testthat::test_that( "CER-AdaptExample2-01 matches fixture outputs", {
  lActual <- CaptureCerRegressionOutputsForTest( lCerScenarios[[ "CER-AdaptExample2-01" ]] )
  lExpected <- LoadCerRegressionFixtures( "CER-AdaptExample2-01" )

  testthat::expect_equal( names( lActual ), names( lExpected ) )

  for( strLook in names( lExpected ) )
  {
    ExpectCerRegressionLookEqual( lActual[[ strLook ]], lExpected[[ strLook ]] )
  }
} )
