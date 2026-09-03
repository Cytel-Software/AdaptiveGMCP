# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

# Perform Single Simulation for combining p-values method
# simID Simulation ID
# gmcpSimObj obj with simulation inputs
# preSimObjs obj with intermediate inputs
SingleSimCombPValue <- function(simID, gmcpSimObj, preSimObjs) {
  # Initialize Intermediate Inputs
  mcpObj <- initialize_mcpObj(gmcpSimObj = gmcpSimObj, preSimObjs = preSimObjs)
  SummStatDF <- mcpObj$SummStatBlank
  ArmWiseDF <- mcpObj$ArmWiseDataBlank

  # look-wise test
  while (mcpObj$ContTrial) {
    # Get the per arm incremental sample size
    Arms.SS.Incr <- getInterimSSIncr(
      lookID = mcpObj$CurrentLook,
      PlanSSIncr = mcpObj$planSS$IncrementalSamples,
      ArmsPresent = mcpObj$ArmsPresent,
      ArmsDropped = mcpObj$ArmsDropped,
      Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
      ImplicitSSR = mcpObj$ImplicitSSR
    )

    if(!is.null(mcpObj$EastSumStat)){
      #Use East Summary Statistics File
      ArmData <- mcpObj$ArmWiseDataBlank
      SummStat <- useEastSumStat(SimID = simID,
                                 mcpObj$EastSumStat)
    }else{
      #Use R Data Generation
      # Get incremental summary
      currLookDataIncr <- genIncrLookSummary(
        SimSeed = mcpObj$SimSeed,
        simID = simID,
        lookID = mcpObj$CurrentLook,
        Arms.Mean = mcpObj$Arms.Mean,
        Arms.std.dev = mcpObj$Arms.std.dev,
        Arms.Prop = mcpObj$Arms.Prop,
        UseCC = mcpObj$UseCC,
        Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
        Arms.SS = Arms.SS.Incr,
        EPCorr = mcpObj$EP.Corr,
        ArmsPresent = mcpObj$ArmsPresent,
        HypoPresent = mcpObj$HypoPresent,
        HypoMap = mcpObj$HypoMap
      )
      ArmData <- currLookDataIncr$ArmData

      # Compute Test Stat(Incr.) and p-values(Incr.)
      SummStat <- getPerLookTestStat(
        simID = simID,
        lookID = mcpObj$CurrentLook,
        TestStatCont = mcpObj$TestStatCont,
        TestStatBin = mcpObj$TestStatBin,
        Arms.std.dev = mcpObj$Arms.std.dev,
        IncrLookSummary = currLookDataIncr,
        HypoMap = mcpObj$HypoMap,
        Cumulative = FALSE
      )
    }
    if (mcpObj$CurrentLook == 1){
      mcpObj$rawpvalues$stage1 <- as.vector(unlist(SummStat[, grep("^RawPvalues", names(SummStat))]))
    } else {
      mcpObj$rawpvalues$stage2 <- as.vector(unlist(SummStat[, grep("^RawPvalues", names(SummStat))]))
    }
    # Perform per look Test
    mcpObj <- perLookTest(Arms.SS.Incr = Arms.SS.Incr, SummStat = SummStat, mcpObj = mcpObj,
                          mvtnorm_algo = gmcpSimObj$mvtnorm_algo)

    # Available Arms & hypothesis after rejection
    mcpObj$ArmsPresent <- getArmsPresent(ArmsPresent = mcpObj$ArmsPresent, rejflags = mcpObj$rej_flag_Curr, HypoMap = mcpObj$HypoMap)
    mcpObj$HypoPresent <- !mcpObj$rej_flag_Curr
    mcpObj$IndexSet <- mcpObj$HypoMap$Hypothesis[mcpObj$HypoPresent]

    rejStatus <- data.frame(matrix(mcpObj$rej_flag_Curr, nrow = 1))
    names(rejStatus) <- paste("RejStatus", get_numeric_part(mcpObj$HypoMap$Hypothesis), sep = "")

    SummStat <- plyr::rbind.fill(mcpObj$SummStatBlank, SummStat)
    SummStat <- fillNa(1, SummStat, rejStatus)
    SummStatDF <- plyr::rbind.fill(SummStatDF, SummStat)
    ArmWiseDF <- plyr::rbind.fill(ArmWiseDF, ArmData)
    mcpObj$SummStatDF <- SummStatDF
    mcpObj$ArmDataDF <- ArmWiseDF

    mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr
    mcpObj$WH_Prev <- mcpObj$WH

    # Check for Early Stopping Conditions
    if (StopTrial(mcpObj)) {
      mcpObj$ContTrial <- F
      break # Early Stopping
    } else # Next look
    {
      # Selection for next look
      if (mcpObj$Selection & (mcpObj$CurrentLook < mcpObj$LastLook)) {
        mcpObj <- do_SelectionSim2(simID = simID,
                                   mcpObj = mcpObj)
      }
    }
    mcpObj$CurrentLook <- mcpObj$CurrentLook + 1

    # End of Look-wise While Loop #
  }

  # Power Table
  powerCountDF <- CountPower(simID = simID, SummaryStatFile = SummStatDF, TrueNull = mcpObj$TrueNull)
  # EffCountDF <- CountEfficacy(simID = simID, SummaryStatFile = SummStatDF)
  EffCountDF <- t(apply(SummStatDF[, grep("^RejStatus", names(SummStatDF))], 2, any))

  SelectionDF <- data.frame()
  # Selection Summary Table
  if (mcpObj$Selection & length(mcpObj$SelectedIndex) > 0) {
    SelectionDF <- data.frame(
      "simID" = simID,
      "SelectedHypothesis" = mcpObj$SelectedIndex
    )
  }

  list(
    "SummStatDF" = SummStatDF,
    "ArmWiseDF" = ArmWiseDF,
    "powerCountDF" = powerCountDF,
    "EfficacyTable" = EffCountDF,
    "SelectionDF" = SelectionDF,
    "rawpvalues" = mcpObj$rawpvalues
  )
}


# Perform Single Simulation for CER method
# simID Simulation ID
# gmcpSimObj obj with simulation inputs
# preSimObjs obj with intermediate inputs
SingleSimCER <- function(simID, gmcpSimObj, preSimObjs) {
  ######################################
  # browser()
  ######################################
  # Initialize Intermediate Inputs
  mcpObj <- initialize_mcpObj(gmcpSimObj = gmcpSimObj, preSimObjs = preSimObjs)
  SummStatDF <- mcpObj$SummStatBlank
  ArmWiseDF <- mcpObj$ArmWiseDataBlank
  # look-wise test
  while (mcpObj$ContTrial) {

    if (mcpObj$CurrentLook == 1) {
      # Get the per arm incremental sample size
      Arms.SS.Incr <- getInterimSSIncr(
        lookID = mcpObj$CurrentLook,
        PlanSSIncr = mcpObj$planSS$IncrementalSamples,
        ArmsPresent = mcpObj$ArmsPresent,
        ArmsDropped = mcpObj$ArmsDropped,
        Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
        ImplicitSSR = mcpObj$ImplicitSSR
      )
      # Get incremental summary for look 1
      currLookDataIncr <- genIncrLookSummary(
        SimSeed = mcpObj$SimSeed,
        simID = simID,
        lookID = mcpObj$CurrentLook,
        Arms.Mean = mcpObj$Arms.Mean,
        Arms.std.dev = mcpObj$Arms.std.dev,
        Arms.Prop = mcpObj$Arms.Prop,
        UseCC = mcpObj$UseCC,
        Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
        Arms.SS = Arms.SS.Incr,
        EPCorr = mcpObj$EP.Corr,
        ArmsPresent = mcpObj$ArmsPresent,
        HypoPresent = mcpObj$HypoPresent,
        HypoMap = mcpObj$HypoMap
      )

      ArmData <- currLookDataIncr$ArmData
      ArmWiseDF_Stage1 <- data.table::rbindlist(list(ArmWiseDF, ArmData), fill = TRUE)
      data.table::setDF(ArmWiseDF_Stage1)
      mcpObj$ArmDataDF <- ArmWiseDF_Stage1

      # Summary Statistics based on first look data
      SummStat_Stage1 <- getPerLookTestStat(
        simID = simID,
        lookID = mcpObj$CurrentLook,
        TestStatCont = mcpObj$TestStatCont,
        TestStatBin = mcpObj$TestStatBin,
        Arms.std.dev = mcpObj$Arms.std.dev,
        IncrLookSummary = currLookDataIncr,
        HypoMap = mcpObj$HypoMap,
        Cumulative = FALSE
      )
      # Storing the first looks incremental data to compute next look cumulative data
      IncrLookSummaryPrev <- currLookDataIncr
      # Perform per look Test
      mcpObj <- perLookTest(Arms.SS.Incr = Arms.SS.Incr, SummStat = SummStat_Stage1, mcpObj = mcpObj,
                            mvtnorm_algo = gmcpSimObj$mvtnorm_algo)

      # Available Arms & hypothesis after rejection
      mcpObj$ArmsPresent <- getArmsPresent(ArmsPresent = mcpObj$ArmsPresent, rejflags = mcpObj$rej_flag_Curr, HypoMap = mcpObj$HypoMap)
      mcpObj$HypoPresent <- !mcpObj$rej_flag_Curr
      mcpObj$IndexSet <- mcpObj$HypoMap$Hypothesis[mcpObj$HypoPresent]

      rejStatus <- data.frame(matrix(mcpObj$rej_flag_Curr, nrow = 1))
      names(rejStatus) <- paste("RejStatus", get_numeric_part(mcpObj$HypoMap$Hypothesis), sep = "")

      # SummStat_Stage1 <- plyr::rbind.fill(mcpObj$SummStatBlank, SummStat_Stage1)
      SummStat_Stage1 <- data.table::rbindlist(list(mcpObj$SummStatBlank, SummStat_Stage1), fill = TRUE)
      data.table::setDF(SummStat_Stage1)
      SummStat_Stage1 <- fillNa(1, SummStat_Stage1, rejStatus)
      # SummStatDF_Stage1 <- plyr::rbind.fill(SummStatDF, SummStat_Stage1)
      SummStatDF_Stage1 <- data.table::rbindlist(list(SummStatDF, SummStat_Stage1), fill = TRUE)
      data.table::setDF(SummStatDF_Stage1)
      mcpObj$SummStatDF <- SummStatDF_Stage1
      mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr
    } else {
      # loop over second stage simulations and save the summary stats for each in a list
      lPowerCountDF <- list()
      lEffCountDF <- vector("list", mcpObj$nSimulation_Stage2)

      # Get the per arm incremental sample size
      Arms.SS.Incr <- getInterimSSIncr(
        lookID = mcpObj$CurrentLook,
        PlanSSIncr = mcpObj$planSS$IncrementalSamples,
        ArmsPresent = mcpObj$ArmsPresent,
        ArmsDropped = mcpObj$ArmsDropped,
        Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
        ImplicitSSR = mcpObj$ImplicitSSR
      )
      # Check for adaptations : Check if this can be moved outside the nSim_Stage2 for loop
      PlanSSLk <- mcpObj$planSS$IncrementalSamples[mcpObj$CurrentLook, ]
      CurrSSLk <- Arms.SS.Incr
      mcpObj$AdaptStage2 <- F
      # If Current look Sample Size is different than the plan Sample size then Adapt.
      # Since when dropping of an arm changes the planned weights the planned stage-2 boundary will not longer be valid.
      if (!all(PlanSSLk == CurrSSLk, na.rm = T) ||
          mcpObj$Selection) {
        mcpObj$AdaptStage2 <- T
        Stage2AllocSampleSize <- unlist(mcpObj$planSS$IncrementalSamples[1, ]) +
          unlist(CurrSSLk)
        mcpObj$Stage2AllocSampleSize <- rbind(
          mcpObj$planSS$IncrementalSamples[1, ],
          Stage2AllocSampleSize
        )
        mcpObj$Stage2allocRatio <- unlist(CurrSSLk) / unlist(CurrSSLk[1])
        # Compute the Stage2 adaptive boundaries

        AdaptResults <- adaptBdryCER(mcpObj, mvtnorm_algo = gmcpSimObj$mvtnorm_algo)

        if (exists("gmcpSimObj") && gmcpSimObj$Debug) {
          ### Added for debugging purposes, should be removed once the code is verified to be working fine
          # browser()
        }

        mcpObj$AdaptObj <- AdaptResults
        ss_stage2_incr <- mcpObj$Stage2AllocSampleSize
        ss_stage2_incr[2,] <- ss_stage2_incr[2,] - ss_stage2_incr[1,]
      } else
      {
        ss_stage2_incr <- mcpObj$planSS$IncrementalSamples
      }

      # Ani: "If" condition added so that the code is executed only when
      # adaptation happens. Earlier the code was executed unconditionally and it
      # would crash if adaptation did not happen
      if(mcpObj$AdaptStage2) {
        # calculate adapted information fraction for each hypothesis for stage 2
        v_adapted_info_fraction_old <- numeric(0)
        ss_control <- as.vector(unlist(ss_stage2_incr['Control']))
        calculate_hm <- function(vControl, vTreatment, stage) (1/vControl[stage] + 1/vTreatment[stage])^-1
        for (hypothesis in mcpObj$HypoMap$Hypothesis) {
          treatment_id <- mcpObj$HypoMap[mcpObj$HypoMap$Hypothesis == hypothesis, "Treatment"] - 1
          ss_treatment <- as.vector(unlist(ss_stage2_incr[paste0("Treatment",treatment_id)]))
          adapted_info_fraction <- calculate_hm(ss_control, ss_treatment, 1)/(calculate_hm(ss_control, ss_treatment, 1) + calculate_hm(ss_control, ss_treatment, 2))
          v_adapted_info_fraction_old <- c(v_adapted_info_fraction_old, adapted_info_fraction)
        }
        # revised calculation for adapted info fraction based on info matrix instead of just sample size
        # Ani: PENDING CHANGE: The next 2 lines should be generalized to work for
        # one, two, or more than two endpoints.
        I1 <- c(preSimObjs$Sigma$InfoMatrix$EP1[,1], preSimObjs$Sigma$InfoMatrix$EP2[,1])
        I2_incr <- c(AdaptResults$Stage2Sigma$Stage2InfoMatrixIncr$EP1[,1], AdaptResults$Stage2Sigma$Stage2InfoMatrixIncr$EP2[,1])
        v_adapted_info_fraction <- I1/(I1 + I2_incr)
      } else {
        # Although we are not adapting in this case, setting the var
        # v_adapted_info_fraction so that dependent code in this function works
        # properly even in non-adapted case
        v_adapted_info_fraction <- preSimObjs$Sigma$InfoMatrix$EP1[,1] /
          preSimObjs$Sigma$InfoMatrix$EP1[,2]
      }

      mcpObj_Stage2 <- mcpObj # This will only be run at the end of stage 1. To be used for all stage 2 sims
      # We allow the user to perform multiple stage 2 simulations for each stage 1 simulation.
      # This is especially useful in case of large problems, i.e. designs with large number of arms and
      # multiple endpoints. In such cases if we run say 10k simulations for stage 1 and only 1 simulation
      # for stage 2 for each stage 1 simulation (like we do in East), then it was found that the confidence
      # intervals for power estimates were quite wide. To make them tighter, one had to run much larger
      # number of stage 1 simulations, like 100k or more which made the overall simulation time very long.
      # This is especially so because in case of CER method, in each of those simulations we would end up
      # calculating the CER and the adapted stage 2 boundaries which is time consuming.
      # With the nested simulations method, we can get tighter confidence intervals by running fewer
      # stage 1 simulations (like 10k) and more stage 2 simulations (like 50 or more) for each stage 1 simulation.
      # This reduces the overall simulation time significantly as the CER and adapted boundary calculations happen
      # far fewer times.
      for (nSim_Stage2 in 1:mcpObj$nSimulation_Stage2)
      {
        mcpObj <- mcpObj_Stage2
        # Get incremental summary for look 2 data
        currLookDataIncr <- genIncrLookSummary(
          SimSeed = mcpObj$SimSeed,
          simID = simID,
          simID_Stage2 = nSim_Stage2,
          lookID = mcpObj$CurrentLook,
          Arms.Mean = mcpObj$Arms.Mean,
          Arms.std.dev = mcpObj$Arms.std.dev,
          Arms.Prop = mcpObj$Arms.Prop,
          UseCC = mcpObj$UseCC,
          Arms.alloc.ratio = mcpObj$Arms.alloc.ratio,
          Arms.SS = Arms.SS.Incr,
          EPCorr = mcpObj$EP.Corr,
          ArmsPresent = mcpObj$ArmsPresent,
          HypoPresent = mcpObj$HypoPresent,
          HypoMap = mcpObj$HypoMap
        )
        ArmData <- currLookDataIncr$ArmData
        # ArmWiseDF <- plyr::rbind.fill(ArmWiseDF_Stage1, ArmData)
        ArmWiseDF <- data.table::rbindlist(list(ArmWiseDF_Stage1, ArmData), fill = TRUE)
        data.table::setDF(ArmWiseDF)

        mcpObj$ArmDataDF <- ArmWiseDF

        # Ani: Our earlier implementation of the stage 2 test stat computation
        # was not consistent with what we say in the help file for the
        # FWERControl parameter. There we say, "CombinationTest: combined two
        # stage incremental test statistics, None: Cumulative test statistics".
        # However, earlier we were calculating a weighted statistic using the
        # modified weights under the 'None' option and a weighted stat using the
        # original weights under the 'CombinationTest' option.
        # I have now moved the weighted test stat computation using modified
        # weights under the 'CombinationTest' option and implemented cumulative
        # test statistic computation under the 'None' option.

        if (mcpObj$FWERControl == "CombinationTest") {
          # Ani: Moved Anoop's weighted stat computation using modified/adapted
          # weights here and commented out earlier computation

          # Summary Statistics based on 2nd look incremental data
          SummStat <- getPerLookTestStat(
            simID = simID,
            lookID = mcpObj$CurrentLook,
            TestStatCont = mcpObj$TestStatCont,
            TestStatBin = mcpObj$TestStatBin,
            Arms.std.dev = mcpObj$Arms.std.dev,
            IncrLookSummary = currLookDataIncr,
            HypoMap = mcpObj$HypoMap,
            Cumulative = FALSE # calculate incremental stat
          )

          # Stage-1 raw p-values(Incr.)
          pValIncrPrev <- as.vector(unlist(mcpObj$SummStatDF[
            mcpObj$SummStatDF$LookID == (mcpObj$CurrentLook - 1),
            grep("RawPvalues", names(mcpObj$SummStatDF))
          ]))

          # Stage-2 raw p-values(Incr.)
          pValIncrCurr <- as.vector(unlist(SummStat[, grep("RawPvalues", names(SummStat))]))
          mcpObj$rawpvalues <- list("stage1" = pValIncrPrev, "stage2" = pValIncrCurr)

          # cumulative stage 2 p-values
          adapted_teststat_stage2 <- sqrt(v_adapted_info_fraction)*qnorm(1 - pValIncrPrev) +
            sqrt(1 - v_adapted_info_fraction)*qnorm(1 - pValIncrCurr)
          adapted_p_value_stage2 <- 1 - pnorm(adapted_teststat_stage2)
          SummStat[, grep("TestStat", names(SummStat))] <- adapted_teststat_stage2
          SummStat[, grep("RawPvalues", names(SummStat))] <- adapted_p_value_stage2
          ######################################################

          # # Stage-1 raw p-values(Incr.)
          # pValIncrPrev <- mcpObj$SummStatDF[
          #   mcpObj$SummStatDF$LookID == (mcpObj$CurrentLook - 1),
          #   grep("RawPvalues", names(mcpObj$SummStatDF))
          # ]
          # # Stage-2 raw p-values(Incr.)
          # pValIncrCurr <- SummStat[, grep("RawPvalues", names(SummStat))]
          #
          # # pValIncr <- plyr::rbind.fill(pValIncrPrev, pValIncrCurr)
          # pValIncr <- data.table::rbindlist(list(pValIncrPrev, pValIncrCurr), fill = TRUE)
          # data.table::setDF(pValIncr)
          # # Converted to Z Statistics(Incr.)
          # zIncr <- apply(pValIncr, 2, function(x) {
          #   qnorm(1 - x)
          # })
          #
          # W_Norm <- mcpObj$InvNormWeights$W_Norm
          # if (is.vector(W_Norm) & mcpObj$CurrentLook == 2) {
          #   # Inverse Normal Weights for two looks based on pre-planed look positions
          #   W_Inv <- W_Norm
          # } else if (is.matrix(W_Norm)) # W_Norm is a matrix for more than 2 looks
          # {
          #   # Inverse Normal Weights for more than two looks  based on pre-planed look positions
          #   W_Inv <- W_Norm[((mcpObj$CurrentLook) - 1), 1:(mcpObj$CurrentLook)]
          # } else {
          #   # Error in CombinedPvalue function
          #   return("Error in CombinedPvalue function")
          # }
          #
          # # Test The computed Inverse Normal Weights
          # if (abs(sum(W_Inv^2) - 1) > 1E-6) stop("Error: abs(sum(W_Inv^2)-1) < 1E-6 not true| function: CombinedPvalue")
          #
          # # Inverse Normal Combination
          # combStage2TestStat <- unlist(lapply(1:ncol(zIncr), function(i) {
          #   sum(W_Inv * zIncr[, i])
          # }))
          #
          # combStage2pVal <- unlist(lapply(1:ncol(zIncr), function(i) {
          #   1 - pnorm(sum(W_Inv * zIncr[, i]))
          # }))
          # SummStat[, grep("TestStat", names(SummStat))] <- combStage2TestStat
          # SummStat[, grep("RawPvalues", names(SummStat))] <- combStage2pVal
        } else { # i.e. mcpObj$FWERControl == "None" meaning cumulative test stats
          # Summary Statistics based on 2nd look cumulative data

          # Ani: FWERControl="None" option means calculating the stage 2 test stat
          # and raw p-values using cumulative data. getPerLookTestStat() is
          # capable of calculating such cumulative stat when its parameter
          # Cumulative is set to TRUE.
          SummStat <- getPerLookTestStat(
            simID = simID,
            lookID = mcpObj$CurrentLook,
            TestStatCont = mcpObj$TestStatCont,
            TestStatBin = mcpObj$TestStatBin,
            Arms.std.dev = mcpObj$Arms.std.dev,
            IncrLookSummary = currLookDataIncr,
            IncrLookSummaryPrev = IncrLookSummaryPrev,
            HypoMap = mcpObj$HypoMap,
            Cumulative = TRUE # Calculate cumulative stage 2 stat
          )

          # Stage-1 raw p-values(Incr.)
          pValIncrPrev <- as.vector(unlist(mcpObj$SummStatDF[
            mcpObj$SummStatDF$LookID == (mcpObj$CurrentLook - 1),
            grep("RawPvalues", names(mcpObj$SummStatDF))
          ]))

          # Stage-2 raw p-values(Incr.)
          pValCumCurr <- as.vector(unlist(SummStat[, grep("RawPvalues", names(SummStat))]))
          mcpObj$rawpvalues <- list("stage1" = pValIncrPrev, "stage2" = pValCumCurr)

          # Note that TestStat and RawPvalues members of SummStat are already
          # correctly set by getPerLookTestStat() based on cumulative data. So,
          # we do not need to do anything more.

          ######################################################
          # Original computation:
          # SummStat <- getPerLookTestStat(
          #   simID = simID,
          #   lookID = mcpObj$CurrentLook,
          #   TestStatCont = mcpObj$TestStatCont,
          #   TestStatBin = mcpObj$TestStatBin,
          #   Arms.std.dev = mcpObj$Arms.std.dev,
          #   IncrLookSummary = currLookDataIncr,
          #   IncrLookSummaryPrev = IncrLookSummaryPrev,
          #   HypoMap = mcpObj$HypoMap,
          #   Cumulative = FALSE
          # )
          #
          # # Stage-1 raw p-values(Incr.)
          # pValIncrPrev <- as.vector(unlist(mcpObj$SummStatDF[
          #   mcpObj$SummStatDF$LookID == (mcpObj$CurrentLook - 1),
          #   grep("RawPvalues", names(mcpObj$SummStatDF))
          # ]))
          # # Stage-2 raw p-values(Incr.)
          # pValIncrCurr <- as.vector(unlist(SummStat[, grep("RawPvalues", names(SummStat))]))
          # mcpObj$rawpvalues <- list("stage1" = pValIncrPrev, "stage2" = pValIncrCurr)
          # # cumulative stage 2 p-values
          #
          # adapted_teststat_stage2 <- sqrt(v_adapted_info_fraction)*qnorm(1 - pValIncrPrev) +
          #   sqrt(1 - v_adapted_info_fraction)*qnorm(1 - pValIncrCurr)
          # adapted_p_value_stage2 <- 1 - pnorm(adapted_teststat_stage2)
          # SummStat[, grep("TestStat", names(SummStat))] <- adapted_teststat_stage2
          # SummStat[, grep("RawPvalues", names(SummStat))] <- adapted_p_value_stage2
        }
        # Perform per look Test
        mcpObj <- perLookTest(Arms.SS.Incr = Arms.SS.Incr, SummStat = SummStat, mcpObj = mcpObj,
                              mvtnorm_algo = gmcpSimObj$mvtnorm_algo)

        # Available Arms & hypothesis after rejection
        mcpObj$ArmsPresent <- getArmsPresent(ArmsPresent = mcpObj$ArmsPresent, rejflags = mcpObj$rej_flag_Curr, HypoMap = mcpObj$HypoMap)
        mcpObj$HypoPresent <- !mcpObj$rej_flag_Curr
        mcpObj$IndexSet <- mcpObj$HypoMap$Hypothesis[mcpObj$HypoPresent]

        rejStatus <- data.frame(matrix(mcpObj$rej_flag_Curr, nrow = 1))
        names(rejStatus) <- paste("RejStatus", get_numeric_part(mcpObj$HypoMap$Hypothesis), sep = "")

        # SummStat <- plyr::rbind.fill(mcpObj$SummStatBlank, SummStat)
        SummStat <- data.table::rbindlist(list(mcpObj$SummStatBlank, SummStat), fill = TRUE)
        data.table::setDF(SummStat)

        SummStat <- fillNa(1, SummStat, rejStatus)
        # SummStatDF <- plyr::rbind.fill(SummStatDF_Stage1, SummStat)
        SummStatDF <- data.table::rbindlist(list(SummStatDF_Stage1, SummStat), fill = TRUE)
        data.table::setDF(SummStatDF)
        mcpObj$SummStatDF <- SummStatDF
        mcpObj$ArmDataDF <- ArmWiseDF

        mcpObj$rej_flag_Prev <- mcpObj$rej_flag_Curr
        # Power Table
        powerCountDF <- tryCatch(
          {
            # browser()
            CountPower(simID = simID, SummaryStatFile = SummStatDF, TrueNull = mcpObj$TrueNull)
          },
          error = function(e) {
            message("Error in CountPower: ", e$message)
            message("Inputs to CountPower: ", capture.output(str(list(
              simID = simID,
              SummaryStatFile = SummStatDF,
              TrueNull = mcpObj$TrueNull
            ))))
            stop(e) # Rethrow the error after logging
          }
        )
        # EffCountDF <- CountEfficacy(simID = simID, SummaryStatFile = SummStatDF)

        lPowerCountDF[[nSim_Stage2]] <- powerCountDF
        # lEffCountDF[[nSim_Stage2]] <- EffCountDF
        # lEffCountDF[[nSim_Stage2]] <- t(apply(SummStatDF[, grep("^RejStatus", names(SummStatDF))], 2, any))
      }

      ###########################################
      # browser()
      ###########################################
      powerCountDF <- data.table::rbindlist(lPowerCountDF, fill = TRUE)
      powerCountDF <- colMeans(powerCountDF, na.rm = TRUE)
      powerCountDF <- as.data.frame(as.list(powerCountDF))

      # powerCountDF$simID_Stage2 <- NULL
      # EffCountDF = do.call(rbind, lEffCountDF)
      # EffCountDF$simID_Stage2 <- NULL
    }

    # Check for Early Stopping Conditions
    if (StopTrial(mcpObj)) {
      mcpObj$ContTrial <- F
      break # Stop the Trial
    } else # Next look
    {
      # Selection for next look
      if (mcpObj$Selection & (mcpObj$CurrentLook < mcpObj$LastLook)) {
        mcpObj <- do_SelectionSim2(simID = simID,
                                   mcpObj = mcpObj)
      }
    }
    # move to next look
    mcpObj$CurrentLook <- mcpObj$CurrentLook + 1
    # End of Look-wise Loop #
  }

  # handle power and efficacy tables if trial stops at look 1
  if (nrow(mcpObj$SummStatDF) == 1) {
    ######################################
    # browser()
    ######################################
    SummStatDF <- mcpObj$SummStatDF
    powerCountDF <- CountPower(simID = simID, SummaryStatFile = SummStatDF, TrueNull = mcpObj$TrueNull)
    # EffCountDF <- t(apply(SummStatDF[, grep("^RejStatus", names(SummStatDF))], 2, any))
  }

  SelectionDF <- data.frame()
  # Selection Summary Table
  if (mcpObj$Selection & length(mcpObj$SelectedIndex) > 0) {
    SelectionDF <- data.frame(
      "simID" = simID,
      "SelectedHypothesis" = mcpObj$SelectedIndex
    )
  }
  list(
    "SummStatDF" = SummStatDF,
    "ArmWiseDF" = mcpObj$ArmDataDF,
    "powerCountDF" = powerCountDF,
    # "EfficacyTable" = EffCountDF,
    "SelectionDF" = SelectionDF,
    "rawpvalues" = mcpObj$rawpvalues
  )
}






# Initialization of mcpObj to run look-wise analysis
initialize_mcpObj <- function(gmcpSimObj, preSimObjs) {
  SummStatNames <- c(
    "SimID", "LookID",
    paste("Delta", 1:gmcpSimObj$nHypothesis, sep = ""),
    paste("StdError", 1:gmcpSimObj$nHypothesis, sep = ""),
    paste("TestStat", 1:gmcpSimObj$nHypothesis, sep = ""),
    paste("RawPvalues", 1:gmcpSimObj$nHypothesis, sep = ""),
    paste("RejStatus", 1:gmcpSimObj$nHypothesis, sep = "")
  )
  SummStatBlank <- data.frame(matrix(nrow = 0, ncol = length(SummStatNames)))
  names(SummStatBlank) <- SummStatNames

  ArmWiseDataName <- c("SimID", "LookID", "ArmID", "EpID", "Completers", "Mean", "SumOfSquares")
  ArmWiseDataBlank <- data.frame(matrix(nrow = 0, ncol = length(ArmWiseDataName)))
  names(ArmWiseDataBlank) <- ArmWiseDataName

  addList <- list(
    "CurrentLook" = 1,
    "HypoPresent" = rep(TRUE, gmcpSimObj$nHypothesis),
    "ArmsPresent" = rep(TRUE, gmcpSimObj$nArms),
    "ArmsDropped" = rep(FALSE, gmcpSimObj$nArms),
    "p_raw" = NA,
    "WH_Prev" = preSimObjs$WH,
    "rej_flag_Prev" = rep(FALSE, gmcpSimObj$nHypothesis),
    "rej_flag_Curr" = rep(FALSE, gmcpSimObj$nHypothesis),
    "DroppedFlag" = rep(FALSE, gmcpSimObj$nHypothesis),
    "LastLook" = gmcpSimObj$nLooks,
    "Modify" = F,
    "ModificationLook" = c(),
    "newWeights" = NA,
    "newG" = NA,
    "SummStatBlank" = SummStatBlank,
    "ArmWiseDataBlank" = ArmWiseDataBlank,
    "SummStatDF" = NA,
    "ArmDataDF" = NA,
    "ContTrial" = T
  )

  mcpObj <- append(append(gmcpSimObj, preSimObjs), addList)
  mcpObj
}

#---------------  -
# Response Generation for interim looks based on the available arms or re-allocated sample size(Implicit SSR)
#--------------- -
getInterimSSIncr <- function(lookID, PlanSSIncr, ArmsPresent, ArmsDropped,
                             Arms.alloc.ratio, ImplicitSSR) {
  if (ncol(PlanSSIncr) != length(ArmsPresent) || ncol(PlanSSIncr) != length(Arms.alloc.ratio)) stop("'ncol(PlanSSIncr)', length(Arms.alloc.ratio) and length(ArmsPresent) are not same")
  if (all(ArmsPresent == F) || all(ArmsDropped == T)) stop("No Arms present to Continue")

  planSS <- SS.arm <- PlanSSIncr[lookID, ] # If no Implicit SSR is needed SS.arm = Planned Sample Size

  if (ImplicitSSR == "Selection" & lookID > 1) # Re-allocate samples from only de-selected arms
  {
    AdditionalSS <- sum(planSS[which(ArmsDropped)])
    ss_frac <- Arms.alloc.ratio[ArmsPresent] / sum(Arms.alloc.ratio[ArmsPresent])
    SS.arm.add <- round(AdditionalSS * ss_frac)
    SS.arm.add[1] <- AdditionalSS - sum(SS.arm.add[-1])
    SS.arm[which(ArmsPresent)] <- SS.arm[which(ArmsPresent)] + SS.arm.add
    SS.arm[which(!ArmsPresent)] <- NA
  } else if (ImplicitSSR == "All" & lookID > 1) # Allocate all the samples planed to the available arms
  {
    AdditionalSS <- sum(planSS[which(!ArmsPresent)])
    ss_frac <- Arms.alloc.ratio[ArmsPresent] / sum(Arms.alloc.ratio[ArmsPresent])
    SS.arm.add <- round(AdditionalSS * ss_frac)
    SS.arm.add[1] <- AdditionalSS - sum(SS.arm.add[-1])
    SS.arm[which(ArmsPresent)] <- SS.arm[which(ArmsPresent)] + SS.arm.add
    SS.arm[which(!ArmsPresent)] <- NA
  } else {
    SS.arm[!ArmsPresent] <- NA
  }
  SS.arm
}

ifElse <- function(test, yes, no) {
  if (test) {
    yes
  } else {
    no
  }
}
