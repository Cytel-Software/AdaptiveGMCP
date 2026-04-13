# --------------------------------------------------------------------------------------------------
#
# ©2025 Cytel, Inc.  All rights reserved.  Licensed pursuant to the GNU General Public License v3.0.
#
# --------------------------------------------------------------------------------------------------

perLookTest <- function(Arms.SS.Incr, SummStat, mcpObj, mvtnorm_algo) {
  tryCatch(
    {
      p_raw <- unlist(SummStat[, grep("RawPvalues", names(SummStat))]) ## Simulated raw p-values
      names(p_raw) <- paste("H", get_numeric_part(names(p_raw)), sep = "")

      mcpObj$p_raw <- addNAPvalue(p_raw, mcpObj$HypoMap$Hypothesis)

      if (mcpObj$Method == "CombPValue") # For combining p-values
        {
          PlanSSLk <- mcpObj$planSS$IncrementalSamples[mcpObj$CurrentLook, ]
          CurrSSLk <- Arms.SS.Incr

          if (!all(PlanSSLk == CurrSSLk, na.rm = T) & mcpObj$test.type == 'Dunnett') {
            # if the sample size is altered the correlation needs to be recomputed
            mcpObj$Correlation <- getPlanCorrelation(
              nHypothesis = mcpObj$nHypothesis,
              SS_Incr = CurrSSLk,
              Arms.std.dev = mcpObj$Arms.std.dev,
              test.type = mcpObj$test.type,
              EpType = mcpObj$lEpType,
              prop.ctr = mcpObj$prop.ctr,
              CommonStdDev = mcpObj$CommonStdDev
            )[[1]]
          } else {
            mcpObj$Correlation <- mcpObj$PlanCorrelation[[mcpObj$CurrentLook]]
          }

          mcpObj$CutOff <- mcpObj$pValBdry$Threshold[mcpObj$CurrentLook]
          mcpObj <- PerLookMCPAnalysis(mcpObj, mvtnorm_algo = mvtnorm_algo) # Perform Combining p-value Test
          return(mcpObj)
          #------------------------------------------------------------------------------------
        } else if (mcpObj$Method == "CER") # For CER
        {
          # mcpObj$IndexSet <- get_numeric_part(mcpObj$HypoMap$Hypothesis[mcpObj$HypoPresent])

          if (mcpObj$CurrentLook == 1) {
            # Stage1 Analysis
            Stage1Analysis <- closedTest(
              WH = mcpObj$WH,
              boundary = mcpObj$plan_Bdry$Stage1Bdry,
              pValues = mcpObj$p_raw,
              Stage1RejStatus = rep(FALSE, length(mcpObj$p_raw))
            )

            Stage1Obj <- list(
              "HypoMap" = mcpObj$HypoMap,
              "info_frac" = mcpObj$InfoFrac,
              "AllocSampleSize" = mcpObj$planSS$CumulativeSamples,
              "Sigma" = mcpObj$Sigma,
              "WH" = mcpObj$WH,
              "plan_Bdry" = mcpObj$plan_Bdry,
              "Stage1Analysis" = Stage1Analysis
            )

            mcpObj$rej_flag_Curr <- unlist(Stage1Analysis$PrimaryHypoTest)
            # mcpObj$IndexSet <-  paste("H",which(!mcpObj$rej_flag_Curr), sep ='')
            mcpObj$Stage1Obj <- Stage1Obj
            mcpObj$AllocSampleSize <- Stage1Obj$AllocSampleSize
            mcpObj$allocRatio <- mcpObj$Arms.alloc.ratio
            mcpObj$sigma <- mcpObj$Arms.std.dev


            if (length(which(mcpObj$rej_flag_Curr == T)) != 0) {
              rejected <- paste("H", which(mcpObj$rej_flag_Curr == T), sep = "")
            } else {
              rejected <- c()
            }

            mcpObj$WH_Prev <- mcpObj$WH

            if (length(rejected) == 0) {
              mcpObj$WH <- mcpObj$WH_Prev
            } else {
              mcpObj$WH <- mcpObj$WH_Prev[
                apply(mcpObj$WH_Prev[rejected], 1, function(x) {
                  any(x == 1)
                }) != 1,
              ]
            }
          } else # Stage-2 test
          {
            Stage1Objs <- mcpObj$Stage1Obj
            if (!mcpObj$AdaptStage2) {
              #This section should be execute only with planned design(no selection/SSR)
              WH_modified_idx <- as.vector(apply(
                mcpObj$WH[, grep("H", names(mcpObj$WH))], 1,
                function(x) {
                  paste(x, collapse = "")
                }
              ))

              WH_old_idx <- as.vector(apply(
                mcpObj$WH_Prev[, grep("H", names(mcpObj$WH_Prev))], 1,
                function(x) {
                  paste(x, collapse = "")
                }
              ))

              oldIdx <- unlist(lapply(
                1:nrow(mcpObj$WH),
                function(i) {
                  which(WH_old_idx == WH_modified_idx[i])
                }
              ))

              boundary <- Stage1Objs$plan_Bdry$Stage2Bdry[oldIdx, ]

              Stage2Analysis <- closedTest(
                WH = mcpObj$WH,
                boundary = boundary,
                pValues = mcpObj$p_raw,
                Stage1RejStatus = mcpObj$rej_flag_Prev
              )

              Stage2Tables <- list(
                "Test_Intersection_Hypothesis" = Stage2Analysis$IntersectHypoTest,
                "Rejection_Status" = Stage2Analysis$PrimaryHypoTest
              )
            } else {
              Stage2Analysis <- closedTest(
                WH = mcpObj$WH,
                boundary = mcpObj$AdaptObj$Stage2AdjBdry,
                pValues = mcpObj$p_raw,
                Stage1RejStatus = mcpObj$rej_flag_Prev
              )
              Stage2Tables <- list(
                "Adapt_Test_Tables" = mcpObj$AdaptObj$Stage2Tables,
                "Test_Intersection_Hypothesis" = Stage2Analysis$IntersectHypoTest,
                "Rejection_Status" = Stage2Analysis$PrimaryHypoTest
              )
            }
            mcpObj$rej_flag_Curr <- unlist(Stage2Analysis$PrimaryHypoTest)
          }
          return(mcpObj)
        }
    },
    error = function(err) {
      print("Error in perLookTest")
      traceback()
    }
  )
}
