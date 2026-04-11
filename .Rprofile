source("renv/activate.R")

# Workaround for VS Code R Language Server startup on Windows with renv enabled.
#
# The `languageserver` package spawns background workers via `callr::r_session`.
# In some environments (corporate AV, OneDrive, cold start), worker startup can
# exceed callr's default 3000ms wait_timeout and the language server aborts.
#
# Only apply this patch in the dedicated VS Code language-server process.
# The R extension sets VSCR_LSP_PORT (may be empty in stdio mode).
if ("VSCR_LSP_PORT" %in% names(Sys.getenv())) {
	try({
		if (requireNamespace("languageserver", quietly = TRUE) &&
			requireNamespace("callr", quietly = TRUE)) {
			tm <- get("TaskManager", envir = asNamespace("languageserver"))
			if (!is.null(tm) && isTRUE(is.function(tm$set))) {
				tm$set(
					"private",
					"find_or_create_session",
					function() {
						if (!isTRUE(private$use_session)) {
							return(NULL)
						}

						for (s in private$sessions) {
							state <- s$get_state()
							if (state == "starting") {
								res <- s$read()
								if (!is.null(res) && res$code == 201) {
									state <- s$get_state()
								}
							}
							if (state == "idle") {
								return(s)
							}
						}

						if (length(private$sessions) < private$max_running_tasks) {
							wait_timeout_ms <- as.integer(
								Sys.getenv("VSCR_CALLR_WAIT_TIMEOUT_MS", unset = "15000")
							)

							session <- callr::r_session$new(
								options = callr::r_session_options(
									system_profile = TRUE,
									user_profile = TRUE
								),
								wait = TRUE,
								wait_timeout = wait_timeout_ms
							)

							private$sessions <- append(private$sessions, session)
							return(session)
						}

						NULL
					}
					,
					overwrite = TRUE
				)
			}
		}
	}, silent = TRUE)
}
