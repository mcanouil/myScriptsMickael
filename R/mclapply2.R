mclapply2 <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE,
                      mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE, mc.allow.recursive = TRUE) {
    if (Sys.info()[["sysname"]] != "Windows") {
        mc.cores.old <- mc.cores
        sysMemFree <- system("egrep 'MemFree' /proc/meminfo", intern = TRUE)
        sysMemAvailable <- 0.90*as.numeric(unlist(regmatches(sysMemFree, regexec("[0-9]+", sysMemFree))))
        sysProc <- as.numeric(unlist(strsplit(system(paste("ps v", Sys.getpid()), intern = TRUE)[2], " +"))[8])
        mc.cores <- max(min(as.integer(mc.cores), floor(sysMemAvailable/sysProc)), 1)
        if (mc.cores != mc.cores.old) {
            msg <- paste('[mclapply2] To avoid memory overload "mc.cores" was decreased to "', mc.cores, '".', sep = "")
            warning(msg, call. = FALSE)
        } else {}
    } else {}
    require(parallel)
    return(mclapply(X = X, FUN = FUN, ...,
                    mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed, mc.silent = mc.silent,
                    mc.cores = mc.cores, mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
}