maxCores <- function(mc.cores = detectCores()) {
    if (Sys.info()[["sysname"]] != "Windows") {
        mc.cores.old <- mc.cores
        sysMemFree <- system("egrep '^MemFree:' /proc/meminfo", intern = TRUE)
        sysMemCached <- system("egrep '^Cached:' /proc/meminfo", intern = TRUE)
        sysMemAvailable <- 0.90*(as.numeric(gsub("[^0-9]*([0-9]*)", "\\1", sysMemFree)) + as.numeric(gsub("[^0-9]*([0-9]*)", "\\1", sysMemCached)))
        sysProc <- as.numeric(unlist(strsplit(system(paste("ps v", Sys.getpid()), intern = TRUE)[2], " +"), use.names = FALSE)[8])
        mc.cores <- max(min(as.integer(mc.cores), floor(sysMemAvailable/sysProc)), 1)
        if (mc.cores != mc.cores.old) {
            warning(paste0('[mclapply2] To avoid memory overload "mc.cores" was decreased to "', mc.cores, '".'), call. = FALSE)
        } else {}
    } else {}
    return(mc.cores)
}