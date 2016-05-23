getGreekLetter <- function(p) {
    if (p == "n")
        return("\U03BC")

    if (p == "l")
        return("\U03B4")

    return("\U03B5")
}

filterTable <- function(params, values, t) {
    filterExpr = paste(params, values, sep="==", collapse=" & ")
    subSet = subset(t, eval(parse(text=filterExpr)))
    return(subSet)
}

threadEval <- function(file, params, values, dataSetName) {
    table = read.table(file, header=TRUE, sep=";", col.names=c("t", "n", "l", "g", "t", "p", "f"))

    subSet = filterTable(params, values, table)

    fName = sprintf("%s_thread.eps", dataSetName)
    title = sprintf("%s dataset", dataSetName)
    cairo_ps(fName, family="Helvetica", width=10, height=10)
    # clip the margins (bottom, left, top, right) number of lines
    par(mar=c(4, 4.5, 1.3, 0))
    mids = barplot(subSet$p, cex.axis=1.48, xlab="Threads", ylab="% of total time", cex.lab=1.5, border="NA",
        main=title, col=c("tomato", rep("skyblue3", length(subSet$p) - 1)))
    axis(side=1, at=mids, labels=subSet$t, cex.axis=1.48)
    dev.off()
}

plotCPUComparison <- function(tables, vParam, params, values, legendNames, legendColors, legendMarkers, dataSetName,
    fileName, legendPosition = "topleft")
{
    valuesForRange = vector()
    for (t in tables)
        valuesForRange = append(valuesForRange, t$t)

    # set the chart ranges, so we can show all points
    xrange = range(tables[[1]][, vParam])
    yrange = range(valuesForRange)

    # get unicode representation of greek letters for each parameter
    greeks = as.vector(sapply(params, getGreekLetter))

    # temporarily create string containing part of the chart title
    paramsStr = paste(greeks[1:length(greeks) - 1], values[1:length(values) - 1], sep="=", collapse=", ")
    # format the the chart title
    title = sprintf("%s dataset. With %s and %s", dataSetName, paramsStr, paste(greeks[length(greeks)],
        values[length(values)], sep="="))

    cairo_ps(fileName, family="Helvetica")
    # clip the margins (bottom, left, top, right) number of lines
    par(mar=c(4, 4.5, 0, 2) + 0.1)
    # set the plot, type="n" means that we don't want anything plotted. xaxt="n" means that we don't want anything in
    # the x axis
    plot(xrange, yrange, type="n", xlab=parse(text=getGreekLetter(vParam)), ylab="Time (secs)", xaxt="n",
        cex.axis=2, cex.lab=2)

    # make sure we draw all values for the x axis (values that are in the fixed param)
    axis(1, at=tables[[1]][, vParam], cex.axis=2)

    # plot the lines! pch=0 means circles and 2 means triangles
    i = 1
    for (t in tables) {
        lines(t[, vParam], t$t, type="o", col=legendColors[i], pch=legendMarkers[i], cex=2)
        i = i + 1
    }

    legend(legendPosition, inset=.02, legendNames, col=legendColors, pch=legendMarkers, cex=2, bty="n")

    dev.off()
}

dataThreads <- function(overallFile, threadFile, dataSetName) {
    overrallTable = read.table(overallFile, header=TRUE, sep=";", col.names=c("t", "ad", "am", "f", "fa", "fr", "gc",
        "dc", "te", "tp", "gt", "dt", "pt", "dp", "tt"))
    threadsTable = read.table(threadFile, header=TRUE, sep=";", col.names=c("t", "ti", "d", "g", "p", "gc", "dc", "df",
        "gt", "dt"))
    ts = length(overrallTable$t)

    disksSum = aggregate(df ~ t, threadsTable, function(x) max(cumsum(x)))

    yRange = range(disksSum$df, overrallTable$am)
    xRange = range(overrallTable$t)
    print(xRange)
    print(overrallTable$t)

    colors = c("coral3", "cadetblue4")
    markers = c(15, 17)
    fName = sprintf("%s_disks_threads.eps", dataSetName)
    cairo_ps(fName, family="Helvetica")
    # clip the margins (bottom, left, top, right) number of lines
    par(mar=c(4, 4.5, 1.5, 2) + 0.1)
    plot(xRange, yRange, type="n", xlab="Time slot", ylab="Number of Disks", cex.axis=2, cex.lab=2)
    lines(overrallTable$t, disksSum$df, type="o", col=colors[1], pch=markers[1])
    lines(overrallTable$t, overrallTable$am, type="o", col=colors[2], pch=markers[2])
    legend("topright", inset=.02, c("Disks from Threads", "Disks after Merge"), col=colors, pch=markers, cex=2, bty="n")
    dev.off()
}

completeEval <- function(file, variableParam, params, values, dataSetName, legendPosition = "topleft") {
    table = read.table(file, header=TRUE, sep=";", col.names=c("th", "ty", "n", "l", "g", "t", "f"))

    filtered = filterTable(params, values, table)
    bfeTable = filterTable(c("ty"), c("\"o\""), filtered)
    bitdfTable = filterTable(c("ty"), c("\"b\""), filtered)

    threadsTable = filterTable(append(params, "ty"), append(values, "\"t\""), filtered)
    t5Table = filterTable(append(params, "th"), append(values, 5), threadsTable)
    t7Table = filterTable(append(params, "th"), append(values, 7), threadsTable)

    fName = sprintf("%s_complete_varying_%s.eps", dataSetName, variableParam)

    plotCPUComparison(c(list(bfeTable), list(bitdfTable), list(t5Table), list(t7Table)), variableParam, params,
        values, c("BFE", "BitDF", "BitDF 5 Threads", "BitDF 7 Threads"), c("blue", "red", "green", "orange"), c(15, 17,
        16, 18), dataSetName, fName, legendPosition)
}

cpuComparison <- function(bfeFile, bbfFile, variableParam, params, values, dataSetName, legendPosition = "topleft") {
    # read files into tables
    bfeTable = read.table(bfeFile, header=TRUE, sep=";", col.names=c("n", "l", "g", "t", "f"))
    bbfTable = read.table(bbfFile, header=TRUE, sep=";", col.names=c("n", "l", "g", "t", "f"))

    # extract the values that we want by using the expression above
    bfeSubSet = filterTable(params, values, bfeTable)
    bbfSubSet = filterTable(params, values, bbfTable)

    paramsString = paste(params, values, sep="_", collapse="_")
    paramsString = gsub(".", "_", paramsString, fixed=TRUE)
    fName = sprintf("%s_%s_varying_%s.eps", dataSetName, paramsString , variableParam)
    plotCPUComparison(c(list(bfeSubSet), list(bbfSubSet)), variableParam, params, values, c("BFE", "BitDF"), c("blue",
        "red"), c(15, 17), dataSetName, fName, legendPosition)
}

columnLetterToYAxisName <- function(column) {
    if (column == "m")
        return("Memory (MB)")

    if (column == "d")
        return(expression("Disks generated x" ~10^3))

    return("Potential Flocks")
}

resourceComparison <- function(bfeFile, bbfFile, column, dataSetName, legendPosition = "topleft", extraTop = 0) {
    # read files into tables
    bfeTable = read.table(bfeFile, header=TRUE, sep=";", col.names=c("t", "m", "d", "f"))
    bbfTable = read.table(bbfFile, header=TRUE, sep=";", col.names=c("t", "m", "d", "f"))

    xNormalized = c(1:length(bfeTable$t))

    bfeYNormalized = c()
    bbfYNormalized = c()
    if (column == "m") {
        bfeYNormalized = sapply(bfeTable[, column], function(v) return(v / 1024.0 / 1024.0))
        bbfYNormalized = sapply(bbfTable[, column], function(v) return(v / 1024.0 / 1024.0))
    } else {
        bfeYNormalized = cumsum(sapply(bfeTable[, column], function(v) return(v / 1000.0)))
        bbfYNormalized = cumsum(sapply(bbfTable[, column], function(v) return(v / 1000.0)))
    }
    # set the chart ranges, so we can show all points
    xrange = range(xNormalized)
    yrange = range(bfeYNormalized, bbfYNormalized)

    # format the the chart title
    title = sprintf("%s dataset", dataSetName)

    fName = sprintf("%s_%s.eps", dataSetName, column)
    cairo_ps(fName, family="Helvetica")
    # clip the margins (bottom, left, top, right) number of lines
    par(mar=c(4, 5.3, 0 + extraTop, 2) + 0.1)
    # set the plot, type="n" means that we don't want anything plotted
    plot(xrange, yrange, type="n", xlab="Time slot", ylab=columnLetterToYAxisName(column), cex.axis=2, cex.lab=2)

    # plot the lines! pch=0 means circles and 2 means triangles
    lines(xNormalized, bfeYNormalized, type="l", lwd=2.5, col="blue")
    lines(xNormalized, bbfYNormalized, type="l", lwd=2.5, col="red")

    legend(legendPosition, inset=.02, c("BFE", "BitDF"), col=c("blue", "red"), lwd=c(2.5, 2.5), cex=2, bty="n")

    # flush
    dev.off()
}

timeConsumption <- function(filePath) {
    t = read.table(filePath, header=TRUE, sep=",")
    print(t)
    m = as.matrix(t)

    fName = "timeConsumption.eps"
    cairo_ps(fName, family="Helvetica")

    par(mfrow=c(1, 1), mar=c(4, 4, 4, 0))
    barplot(m, border=NA, xlab="Dataset", ylab="% of time", col=c("tomato", "skyblue3"), cex.lab=1.3, legend.text=c("Disk processing",
        "Non disk related processing"), args.legend=list(x="topright", border=NA, bty="n", inset=c(0.35, -0.13)))

    dev.off()
}

generateAllTrucks <- function() {
    resourceComparison("results/performance_log_trucks_o_n4l20g1_5.txt",
        "results/performance_log_trucks_b_n4l20g1_5.txt", "d", "Trucks")

    cpuComparison("results/truckspaper_online_tun_2.txt", "results/truckspaper_buffering_tun_2.txt", "n", c("l", "g"),
        c(20, 1.5), "Trucks", "topright")

    cpuComparison("results/truckspaper_online_tun_2.txt", "results/truckspaper_buffering_tun_2.txt", "l", c("n", "g"),
        c(4, 1.5), "Trucks")

    cpuComparison("results/truckspaper_online_tun_2.txt", "results/truckspaper_buffering_tun_2.txt", "g", c("n", "l"),
        c(4, 20), "Trucks")
}

generateAllBerlinMOD <- function() {
    resourceComparison("results/performance_log_berlinmod_o_n4l8g200t120_tun_2.txt",
        "results/performance_log_berlinmod_b_n4l8g200t120_tun_2.txt", "d", "BerlinMOD", extraTop = 1.2)

    cpuComparison("results/berlinmod_online_tun_2.txt", "results/berlinmod_buffering_tun_2.txt", "n", c("l", "g"),
        c(8, 100), "BerlinMOD", "topright")

    cpuComparison("results/berlinmod_online_tun_2.txt", "results/berlinmod_buffering_tun_2.txt", "l", c("n", "g"),
        c(4, 100), "BerlinMOD", "center")

    cpuComparison("results/berlinmod_online_tun_2.txt", "results/berlinmod_buffering_tun_2.txt", "g", c("n", "l"),
        c(4, 8), "BerlinMOD")
}

generateAllBrinkhoff <- function() {
    resourceComparison("results/performance_log_brinkhoff_o_n4l8g200t10.txt",
        "results/performance_log_brinkhoff_b_n4l8g200t10.txt", "d", "Brinkhoff")

    cpuComparison("results/brinkhoff_online_tun_2.txt", "results/brinkhoff_buffering_tun_2.txt", "n", c("l", "g"),
        c(8, 200), "Brinkhoff", "topright")

    cpuComparison("results/brinkhoff_online_tun_2.txt", "results/brinkhoff_buffering_tun_2.txt", "l", c("n", "g"),
        c(4, 200), "Brinkhoff", "center")

    cpuComparison("results/brinkhoff_online_tun_2.txt", "results/brinkhoff_buffering_tun_2.txt", "g", c("n", "l"),
        c(4, 8), "Brinkhoff")
}

generateAllTdrive <- function() {
    resourceComparison("results/performance_log_tdrive_o_n4l20g100t295_tun_2.txt",
        "results/performance_log_tdrive_b_n4l20g100t295_tun_2.txt", "d", "TDrive")

    cpuComparison("results/tdrive_online_tun_2.txt", "results/tdrive_buffering_tun_2.txt", "n", c("l", "g"), c(8, 100),
        "TDrive", "topright")

    cpuComparison("results/tdrive_online_tun_2.txt", "results/tdrive_buffering_tun_2.txt", "l", c("n", "g"), c(4, 100),
        "TDrive", "center")

    cpuComparison("results/tdrive_online_tun_2.txt", "results/tdrive_buffering_tun_2.txt", "g", c("n", "l"), c(4, 8),
        "TDrive")
}

generateAllThreads <- function() {
    threadEval("results/tdrive_thread_eval.txt", c("l", "g"), c("4", "100"), "TDrive")
    threadEval("results/berlinmod_thread_eval.txt", c("l", "g"), c("8", "200"), "BerlinMOD")
    threadEval("results/trucks_thread_eval.txt", c("l", "g"), c("4", "0.8"), "Trucks")
    threadEval("results/brinkhoff_thread_eval.txt", c("l", "g"), c("4", "200"), "Brinkhoff")
}

generateAllComplete <- function() {
    completeEval("results/trucks_complete_eval.txt", "n", c("l", "g"), c(20, 1.5), "Trucks", "topright")
    completeEval("results/trucks_complete_eval.txt", "l", c("n", "g"), c(4, 1.5), "Trucks", "bottomright")
    completeEval("results/trucks_complete_eval.txt", "g", c("n", "l"), c(4, 20), "Trucks")

    completeEval("results/berlinmod_complete_eval.txt", "n", c("l", "g"), c(8, 100), "BerlinMOD", "topright")
    completeEval("results/berlinmod_complete_eval.txt", "l", c("n", "g"), c(4, 100), "BerlinMOD", "right")
    completeEval("results/berlinmod_complete_eval.txt", "g", c("n", "l"), c(4, 8), "BerlinMOD")

    completeEval("results/tdrive_complete_eval.txt", "n", c("l", "g"), c(8, 100), "TDrive", "topright")
    completeEval("results/tdrive_complete_eval.txt", "l", c("n", "g"), c(4, 100), "TDrive", "right")
    completeEval("results/tdrive_complete_eval.txt", "g", c("n", "l"), c(4, 8), "TDrive")

    completeEval("results/brinkhoff_complete_eval.txt", "n", c("l", "g"), c(8, 200), "Brinkhoff", "topright")
    completeEval("results/brinkhoff_complete_eval.txt", "l", c("n", "g"), c(4, 200), "Brinkhoff", "right")
    completeEval("results/brinkhoff_complete_eval.txt", "g", c("n", "l"), c(4, 8), "Brinkhoff")
}

genAll <- function() {
    generateAllTdrive()
    generateAllBerlinMOD()
    generateAllBrinkhoff()
    generateAllTrucks()
}
