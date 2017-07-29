
##' Run Statistical Analysis of Monthly Background Checks of Gun Purchase
##'
##' @param verbose Optional boolean switch to indicate whether verbose operation is
##' desired, default is \sQuote{FALSE}
##' @param debug Optional boolean switch to indicate whether interim data is displayed;
##' default is \sQuote{FALSE}
##'
##' @return A \code{data.frame} is returned, contained all different prepared columns.
##'
##' @author Gregor Aisch and Josh Keller wrote the (initial) R code; Dirk Eddelbuettel created
##' and maintains the \code{gunsales} package as well as this package.
##' @seealso The NY Times article presenting this analsysi undertaken by the \code{gunsales} package is
##' at \url{http://www.nytimes.com/interactive/2015/12/10/us/gun-sales-terrorism-obama-restrictions.html?}
##'
##' @examples
##' \dontrun{
##'   gs <- analysis()
##'   gunsales::plot_gunsales(gs)
##'   gunsales::ggplot_gunsales(gs)
##' }
analysis <- function(debug=FALSE, verbose=FALSE) {
    .hasData()

    alldata <- fread(system.file("rawdata/ncis_bystate_bymonth_bytype.csv", package="gunsales"))
    poptotal <- fread(system.file("rawdata/population.csv", package="gunsales"))

    alldata[, guns_sold := (handgun + longgun) * 1.1 + multiple_corrected * 2]

    total <- state_ts(alldata, "Totals")
    totalSeas <- final(seas(total))

    poptotalTS <- ts(poptotal[year >= 2000, list(res_pop)], start=c(2000,1), frequency=12)

    totalSeasPop <- totalSeas / poptotalTS * 1000
    totalSeasScaled <- totalSeas / 280726

    ## create a new data frame that eventually stores all the data we need in the final piece
    out_data <- data.table(year = as.numeric(floor(time(total))),
                           month = as.numeric(round(1+(time(total) - floor(time(total))) * 12)),
                           guns_total = round(as.vector(total), 3))
    ## expand the data.frame, adding more volumns
    out_data[, `:=`(guns_total_seas=as.vector(totalSeas),
                    guns_total_per_1000=round(as.vector(totalSeasPop), digits=3),
                    guns_total_per_1000_scaled=round(as.vector(totalSeasScaled), digits=3))]

    ## create a temporary matrix for computing the handgun_share and longgun_share columns
    out_data[, `:=`(handgun=final(seas(state_ts(alldata, "Totals", "handgun"))),
                    longgun=final(seas(state_ts(alldata, "Totals", "longgun"))),
                    #other=final(seas(state_ts(alldata, "Totals", "other"))),   ## TODO: needs work
                    multiple=final(seas(state_ts(alldata, "Totals", "multiple_corrected"))))]
    ot <- with(alldata[state=="Totals" & year >= 2000, list(year=year, month=month.num, value=other)], ts(value, start=c(year[1], month[1]), frequency=12))
    ot[ot==0] <- NA
    out_data[, other:=as.vector(ot)]
    out_data[ is.na(other), other:=0]

    out_data[, `:=`(handgun_share=round(handgun / (handgun+longgun+other+multiple*0.5), 4),
                    longgun_share=round(longgun / (handgun+longgun+other+multiple*0.5), 4))]

    ## plot percent of national for selected states
    show_states <- c('New Jersey', 'Maryland', 'Georgia', 'Louisiana', 'Mississippi', 'Missouri')
    for (s in show_states) {
        out_data <- out_data[ ts2dt(state_data(alldata, s, total, totalSeas), s), on=c("year", "month")]
    }

    ## compute handgun sales for DC: handung * 1.1 + multiple
    dchandgun_checks <- state_ts(alldata, "District of Columbia", "handgun", outer_zeros_to_na=FALSE)
    dcmultiple <- state_ts(alldata, "District of Columbia", "multiple", outer_zeros_to_na=FALSE)
    dchandgun <- final(seas(dchandgun_checks * 1.1 + dcmultiple + 1)) - 1
    totalHandgun <- final(seas(state_ts(alldata, 'Totals', 'handgun') * 1.1 + state_ts(alldata, 'Totals', 'multiple')))
    dchandgunPct <- dchandgun / totalHandgun * 100000

    ## merge with out_data
    out_data <- out_data[ ts2dt(dchandgunPct, "dc_handguns_per_100k_national_sales"), on=c("year", "month")]

    missouri <- state_data(alldata, 'Missouri', normalize = FALSE, adj_seasonal = FALSE)
    miss.avg_pre_2007 <- mean(missouri[73:84])
    miss.avg_post_2008 <- mean(missouri[97:108])
    if (verbose)
        cat("Increase in monthly gun sales in Missouri =",  round(miss.avg_post_2008 - miss.avg_pre_2007, digits=2), "\n")

    setnames(out_data, show_states, tolower(gsub(" ", "_", show_states)))  # plot functions expects lowercase
    as.data.frame(out_data)
}

## R-devel CMD check still whines about these:
"column" <- "guns_sold" <- "handgun" <- "longgun" <- "month.num" <- "multiple" <- NULL
"multiple_corrected" <- "other" <- "res_pop" <- "show_statesb" <- "state" <- "year" <- NULL

replace_outer_zeros <- function(x) {
    for (i in 1:length(x)) if (x[i] != 0) break else x[i] <- NA
    for (i in length(x):1) if (x[i] != 0) break else x[i] <- NA
    x
}
state_ts <- function(data, statename, column="guns_sold", outer_zeros_to_na=TRUE) {
    d <- data[state == statename & (year >= 2000), c("year", "month.num", column), with=FALSE]
    setnames(d, c("year", "month", "value"))
    series <- with(d, ts(value, start=c(year[1],month[1]), frequency = 12))
    if (outer_zeros_to_na) series <- replace_outer_zeros(series)
    series
}
state_data <- function(all, statename, total, totalSeas,  normalize=TRUE, adj_seasonal=TRUE, column="guns_sold") {
    state <- state_ts(all, statename, column)
    if (adj_seasonal) {
        pct <- final(seas(state))
        if (normalize) pct <- pct / totalSeas * 100
    } else {
        pct <- if (normalize) state / total * 100 else state
    }
    pct
}
ts2dt <- function(t, valueName='value') {
    df <- data.table(year=as.numeric(floor(time(t))),
                     month=as.numeric(round(1+(time(t) - floor(time(t))) * 12)),
                     value=as.matrix(t))
    setnames(df, c("year", "month", valueName))
    df
}
dt2ts <- function(dt) {
    d <- dt[, c("year", "month", column), with=FALSE]
    setnames(d, c("year", "month", "value"))
    series <- with(d, ts(value, start=c(year[1],month[1]), frequency = 12))
}
