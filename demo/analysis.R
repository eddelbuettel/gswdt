library(seasonal)
suppressMessages(library(data.table))

verbose <- FALSE

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

if (requireNamespace("gunsales", quietly=TRUE)) {
    alldata <- fread(system.file("rawdata/ncis_bystate_bymonth_bytype.csv", package="gunsales"))
    poptotal <- fread(system.file("rawdata/population.csv", package="gunsales"))
}

# alldata <- alldata %>% mutate(guns_sold=(handgun + longgun) * 1.1 + multiple_corrected * 2)
alldata[, guns_sold := (handgun + longgun) * 1.1 + multiple_corrected * 2]

# total <- alldata %>% state_ts('Totals', 'guns_sold')
#state_ts <- function(data, state_ts, column='guns_sold', outer_zeros_to_na=TRUE) {
#    d <- data %>%
#        filter(state == state_ts & (year >= 2000)) %>%
#        arrange(year, month.num) %>%
#        select_('year', month='month.num', value=column)
#    # d$value[d$value == 0] <- NA
#    series <- ts(d$value, start=c(d$year[1],d$month[1]), end=c(last(d$year), last(d$month)), frequency = 12)
#    if (outer_zeros_to_na) series <- replace_outer_zeros(series)
#    series
#}
#series <- ts(d$value, start=c(d$year[1],d$month[1]), end=c(last(d$year), last(d$month)), frequency = 12)
#total <- with(alldata[ state=="Totals" & year >= 2000, .(year=year, month=month.num, value=guns_sold)],
#              ts(value, start=c(year[1], month[1]), frequency=12))
total <- state_ts(alldata, "Totals")
# totalSeas <- total %>% seas %>% final
totalSeas <- final(seas(total))



#poptotal <- poptotal %>%
#    filter(year >= 2000) %>%
#    #filter(year < 2015 | month <= 11) %>%
#    with(ts(res_pop, start=c(2000,1), frequency = 12))
poptotalTS <- ts(poptotal[year >= 2000, .(res_pop)], start=c(2000,1), frequency=12)


totalSeasPop <- totalSeas / poptotalTS * 1000
totalSeasScaled <- totalSeas / 280726


## create a new data frame that eventually stores all the
## data we need in the final piece
#out_data <- ts_to_dataframe(total, 'guns_total') %>%
#    mutate(guns_total=round(guns_total, 3))
out_data <- data.table(year = as.numeric(floor(time(total))),
                       month = as.numeric(round(1+(time(total) - floor(time(total))) * 12)),
                       guns_total = round(as.vector(total), 3))

## expand the data.frame, adding more volumns
#out_data <- data.frame(out_data,
#                       guns_total_seas=as.matrix(totalSeas),
#                       guns_total_per_1000=round(as.matrix(totalSeasPop), digits=3),
#                       guns_total_per_1000_scaled=round(as.matrix(totalSeasScaled), digits=3))
out_data[, `:=`(guns_total_seas=as.vector(totalSeas),
                guns_total_per_1000=round(as.vector(totalSeasPop), digits=3),
                guns_total_per_1000_scaled=round(as.vector(totalSeasScaled), digits=3))]

## create a temporary matrix for computing the
## handgun_share and longgun_share columns
## cbind works correctly here as it operates on timeseries object
#tmp <- cbind(final(seas(state_ts(alldata, 'Totals', 'handgun'))),
#             final(seas(state_ts(alldata, 'Totals', 'longgun'))),
#             final(seas(state_ts(alldata, 'Totals', 'other'))),
#             final(seas(state_ts(alldata, 'Totals', 'multiple_corrected'))))
#colnames(tmp) <- c('handgun', 'longgun', 'other', 'multiple')
out_data[, `:=`(handgun=final(seas(state_ts(alldata, "Totals", "handgun"))),
                longgun=final(seas(state_ts(alldata, "Totals", "longgun"))),
                #other=final(seas(state_ts(alldata, "Totals", "other"))),
                multiple=final(seas(state_ts(alldata, "Totals", "multiple_corrected"))))]
ot <- with(alldata[state=="Totals" & year >= 2000, .(year=year, month=month.num, value=other)], ts(value, start=c(year[1], month[1]), frequency=12))
ot[ot==0] <- NA
out_data[, other:=as.vector(ot)]
out_data[ is.na(other), other:=0]

out_data[, `:=`(handgun_share=round(handgun / (handgun+longgun+other+multiple*0.5), 4),
                longgun_share=round(longgun / (handgun+longgun+other+multiple*0.5), 4))]


## plot percent of national for selected states
show_states <- c('New Jersey', 'Maryland', 'Georgia',
                 'Louisiana', 'Mississippi', 'Missouri')
for (s in show_states) {
    out_data <- out_data[ ts2dt(state_data(alldata, s, total, totalSeas), s), on=c("year", "month")]
}

## compute handgun sales for DC: handung * 1.1 + multiple
#out_data <- out_data[ ts2dt(state_ts(alldata, "District of Columbia", "handgun", outer_zeros_to_na=FALSE), "dchandgun_checks"), on=c("year", "month")]
#out_data <- out_data[ ts2dt(state_ts(alldata, 'District of Columbia', 'multiple', outer_zeros_to_na=FALSE), "dcmultiple"), on=c("year", "month")]
#out_data <- out_data[ ts2dt(final(seas(state_ts(out_data, "dchandgun_checks") * 1.1 + state_ts(out_data, "dcmultiple") + 1)) - 1), on=c("year", "month")]
dchandgun_checks <- state_ts(alldata, "District of Columbia", "handgun", outer_zeros_to_na=FALSE)
dcmultiple <- state_ts(alldata, "District of Columbia", "multiple", outer_zeros_to_na=FALSE)
dchandgun <- final(seas(dchandgun_checks * 1.1 + dcmultiple + 1)) - 1
totalHandgun <- final(seas(state_ts(alldata, 'Totals', 'handgun') * 1.1 + state_ts(alldata, 'Totals', 'multiple')))
dchandgunPct <- dchandgun / totalHandgun * 100000

## merge with out_data
out_data <- out_data[ ts2dt(dchandgunPct, "dc_handguns_per_100k_national_sales"), on=c("year", "month")]
#temp <- ts_to_dataframe(round(dchandgunPct, 1), 'dc_handguns_per_100k_national_sales')
#out_data <- data.frame(out_data, temp[,3,drop=FALSE])



missouri <- state_data(alldata, 'Missouri', normalize = FALSE, adj_seasonal = FALSE)
missouri.avg_pre_2007 <- mean(missouri[73:84])
missouri.avg_post_2008 <- mean(missouri[97:108])
if (verbose)
    cat("Increase in monthly gun sales in Missouri =",
        round(missouri.avg_post_2008 - missouri.avg_pre_2007, digits=2), "\n")
