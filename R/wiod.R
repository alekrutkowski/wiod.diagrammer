#' @import magrittr data.table
NULL

#' Load WIOD data from a WIOD .RData file
#'
#' Loads WIOD data for a given year from an official WIOD .Rdata file
#' (release 2016), which has to be first downloaded from
#' \url{http://www.wiod.org/protected3/data16/wiot_ROW/wiot_r_Nov16.zip}
#' and extracted (unzipped).
#'
#' @param file_name A character string giving the name of the file. Default:
#' \code{"WIOT2014_October16_ROW.RData"}. The file must contain an object
#' named \code{wiot}.
#'
#' @return The \code{wiot} object contained in the .Rdata file
#' (a \code{\link[data.table]{data.table}}).
#'
#' @export
loadWIOD <- function(file_name='WIOT2014_October16_ROW.RData') {
    message('Loading ',quo(file_name),'...')
	stopifnot(file.exists(file_name))
    load(file_name)
    wiot # check if the object exists
    `wiot column names` <- colnames(wiot)
    stopifnot(ncol(wiot)==2690,
              nrow(wiot)==2472,
              'IndustryCode' %in% `wiot column names`,
              'IndustryDescription' %in% `wiot column names`,
              'Country' %in% `wiot column names`,
              'RNr' %in% `wiot column names`,
              'Year' %in% `wiot column names`,
              wiot$RNr %>% inRanges(1:56, 65:71, 73))
    checkCountries(wiot)
    data.table::setattr(wiot, 'isWIOD',
                        TRUE)
    wiot[, RNr := RNr %>% as.integer]
    data.table::setattr(wiot, 'Year',
                        wiot$Year %>%
                            unique %>%
                            as.integer)
    wiot[, Year := NULL]
    wiot
}

inRange <- function(numvec, x, y)
    x<=numvec & numvec<=y

inRanges <- function(numvec, ...)
    list(...) %>%
    Reduce(function(p,x)
        p | numvec %>% inRange(min(x),max(x)),
        x=.,
        init=FALSE) %>%
    all

`%not.in%` <- Negate(`%in%`)

checkCountries <- function(wiot) {
    countries_db <-
        countries()$Country
    wiod_countries <-
        wiot$Country %>%
        unique
    err <- function(txt, logvec, vec)
        if (any(logvec))
            stop(paste(txt,'country/countries:\n'),
                 paste(vec[logvec],
                       collapse=', '),
                 call.=FALSE)
    err('Missing',
        countries_db %not.in% wiod_countries,
        countries_db)
    err('Unknown',
        wiod_countries %not.in% countries_db,
        wiod_countries)
}

#' A table of WIOD countries (their 3-letter codes and full names)
#'
#' A \code{\link[data.table]{data.table}} with three columns:
#' \describe{
#'   \item{\code{Country}}{Three-letter ISO country code, as in the WIOD file (character vector)}
#'   \item{\code{CountryLab}}{Full name of a country (character vector)}
#'   \item{\code{isEUmember}}{Is a country a member of the EU (2017)? (logical vector)}
#' }
#'
#' @export
countries <- function()
    c("AUS" = "Australia",
      "AUTeu" = "Austria",
      "BELeu" = "Belgium",
      "BGReu" = "Bulgaria",
      "BRA" = "Brazil",
      "CAN" = "Canada",
      "CHE" = "Switzerland",
      "CHN" = "China",
      "CYPeu" = "Cyprus",
      "CZEeu" = "Czech Republic",
      "DEUeu" = "Germany",
      "DNKeu" = "Denmark",
      "ESPeu" = "Spain",
      "ESTeu" = "Estonia",
      "FINeu" = "Finland",
      "FRAeu" = "France",
      "GBReu" = "United Kingdom",
      "GRCeu" = "Greece",
      "HRVeu" = "Croatia",
      "HUNeu" = "Hungary",
      "IDN" = "Indonesia",
      "IND" = "India",
      "IRLeu" = "Ireland",
      "ITAeu" = "Italy",
      "JPN" = "Japan",
      "KOR" = "Korea",
      "LTUeu" = "Lithuania",
      "LUXeu" = "Luxembourg",
      "LVAeu" = "Latvia",
      "MEX" = "Mexico",
      "MLTeu" = "Malta",
      "NLDeu" = "Netherlands",
      "NOR" = "Norway",
      "POLeu" = "Poland",
      "PRTeu" = "Portugal",
      "ROUeu" = "Romania",
      "ROW" = "Rest of the World",
      "RUS" = "Russian Federation",
      "SVKeu" = "Slovak Republic",
      "SVNeu" = "Slovenia",
      "SWEeu" = "Sweden",
      "TOT" = "TOTAL",
      "TUR" = "Turkey",
      "TWN" = "Taiwan",
      "USA" = "United States") %>%
    data.table::data.table(CountryLab = .,
                           Country = names(.)) %>%
    `[`(, isEUmember := grepl('...eu',Country)) %>%
    `[`(, Country := substr(Country,1,3))

#' Extract sector codes and labels from a WIOD data.table
#'
#' @param wiot WIOD data.table returned by \code{\link[wiod.diagrammer]{loadWIOD}}
#'
#' @return A \code{\link[data.table]{data.table}} with four columns:
#' \describe{
#'   \item{\code{SectorNr}}{WIOD numeric codes for industries and final use sectors (integer vector)}
#'   \item{\code{SectorLab}}{Full names of the WIOD sectors (character vector)}
#'   \item{\code{SectorCode}}{Short alphanumeric sector codes used in WIOD (character vector)}
#'   \item{\code{isFinal}}{Is a sector an industry (i.e. one of the 56 primary, secondary/manufacturing,
#'   or tertiary/services sectors that produce something) or a  final use sector
#'   (i.e. one of the 5 final consumption or investment sectors)? (logical vector)}
#' }
#'
#' @export
sectors <- function(wiot) {
    (wiot %>%
         # stopIfNotWIOD %>%
         `[`(RNr<=56) %>%
         RowSectorsWithLabels %>%
         data.table::setnames(c('RNr','IndustryCode','IndustryDescription'),
                              c('SectorNr','SectorCode','SectorLab')) %>%
         rbind(NonIndustryColSectors, fill=TRUE) %>%
         `[`(, isFinal := SectorNr>56))[] # due to bug: https://stackoverflow.com/questions/32988099/data-table-objects-not-printed-after-returned-from-function
}

#' Extract the aggregate variables from a WIOD data.table
#'
#' @param wiot WIOD data.table returned by \code{\link[wiod.diagrammer]{loadWIOD}}
#'
#' @return A \code{\link[data.table]{data.table}} with 10 columns
#' (all numeric, in US dollars as in the WIOD source file, except for \code{SectorNr} and\code{Country}):
#' \describe{
#'   \item{\code{SectorNr}}{WIOD numeric codes for industries and final use sectors (integer vector)}
#'   \item{\code{Country}}{Three-letter ISO country code, as in the WIOD file (character vector)}
#'   \item{\code{II_fob}}{Total intermediate consumption}
#'   \item{\code{TXSP}}{Taxes less subsidies on products}
#'   \item{\code{EXP_adj}}{CIF/FOB adjustments on exports}
#'   \item{\code{PURR}}{Direct purchases abroad by residents}
#'   \item{\code{PURNR}}{Purchases on the domestic territory by non-residents }
#'   \item{\code{VA}}{Value added at basic prices}
#'   \item{\code{IntTTM}}{International Transport Margins}
#'   \item{\code{GO}}{Output at basic prices}
#' }
#'
#' @export
aggregates <- function(wiot) {
    wiot %>% stopIfNotWIOD
    w <- wiot[RNr>56,
              colnames(wiot) %not.in%
                  c('IndustryCode','IndustryDescription','Country','RNr') %>%
                  which,
              with=FALSE]
    wc <- colnames(w)
    data.table::transpose(w) %>%
        data.table::setnames(colnames(.),
                             wiot[RNr>56]$IndustryCode) %>%
        cbind(data.table::data.table(SectorNr = wc %>% industryPostfix,
                                     Country = wc %>% countryPrefix),
              .) %>%
        `[`(Country != 'TOT')
}

isWIOD <- function(x)
    x %>%
    attr('isWIOD') %>%
    falseIfNull

stopIfNotWIOD <- function(wiot)
    if (wiot %>% isWIOD)
        wiot else
            stop("This is not a WIOD dataset ",
                 'returned by the function `loadWIOD`.',
                 call.=FALSE)

data.table. <- function(x, ...) # magrittr-pipe-friendly
    data.table::data.table(...)

NonIndustryColSectors <-
    c('57' = 'CONS_h -- Final consumption expenditure by households',
      '58' = 'CONS_np -- Final consumption expenditure by non-profit organisations serving households (NPISH)',
      '59' = 'CONS_g -- Final consumption expenditure by government',
      '60' = 'GFCF -- Gross fixed capital formation',
      '61' = 'INVEN -- Changes in inventories and valuables') %>%
    data.table.(SectorNr = names(.) %>% as.integer,
                SectorCode = sub('^(.*) -- .*$','\\1',.),
                SectorLab = sub('^.* -- (.*)$','\\1',.))

RowSectorsWithLabels <- function(wiot)
    wiot %>%
    `[`(, list(RNr, IndustryDescription, IndustryCode)) %>%
    unique

info <- function(x, ...) { # magrittr-pipe-friendly
    message(...)
    x
}

countryPrefix <- function(charvec)
    charvec %>%
    substr(1, 3)

industryPostfix <- function(charvec)
    charvec %>%
    substr(4, nchar(.)) %>%
    {suppressWarnings(as.integer(.))} # no warning on NAs due to empty strings

`%=/=%` <- Negate(identical)

falseIfNull <- function(x)
    if (x %>% is.null)
        FALSE else x

#' Transform the WIOD data.table into a flat/long data.table
#'
#' @param wiot WIOD data.table returned by \code{\link[wiod.diagrammer]{loadWIOD}}
#' @param rows \code{wiot}'s rows to be dropped (a numeric vector with negative numbers) or
#' kept (a numeric vector with positive) prior to flattening/reshaping;
#' default: all rows kept
#' @param columns \code{wiot}'s columns to be dropped (a numeric vector with negative numbers) or
#' kept (a numeric vector with positive) prior to flattening/reshaping;
#' default: all columns kept
#'
#' @return A \code{\link[data.table]{data.table}} where each row corresponds to
#' a combination of a source (exporting) and destination (importing) sectors
#' and countries, with ten columns:
#' \describe{
#'   \item{\code{ExpSectorNr}}{Exporting sector -- WIOD numeric codes for industries (integer vector),
#'   see also \code{\link[wiod.diagrammer]{sectors}}}
#'   \item{\code{ExpCountry}}{Exporting country -- a three-letter ISO country code,
#'   as in the WIOD file (character vector), see also \code{\link[wiod.diagrammer]{countries}}}
#'   \item{\code{value}}{An absolute value of the flow (in US dollars as in the WIOD source file)}
#'   \item{\code{ImpSectorNr}}{Importing sector -- WIOD numeric codes for industries and
#'   final use sectors (integer vector), see also \code{\link[wiod.diagrammer]{sectors}}}
#'   \item{\code{ImpCountry}}{Importing country -- a three-letter ISO country code,
#'   as in the WIOD file (character vector), see also \code{\link[wiod.diagrammer]{countries}}}
#' }
#'
#' @export
flatWIOD <- function(wiot, rows=seq_len(dim(wiot)[1]), columns=seq_len(dim(wiot)[2])) {
    wiot %>% stopIfNotWIOD
    stopifnot(rows %>% is.numeric,
              columns %>% is.numeric,
              all(rows==as.integer(rows)),
              all(columns==as.integer(columns)),
              all(rows>0) | all(columns<0),
              all(columns>0) | all(columns<0))
    wiot2 <-
        wiot[RNr<=56] %>% # removing non-industry rows
        `if`(rows %=/=% seq_len(dim(wiot)[1]) | columns %=/=% seq_len(dim(wiot)[2]),
             info(.,'Filtering rows and/or columns...') %>%
                 `[`(rows[abs(rows) %>% inRange(1,dim(wiot)[1])],
                     columns[abs(columns) %>% inRange(4,dim(wiot)[2])],
                     with=FALSE), .)
    suppressWarnings(wiot2[, c('IndustryCode','IndustryDescription','TOT') := NULL]) # supprWarn. in case TOT deleted with the `columns` arg.
    message('Reshaping into long/flat format...')
    data.table::melt(wiot2,
                     id.vars=c('RNr','Country'),
                     variable.name='ColName') %>%
        data.table::setnames(c('RNr','Country'),
                             c('ExpSectorNr','ExpCountry')) %>%
        `[`(, ImpSectorNr := ColName %>% as.character %>% industryPostfix) %>%
        `[`(, ImpCountry := ColName %>% as.character %>% countryPrefix) %>%
        `[`(, ColName := NULL) %>%
        data.table::setattr('isFlatWIOD', TRUE)
}

#' Find top (or bottom) n values with possible ties
#'
#' A helper function -- tie-robust-rank cut-off
#'
#' @param numvec A numeric vector
#' @param nth A single number (a constant i.e. vector of length 1)
#'
#' @return A logical vector of the same length as \code{numvec}
#'
#' @examples
#' x <- c(1,1,2,2,2,3,3)
#' p <- function(d) # Just for clarity
#'     print(`colnames<-`(d, paste0(colnames(d),' ')),
#'           right=FALSE,
#'           row.names=FALSE)
#' p(data.frame(x,
#'              rank(x) <= 2,
#'              tieRobustRankLessOrEqual(x, 2),
#'              check.names = FALSE))
#' #  x  rank(x) <= 2  tieRobustRankLessOrEqual(x, 2)
#' #  1   TRUE          TRUE
#' #  1   TRUE          TRUE
#' #  2  FALSE          TRUE
#' #  2  FALSE          TRUE
#' #  2  FALSE          TRUE
#' #  3  FALSE         FALSE
#' #  3  FALSE         FALSE
#' @export
tieRobustRankLessOrEqual <- function(numvec, nth) {
    stopifnot(numvec %>% is.numeric,
              nth %>% is.numeric,
              length(nth)==1)
    r <-
        numvec %>%
        rank
    threshold <-
        r %>%
        unique %>%
        sort %>%
        `[`(min(length(.),
                nth)) # so that no NAs if vector shorter than nth
    r <= threshold
}

findPartnersOfPartners <- function(List,
                                   partners,
                                   full_df,
                                   start_countries,
                                   start_sectors,
                                   selectionFun,
                                   by,
                                   isFinalRound) {
    first_round <-
        List$accum_df %>%
        is.null
    idvars <-
        c('SectorNr','Country')
    prefix <-
        c('Imp', 'Exp') %>%
        `if`(partners=='suppliers',
             .,
             rev(.))
    originX <-
        prefix[1] %>%
        paste0(idvars)
    partnerY <-
        prefix[2] %>%
        paste0(idvars)
    top_partners <-
        List$previous_df %>%
        info('Selecting ',partners,' (country and sector combinations)',
             `if`(by=="``", "",
                  paste('\nfor each combination of:', by)),
             '...') %>%
        `if`(first_round,
             (.) %>%
                 `[`(eval(bquote(.(originX[1] %>% as.symbol))) %in%
                         start_sectors) %>%
                 `[`(eval(bquote(.(originX[2] %>% as.symbol))) %in%
                         start_countries),
             .) %>%
        .[(.[, .I[selectionFun[[1]][[1]](.SD) %>%
                      `if`(is.logical(.), .,
                           stop('The selection function does not return a logical (Boolean) vector.\n',
                                'It returns ',class(.),'.', call.=FALSE))],
             by = by])$V1] # following https://stackoverflow.com/a/16574176
    partners_of_partners <-
        `if`(!isFinalRound,
             top_partners%>%
                 `[`(, partnerY, with=FALSE)  %>%
                 unique %>%
                 data.table::setnames(partnerY,
                                      originX) %>%
                 merge(full_df, # get partners of previous-round partners
                       by = originX))
    list(previous_df =
             partners_of_partners ,
         accum_df =
             rbind(List$accum_df, top_partners)%>%
             unique %>%
             info('Accumulated ',nrow(.),' unique linkages.'))
}

allIntegers <- function(numvec)
    all(numvec==as.integer(numvec))

`%and%` <- function(cond, x)
    `if`(cond, x, FALSE)

hasNumericColumn <- function(dt, cname)
    cname %in% colnames(dt) %and%
    (dt[[cname]] %>% is.numeric)

hasNumericColumnWithOnlyWiodSectorNumbersNamed <- function(dt, cname)
    dt %>% hasNumericColumn(cname) %and%
    all(dt[[cname]] %>% inRange(1,61))

hasCharacterColumn <- function(dt, cname)
    cname %in% colnames(dt) %and%
    (dt[[cname]] %>% is.character)

has3LetterColumn <- function(dt, cname)
    dt %>% hasCharacterColumn(cname) %and%
    (max(nchar(dt[[cname]]))==3) %and%
    (min(nchar(dt[[cname]]))==3)

areAllFunctionsWith1argument <- function(List)
    List %>%
    sapply(. %>% isFunctionWith1Argument) %>%
    all

#' Find the specific linkages in the WIOD
#'
#' @param partners String: either \code{"suppliers"} or \code{"users"}.
#'
#' @param flat_wiod "Flat" WIOD \code{\link[data.table]{data.table}} returned by
#' \code{\link[wiod.diagrammer]{flatWIOD}}, possibly modified by a user
#' (e.g. with added columns to be used through \code{by} or with some irrelevant linkages/rows removed).
#' None of the basic 5 columns returned by \code{\link[wiod.diagrammer]{flatWIOD}}
#' must be removed.
#'
#' @param start_countries Character vector: a three-letter ISO country code,
#' as in the WIOD file (character vector), see also \code{\link[wiod.diagrammer]{countries}},
#' of the countries whose top partners are to be found in the first round.
#'
#' @param start_sectors Numeric vector: WIOD numeric codes (integers) for industries and
#' final use sectors, see also \code{\link[wiod.diagrammer]{sectors}},
#' of the sectors whose top partners are to be found in the first round.
#'
#' @param ListOfselectionFuns List (with at least 1 element) of functions, where each
#' function takes \code{flat_wiod} as an argument and returns a logical vector:
#' which partners should be selected at each round.
#'
#' @param by Optional character vector: the names of the \code{flat_wiod}'s columns containing
#' the groupings by which the partners will be selected. Default: none i.e.
#' empty string (\code{""}). Default: importing country and sector (when extracting suppliers)
#' or exporting country and sector (when extracting customers).
#'
#' @return A \code{\link[data.table]{data.table}} (with added S3 class 'SelectedLinksDT' with
#' a corresponding \code{\link[wiod.diagrammer]{plotLinks}} implementation) with the same columns
#' as in \code{flat_wiod} (but ordered differently), with only those rows that represent
#' the top direct and (possibly) indirect linkages.
#'
#' This function extracts the first-round (direct) and further (indirect)
#' backward or forward linkages (i.e. suppliers of suppliers of suppliers etc. or
#' customers of customers of customers etc.), based on the functions in \code{ListOfselectionFuns}
#' which take \code{flat_wiod} and for each combination of the dimensions specified in \code{by}
#' return a logical (Boolean) vector which determines if a given partner is selected or not.
#'
#' The direction of the flows (backward or forward) is determined by the string argument
#' \code{partners} which must be either \code{"suppliers"} or \code{"users"}.
#'
#' The number of rounds is determined by the length of \code{ListOfselectionFuns} i.e.
#' if this vector contains e.g. two functions, \code{findLinks} will extract the direct linkages and
#' one layer of indirect linkages. The functions in the \code{breadth_vec} vector determine
#' which partners selected at round (for each combination/grouping as described above).
#'
#' The starting point of the search for top partners is determined by the arguments
#' \code{start_countries} and \code{start_sectors} -- the partners of those countries and
#' sectors will be searched in the first round.
#'
#' @export
findLinks <- function(partners,
                      flat_wiod,
                      start_countries,
                      start_sectors,
                      ListOfselectionFuns,
                      by = `if`(partners=='suppliers',
                                c('ImpCountry','ImpSectorNr'),
                                c('ExpCountry','ExpSectorNr'))) {
    stopifnot(partners %>% is.character,
              length(partners)==1,
              partners=='suppliers' | partners=='users',
              flat_wiod %>% isDataTable,
              flat_wiod %>% has3LetterColumn('ExpCountry'),
              flat_wiod %>% has3LetterColumn('ImpCountry'),
              flat_wiod %>% hasNumericColumnWithOnlyWiodSectorNumbersNamed('ImpSectorNr'),
              flat_wiod %>% hasNumericColumnWithOnlyWiodSectorNumbersNamed('ExpSectorNr'),
              flat_wiod %>% hasNumericColumn('value'),
              start_countries %>% is.character,
              all(start_countries %in% countries()$Country),
              start_sectors %>% is.numeric,
              all(start_sectors %>% inRange(1,61)),
              ListOfselectionFuns %>% is.list,
              length(ListOfselectionFuns)>=1,
              ListOfselectionFuns %>% areAllFunctionsWith1argument,
              by %>% is.character,
              all(by %in% colnames(flat_wiod)) | by=="")
    Reduce(function(previous_result, x)
        previous_result %>%
            info('Round ',x$round,'...') %>%
            findPartnersOfPartners(partners,
                                   full_df = flat_wiod,
                                   start_countries,
                                   start_sectors,
                                   selectionFun = x$selectionFun,
                                   by = by %>%
                                       paste0('`',.,'`') %>%
                                       paste(collapse=','),
                                   isFinalRound = x$round==length(ListOfselectionFuns)),
        init =
            list(previous_df = flat_wiod),
        x =
            ListOfselectionFuns %>%
            lapply(list) %>%
            data.table.(selectionFun = I(.),
                        round = seq_along(.)) %>%
            split(row.names(.))) $ accum_df
}



