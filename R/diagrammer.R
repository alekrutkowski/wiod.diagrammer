#' @import magrittr
NULL

quo <- function(charvec)
    charvec %>%
    paste0('"',.,'"')


#' Rescale values to the [0,1] range
#'
#' A helper function -- normalise/rescale values so that they fit
#' into a range from 0 to 1. (The minimum value becomes 0 and the
#' maximum value becomes 1).
#'
#' @param numvec A numeric vector
#'
#' @return A numeric vector with values in the range [0,1].
#'
#' @examples
#' x <- seq(from=-3.7, to=6.5, by=1.36)
#' y <- normalise(x)
#' data.frame(x,y)
#' #     x          y
#' # -3.70  0.0000000
#' # -2.34  0.1428571
#' # -0.98  0.2857143
#' #  0.38  0.4285714
#' #  1.74  0.5714286
#' #  3.10  0.7142857
#' #  4.46  0.8571429
#' #  5.82  1.0000000
#' @export
normalise <- function(numvec) {
    stopifnot(numvec %>% is.numeric)
    min <- min(numvec, na.rm=TRUE)
    spread <- max(numvec, na.rm=TRUE) - min
    (numvec - min)/spread
}

formatOptions <- function(...)
    paste(..., sep=',') %>%
    gsub('(,)+', ",", .) %>% # to avoid empty spaces between commas for empty strings
    paste0('[',.,']') %>%
    sub('[,',
        '[',.,fixed=TRUE) %>% # to avoid the initial empty space before comma
    sub(',]',
        ']',.,fixed=TRUE) # to avoid the final empty space after comma

isString <- function(s)
    is.character(s) & length(s)==1

isNumericConstant <- function(n)
    n %>% is.numeric &
    length(n)==1

numberOfArguments <- function(f)
    f %>% formals %>% length

ifelse. <- function(x, ...) # magrittr-pipe-friendly ifelse
    ifelse(...)

paste. <- function(x, ...) # magrittr-pipe-friendly paste
    paste(...)

asLabel <- function(charvec)
    charvec %>%
    quo %>%
    paste0('label=',.)

wrapText <- function(charvec, numofchars)
    charvec %>%
    strwrap(numofchars, simplify=FALSE) %>%
    sapply(paste, collapse='\\n')

isDataTable <- data.table::is.data.table

stopifnot. <- function(x, expr) # magrittr-pipe-friendly stopifnot
    `if`(!expr,
         stop(deparse(bquote(.(substitute(expr)))),
              call.=FALSE),
         x)

`%and%` <- function(cond, x)
    `if`(cond, x, FALSE)

has3letterCountry <- function(dt)
    'Country' %in% colnames(dt) %and%
    (dt$Country %>% is.character) %and%
    (max(nchar(dt$Country))==3) %and%
    (min(nchar(dt$Country))==3)

hasWiodSectorNr <- function(dt)
    'SectorNr' %in% colnames(dt) %and%
    (dt$SectorNr %>% is.numeric) %and%
    all(dt$SectorNr >= 1) %and%
    all(dt$SectorNr <= 73) %and%
    all(c(63,64) %not.in% dt$SectorNr)

hasCharacterColumn <- function(dt, cname)
    cname %in% colnames(dt) %and%
    (dt[[cname]] %>% is.character)

isFunctionWith1Argument <- function(f)
    f %>% is.function %and%
    (numberOfArguments(f)==1)

evalAndCheck <- function(val, fname, e, testf, ...)
    get(fname, envir=e)(val, ...) %>%
    `if`(!testf(.),
         stop('The value returned (`vr`) by the function `',fname,'`\n',
              'does not satisfy the following condition:\n',
              capture.output(body(testf)),'\n',
              capture.output(cat('The structure of `vr` is:')),'\n',
              capture.output(str(.)),'\n',
              call.=FALSE),
         .)

containsGraphVizAttributes <- function(x)
    x %>% is.character %and%
    (x %>%
         trimws %>%
         sapply(. %>% # All Graphviz attributes are specified by name-value pairs (separated by =)
                    # http://www.graphviz.org/doc/info/attrs.html
         {grepl(paste0('^[_[:alpha:]]+[ ]*\\=.+$'),.) | .==""}) %>%
         all)

collapseAndFormatOptions <- function(charvec)
    charvec %>%
    paste(collapse=',') %>%
    formatOptions

#' Plot a diagram (directed graph) of top linkages
#'
#' @param top_links_dt A data.table returned by \code{\link[wiod.diagrammer]{topLinks}}
#'
#' @param wiot WIOD data.table returned by \code{\link[wiod.diagrammer]{loadWIOD}}
#'
#' @param units Optional: a list of two named interrelated elements:
#' \describe{
#'   \item{\code{units_suffix}}{a string (character vector of length 1) with a name
#'   of the monetary units (default: \code{"bn USD"}). This element
#'   should be consistent with the next element \code{mln_USD_exchange_rate}!}
#'   \item{\code{mln_USD_exchange_rate}}{a single value (numeric vector of length 1)
#'   by which the original WIOD numbers (which are in million US dollars) will be
#'   multiplied (default: \code{0.001}, so that the values become billion US dollars).
#'   This element should be consistent with the previous element \code{units_suffix}!}
#' }
#'
#' @param country_labels_dt Optional: a \link[data.table]{data.table} with specific
#' columns -- see the documentation for \link[wiod.diagrammer]{countries} which is
#' the function called to produce the default value for \code{country_labels_dt}.
#' With this data.table, you may provide alternative (non-default) country names/labels
#' and/or additional country-level variables (columns) to be used by your (i.e. non-default)
#' functions for the arguments \code{nodeSizeFun}, \code{nodeLabelFun}, and
#' \code{specificNodeOptionsFun}.
#'
#' @param sector_labels_dt Optional: a \link[data.table]{data.table} with specific
#' columns -- see the documentation for \link[wiod.diagrammer]{sectors} which is
#' the function called to produce the default value for \code{country_labels_dt}
#' With this data.table, you may provide alternative (non-default) sector names/labels
#' and/or additional sector-level variables (columns) to be used by your (i.e. non-default)
#' functions for the arguments \code{nodeSizeFun}, \code{nodeLabelFun}, and
#' \code{specificNodeOptionsFun}.
#'
#' @param aggregates_dt Optional: a \link[data.table]{data.table} with specific
#' columns -- see the documentation for \link[wiod.diagrammer]{aggregates} which is
#' the function called to produce the default value for \code{aggregates_dt}.
#' With this data.table, you may provide additional or alternative country-and-sector-level
#' variables (columns) to be used by your (i.e. non-default) functions for the arguments
#' \code{nodeSizeFun}, \code{nodeLabelFun}, and \code{specificNodeOptionsFun}.
#'
#' @param arrowSizeFun Optional: a one-numeric-argument function which translates
#' \code{top_links_dt}'s \code{value} column into GraphViz's \code{penwidth} values
#' for arrows (edges), see \url{http://www.graphviz.org/content/attrs#dpenwidth}).
#'
#' @param arrowLabelFun Optional: a one-numeric-argument function which translates
#' \code{top_links_dt}'s \code{value} column (multiplied by \code{units$mln_USD_exchange_rate},
#' transformed by the \code{numberFormattingFun} function, and with \code{units_suffix}
#' \link{paste}d) into GraphViz's \code{label}s for arrows (edges). Default: \link{identity}.
#'
#' @param nodeSizeFun Optional: a function which takes a country-and-sector level
#' \link[data.table]{data.table} (including the columns of \code{aggregates_dt}) and returns
#' a numeric vector representing the sizes of the country-sector nodes (boxes).
#' Default: the value of 'output at basic prices' (\code{GO}) for the industrial sectors
#' (i.e. those that produce something -- goods or services)
#' and 'total intermediate consumption' (\code{II_fob}) for the final use sectors
#'
#' @param nodeSizeTransformFun Optional: a one-argument function which takes
#' the numeric value returned by the function \code{nodeSizeFun} and translates it
#' into GraphViz's \code{fontsize} values for the country-sector nodes (boxes), see
#' \url{http://www.graphviz.org/content/attrs#dfontsize}).
#'
#' @param nodeLabelFun Optional: a function which takes the country-and-sector-level
#' \link[data.table]{data.table} -- a merger of \code{aggregates_dt},
#' \code{country_labels_dt}, and \code{sector_labels_dt} for the country-sector combinations
#' present in \code{top_links_dt} -- and returns a character vector used as GraphViz's
#' \code{label}s for nodes (boxes). If the function needs to refer to the node size
#' (produced by the function \code{nodeSizeFun}) it should refer to data.table's column
#' \code{NodeSize}. Default: it returns, for each node, the following
#' components \link{paste}d together: country label, sector label, a value returned by
#' \code{nodeSizeFun} and the \code{units$units_suffix}.
#'
#' @param specificNodeOptionsFun Optional: a function which takes the country-and-sector-level
#' \link[data.table]{data.table} and returns a character vector of GraphViz's node attributes*,
#' see ** with 'node' highlighted in
#' the 'COMPONENTS' section. Default: \code{'fontname="times-italic"'} for the final
#' use sectors (such as final consumption or investment).
#'
#' @param specificArrowOptionsFun Optional: a function which takes \code{top_links_dt}
#' as an argument and returns a character vector of GraphViz's edge attributes*,
#' see ** with 'edge' highlighted in
#' the 'COMPONENTS' section. Default: \code{'style=solid'} for the domestic flows and
#' 'style=dashed' for the international flows.
#' @param general_arrow_options Optional: a character vector with the GraphViz's edge
#' attributes of all the arrows (edges)*. Default: \code{'color=grey'}.
#' @param general_node_options Optional: a character vector with the GraphViz's node
#' attributes of all the nodes (country-sector boxes)*. Default: \code{'shape=box'}.
#' @param graph_options Optional: a character with the GraphViz's graph attributes*.
#' See ** with 'graph' highlighted in the 'COMPONENTS' section.
#' Default: \code{c('layout=dot', 'rankdir=LR')}.
#' For the 'layout' see \url{https://en.wikipedia.org/wiki/Graphviz#Software_architecture};
#' 'rankdir' is applicable only if \code{'layout=dot'}, \code{LR} = left-to-right,
#' \code{TB} = top-to-bottom.
#'
#' @param numberFormattingFun Optional: a one-numeric-argument function used to format all
#' the numbers displayed in the graph. Default: it returns a character vector with values
#' formated as one digit after decimal dot and blank as a digit-grouping separator.
#'
#' @param nchar_wrap Optional: a numeric constant -- the number of characters arounnd which the text
#' of arrow labels (returned by \code{arrowLabelFun}) is wraped into another line.
#' Default: 30.
#'
#' @param gvcode Optional: should the generated GraphViz code be returned instead
#' of the \code{\link[DiagrammeR]{grViz}} object (logical)? This is useful for debugging.
#' Default: FALSE.
#'
#' @section Footnotes:
#'
#' * in the form of
#' \code{c('attribute_name1=attribute_value1', 'attribute_name2=attribute_value2, etc.')}
#'
#' ** \url{http://www.graphviz.org/doc/schema/attributes.xml}
#'
#' @return The return value of \code{\link[DiagrammeR]{grViz}} or the Graphviz code
#' if \code{gvcode = TRUE}.
#'
#' @export
plot.SelectedLinksDT <- function(top_links_dt,
                                 wiot,
                                 units =
                                     list(units_suffix =
                                              'bn USD',
                                          mln_USD_exchange_rate = # should correspond to the units_suffix above
                                              0.001), # original WIOD in mln USD
                                 country_labels_dt =
                                     wiod.diagrammer::countries(),
                                 sector_labels_dt =
                                     wiod.diagrammer::sectors(wiot),
                                 aggregates_dt =
                                     wiod.diagrammer::aggregates(wiot),
                                 arrowSizeFun = function(value)
                                     wiod.diagrammer:::normalise(value)*16 + 1,
                                 arrowLabelFun =
                                     identity,
                                 nodeSizeFun = function(country_sector_dt)
                                     country_sector_dt %>%
                                     ifelse.(.$isFinal,
                                             .$II_fob,
                                             .$GO),
                                 nodeSizeTransformFun = function(node_size_val)
                                     wiod.diagrammer::normalise(log(node_size_val)+1)*8 + 8,
                                 nodeLabelFun = function(country_sector_dt)
                                     country_sector_dt %>%
                                     paste.(.$CountryLab,
                                            .$SectorLab,
                                            paste(.$NodeSize %>%
                                                      numberFormattingFun,
                                                  units$units_suffix),
                                            sep=', '),
                                 specificNodeOptionsFun = function(country_sector_dt)
                                     ifelse(country_sector_dt$isFinal,
                                            'fontname="times-italic"', ""),
                                 specificArrowOptionsFun = function(top_links_dt)
                                     ifelse(top_links_dt$ExpCountry==top_links_dt$ImpCountry, # domestic flows
                                            'style=solid', 'style=dashed'),
                                 general_arrow_options =
                                     'color=grey',
                                 general_node_options =
                                     'shape=box',
                                 graph_options =
                                     c('layout=dot','rankdir=LR'),
                                 numberFormattingFun = function(value)
                                     value %>%
                                     formatC(digits=1, format='f',big.mark=' '),
                                 nchar_wrap =
                                     30,
                                 gvcode =
                                     FALSE) {
    stopifnot(wiot %>% isWIOD,
              units %>% is.list,
              length(units)==2,
              'units_suffix' %in% names(units),
              'mln_USD_exchange_rate' %in% names(units),
              units$units_suffix %>% isString,
              units$mln_USD_exchange_rate %>% isNumericConstant,
              country_labels_dt %>% isDataTable,
              country_labels_dt %>% has3letterCountry,
              country_labels_dt %>% hasCharacterColumn('CountryLab'),
              sector_labels_dt %>% isDataTable,
              sector_labels_dt %>% hasWiodSectorNr,
              sector_labels_dt %>% hasCharacterColumn('SectorLab'),
              aggregates_dt %>% isDataTable,
              aggregates_dt  %>% has3letterCountry,
              aggregates_dt  %>% hasWiodSectorNr,
              arrowSizeFun %>% isFunctionWith1Argument,
              arrowLabelFun %>% isFunctionWith1Argument,
              nodeSizeFun %>% isFunctionWith1Argument,
              nodeSizeTransformFun %>% isFunctionWith1Argument,
              nodeLabelFun %>% isFunctionWith1Argument,
              specificNodeOptionsFun %>% isFunctionWith1Argument,
              specificArrowOptionsFun %>% isFunctionWith1Argument,
              general_arrow_options %>% containsGraphVizAttributes,
              general_node_options %>% containsGraphVizAttributes,
              graph_options %>% containsGraphVizAttributes,
              gvcode %>% is.logical,
              length(gvcode)==1)
    message('Preparing arrows...')
    E <- environment()
    arrows <-
        top_links_dt %>%
        paste.(paste0(.$ExpCountry,.$ExpSectorNr),
               '->',
               paste0(.$ImpCountry,.$ImpSectorNr),
               formatOptions((.$value*units$mln_USD_exchange_rate) %>%
                                 evalAndCheck('numberFormattingFun', E,
                                              function(vr) vr %>% is.character) %>%
                                 paste(units$units_suffix) %>%
                                 evalAndCheck('arrowLabelFun', E,
                                              function(vr) vr %>% is.character) %>%
                                 asLabel,
                             .$value %>%
                                 evalAndCheck('arrowSizeFun', E,
                                              function(vr) vr %>% is.numeric) %>%
                                 paste0('penwidth=',.),
                             evalAndCheck(., 'specificArrowOptionsFun', E,
                                          function(vr) vr %>% containsGraphVizAttributes)))
    message('Preparing nodes...')
    nodes <-
        top_links_dt %>%
        {rbind(.[, list(ExpCountry, ExpSectorNr)] %>%
                   data.table::setnames(c('ExpCountry', 'ExpSectorNr'),
                                        c('Country', 'SectorNr')),
               .[, list(ImpCountry, ImpSectorNr)] %>%
                   data.table::setnames(c('ImpCountry', 'ImpSectorNr'),
                                        c('Country', 'SectorNr')))} %>%
        unique %>%
        merge(aggregates_dt,
              by=c('Country','SectorNr')) %>%
        merge(country_labels_dt,
              by='Country') %>%
        merge(sector_labels_dt,
              by='SectorNr') %>%
        `[`(, NodeSize :=
                evalAndCheck(., 'nodeSizeFun', E,
                             function(vr) vr %>% is.numeric)*
                units$mln_USD_exchange_rate) %>%
        `[`(, NodeSizeTransformed :=
                NodeSize %>%
                evalAndCheck('nodeSizeTransformFun', E,
                             function(vr) vr %>% is.numeric)) %>%
        `[`(, NodeLabel :=
                evalAndCheck(., 'nodeLabelFun', E,
                             function(vr) vr %>% is.character)) %>%
        paste.(paste0(.$Country,.$SectorNr),
               formatOptions(.$NodeLabel %>%
                                 wrapText(nchar_wrap) %>%
                                 asLabel,
                             .$NodeSizeTransformed %>%
                                 paste0('fontsize=',.),
                             evalAndCheck(., 'specificNodeOptionsFun', E,
                                          function(vr) vr %>% containsGraphVizAttributes)))
    message('Preparing the graph...')
    c('digraph graphname {',
      paste('graph', graph_options %>% collapseAndFormatOptions),
      paste('node', general_node_options %>% collapseAndFormatOptions),
      paste('edge', general_arrow_options %>% collapseAndFormatOptions),
      arrows,
      nodes,
      '}') %>%
        paste(collapse='\n') %>%
        `if`(gvcode,
             .,
             DiagrammeR::grViz(.))
}

