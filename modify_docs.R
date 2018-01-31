library(magrittr)

mymods <-
    list(c(from='<p>Developed by Aleksander Rutkowski.</p>',
           to='<p>Developed by <a href="mailto:alek.rutkowski@gmail.com?subject=wiod.diagrammer">Aleksander Rutkowski</a>.</p>'),
         c(to="",
           from=
"<div class='input'>#     x          y
# -3.70  0.0000000
# -2.34  0.1428571
# -0.98  0.2857143
#  0.38  0.4285714
#  1.74  0.5714286
#  3.10  0.7142857
#  4.46  0.8571429
#  5.82  1.0000000
</div></pre>"),
c(to="",
  from=
"<div class='input'>#  x  rank(x) <= 2  tieRobustRankLessOrEqual(x, 2)
#  1   TRUE          TRUE
#  1   TRUE          TRUE
#  2  FALSE          TRUE
#  2  FALSE          TRUE
#  2  FALSE          TRUE
#  3  FALSE         FALSE
#  3  FALSE         FALSE
</div></pre>"))

modifyFile <- function(path, mods)
    path %>%
    readLines(warn=FALSE) %>%
    paste(collapse='\n') %>%
    Reduce(function(i,x)
        gsub(x['from'], x['to'], i, fixed=TRUE),
           x=mods,
           init=.) %>%
    cat(file=path)

list.files('docs',
           pattern='\\.html$',
           full.names=TRUE, recursive=TRUE) %>%
    lapply(. %>% modifyFile(mymods))
