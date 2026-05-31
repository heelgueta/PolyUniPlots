polyOrdOptions <- R6::R6Class(
    "polyOrdOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars        = NULL,
            diverging   = FALSE,
            sortVars    = "none",
            reverseVars = FALSE,
            showPct     = TRUE,
            minPctLabel = 5L,
            showN       = FALSE,
            colorScheme = "tableau",
            legendPos   = "bottom",
            themeChoice = "minimal",
            barHeight   = 70L,
            title       = "",
            plotWidth   = 700L,
            plotHeight  = 400L,
            ...) {

            super$initialize(
                package      = "PolyUniPlots",
                name         = "polyOrd",
                requiresData = TRUE,
                ...)

            private$..vars        <- jmvcore::OptionVariables$new("vars", vars, suggested=list("ordinal","nominal"), permitted=list("factor"))
            private$..diverging   <- jmvcore::OptionBool$new("diverging",   diverging,   default=FALSE)
            private$..reverseVars <- jmvcore::OptionBool$new("reverseVars", reverseVars, default=FALSE)
            private$..showPct     <- jmvcore::OptionBool$new("showPct",     showPct,     default=TRUE)
            private$..showN       <- jmvcore::OptionBool$new("showN",       showN,       default=FALSE)
            private$..sortVars    <- jmvcore::OptionList$new("sortVars",    sortVars,    options=list("none","name","freq_first","freq_last"), default="none")
            private$..minPctLabel <- jmvcore::OptionInteger$new("minPctLabel", minPctLabel, min=0L, max=50L, default=5L)
            private$..colorScheme <- jmvcore::OptionList$new("colorScheme", colorScheme, options=list("tableau","rdbulite","rdylgn","piyg","prgn","pastel","dark2","set2","viridis","plasma"), default="tableau")
            private$..legendPos   <- jmvcore::OptionList$new("legendPos",   legendPos,   options=list("bottom","right","top","none"), default="bottom")
            private$..themeChoice <- jmvcore::OptionList$new("themeChoice", themeChoice, options=list("minimal","classic","bw","light"), default="minimal")
            private$..barHeight   <- jmvcore::OptionInteger$new("barHeight",  barHeight,  min=10L, max=100L, default=70L)
            private$..title       <- jmvcore::OptionString$new("title",      title,      default="")
            private$..plotWidth   <- jmvcore::OptionInteger$new("plotWidth",  plotWidth,  min=300L, max=2000L, default=700L)
            private$..plotHeight  <- jmvcore::OptionInteger$new("plotHeight", plotHeight, min=150L, max=2000L, default=400L)

            self$.addOption(private$..vars)
            self$.addOption(private$..diverging)
            self$.addOption(private$..sortVars)
            self$.addOption(private$..reverseVars)
            self$.addOption(private$..showPct)
            self$.addOption(private$..minPctLabel)
            self$.addOption(private$..showN)
            self$.addOption(private$..colorScheme)
            self$.addOption(private$..legendPos)
            self$.addOption(private$..themeChoice)
            self$.addOption(private$..barHeight)
            self$.addOption(private$..title)
            self$.addOption(private$..plotWidth)
            self$.addOption(private$..plotHeight)
        }),
    active = list(
        vars        = function() private$..vars$value,
        diverging   = function() private$..diverging$value,
        sortVars    = function() private$..sortVars$value,
        reverseVars = function() private$..reverseVars$value,
        showPct     = function() private$..showPct$value,
        minPctLabel = function() private$..minPctLabel$value,
        showN       = function() private$..showN$value,
        colorScheme = function() private$..colorScheme$value,
        legendPos   = function() private$..legendPos$value,
        themeChoice = function() private$..themeChoice$value,
        barHeight   = function() private$..barHeight$value,
        title       = function() private$..title$value,
        plotWidth   = function() private$..plotWidth$value,
        plotHeight  = function() private$..plotHeight$value),
    private = list(
        ..vars=NA, ..diverging=NA, ..sortVars=NA, ..reverseVars=NA,
        ..showPct=NA, ..minPctLabel=NA, ..showN=NA, ..colorScheme=NA,
        ..legendPos=NA, ..themeChoice=NA, ..barHeight=NA,
        ..title=NA, ..plotWidth=NA, ..plotHeight=NA))

polyOrdResults <- R6::R6Class(
    "polyOrdResults",
    inherit = jmvcore::Group,
    active = list(
        plot = function() private$.items[["plot"]]),
    private = list(),
    public = list(
        initialize = function(options) {
            super$initialize(options=options, name="", title="Multiple Ordinal/Nominal Plots")
            self$add(jmvcore::Image$new(
                options=options, name="plot",
                title="Proportional Response Chart",
                width=700, height=400,
                renderFun=".plot", requiresData=TRUE,
                refs=list("ggplot2")))
        })
)

polyOrdBase <- R6::R6Class(
    "polyOrdBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package="PolyUniPlots", name="polyOrd", version=c(0,1,0),
                options=options, results=polyOrdResults$new(options=options),
                data=data, datasetId=datasetId, analysisId=analysisId, revision=revision)
        })
)

#' @export
polyOrd <- function(data, vars, ...) {
    options  <- polyOrdOptions$new(vars=vars, ...)
    analysis <- polyOrdClass$new(options=options, data=data)
    analysis$run()
    analysis$results
}
