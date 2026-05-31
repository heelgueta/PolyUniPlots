polyNumOptions <- R6::R6Class(
    "polyNumOptions",
    inherit = jmvcore::Options,
    public = list(
        initialize = function(
            vars        = NULL,
            plotType    = "box",
            orientation = "vertical",
            showJitter  = FALSE,
            showMean    = FALSE,
            showMeanCI  = FALSE,
            ciWidth     = 95L,
            showRug     = FALSE,
            showOutliers = TRUE,
            violinScale = "area",
            plotAlpha   = 80L,
            boxWidth    = 55L,
            jitterWidth = 20L,
            colorScheme = "viridis",
            themeChoice = "minimal",
            title       = "",
            plotWidth   = 650L,
            plotHeight  = 480L,
            ...) {

            super$initialize(
                package      = "PolyUniPlots",
                name         = "polyNum",
                requiresData = TRUE,
                ...)

            private$..vars <- jmvcore::OptionVariables$new(
                "vars", vars,
                suggested = list("continuous"),
                permitted = list("numeric"))

            private$..plotType    <- jmvcore::OptionList$new("plotType",    plotType,    options=list("box","violin","strip","histogram","ridge"), default="box")
            private$..orientation <- jmvcore::OptionList$new("orientation", orientation, options=list("vertical","horizontal"), default="vertical")
            private$..showJitter  <- jmvcore::OptionBool$new("showJitter",  showJitter,  default=FALSE)
            private$..showMean    <- jmvcore::OptionBool$new("showMean",    showMean,    default=FALSE)
            private$..showMeanCI  <- jmvcore::OptionBool$new("showMeanCI",  showMeanCI,  default=FALSE)
            private$..ciWidth     <- jmvcore::OptionInteger$new("ciWidth",   ciWidth,    min=50L, max=99L, default=95L)
            private$..showRug     <- jmvcore::OptionBool$new("showRug",     showRug,     default=FALSE)
            private$..showOutliers <- jmvcore::OptionBool$new("showOutliers", showOutliers, default=TRUE)
            private$..violinScale  <- jmvcore::OptionList$new("violinScale",  violinScale,  options=list("area","count","width"), default="area")
            private$..plotAlpha    <- jmvcore::OptionInteger$new("plotAlpha",   plotAlpha,   min=0L,   max=100L, default=80L)
            private$..boxWidth     <- jmvcore::OptionInteger$new("boxWidth",    boxWidth,    min=5L,   max=100L, default=55L)
            private$..jitterWidth  <- jmvcore::OptionInteger$new("jitterWidth", jitterWidth, min=0L,   max=50L,  default=20L)
            private$..colorScheme  <- jmvcore::OptionList$new("colorScheme",  colorScheme,  options=list("viridis","plasma","mako","rocket","turbo","dark","pastel","warm","cold"), default="viridis")
            private$..themeChoice  <- jmvcore::OptionList$new("themeChoice",  themeChoice,  options=list("minimal","classic","bw","light"), default="minimal")
            private$..title        <- jmvcore::OptionString$new("title",      title,      default="")
            private$..plotWidth    <- jmvcore::OptionInteger$new("plotWidth",  plotWidth,  min=300L, max=2000L, default=650L)
            private$..plotHeight   <- jmvcore::OptionInteger$new("plotHeight", plotHeight, min=200L, max=2000L, default=480L)

            self$.addOption(private$..vars)
            self$.addOption(private$..plotType)
            self$.addOption(private$..orientation)
            self$.addOption(private$..showJitter)
            self$.addOption(private$..showMean)
            self$.addOption(private$..showMeanCI)
            self$.addOption(private$..ciWidth)
            self$.addOption(private$..showRug)
            self$.addOption(private$..showOutliers)
            self$.addOption(private$..violinScale)
            self$.addOption(private$..plotAlpha)
            self$.addOption(private$..boxWidth)
            self$.addOption(private$..jitterWidth)
            self$.addOption(private$..colorScheme)
            self$.addOption(private$..themeChoice)
            self$.addOption(private$..title)
            self$.addOption(private$..plotWidth)
            self$.addOption(private$..plotHeight)
        }),
    active = list(
        vars        = function() private$..vars$value,
        plotType    = function() private$..plotType$value,
        orientation = function() private$..orientation$value,
        showJitter  = function() private$..showJitter$value,
        showMean    = function() private$..showMean$value,
        showMeanCI  = function() private$..showMeanCI$value,
        ciWidth     = function() private$..ciWidth$value,
        showRug     = function() private$..showRug$value,
        showOutliers= function() private$..showOutliers$value,
        violinScale = function() private$..violinScale$value,
        plotAlpha   = function() private$..plotAlpha$value,
        boxWidth    = function() private$..boxWidth$value,
        jitterWidth = function() private$..jitterWidth$value,
        colorScheme = function() private$..colorScheme$value,
        themeChoice = function() private$..themeChoice$value,
        title       = function() private$..title$value,
        plotWidth   = function() private$..plotWidth$value,
        plotHeight  = function() private$..plotHeight$value),
    private = list(
        ..vars=NA, ..plotType=NA, ..orientation=NA, ..showJitter=NA,
        ..showMean=NA, ..showMeanCI=NA, ..ciWidth=NA,
        ..showRug=NA, ..showOutliers=NA, ..violinScale=NA,
        ..plotAlpha=NA, ..boxWidth=NA, ..jitterWidth=NA,
        ..colorScheme=NA, ..themeChoice=NA, ..title=NA,
        ..plotWidth=NA, ..plotHeight=NA))

polyNumResults <- R6::R6Class(
    "polyNumResults",
    inherit = jmvcore::Group,
    active = list(
        plot = function() private$.items[["plot"]]),
    private = list(),
    public = list(
        initialize = function(options) {
            super$initialize(options=options, name="", title="Multiple Numeric Plots")
            self$add(jmvcore::Image$new(
                options      = options,
                name         = "plot",
                title        = "Distribution Comparison",
                width        = 650,
                height       = 480,
                renderFun    = ".plot",
                requiresData = TRUE,
                refs         = list("ggplot2")))
        })
)

polyNumBase <- R6::R6Class(
    "polyNumBase",
    inherit = jmvcore::Analysis,
    public = list(
        initialize = function(options, data=NULL, datasetId="", analysisId="", revision=0) {
            super$initialize(
                package    = "PolyUniPlots",
                name       = "polyNum",
                version    = c(0, 2, 0),
                options    = options,
                results    = polyNumResults$new(options=options),
                data       = data,
                datasetId  = datasetId,
                analysisId = analysisId,
                revision   = revision)
        })
)

#' @export
polyNum <- function(data, vars, ...) {
    options  <- polyNumOptions$new(vars=vars, ...)
    analysis <- polyNumClass$new(options=options, data=data)
    analysis$run()
    analysis$results
}
