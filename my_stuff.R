#' ---
#' title: "My Reusable Objects"
#' author: "Jonathan Ferrari"
#' date: "December 2, 2021"
#' output:
#'     html_document:
#'       theme: readable
#'       css: zenburn.css
#' ---
#+ message = FALSE, warning = FALSE

#'## Packages
#' Here I import all downloaded packages.
#+ message = FALSE, warning = FALSE
library(psych)
library(ggplot2, quietly = T)
all_lib = .packages(all.available = TRUE)
invisible(lapply(all_lib,
                 function(x)
                     (library(
                         x, character.only = TRUE, quietly = T
                     ))))
sprintf('All %s packages have been loaded!', length(all_lib))

#'## Directory
#' Now I will set my working directory.

setwd("~/psych")

#'## Colors
#' These are some common colors I will use and so I create objects to represent them.

berk_blue = "#003262"
found_rock = "#3b7ea1"
cal_gold = "#fdb515"
gold_medal = "#c4820e"
wcol1 = "#e2c258"
wcol2 = "#c9b676"
wcol3 = "#222222"
wcol4 = "#3B3B3B"
wcol5 = "#515151"
wcol6 = "#626262"
wcol7 = "#d8d0c7"
wcol8 = "#eee9e1"
wcol9 = "#ffffff"
dwcol1 = "#ac2b2b"
dwcol2 = "#872121"
dwcol3 = "#f0ecf0"
dwcol4 = "#f3f3f3"
dwcol5 = "#eeeeee"
dwcol6 = "#ffffff"
dwcol7 = "#3f3f3f"
dwcol8 = "#050505"
dwcol9 = "#000000"

#'##  Functions
#' This section consists entirely of functions written by myself.

#' ### Quiet Return

shh = (function (x) 
    #' quietly returns x
    #' @param x any. What will be returned
    #' @usage shh(x)
    return(invisible(x)))

#' ### Color Opacity

opacity = function(col, opacity_prop)
{
    #' Makes colors opaque
    #' @description This function makes the given color more opaque, 
    #' with .0 having no opacity, and 1 being transparent
    #' @param col hex color. The color to be made opaque
    #' @param opacity_prop number. Proportion 0:1 inclusive
    #' @return color. A color more opaque than `col`
    #' @usage opacity('#your_hex_code', opacity_proportion)
    alp_dec = round(opacity_prop * 255)
    hex_alp = as.hexmode(alp_dec)
    return(sprintf("%s%s", col, hex_alp))
}

#' ### Statistical Mode

mode =
    function(x)
    {
        #' Finds mode
        #' @description This function determines the mode of a 
        #' one-dimensional data structure
        #' @return number. The mode of `x`
        #' @usage mode(your data structure)
        ux <- unique(x)
        return(ux[which.max(tabulate(match(x, ux)))])
    }

#' ### Scaled Table

make_scale =
    function(table, columns, name = 'array', func = rowMeans)
    {
        #' Creates scale
        #' @description Creates a scale from `columns` of `table` using given `func`
        #' @param table string. String name of table
        #' @param columns 1-D data structure. Array of column names as strings
        #' @param name string. X-axis label for plot
        #' @param func function. Function for aggregation; built-in or user-defined
        #' @return table. Scaled Table with aggregated column `Scale`
        #' @usage make_scale("table", c('col1','col2', 'col3', ...),
        #''x-axis label', aggregation function)
        new_t = na.omit(table[, columns])
        new_t$Scale = func(new_t)
        capt =  sprintf('Alpha: \u03b1 = %s',
                        round(
                            psych::alpha(new_t)$total$raw_alpha, 4
                        ))
        fncy_hist(new_t$Scale,
                      name = name,
                      subtitle = capt)
        invisible(new_t)
    }

#' ### All-In-One Modeling Function

model =
    function(table,
             DV,
             IV,
             IV2 = "NULL",
             IV3 = "NULL",
             numeric = T,
             mv = F,
             ylim = NULL,
             xlim = NULL)
    {
        #' @title All-In-One Modeling Function
        #' @description Performs: linear, categorical, multi-level, 
        #' numeric, and multi-variable regression.
        #' @return summary. Returns summary of model that was calculated 
        #' and plots a graph of it.
        #' @param table string. Indicates what `table` should be used for the model.
        #' @param DV string. Indicates what `DV` should be used for the model.
        #' @param IV string. Indicates what `IV` should be used for the model.
        #' @param IV2 (optional) string. Indicates what `IV2` 
        #' should be used for the model.
        #' @param IV3 (optional) string. Indicates what `IV3` 
        #' should be used for the model.
        #' @param numeric (optional) bool. Determines if the model is numeric.
        #' @param mv (optional) bool. Determines if the model is multivariate.
        #' @param ylim (optional) range. Determines range of y-axis.
        #' @param xlim (optional) range. Determines range of x-axis.
        #' @usage model('table', "DV", "IV", "IV2", numeric = F, mv = T)
        if (mv == T)
        {
            multivariate(
                table,
                DV,
                IV,
                IV2 = IV2,
                IV3 = IV3,
                ylim = ylim,
                xlim = xlim
            )
        }
        else
        {
            if (numeric == F & IV2 == "NULL")
            {
                mod = eval(str2expression(sprintf(
                    "lm(%s ~ %s , data = %s)", DV, IV, table
                )))
                coefs = summary(mod)$coefficients
                get = function(x,
                               y,
                               md = 'summary(mod)',
                               type = "coefficients") {
                    eval(str2expression(
                        sprintf("round(mean(%s$%s[%s,%s]),4)", md, type, x, y)
                    ))
                }
                r_square = round(summary(mod)$r.squared, 4)
                slope = get(-1, 1)
                intercept = get(1, 1)
                t_val = get(-1, 3)
                se = get(-1, 2)
                p_val = formatC(get(-1, 4), format = "e", digits = 3)
                ci_l = round(coef(mod)[2] - 1.96 * se, 4)
                ci_r = round(coef(mod)[2] + 1.96 * se, 4)
                capt = sprintf('T-Value: \U03c4 = %s Standard Error: \u03b5 = %s P-Value: \U03c1 = %s,
                            95%% Confidence Interval: [%s,%s]',
                    t_val,
                    se,
                    p_val,
                    ci_l,
                    ci_r
                )
                par(mar = c(6.1, 4.1, 3.1, 2.1))
                plotmeans(
                    eval(parse(text = DV)) ~
                        as.factor(eval(parse(text = IV))),
                    data = eval(str2expression(table)),
                    ylab = DV,
                    col = cal_gold,
                    barcol = berk_blue,
                    main = bquote(
                        'CoD:'~ R ^ 2 == .(r_square) ~ 'Slope: \u03b2' == .(slope) ~
                            "Intercept: \u03b1" == .(intercept)
                    ),
                    xlab = IV,
                    lwd = 3,
                    ylim = ylim
                )
                mtext(capt, side = 1, line = 5)
            }
            else
            {
                mod = eval(str2expression(
                    sprintf("lm(%s ~ %s + %s +%s, data = %s)"
                            , DV, IV, IV2, IV3, table)
                ))
                plot(
                    eval(parse(text = DV)) ~
                        eval(parse(text = IV)),
                    data = eval(str2expression(table)),
                    pch = 16,
                    col = cal_gold,
                    main =
                        bquote(
                            "CoD:"~ R ^ 2 ==
                                .(summary(mod)$r.squared) ~ 'Slope: \u03b2' == .(coef(mod)[2])
                            ~ "Intercept: \u03b1" == .(coef(mod)[1])
                        ),
                    xlab =
                        sprintf(
                            '%s \n T-Value: \U03c4 = %s Standard Error: \u03b5 = %s P-Value: \U03c1 = %s,
                            95%% Confidence Interval: [%s,%s]',
                            IV,
                            round(summary(mod)$coefficients[2, 3], 4),
                            round(summary(mod)$coefficients[2, 2], 4),
                            summary(mod)$coefficients[2, 4],
                            round(coef(mod)[2] -
                                      1.96 * summary(mod)$coefficients[2, 2], 4),
                            round(coef(mod)[2] +
                                      1.96 * summary(mod)$coefficients[2, 2], 4)
                        ),
                    ylab = DV,
                    xlim = xlim,
                    ylim = ylim
                )
                abline(mod, col = berk_blue, lwd = 5)
            }
            invisible(summary(mod))
            }
    }

#' ### Randomly Sampled Table

sample_table =
    function(table,
             sample_size = nrow(table),
             replacement = TRUE)
    {
        #' @title Samples table 
        #' @description Samples a table ar random with replacement; 
        #' suitable for bootstrapping.
        #' @param table string. String name of table.
        #' @param sample_size How many random selections the 
        #' function will take from `table`.
        #' @param replacement bool. Whether or not the table should 
        #' be sampled with replacement
        #' @return table. New table sampled `sample_size` times 
        #' @usage sample_table('table', sample_size = round(nrow(table)/2), 
        #' replacement = TRUE)
        sampled_table =
            table[sample(nrow(table), sample_size, replace = replacement), ]
        return(sampled_table)
    }

#' ### Descriptive Histogram

fncy_hist =
    function(array,
             subtitle = 'NULL',
             name = 'array',
             xlim = range(min(array) - 1, max(array) + 1),
             categorical = F,
             fill = opacity(berk_blue, .7),
             bord = opacity(cal_gold, .7),
             bord.w = 2)
    {
        #' @title Descriptive Histogram
        #' @description Plots styled histogram of `array` and states 
        #' Mean, Median, Mode, Standard Deviation, and Range
        #' @param array 1-D data structure. Data to be plotted
        #' @param subtitle string. Subtitle for histogram
        #' @param name string. X-label for histogram
        #' @param xlim range. Determines the x-axis range
        #' @param categorical bool. Indicates whether `array` is categorical or not.
        #' @param fill color. Indicates color of bar fill.
        #' @param bord color. Indicates color of border lines.
        #' @param bord.w number. Indicates width of border lines
        #' @usage fncy_hist(array, "my plot", "array name", 
        #'                      range(max(array), min(array)), categorical = F)
        if (categorical)
        {
            line <- par(lwd = bord.w)
            plot(array,
                 xlab = name,
                 col = fill,
                 border = bord)
        }
        else{
            line <- par(lwd = bord.w)
            hist(
                array,
                xlab = name,
                xlim = xlim,
                main =
                    sprintf(
                        "Mean: \U03bc = %s, Median: \U03b7 = %s, Mode: \U03a7 = %s, \n SD: \U03c3 = %s, Range: [%s,%s]",
                        round(mean(array, na.rm = T), 4),
                        round(median(array, na.rm = T), 4),
                        round(mode(array), 4),
                        round(sd(array, na.rm = T), 2),
                        round(min(array, na.rm = T), 4),
                        round(max(array, na.rm = T), 4)
                    ),
                col = fill,
                border = bord
            )
            if (subtitle != 'NULL')
            {
                par(mar = c(6.1, 3.1, 5.1, 3.1))
                mtext(subtitle, side = 1, line = 5)
            }
        }
    }

#' ### General Statistical Bootstrapping

bootstrap =
    function(table, DV, IV, statistic, hist = T, trials = 1000, name = 'array', cateorical = F)
    {
        #' @title Perform Bootstrap
        #' @description Does bootstrap analysis to aggregate information about re-samples.
        #' @param table data-frame. The data to be used.
        #' @param DV string. The dependent variable.
        #' @param IV string. The independent variable.
        #' @param statistic function. The function to collect from each trial model.
        #' @param hist bool. Determines if histogram should be drawn.
        #' @param trials number. How many re-samples should be collected.
        #' @param name string. X-axis label.
        #' @param categorical bool. Whether or not the information is categorical.
        #' @usage bootstrap(table, DV, IV, statistic, hist)
        results = c()
        for (i in c(1:trials))
        {
            samp = sample_table(table)
            samp = samp[c(DV, IV)]
            model = lm(eval(parse(text = DV)) ~
                           eval(parse(text = IV)), data = eval(samp))
            sim = statistic(model)
            results = append(results, sim)
        }
        if (hist) {
            std = sd(results, na.rm = T)
            mn = mean(results, na.rm = T)
            fncy_hist(results, name = name, categorical = F)
            abline(v = mn - 1.96 * std,
                   col = cal_gold,
                   lwd = 3)
            abline(v = mn + 1.96 * std,
                   col = cal_gold,
                   lwd = 3)
            abline(v = mn,
                   col = berk_blue,
                   lwd = 3)
        }
        shh(results)
    }

#' ### Statistical Bootstrapping for Slope Directionality

in_same_direction =
    function(table,
             DV,
             IV,
             numerical = T,
             name = 'array', trials = 1000)
    {
        #' @title How many slopes show same type of correlation
        #' @description Determines out `trials` bootstrapped slope, 
        #' what % of them are in the same direction.
        #' (negative or positive slope)
        #' @param table data-frame. The data to be used.
        #' @param DV string. The dependent variable.
        #' @param IV string. The independent variable.
        #' @param numerical bool. Determines the data is numerical.
        #' @param trials number. How many re-samples should be collected.
        #' @usage in_same_direction(table, DV, IV)
        is_pos = (function (x)
            x >= 0)
        is_neg = (function (x)
            x <= 0)
        og_slope = model(table, DV, IV, numerical)$coefficients[2]
        if (is_pos(og_slope))
        {
            direction = is_pos
        }
        else
        {
            direction = is_neg
        }
        slope_bucket = boot_slope(table, DV, IV, hist = F, trials = trials)
        slope_percent = round(mean(direction(slope_bucket)) * 100, 4)
        fncy_hist(
            slope_bucket,
            subtitle =
                sprintf(
                    'Bootstrapped slopes in the same diection as the original slope: %s%%',
                    slope_percent
                ),
            name = name
            
        )
        abline(v = og_slope, col = cal_gold, lwd = 3)
        shh(slope_bucket)
    }

#' ### Statistical Bootstrapping for Intercept

boot_intercept =
    (function (table, DV, IV, hist = T, trials = 1000, name = 'array')
        #' @title Bootstrapped Intercept
        #' @description Finds `intercept` from `table` 
        #' using `DV` as y and `IV` as x with `trials` 
        #' repetitions.
        #' @param table data-frame. The data to be used.
        #' @param DV string. The dependent variable.
        #' @param IV string. The independent variable.
        #' @param (optional) hist bool. Determines if histogram should be drawn.
        #' @param trials (optional) number. How many re-samples should be collected.
        #' @param name (optional) string. X-axis label.
        #' @usage boot_intercept("table", "DV", "IV")
        return(bootstrap(table, DV, IV, (function(mod)
            coef(mod)[1]), hist, trials = trials, name = name)))

#' ### Statistical Bootstrapping for Slope

boot_slope =
    (function (table, DV, IV, hist = T, trials = 1000, name = 'array')
        #' @title Bootstrapped Slope
        #' @description Finds `slope` from `table` 
        #' using `DV` as y and `IV` as x with `trials` 
        #' repetitions.
        #' @param table data-frame. The data to be used.
        #' @param DV string. The dependent variable.
        #' @param IV string. The independent variable.
        #' @param (optional) hist bool. Determines if histogram should be drawn.
        #' @param trials (optional) number. How many re-samples should be collected.
        #' @param name (optional) string. X-axis label.
        #' @usage boot_slope("table", "DV", "IV")
        return(bootstrap(table, DV, IV, (function(mod)
            coef(mod)[2]), hist, trials)))

#' ### Statistical Bootstrapping for R^2

boot_r.squared =
    (function (table, DV, IV, hist = T, trials = 1000, name = 'array')
        #' @title Bootstrapped R^2
        #' @description Finds `R^2` from `table` 
        #' using `DV` as y and `IV` as x with `trials` 
        #' repetitions.
        #' @param table data-frame. The data to be used.
        #' @param DV string. The dependent variable.
        #' @param IV string. The independent variable.
        #' @param (optional) hist bool. Determines if histogram should be drawn.
        #' @param trials (optional) number. How many re-samples should be collected.
        #' @param name (optional) string. X-axis label.
        #' @usage boot_r.squared("table", "DV", "IV")
        return(bootstrap(table, DV, IV, (function(mod)
            summary(mod)$r.squared), hist, trials = trials)))

#' ### Multivariate Modeling

multivariate = function(tbl,
                        DV,
                        IV1,
                        IV2,
                        IV3 = 'NULL',
                        ylim = NULL,
                        xlim = NULL) 
    {
    #' @title Multivariate Regression
    #' @description Does multivariate/multiple regression and shows a plot.
    #' @param tbl string. String name of table.
    #' @param DV string. String name of dependent variable.
    #' @param IV1 string. String name of first independent variable.
    #' @param IV2 string. String name of second independent variable.
    #' @param IV3 (optional) string. String name of third independent variable.
    #' @param ylim (optional) range. Y-range of graph.
    #' @param xlim (optional) range. X-range of graph.
    #' @usage multivariate('table', "DV", "IV1", "IV2")
    modmv = eval(str2expression(sprintf(
        "lm(%s ~ %s + %s +%s, data = %s)"
        , DV, IV1, IV2, IV3, tbl
    )))
    show(ggPredict(
        modmv,
        point = T,
        se = T,
        interactive = F,
        xlim = xlim,
        ylim = ylim
    ))
    show(summary(modmv))
    }

#'## Data
#' Finally, I will load all `.csv` files and assign names to them.

test = read.csv("~/psych/data/test.csv")
mini = read.csv("~/psych/data/mini_data.csv")
anchoring = read.csv("~/psych/data/anchoring_data.csv")
protest = read.csv("~/psych/data/animal_protest_data.csv")
chile = read.csv("~/psych/data/Chile.csv")
davis = read.csv("~/psych/data/davis_data.csv")
donate = read.csv("~/psych/data/donation_data.csv")
ec = read.csv("~/psych/data/ec_data.csv")
final = read.csv("~/psych/data/Final Project.csv")
final_test = read.csv("~/psych/data/final_proj.csv")
interrupt = read.csv("~/psych/data/interruptions_data.csv")
mascot = read.csv("~/psych/data/mascot_data.csv")
politics = read.csv("~/psych/data/politics_data.csv")
prestige = read.csv("~/psych/data/prestige_data.csv")
protest_news = read.csv("~/psych/data/protest_news_data.csv")
self_esteem = read.csv("~/psych/data/selfesteem_data.csv")

#' All reusable objects have been loaded.\n
#' Let's get to work!