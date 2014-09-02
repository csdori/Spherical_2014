image.plot.info<-function (...) 
{
    temp <- list(...)
    xlim <- NA
    ylim <- NA
    zlim <- NA
    poly.grid <- FALSE
    if (is.list(temp[[1]])) {
        xlim <- range(temp[[1]]$x, na.rm = TRUE)
        ylim <- range(temp[[1]]$y, na.rm = TRUE)
        zlim <- range(temp[[1]]$z, na.rm = TRUE)
        if (is.matrix(temp[[1]]$x) & is.matrix(temp[[1]]$y) & 
            is.matrix(temp[[1]]$z)) {
            poly.grid <- TRUE
        }
    }
    if (length(temp) >= 3) {
        if (is.matrix(temp[[1]]) & is.matrix(temp[[2]]) & is.matrix(temp[[3]])) {
            poly.grid <- TRUE
        }
    }
    if (is.matrix(temp[[1]]) & !poly.grid) {
        xlim <- c(0, 1)
        ylim <- c(0, 1)
        zlim <- range(temp[[1]], na.rm = TRUE)
    }
    if (length(temp) >= 3) {
        if (is.matrix(temp[[3]])) {
            xlim <- range(temp[[1]], na.rm = TRUE)
            ylim <- range(temp[[2]], na.rm = TRUE)
            zlim <- range(temp[[3]], na.rm = TRUE)
        }
    }
    if (is.matrix(temp$x) & is.matrix(temp$y) & is.matrix(temp$z)) {
        poly.grid <- TRUE
    }
    xthere <- match("x", names(temp))
    ythere <- match("y", names(temp))
    zthere <- match("z", names(temp))
    if (!is.na(zthere)) 
        zlim <- range(temp$z, na.rm = TRUE)
    if (!is.na(xthere)) 
        xlim <- range(temp$x, na.rm = TRUE)
    if (!is.na(ythere)) 
        ylim <- range(temp$y, na.rm = TRUE)
    if (!is.null(temp$zlim)) 
        zlim <- temp$zlim
    if (!is.null(temp$xlim)) 
        xlim <- temp$xlim
    if (!is.null(temp$ylim)) 
        ylim <- temp$ylim
    list(xlim = xlim, ylim = ylim, zlim = zlim, poly.grid = poly.grid)
}


###################################
image.plot.plt<-function (x, add = FALSE, legend.shrink = 0.9, legend.width = 1, 
    horizontal = FALSE, legend.mar = NULL, bigplot = NULL, smallplot = NULL, 
    ...) 
{
    old.par <- par(no.readonly = TRUE)
    if (is.null(smallplot)) 
        stick <- TRUE
    else stick <- FALSE
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    char.size <- ifelse(horizontal, par()$cin[2]/par()$din[2], 
        par()$cin[1]/par()$din[1])
    offset <- char.size * ifelse(horizontal, par()$mar[1], par()$mar[4])
    legend.width <- char.size * legend.width
    legend.mar <- legend.mar * char.size
    if (is.null(smallplot)) {
        smallplot <- old.par$plt
        if (horizontal) {
            smallplot[3] <- legend.mar
            smallplot[4] <- legend.width + smallplot[3]
            pr <- (smallplot[2] - smallplot[1]) * ((1 - legend.shrink)/2)
            smallplot[1] <- smallplot[1] + pr
            smallplot[2] <- smallplot[2] - pr
        }
        else {
            smallplot[2] <- 1 - legend.mar
            smallplot[1] <- smallplot[2] - legend.width
            pr <- (smallplot[4] - smallplot[3]) * ((1 - legend.shrink)/2)
            smallplot[4] <- smallplot[4] - pr
            smallplot[3] <- smallplot[3] + pr
        }
    }
    if (is.null(bigplot)) {
        bigplot <- old.par$plt
        if (!horizontal) {
            bigplot[2] <- min(bigplot[2], smallplot[1] - offset)
        }
        else {
            bottom.space <- old.par$mar[1] * char.size
            bigplot[3] <- smallplot[4] + offset
        }
    }
    if (stick & (!horizontal)) {
        dp <- smallplot[2] - smallplot[1]
        smallplot[1] <- min(bigplot[2] + offset, smallplot[1])
        smallplot[2] <- smallplot[1] + dp
    }
    return(list(smallplot = smallplot, bigplot = bigplot))
}

#####################################################

image.plot<-function (..., add = FALSE, nlevel = 64, horizontal = FALSE, 
    legend.shrink = 0.9, legend.width = 1.2, legend.mar = ifelse(horizontal, 
        3.1, 5.1), legend.lab = NULL, graphics.reset = FALSE, 
    bigplot = NULL, smallplot = NULL, legend.only = FALSE, col = tim.colors(nlevel), 
    lab.breaks = NULL, axis.args = NULL, legend.args = NULL, 
    midpoint = FALSE, border = NA, lwd.poly = 1) 
{
    old.par <- par(no.readonly = TRUE)
    info <- image.plot.info(...)
    if (add) {
        big.plot <- old.par$plt
    }
    if (legend.only) {
        graphics.reset <- TRUE
    }
    if (is.null(legend.mar)) {
        legend.mar <- ifelse(horizontal, 3.1, 5.1)
    }
    temp <- image.plot.plt(add = add, legend.shrink = legend.shrink, 
        legend.width = legend.width, legend.mar = legend.mar, 
        horizontal = horizontal, bigplot = bigplot, smallplot = smallplot)
    smallplot <- temp$smallplot
    bigplot <- temp$bigplot
    if (!legend.only) {
        if (!add) {
            par(plt = bigplot)
        }
        if (!info$poly.grid) {
            image(..., add = add, col = col)
        }
        else {
            poly.image(..., add = add, col = col, midpoint = midpoint, 
                border = border, lwd.poly = lwd.poly)
        }
        big.par <- par(no.readonly = TRUE)
    }
    if ((smallplot[2] < smallplot[1]) | (smallplot[4] < smallplot[3])) {
        par(old.par)
        stop("plot region too small to add legend\n")
    }
    ix <- 1
    minz <- info$zlim[1]
    maxz <- info$zlim[2]
    binwidth <- (maxz - minz)/nlevel
    midpoints <- seq(minz + binwidth/2, maxz - binwidth/2, by = binwidth)
    iy <- midpoints
    iz <- matrix(iy, nrow = 1, ncol = length(iy))
    breaks <- list(...)$breaks
    par(new = TRUE, pty = "m", plt = smallplot, err = -1)
    if (!is.null(breaks) & !is.null(lab.breaks)) {
        axis.args <- c(list(side = ifelse(horizontal, 1, 4), 
            mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2), 
            at = breaks, labels = lab.breaks), axis.args)
    }
    else {
        axis.args <- c(list(side = ifelse(horizontal, 1, 4), 
            mgp = c(3, 1, 0), las = ifelse(horizontal, 0, 2)), 
            axis.args)
    }
    if (!horizontal) {
        if (is.null(breaks)) {
            image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col)
        }
        else {
            image(ix, iy, iz, xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col, breaks = breaks)
        }
    }
    else {
        if (is.null(breaks)) {
            image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col)
        }
        else {
            image(iy, ix, t(iz), xaxt = "n", yaxt = "n", xlab = "", 
                ylab = "", col = col, breaks = breaks)
        }
    }
    do.call("axis", axis.args)
    box()
    if (!is.null(legend.lab)) {
        legend.args <- list(text = legend.lab, side = ifelse(horizontal, 
            1, 4), line = legend.mar - 2)
    }
    if (!is.null(legend.args)) {
        do.call(mtext, legend.args)
    }
    mfg.save <- par()$mfg
    if (graphics.reset | add) {
        par(old.par)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
    else {
        par(big.par)
        par(plt = big.par$plt, xpd = FALSE)
        par(mfg = mfg.save, new = FALSE)
        invisible()
    }
}

##############################################################################
image.default
function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = heat.colors(12), 
    add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab, breaks, 
    oldstyle = FALSE, useRaster, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                if (is.null(dim(x))) 
                  stop("argument must be matrix-like")
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
            if (missing(xlab)) 
                xlab <- ""
            if (missing(ylab)) 
                ylab <- ""
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        xn <- deparse(substitute(x))
        if (missing(xlab)) 
            xlab <- paste(xn, "x", sep = "$")
        if (missing(ylab)) 
            ylab <- paste(xn, "y", sep = "$")
        y <- x$y
        x <- x$x
    }
    else {
        if (missing(xlab)) 
            xlab <- if (missing(x)) 
                ""
            else deparse(substitute(x))
        if (missing(ylab)) 
            ylab <- if (missing(y)) 
                ""
            else deparse(substitute(y))
    }
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("'x' and 'y' values must be finite and non-missing")
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z)) 
        stop("'z' must be a matrix")
    if (length(x) > 1 && length(x) == nrow(z)) {
        dx <- 0.5 * diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] + 
            dx[length(x) - 1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) {
        dy <- 0.5 * diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] + 
            dy[length(y) - 1])
    }
    if (missing(breaks)) {
        nc <- length(col)
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 
            0)) 
            stop("invalid z limits")
        if (diff(zlim) == 0) 
            zlim <- if (zlim[1L] == 0) 
                c(-1, 1)
            else zlim[1L] + c(-0.4, 0.4) * abs(zlim[1L])
        z <- (z - zlim[1L])/diff(zlim)
        zi <- if (oldstyle) 
            floor((nc - 1) * z + 0.5)
        else floor((nc - 1e-05) * z + 1e-07)
        zi[zi < 0 | zi >= nc] <- NA
    }
    else {
        if (length(breaks) != length(col) + 1) 
            stop("must have one more break than colour")
        if (any(!is.finite(breaks))) 
            stop("breaks must all be finite")
        zi <- .C("bincode", as.double(z), length(z), as.double(breaks), 
            length(breaks), code = integer(length(z)), (TRUE), 
            (TRUE), nok = TRUE, NAOK = TRUE, DUP = FALSE, PACKAGE = "base")$code - 
            1
    }
    if (!add) 
        plot(NA, NA, xlim = xlim, ylim = ylim, type = "n", xaxs = xaxs, 
            yaxs = yaxs, xlab = xlab, ylab = ylab, ...)
    if (length(x) <= 1) 
        x <- par("usr")[1L:2]
    if (length(y) <= 1) 
        y <- par("usr")[3:4]
    if (length(x) != nrow(z) + 1 || length(y) != ncol(z) + 1) 
        stop("dimensions of z are not length(x)(-1) times length(y)(-1)")
    check_irregular <- function(x, y) {
        dx <- diff(x)
        dy <- diff(y)
        (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) || 
            (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))
    }
    if (missing(useRaster)) {
        useRaster <- getOption("preferRaster", FALSE)
        if (useRaster && check_irregular(x, y)) 
            useRaster <- FALSE
        if (useRaster) {
            useRaster <- FALSE
            ras <- dev.capabilities("raster")
            if (identical(ras, "yes")) 
                useRaster <- TRUE
            if (identical(ras, "non-missing")) 
                useRaster <- all(!is.na(zi))
        }
    }
    if (useRaster) {
        if (check_irregular(x, y)) 
            stop("useRaster=TRUE can only be used with a regular grid")
        if (!is.character(col)) {
            p <- palette()
            pl <- length(p)
            col <- as.integer(col)
            col[col < 1L] <- NA_integer_
            col <- p[((col - 1L)%%pl) + 1L]
        }
        zc <- col[zi + 1L]
        dim(zc) <- dim(z)
        zc <- t(zc)[ncol(zc):1L, ]
        rasterImage(as.raster(zc), min(x), min(y), max(x), max(y), 
            interpolate = FALSE)
    }
    else .Internal(image(as.double(x), as.double(y), as.integer(zi), 
        col))
}
###############################################

image.spam<-function (x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)), 
    z, zlim = range(z), xlim = range(x), ylim = range(y), col = heat.colors(12), 
    add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab, breaks, 
    oldstyle = FALSE, cex = NULL, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                if (is.null(dim(x))) 
                  stop("argument must be matrix-like")
                z <- x
                x <- seq(0, 1, len = nrow(z))
            }
            if (missing(xlab)) 
                xlab <- ""
            if (missing(ylab)) 
                ylab <- ""
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        xn <- deparse(substitute(x))
        if (missing(xlab)) 
            xlab <- paste(xn, "x", sep = "$")
        if (missing(ylab)) 
            ylab <- paste(xn, "y", sep = "$")
        y <- x$y
        x <- x$x
    }
    else {
        if (missing(xlab)) 
            xlab <- if (missing(x)) 
                ""
            else deparse(substitute(x))
        if (missing(ylab)) 
            ylab <- if (missing(y)) 
                ""
            else deparse(substitute(y))
    }
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("'x' and 'y' values must be finite and non-missing")
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    spamversion <- (prod(z@dimension) > .Spam$imagesize)
    if (!spamversion) {
        image.default(x, y, as.matrix(z), ...)
    }
    else {
        if (!is.spam(z)) 
            stop("'z' must be a matrix")
        xx <- x
        yy <- y
        if (length(x) > 1 && length(x) == nrow(z)) {
            dx <- 0.5 * diff(x)
            x <- c(x[1] - dx[1], x[-length(x)] + dx, x[length(x)] + 
                dx[length(x) - 1])
        }
        if (length(y) > 1 && length(y) == ncol(z)) {
            dy <- 0.5 * diff(y)
            y <- c(y[1] - dy[1], y[-length(y)] + dy, y[length(y)] + 
                dy[length(y) - 1])
        }
        zvals <- z@entries
        if (missing(breaks)) {
            nc <- length(col)
            if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 
                0)) 
                stop("invalid z limits")
            if (diff(zlim) == 0) 
                zlim <- if (zlim[1] == 0) {
                  c(-1, 1)
                }
                else zlim[1] + c(-0.4, 0.4) * abs(zlim[1])
            zvals <- (zvals - zlim[1])/diff(zlim)
            zi <- if (oldstyle) {
                floor((nc - 1) * zvals + 0.5)
            }
            else floor((nc - 1e-05) * zvals + 1e-07)
            zi[zi < 0 | zi >= nc] <- NA
        }
        else {
            if (length(breaks) != length(col) + 1) 
                stop("must have one more break than colour")
            if (any(!is.finite(breaks))) 
                stop("breaks must all be finite")
            zi <- .C("bincode", as.double(zvals), length(zvals), 
                as.double(breaks), length(breaks), code = vector("integer", 
                  length(zvals)), (TRUE), (TRUE), nok = TRUE, 
                NAOK = TRUE, DUP = FALSE, PACKAGE = "base")$code - 
                1
        }
        if (!add) 
            plot(NA, NA, xlim = xlim, ylim = ylim, type = "n", 
                xaxs = xaxs, yaxs = yaxs, xlab = xlab, ylab = ylab, 
                ...)
        if (length(xx) != nrow(z) || length(yy) != ncol(z)) 
            stop("dimensions of z are not length(x) times length(y)")
        if (missing(cex)) {
            warning("default value for 'cex' in 'image' might be a bad choice", 
                call. = FALSE)
            cex <- 1
        }
        points(xx[rep.int((1:nrow(z)), diff(z@rowpointers))], 
            yy[z@colindices], pch = ".", cex = cex * .Spam$cex/sum(z@dimension), 
            col = col[zi + 1])
    }
    box()
}
#########################################################################

image.smooth<-function (x, wght = NULL, dx = 1, dy = 1, kernel.function = double.exp, 
    theta = 1, grid = NULL, tol = 1e-08, xwidth = NULL, ywidth = NULL, 
    weights = NULL, ...) 
{
    if (is.list(x)) {
        Y <- x$z
        grid <- list(x = x$x, y = x$y)
    }
    else {
        Y <- x
    }
    if (!is.matrix(Y)) {
        stop("Requires a matrix")
    }
    m <- nrow(Y)
    n <- ncol(Y)
    if (!is.null(wght)) {
        dx <- wght$dx
        dy <- wght$dy
        xwidth <- wght$xwidth
        ywidth <- wght$ywidth
    }
    if (is.null(grid)) {
        grid <- list(x = (1:m) * dx, y = (1:n) * dy)
    }
    else {
        dx <- grid$x[2] - grid$x[1]
        dy <- grid$y[2] - grid$y[1]
    }
    if (is.null(xwidth)) {
        xwidth <- dx * m
    }
    if (is.null(ywidth)) {
        ywidth <- dy * n
    }
    if (is.null(wght)) {
        wght <- setup.image.smooth(nrow = m, ncol = n, xwidth = xwidth, 
            ywidth = ywidth, dx = dx, dy = dy, kernel.function = kernel.function, 
            theta = theta)
    }
    M <- nrow(wght$W)
    N <- ncol(wght$W)
    temp <- matrix(0, nrow = M, ncol = N)
    temp2 <- matrix(0, nrow = M, ncol = N)
    if (!is.null(weights)) {
        temp[1:m, 1:n] <- Y * weights
        temp[is.na(temp)] <- 0
        temp2[1:m, 1:n] <- ifelse(!is.na(Y), weights, 0)
    }
    else {
        temp[1:m, 1:n] <- Y
        temp[is.na(temp)] <- 0
        temp2[1:m, 1:n] <- ifelse(!is.na(Y), 1, 0)
    }
    temp <- Re(fft(fft(temp) * wght$W, inverse = TRUE))[1:m, 
        1:n]
    temp2 <- Re(fft(fft(temp2) * wght$W, inverse = TRUE))[1:m, 
        1:n]
    temp <- ifelse((temp2 > tol), (temp/temp2), NA)
    list(x = grid$x, y = grid$y, z = temp)
}



