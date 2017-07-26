rotate_points <- function(xyz, ax, ay, az, reverse = F) {
    ax <- ax * pi / 180
    ay <- ay * pi / 180
    az <- az * pi / 180
    Rx <- matrix(c(1, 0, 0, 0, cos(ax), sin(ax), 0, - sin(ax), cos(ax)), nrow = 3, ncol = 3)
    Ry <- matrix(c(cos(ay), 0, - sin(ay), 0, 1, 0, sin(ay), 0, cos(ay)), nrow = 3, ncol = 3)
    Rz <- matrix(c(cos(az), sin(az), 0, - sin(az), cos(az), 0, 0, 0, 1), nrow = 3, ncol = 3)
    xyzr <- xyz
    if (!reverse) {
        xyzr <- t(apply(xyzr, 1, function(x) Rx %*% as.matrix(x)))
        xyzr <- t(apply(xyzr, 1, function(x) Ry %*% as.matrix(x)))
        xyzr <- t(apply(xyzr, 1, function(x) Rz %*% as.matrix(x)))
    } else {
        xyzr <- t(apply(xyzr, 1, function(x) Rz %*% as.matrix(x)))
        xyzr <- t(apply(xyzr, 1, function(x) Ry %*% as.matrix(x)))
        xyzr <- t(apply(xyzr, 1, function(x) Rx %*% as.matrix(x)))
    }

    xyzr <- data.frame(x = xyzr[, 1], y = xyzr[, 2], z = xyzr[, 3])
    xyzr
}

gen_circle <- function(n, r, ax, ay, az) {
    a <- seq(0, 360 - 360 / n, 360 / n) / 180 * pi
    x <- r * cos(a)
    y <- r * sin(a)
    z <- rep(0, n)
    circle <- data.frame(x = x, y = y, z = z)
    circle <- rotate_points(circle, ax, ay, az)
    circle
}

plot_data_3D <- function(xyz) {
    plotxy <- ggplot(data = xyz, aes(x = x, y = y, color = z)) +
        geom_point() +
        theme(legend.position = "none")
    plotxz <- ggplot(data = xyz, aes(x = x, y = z, color = y)) +
        geom_point() +
        theme(axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              legend.position = "none")
    plotyz <- ggplot(data = xyz, aes(x = y, y = z, color = x)) +
        geom_point() +
        theme(axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              legend.position = "none")

    lay <- rbind(c(3, 2), c(NA, 1))

    grid.arrange(plotxy, plotxz, plotyz, layout_matrix = lay)
}

best_fit_ucs <- function(xyz) {
    pca <- prcomp(xyz)
    v <- pca$rotation
    beta <- -v[-ncol(v), ncol(v)] / v[ncol(v), ncol(v)]
    ay <- atan(beta[["x"]]) * 180 / pi

    tx <- pca$center[1]
    ty <- pca$center[2]
    tz <- pca$center[3]

    xyz <- rotate_points(xyz, 0, ay, 0)

    pca <- prcomp(xyz)
    v <- pca$rotation
    beta <- -v[-ncol(v), ncol(v)] / v[ncol(v), ncol(v)]
    ax <- -atan(beta[["y"]]) * 180 / pi

    list(tx = tx, ty = ty, tz = tz, ax = -ax, ay = -ay, az = 0)
}

best_fit_circle <- function(xyz) {
    x <- xyz$x + 100
    y <- xyz$y + 100

    f <- function(c) {
        d <- sqrt((x - c[1]) ** 2 + (y - c[2]) ** 2)
        r <- mean(d)
        ls <- sum((r - d) ** 2)
        ls
    }

    cxy <- optim(c(mean(x), mean(y)), f)$par

    cx <- cxy[1]
    cy <- cxy[2]
    r <- mean(sqrt((x - cx) ** 2 + (y - cy) ** 2))

    data.frame(x = cx - 100, y = cy - 100, z = 0, r = r)
}

with_ucs <- function(ucs, xyz, fun = NULL, ..., convert_back = T) {
    xyz$x <- xyz$x - ucs$tx
    xyz$y <- xyz$y - ucs$ty
    xyz$z <- xyz$z - ucs$tz

    xyz <- rotate_points(xyz, - ucs$ax, - ucs$ay, - ucs$az, T)

    res <- fun(xyz, ...)

    if (sum(c("x", "y", "z") %in% names(res)) == 3 & convert_back) {
        resxyz <- data.frame(x = res$x, y = res$y, z = res$z)
        resxyz <- rotate_points(resxyz, ucs$ax, ucs$ay, ucs$az)
        resxyz$x <- resxyz$x + ucs$tx
        resxyz$y <- resxyz$y + ucs$ty
        resxyz$z <- resxyz$z + ucs$tz
        res$x <- resxyz$x
        res$y <- resxyz$y
        res$z <- resxyz$z
    }

    res
}

get_ucs_xy <- function(ucs) {
    xyz <- data.frame(x = c(0, 1, -1),
                      y = c(0, 0, 1),
                      z = c(0, 0, 0))
    xyz <- rotate_points(xyz, ucs$ax, ucs$ay, ucs$az)
    xyz$x[1] <- xyz$x[1] + ucs$tx
    xyz$y[1] <- xyz$y[1] + ucs$ty
    xyz$z[1] <- xyz$z[1] + ucs$tz

    xyz
}

xyz_filt <- function(pts, id_starts_with) {
    (pts[startsWith(as.character(pts$id), id_starts_with), c("x", "y", "z")])
}
