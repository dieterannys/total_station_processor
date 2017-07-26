scr_init <- function() {
    "PDMODE\n3\n"
}

scr_export <- function(scr_data) {
    scr_data <- substring(scr_data, 1, nchar(scr_data) - 1)
    write(scr_data, "out.scr")
}

scr_com_ucs <- function(ucs) {
    if (missing(ucs)) {
        return(paste("UCS\n\n",
              sep = ""))
    } else {
        ucs_xy <- round(get_ucs_xy(ucs), 5)
        return(paste("UCS\n3\n",
              paste(ucs_xy$x[1], ucs_xy$y[1], ucs_xy$z[1], sep = ","), "\n",
              "@", paste(ucs_xy$x[2], ucs_xy$y[2], ucs_xy$z[2], sep = ","), "\n",
              "@", paste(ucs_xy$x[3], ucs_xy$y[3], ucs_xy$z[3], sep = ","), "\n",
              sep = ""))
    }
}

scr_com_circle <- function(circle) {
    paste("CIRCLE\n",
          paste(circle$x, circle$y, circle$z, sep = ","), "\n",
          circle$r, "\n",
          sep = "")
}
