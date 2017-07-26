easy_bf_circle <- function(xyz) {
    scr_data <- ""
    ucs <- best_fit_ucs(xyz)
    circle_in_ucs <- round(with_ucs(ucs, xyz, best_fit_circle, convert_back = F), 5)

    scr_data <- paste(scr_data, scr_com_ucs(ucs), sep = "")
    scr_data <- paste(scr_data, scr_com_circle(circle_in_ucs), sep = "")
    scr_data <- paste(scr_data, scr_com_ucs(), sep = "")

    scr_data
}
