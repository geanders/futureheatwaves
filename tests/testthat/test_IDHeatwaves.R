library(futureheatwaves)
context("Identifying heatwaves")

data("datafr", package = "futureheatwaves")
hws_r <- IDHeatwavesR(datafr = datafr, threshold = 84.632, numDays = 2)
hws_cpp <- IDHeatwavesCPPwrapper(datafr = datafr, threshold = 84.632,
                                 numDays)

test_that("C++ and R ID heatwaves identify same heatwaves", {
        expect_equal(hws_r$hw, hws_cpp$hw)
        expect_equal(hws_r$hw.number, hws_cpp$hw.number)
})
