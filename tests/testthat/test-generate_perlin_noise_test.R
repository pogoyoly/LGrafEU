test_that("percentage function works", {
  expect_equal(raster::freq(generate_perlin_noise(100,100,1,2,3,0.01,TRUE, "land_percentage", percetange = 50))[, "count"][1], 5000)

})
