set.seed(123)

r <- raster::raster(matrix(1, nrow=50, ncol=50))
output <- establish_by_place_conquer(potential_space= r,
                                   cell_size=1,
                                   includsion_value = 1,
                                   mean_field_size = 200,
                                   sd_field_size = 100,
                                   distribution = "norm",
                                   mean_shape_index = 3,
                                   sd_shape_index = 0.3,
                                   percent = 50,
                                   assign_farmers = TRUE,
                                   assign_mode = 2,
                                   mean_fields_per_farm = 3,
                                   sd_fields_per_farm = 3)

test_obj1 <- return_by_arable_land(output, method = 2)
test_obj2 <- return_by_field(output, method = 2)

test_that("basic placment works", {

  expect_equal(raster::freq(test_obj1)[, "count"][1], 1222)
  expect_equal(raster::freq(test_obj1)[, "count"][2], 1278)
  expect_equal(raster::freq(test_obj2)[, "count"][1], 1222)
  expect_equal(length(raster::freq(test_obj2)[, 1]), 10)

})
