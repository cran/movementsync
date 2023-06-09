
test_that("Granger Causality Test functions agree on a window", {
  
  
  r1 <- get_sample_recording()
  fv_list <- get_filtered_views(r1, data_points = "Nose", n = 41, p =3)
  jv <- get_joined_view(fv_list)
  d1 <- get_duration_annotation_data(r1)
  splicing_df <- splice_time(jv, win_size = 5, step_size = 1)
  sv <- get_spliced_view(jv, splicing_df)
  
  
  v_list <- split(sv)
  w1 <- v_list$w1
  df <- w1$df
  g1 <- lmtest::grangertest(df$Nose_x_Central_Sitar, df$Nose_x_Central_Tabla, order = 5)
  g2 <- ms_grangertest1(df$Nose_x_Central_Sitar, df$Nose_x_Central_Tabla, order = 5)
  g3 <- ms_grangertest2(df$Nose_x_Central_Sitar, df$Nose_x_Central_Tabla, order = 5)
  expect_identical(g1, g2)
  expect_identical(g2, g3)
})
