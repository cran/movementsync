l1 <- list(a1 = c(100, 200), a2 = c(250, 300), a3 = c(400, 550), a4 = c(600, 650))
split1_dfr <- splice_time(l1)

l2 <- list(b1 = c(150, 275), b2 = c(610, 640))
split2_dfr <- splice_time(l2)

l3 <- list(c1 = c(275, 325), c2 = c(600, 675), c3 = c(700, 725))
split3_dfr <- splice_time(l3)

l4 <- list(d1 = c(0, 50))
split4_dfr <- splice_time(l4)

test_that("Union of splices is correct", {
  output_splice <- merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr,
                                operation = 'union')
  expect_equal(output_splice$Start, c(100, 400, 600, 700))
  expect_equal(output_splice$End, c(325, 550, 675, 725))
})

test_that("Intersection of splices is correct", {
  output_splice <- merge_splice(x = split2_dfr, y = split4_dfr, operation = 'intersection')
  expect_equal(nrow(output_splice), 0)
  output_splice <- merge_splice(x = split1_dfr, y = split2_dfr, z = split3_dfr,
                                operation = 'intersection')
  expect_equal(output_splice$Start, 610)
  expect_equal(output_splice$End, 640)
})
