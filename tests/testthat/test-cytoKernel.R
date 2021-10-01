library(testthat)
context("checking the cytoKernel package")

library(cytoKernel)

test_that("checking dimensions of CytoK object", {
  n_features<- 200
  n_samples<- 12
  set.seed(3456)
  data_test <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
                            nrow=n_features, ncol=(n_samples/2)), matrix(rnorm(1200,mean=5, sd=1.9),
                                                                         nrow=n_features, ncol=(n_samples/2)))
  group_test<- rep(c(0,1), each=(n_samples/2))
  CytoK_test <- CytoK(object=data_test,
                      group_factor = group_test)
  
  expect_equal(nrow(CytoKData(CytoK_test)),
               n_features)
  expect_equal(ncol(CytoKData(CytoK_test)),
               n_samples)
  expect_equal(nrow(CytoKDEData(CytoK_test)$DEdata),
  sum(CytoKFeatures(CytoK_test)$padj
      <=CytoKalpha(CytoK_test)))
  expect_equal(ncol(CytoKDEData(CytoK_test)$DEdata),
               n_samples)
})


test_that("checking CytoK output", {
  n_features<- 200
  n_samples<- 12
  set.seed(3456)
  data_test <- cbind(matrix(rnorm(1200,mean=2, sd=1.5),
                            nrow=n_features, ncol=(n_samples/2)), matrix(rnorm(1200,mean=5, sd=1.9),
                                                                         nrow=n_features, ncol=(n_samples/2)))
  group_test<- rep(c(0,1), each=(n_samples/2))
  CytoK_test <- CytoK(object=data_test,
                      group_factor = group_test)
  
  expect_equal(CytoKFeatures(CytoK_test)$EffectSize[5],
               9.003771,tolerance = 1e-06)
  expect_equal(CytoKFeatures(CytoK_test)$EffectSizeSD[50],
               1.304391,tolerance = 1e-06)
  expect_equal(CytoKFeatures(CytoK_test)$pval[113],
               0.3079874,tolerance = 1e-06)
  expect_equal(CytoKFeatures(CytoK_test)$padj[198],
               0.1278994,tolerance = 1e-06)
  expect_equal(CytoKFeaturesOrdered(CytoK_test)$EffectSize[25],
               9.484145,tolerance = 1e-06)
  expect_equal(CytoKFeaturesOrdered(CytoK_test)$EffectSizeSD[97],
               1.393466,tolerance = 1e-06)
  expect_equal(CytoKFeaturesOrdered(CytoK_test)$pval[167],
               0.1695305,tolerance = 1e-06)
  expect_equal(CytoKFeaturesOrdered(CytoK_test)$padj[14],
               3.191488e-08,tolerance = 1e-06)
  expect_equal(CytoKDEfeatures(CytoK_test),
               72.5,tolerance = 1e-06)
  
})


