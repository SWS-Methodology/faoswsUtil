context("Normalise")

test_that("Normalise correctly transforms data", {
    d <- data.table::data.table(geographicAreaM49 = c("4", "466", "466"), 
                    measuredElement = c("5417", "5031", "5417"), 
                    measuredItemCPC = c("02111", "02111", "02111"), 
                    Value_timePointYears_2005 = c(0, 4500000, 0), 
                    flagObservationStatus_timePointYears_2005 = c("M", "E", "M"), 
                    flagMethod_timePointYears_2005 = c("u", "f", "u"), 
                    Value_timePointYears_2015 = c(NA_real_, NA_real_, NA_real_), 
                    flagObservationStatus_timePointYears_2015 = c(NA_character_, NA_character_, NA_character_), 
                    flagMethod_timePointYears_2015 = c(NA_character_, NA_character_, NA_character_))
    nd <- data.table::data.table(geographicAreaM49 = c("4", "466", "466"), 
                     measuredItemCPC = c("02111", "02111", "02111"), 
                     measuredElement = c("5417", "5031", "5417"), 
                     timePointYears = c("2005", "2005", "2005"), 
                     Value = c(0, 4500000, 0), 
                     flagObservationStatus = c("M", "E", "M"), 
                     flagMethod = c("u", "f", "u"))
    
    expect_equivalent(nd, normalise(d))
})

test_that("Normalise transforms types correctly",{
    d <- data.table::data.table(geographicAreaM49 = c("4", "466", "466"), 
                                measuredElement = c("5417", "5031", "5417"), 
                                measuredItemCPC = c("02111", "02111", "02111"), 
                                Value_timePointYears_2005 = c(0, 4500000, 0), 
                                flagObservationStatus_timePointYears_2005 = c("M", "E", "M"), 
                                flagMethod_timePointYears_2005 = c("u", "f", "u"), 
                                Value_timePointYears_2015 = c(NA_real_, NA_real_, NA_real_), 
                                flagObservationStatus_timePointYears_2015 = c(NA_character_, NA_character_, NA_character_), 
                                flagMethod_timePointYears_2015 = c(NA_character_, NA_character_, NA_character_))
    
    expect_is(normalise(d), "data.table")
    expect_is(normalise(as.data.frame(d)), "data.frame")
})
