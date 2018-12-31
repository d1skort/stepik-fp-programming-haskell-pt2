{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Temperatures where


newtype Temperature a = Temperature Double deriving (Num, Show, Fractional, Eq)

data Celsius
data Fahrenheit
data Kelvin

comfortTemperature :: Temperature Celsius
comfortTemperature = Temperature 23

c2f :: Temperature Celsius -> Temperature Fahrenheit
c2f (Temperature c) = Temperature (1.8 * c + 32)

k2c :: Temperature Kelvin -> Temperature Celsius
k2c (Temperature k) = Temperature (k - 273.15)
