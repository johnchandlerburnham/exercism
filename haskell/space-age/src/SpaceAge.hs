module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Earth   sec = sec / 31557600
ageOn Mercury sec = (ageOn Earth sec) /   0.24084670
ageOn Venus   sec = (ageOn Earth sec) /   0.61519726
ageOn Mars    sec = (ageOn Earth sec) /   1.88081580
ageOn Jupiter sec = (ageOn Earth sec) /  11.86261500
ageOn Saturn  sec = (ageOn Earth sec) /  29.44749800
ageOn Uranus  sec = (ageOn Earth sec) /  84.01684600
ageOn Neptune sec = (ageOn Earth sec) / 164.79132000
