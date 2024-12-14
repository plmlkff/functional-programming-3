module Main (main) where

import InterpolationUtil
import REPLUtil

-- | Welcome message
welcomeInvite :: IO ()
welcomeInvite =
  putStrLn "Type 'exit' to exit."
    >> putStrLn "Enter a point (x, y) to calculate dots from the origin by step = 1."
    >> putStrLn "x and y should be decimal numbers."

-- Main function to read input lazily, parse it, and print the integers
main :: IO ()
main = welcomeInvite >> loop_ [linearInterpolation, lagrangeInterpolation]
