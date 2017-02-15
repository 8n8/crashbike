-- Copyright 2017 True Ghiassi true@ghiassitrio.co.uk

-- This file is part of Crashbike.

-- Crashbike is free software: you can redistribute it
-- and/or modify it under the terms of the GNU General
-- Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at
-- your option) any later version.

-- Crashbike is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the
-- implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public
-- License for more details.

-- You should have received a copy of the GNU General
-- Public License along with Crashbike.  If not, see
-- <http://www.gnu.org/licenses/>.

-- DESCRIPTION

-- This module provides a function to choose the best
-- PID parameters.

module Optimise ( optimumPID ) where

import BikeState
import qualified Control.Monad as Cm
import qualified Data.Complex as Dc
import qualified Data.List as Dl
import qualified Data.List.Extras.Argmax as Min
import qualified Graphics.Gloss.Data.ViewPort as Gdv
import qualified Numeric.GSL.Minimization as Ngm
import qualified Stepper as St

optimumPID :: Bike -> [Double]
optimumPID b = Min.argmin minfunc perms
  -- The arguments to 'minimize' are
  -- 1) the algorithm to use
  -- 2) the required accuracy
  -- 3) the maximum number of runs
  -- 4) the initial search box.  I assume that this is the
  --    difference from the initial guess to search within
  --    initially.
  -- 5) the function to minimize
  -- 6) the initial guess
  
  -- fst (Ngm.minimize Ngm.NMSimplex2 1E-2 1000000 [10,10,10] minfunc [1,1,1])
  -- where
    -- It gives the largest error of the simulation run
    -- corresponding to the PID values.
  where
    perms :: [[Double]] 
    perms = Cm.replicateM 3 [-20,-19..20]
    minfunc pid = leastSquaredError b pid

-- It works out the least-squared difference between a gain
-- magnitude of 1 for a range of frequencies and the actual
-- gain for the given PID parameters.
leastSquaredError :: Bike -> [Double] -> Double
leastSquaredError b pid = sum [(1-g)**2 | g <- gains]
  where
    gains = [Dc.magnitude i | i <- freqResponses]
    freqResponses = [frequencyResponse omega pid b | omega <- omegas]
    omegas = [1,1.2..6]
    
frequencyResponse :: Double -> [Double] -> Bike -> Dc.Complex Double
frequencyResponse omega pid b =
  (d*w2 - j*p*z omega - i) /
  ((z (m21 b) + d)*w2 +
   (-j* z (c21 b) - j*p)* z omega - k21 - i)
  where
    w2 = omega**2 Dc.:+ 0
    j = 0 Dc.:+ 1
    p = z (head pid)
    i = z (pid !! 1) 
    d = z (pid !! 2) 
    v2 = z (v b**2)
    k21 = z 9.81 * z (ko21 b) + v2 * z (kt21 b)
    z :: Double -> Dc.Complex Double
    z a = a Dc.:+ 0

-- It runs a single simulation of 2000 steps of 0.03 seconds each
-- and generates a list of values of y or phi corresponding to it.
-- The choice of phi or y depends on the command-line arguments.
errlist :: Bike -> [Double] -> String -> [Double] 
errlist b pid tunetype
  -- It calculates the list of y-values of the contact point
  -- of the back wheel with the ground corresponding to all
  -- the states of one simulation.
  | tunetype == "steer" =
    take 2000 [abs (y s + w s * cos (psi s)) | s <- statelist]
  -- It calculates a list of bike lean values.
  | tunetype == "lean" =
    take 2000 [abs (phi s) | s <- statelist]
  | otherwise = error "Invalid arguments." 
  where
    -- It constructs an infinite list of bike states, each
    -- constructred from the previous one using the 'stepper'
    -- function.
    statelist = Dl.unfoldr (\a -> Just (a, state a)) withpid 
    state :: Bike -> Bike
    state d = St.stepper junk 0.03 d
    withpid = pidb pid
    -- It updates the bike state with the new pid values.
    pidb :: [Double] -> Bike
    pidb [p,i,d]
      | tunetype == "steer" = b { piddirP = p
                                , piddirI = i
                                , piddirD = d }
      | tunetype == "lean" = b { pidphiP = p
                               , pidphiI = i
                               , pidphiD = d }
      | otherwise = error "Wrong arguments"
    pidb _ = error "Wrong input to pidb in optimumPID."
    -- The first element of the stepper function passed to
    -- the simulate has to be of type Gdv.ViewPort.  This
    -- contains information about the user input to the graphics
    -- window, such as scaling and moving.  In this case it is not
    -- used, so this is a token instance to fill up the argument.
    -- It does not matter what the values are.
    junk = Gdv.ViewPort {Gdv.viewPortTranslate = (0,0)
                        ,Gdv.viewPortRotate = 0
                        ,Gdv.viewPortScale = 0 }

