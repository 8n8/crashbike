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
import qualified Data.Complex as Dc
import qualified Data.List as Dl
import qualified Graphics.Gloss.Data.ViewPort as Gdv
import qualified Numeric.GSL.Minimization as Ngm
import qualified Stepper as St

-- It is supposed to find the optimum PID parameters for one
-- of the tuners, depending on the command line arguments.
-- It is actually a pretty rubbish optimiser.  I think that
-- the 'minfunc' output must be quite erratic, so possibly
-- it misses minimums.  Whatever the reason, it works very
-- badly.  One possibility is to replace it with some frequency-
-- doman analysis, using Bode plots.  This could use the
-- Laplace transforms of the equations of motion and try
-- to find PID parameters that produce a low gain for a
-- reasonable range of, say, 30 different frequencies.
optimumPID :: Bike -> String -> [Double]
optimumPID b tunetype = 
  -- The arguments to 'minimize' are
  -- 1) the algorithm to use
  -- 2) the required accuracy
  -- 3) the maximum number of runs
  -- 4) the initial search box.  I assume that this is the
  --    difference from the initial guess to search within
  --    initially.
  -- 5) the function to minimize
  -- 6) the initial guess
  fst (Ngm.minimize Ngm.NMSimplex2 1E-3 20000 [5,5,5] minfunc [1,1,1])
  where
    -- It gives the largest error of the simulation run
    -- corresponding to the PID values.
    minfunc :: [Double] -> Double
    minfunc pid = maximum (errlist b pid tunetype)

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

frequencyResponse :: Float -> Bike -> Dc.Complex
frequencyResponse omega b =
  (d*w2 - j*p*omega - i) /
  ((m21 b + d)*w2+(-j*c21 b - j*p)*omega - k21 b - i)
  where
    w2 = omega**2
    j = 0 Dc.:+ 1
    p = pidphiP b
    i = pidphiI b
    d = pidphiD b