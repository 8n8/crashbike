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
-- PID parameters for the lean controller.


module Optimise ( optimumPID ) where


import BikeState
import qualified Data.List as Dl
import qualified Graphics.Gloss.Data.ViewPort as Gdv
import qualified Numeric.GSL.Minimization as Ngm
import qualified Stepper as St


-- It tries to find the best PID values for the lean controller.
-- It runs the simulation many times and tries to minimise the
-- sum of the squared distance from the vertical.
optimumPID :: Bike -> [Double]
optimumPID b = 
  fst (Ngm.minimize Ngm.NMSimplex2 1E-4 1000 [1,1,1] minfunc [0,0,0])
  where
    minfunc :: [Double] -> Double
    minfunc pid = sum [i*i | i <- (errlist b pid)]


-- It runs a single simulation of 2000 steps of the simulator
-- and generates a list of values of the lean angle corresponding 
-- to it.
errlist :: Bike -> [Double] -> [Double] 
errlist b pid = take 2000 [phi s | s <- statelist]
  where
    -- It constructs an infinite list of bike states, each
    -- constructred from the previous one using the 'stepper'
    -- function from the Stepper module.
    statelist = Dl.unfoldr (\a -> Just (a, state a)) withpid 
    state :: Bike -> Bike
    state d = St.stepper junk (1/30) d
    withpid = pidb pid
    -- It updates the bike state with the new pid values.
    pidb :: [Double] -> Bike
    pidb [p,i,d] = b { pidphiP = p
                     , pidphiI = i
                     , pidphiD = d }
    pidb _ = error "The input to pidb function was wrong."
    -- The first element of the stepper function passed to
    -- the simulate has to be of type Gdv.ViewPort.  This
    -- contains information about the user input to the graphics
    -- window, such as scaling and moving.  In this case it is not
    -- used, so this is a token instance to fill up the argument.
    -- It does not matter what the values are.
    junk = Gdv.ViewPort {Gdv.viewPortTranslate = (0,0)
                        ,Gdv.viewPortRotate = 0
                        ,Gdv.viewPortScale = 0 }
