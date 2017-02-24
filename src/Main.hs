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

-- Read the README.

module Main where

import BikeState
import qualified Data.Ini as Di
import qualified Graphics.Gloss as Gg
import qualified MakeStartState as MSS
import qualified Optimise as Op
import qualified System.Environment as Se
import qualified State2Picture as S2P
import qualified Stepper as St
  
-- It reads the ini file and the command line arguments
-- and passes them to the 'switcher' function.
main :: IO()
main = do
  args <- Se.getArgs
  -- It reads the ini file, which contains the parameters
  -- of the bike.
  ini <- Di.readIniFile (head args)
  switcher args (MSS.makeStartState ini)

-- It makes decisions about how to obey the command-line
-- arguments
switcher :: [String] -> Bike -> IO()
switcher [_] start = rungraphics start
switcher [_,"tune"] start = print (Op.optimumPID start)
switcher _ _ = print "Something was wrong with the\
  \ arguments.  The first argument should be the path\
  \ to the ini file containing the bike parameters.  This\
  \ is compulsory.  You can add a second argument, the word\
  \ 'tune', if you want to try to find the optimum PID\
  \ parameters instead."
  
rungraphics :: Bike -> IO()
rungraphics startstate = 
  Gg.simulate window Gg.green 30 startstate S2P.toPic St.stepper 

-- It sets the parameters for the graphics window.
window :: Gg.Display
window = Gg.InWindow "Robobike" (200,200) (10,10) 
