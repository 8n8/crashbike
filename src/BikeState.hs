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

-- This module defines the Bike record, which contains the
-- state data and other constants needed for the simulation.
-- It also sets the initial state of the bicycle.

module BikeState where

import qualified System.Random as R
import qualified Data.Default as Dd
  
-- It sets the initial values for the bike state.  The final
-- value is the random generator used to make the random
-- disturbances.  See the Random module on the Hackage
-- website for more details.
instance Dd.Default Bike where
  def = Bike 0 0 0 0 0 0 0 0 0 0
             0 0 0 0 0 0 0 0 0 0   
             0 0 0 0 0 0 0 0 0 0   
             0 0 0 0 0 0 0 0 0 0   
             0 0 0 0 0 0 0 0 0 0   
             0 0 0 0 0 0 0 0 0 0   
             0 0 0 0 0 0 0 0 0 0 
             (R.mkStdGen 1)

-- It contains all the information needed for the bike
-- state.   
data Bike = Bike { w :: Double
                 , c :: Double 
                 , lamd :: Double
                 , rr :: Double
                 , mr :: Double
                 , irxx :: Double
                 , iryy :: Double
                 , xb :: Double
                 , zb :: Double
                 , mb :: Double
                 , ibxx :: Double
                 , ibyy :: Double
                 , ibzz :: Double
                 , ibxz :: Double
                 , xh :: Double
                 , zh :: Double
                 , ihxx :: Double
                 , ihxz :: Double
                 , ihyy :: Double
                 , ihzz :: Double
                 , mh :: Double
                 , rf :: Double
                 , mf :: Double
                 , ifxx :: Double
                 , ifyy :: Double
                 , m11 :: Double
                 , m12 :: Double
                 , m21 :: Double
                 , m22 :: Double
                 , ko11 :: Double
                 , ko12 :: Double
                 , ko21 :: Double
                 , ko22 :: Double
                 , kt11 :: Double
                 , kt12 :: Double
                 , kt21 :: Double
                 , kt22 :: Double
                 , c11 :: Double
                 , c12 :: Double
                 , c21 :: Double
                 , c22 :: Double
                 , phi :: Double
                 , phidotdot :: Double
                 , delta :: Double
                 , deltadot :: Double
                 , deltadotdot :: Double
                 , psi :: Double
                 , psidot :: Double
                 , x :: Double
                 , xdot :: Double
                 , ydot :: Double
                 , tphi :: Double
                 , tdelta :: Double
                 , v :: Double
                 , thetaf :: Double
                 , thetadotf :: Double
                 , thetar :: Double
                 , thetadotr :: Double
                 , pidphiP :: Double
                 , pidphiI :: Double
                 , pidphiD :: Double
                 , piddirP :: Double
                 , piddirI :: Double
                 , piddirD :: Double
                 , phidot :: Double
                 , maxbiff :: Double
                 , y :: Double
                 , ySum :: Double
                 , counter :: Int
                 , phidotSum :: Double
                 , randGen :: R.StdGen }
