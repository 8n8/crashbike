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

-- The purpose of this module is to move the bike
-- simulation forward by one step.  The 'stepper'
-- function takes the bike state and a small period
-- of time and finds the next state.


module Stepper ( stepper ) where


import BikeState
import qualified Numeric.GSL.ODE as Ode
import qualified Numeric.LinearAlgebra as Nla
import qualified System.Random as R
import qualified GHC.Float as Gf
import qualified Graphics.Gloss.Data.ViewPort as Gdv


-- It calculates the derivatives used in the numerical
-- integration.  See the reference Meijaard 07 in the
-- README for the theory.
derivatives :: Bike -> Bike
derivatives b = 
  b { phidot = p2
    , phidotdot = -(m22 b*tphi b -
                     m12 b*tdelta b +
                     (cv22*m12 b - cv12*m22 b)*d2 +
                     (k22 *m12 b - k12 *m22 b)*d1 +
                     (cv21*m12 b - cv11*m22 b)*p2 +
                     (k21 *m12 b - k11 *m22 b)*p1) /
                  (m12 b*m21 b - m11 b*m22 b)
    , deltadot = d2
    , deltadotdot = (m21 b*tphi b -
                      m11 b*tdelta b +
                      (cv22*m11 b - cv12*m21 b)*d2 +
                      (k22*m11 b - k12*m21 b)*d1 +
                      (cv21*m11 b - cv11*m21 b)*p2 +
                      (k21*m11 b - k11*m21 b)*p1) /
                    (m12 b*m21 b - m11 b*m22 b)
    , psidot = (v b*d1 + c b*d2)*(cos (lamd b))/w b
    , xdot = v b*(cos (psi b))
    , ydot = v b*(sin (psi b))
    , thetadotr = v b/rr b
    , thetadotf = v b/rf b }
  where
    p1 = phi b
    p2 = phidot b
    d1 = delta b
    d2 = deltadot b
    v2 = v b**2
    cv11 = v b*c11 b
    cv12 = v b*c12 b
    cv21 = v b*c21 b
    cv22 = v b*c22 b
    g = 9.81
    k11 = g*ko11 b + v2*kt11 b
    k12 = g*ko12 b + v2*kt12 b
    k21 = g*ko21 b + v2*kt21 b
    k22 = g*ko22 b + v2*kt22 b


-- It steps the state of the bike on by one time interval.
stepper :: Gdv.ViewPort -> Float -> Bike -> Bike
stepper _ t b =  d { phi = head sol
                   , phidot = sol !! 1
                   , counter = counter b + 1
                   , phidotSum = (phidotSum b) + phidot b
                   , delta = sol !! 2
                   , deltadot = sol !! 3
                   , psi = sol !! 4
                   , x = sol !! 5
                   , y = sol !! 6
                   , xdot = v b*(cos (psi b))
                   , ydot = v b*(sin (psi b))
                   , ySum = (ySum b) + y b
                   , thetaf = sol !! 7
                   , thetar = sol !! 8
                   , tphi =
                     if mod (counter b) 60 == 0
                     then fst randTorque
                     else tphi b
                   , tdelta = controller b
                   , randGen = snd randTorque }
  where
    randTorque = R.randomR (-maxbiff b,maxbiff b) (randGen b)
    tVec = Nla.fromList ([0,Gf.float2Double t])
    sol = (Nla.toLists solutionmatrix) !! 1
    solutionmatrix = Ode.odeSolve xd initialConditions tVec
    initialConditions = [ phi b
                        , phidot b
                        , delta b
                        , deltadot b
                        , psi b
                        , x b
                        , y b
                        , thetaf b
                        , thetar b ]
    xd :: Double -> [Double] -> [Double]
    xd _ _ = [ phidot d
             , phidotdot d
             , deltadot d
             , deltadotdot d
             , psidot d
             , xdot d
             , ydot d
             , thetadotf d
             , thetadotr d ]
    d = derivatives b


-- It calculates the torque to apply to the steering
-- from the PID parameters and the errors in position
-- and lean angle.
controller :: Bike -> Double 
controller b =
  pidphiP b * phidot b + pidphiI b * phidotSum b +
  pidphiD b * phidotdot b +
  piddirP b * y b + piddirI b * ySum b +
  piddirD b * ydot b
