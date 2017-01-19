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

-- This module makes the initial state of the bike.  It
-- takes the contents of the ini file, and calculates
-- the intermediate constants needed for the equations
-- of motion.

module MakeStartState ( makeStartState ) where

import BikeState
import qualified Data.Default as Dd
import qualified Data.Either as De
import qualified Data.Ini as Di
import qualified Data.Text as Dt
import qualified System.Random as R

makeStartState :: Either String Di.Ini -> Bike
makeStartState ini = calculateConstants basic
  where basic = ini2bike (
          De.either (\a -> error a) id ini) Dd.def

-- It takes the contents of an ini file (see the Data.Ini
-- module on the Hackage website) and the state of the
-- bike and updates the bike state with the ini values.

-- The ini file contains the parameters that define
-- the bike, such as its mass and configuration and
-- so on.

-- An ini file is like this:

-- [header one]
-- someval = 23
-- anotherval = "hey"
  
-- [another header]
-- valval = 42
ini2bike :: Di.Ini -> Bike -> Bike
ini2bike ini b = b { w = gv "bike" "wheelbase" 
                   , c = gv "bike" "trail" 
                   , lamd = gv "bike" "steertilt" 
                   , rr = gv "back wheel" "radius"
                   , mr = gv "back wheel" "mass"
                   , irxx = gv "back wheel" "ixx"
                   , iryy = gv "back wheel" "iyy" 
                   , xb = gv "main frame" "comx"
                   , zb = gv "main frame" "comz"
                   , mb = gv "main frame" "mass"
                   , ibxx = gv "main frame" "ixx"
                   , ibyy = gv "main frame" "iyy"
                   , ibzz = gv "main frame" "izz"
                   , ibxz = gv "main frame" "ixz" 
                   , xh = gv "front fork" "comx"
                   , zh = gv "front fork" "comz"
                   , ihxx = gv "front fork" "ixx"
                   , ihxz = gv "front fork" "ixz"
                   , ihyy = gv "front fork" "iyy"
                   , ihzz = gv "front fork" "izz"
                   , mh = gv "front fork" "mass"
                   , rf = gv "front wheel" "radius"
                   , mf = gv "front wheel" "mass"
                   , ifxx = gv "front wheel" "ixx"
                   , ifyy = gv "front wheel" "iyy"
                   , maxbiff = gv "simulation" "maxbiff"
                   , randGen =
                     R.mkStdGen (round (gv "simulation" "randseed"))
                   , v = gv "simulation" "velocity"
                   , pidphiP = gv "lean controller" "p"
                   , pidphiI = gv "lean controller" "i"
                   , pidphiD = gv "lean controller" "d"
                   , piddirP = gv "route controller" "p"
                   , piddirI = gv "route controller" "i"
                   , piddirD = gv "route controller" "d" }
  where gv header key = getinival ini header key

-- It takes in an ini file (see the Data.Ini module)
-- and a header and a key and looks up the value.
-- The reason the output is set to 'Double' is that
-- in this case all the values in the ini are doubles.
getinival :: Di.Ini -> String -> String -> Double
getinival ini header key =
  De.either (\a -> error a) text2double val 
  where
    val = Di.lookupValue h k ini
    h = Dt.pack header
    k = Dt.pack key

-- It converts text containing numeric characters
-- into a Double.  The text type is a sort of
-- compressed string.
text2double :: Dt.Text -> Double
text2double a = read (Dt.unpack a) :: Double

-- It updates the bike state with constants derived
-- from the bike parameters given in the ini file.
-- These constants are needed later on for the
-- equations of motion.  See the main reference
-- Meijaard 07 in the README for all the formulas
-- used here.  Matrix elements are referred to as,
-- say, m12 for the element in the first row and
-- second column of matrix M.  Elements beginning
-- with kt belong to matrix K2.
calculateConstants :: Bike -> Bike
calculateConstants b =
  b { m11 = itxx 
    , m12 = ialx + mu*itxz
    , m21 = ialx + mu*itxz 
    , m22 = iall + 2*mu*ialz + (mu**2)*itzz
    , ko11 = mt*zt
    , ko12 = -sa
    , ko21 = -sa 
    , ko22 = -sa*sin (lamd b)
    , kt11 = 0 
    , kt12 = (st - mt*zt*cos (lamd b)) / w b
    , kt21 = 0 
    , kt22 = (sa + sf*sin (lamd b)) * cos (lamd b) / w b
    , c11 = 0 
    , c12 = mu*st + sf*cos (lamd b) +
            (itxz/w b)*cos (lamd b) - mu*mt*zt
    , c21 = -(mu*st + sf*cos (lamd b))
    , c22 = (ialz/w b)*cos (lamd b) +
            mu*(sa + (itzz/w b)*cos (lamd b)) }
  where
    itxx = irxx b + ibxx b + ihxx b + ifxx b +
           mr b *rr b**2 + mb b*zb b**2 + mh b*zh b**2 +
           mf b*rf b**2
    itxz = ibxz b + ihxz b - mb b *xb b*zb b -
           mh b*xh b*zh b + mf b*w b*rf b
    itzz = irzz + ibzz b + ihzz b + ifzz + mb b*xb b**2
           + mh b*xh b**2 + mf b*w b**2
    ialx = -ma*ua*za + iaxx*sin (lamd b) + iaxz*cos (lamd b)
    ma = mh b + mf b
    ua = (xa-w b-c b)*cos (lamd b) - za*sin (lamd b)
    xa = (xh b*mh b + w b*mf b) / ma
    za = (zh b*mh b - rf b*mf b) / ma
    iaxx = ihxx b + ifxx b + mh b*(zh b-za)**2 +
           mf b*(rf b+za)**2
    iaxz = ihxz b- mh b*(xh b-xa)*(zh b-za) +
           mf b*(w b-xa)*(rf b+za)
    iazz = ihzz b+ ifzz+ mh b*(xh b-xa)**2 +
           mf b*(w b-xa)**2 
    iall = ma*ua**2 + iaxx*(sin (lamd b))**2 +
           2*iaxz*sin (lamd b)* cos (lamd b) +
           iazz*(cos (lamd b))**2
    ialz = ma*ua*xa + iaxz*sin (lamd b) +
           iazz*cos (lamd b)
    mu = c b*(cos (lamd b))/w b
    mt = mr b + mb b + mh b + mf b
    zt = (-rr b*mr b + zb b*mb b + zh b*mh b -
          rf b*mf b) / mt
    xt = (xb b*mb b + xh b*mh b + w b*mf b) / mt
    sa = ma*ua + mu*mt*xt
    st = sr + sf
    sr = iryy b / rr b
    sf = ifyy b / rf b
    irzz = irxx b
    ifzz = ifxx b
