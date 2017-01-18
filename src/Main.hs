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

import qualified Data.Default as Dd
import qualified Data.Either as De
import qualified Data.Ini as Di
import qualified Data.List as Dl
import qualified Data.Text as Dt
import qualified GHC.Float as Gf
import qualified Graphics.Gloss as Gg
import qualified Graphics.Gloss.Data.ViewPort as Gdv
import qualified Numeric.LinearAlgebra as Nla
import qualified Numeric.GSL.ODE as Ode
import qualified Numeric.GSL.Minimization as Ngm
import qualified System.Environment as Se
import qualified System.Random as R
  
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
  
-- It takes the contents of an ini file (see the Data.Ini
-- module on the Hackage website) and the state of the
-- bike and updates the bike state with the ini values.
--  
-- The ini file contains the parameters that define
-- the bike, such as its mass and configuration and
-- so on.
--
-- An ini file is like this:
--
-- [header one]
-- someval = 23
-- anotherval = "hey"
--
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
  
-- It reads the ini file and the command line arguments
-- and passes them to the 'switcher' function.
main :: IO()
main = do
  args <- Se.getArgs
  ini <- Di.readIniFile (head args)
  switcher args ini

-- It makes decisions about how to obey the command-line
-- arguments
switcher :: [String] -> Either String Di.Ini -> IO()
switcher args ini  
  -- The first argument is the name of the ini file.  If
  -- is all there is, then just run the simulation.
  | length args == 1 = rungraphics start
  -- If there is tuning to be done, then there will be
  -- three input arguments, first the ini file name, then
  -- the word "tune", and then either "lean" or "steer" to
  -- tell it which controller to tune.
  | length args == 3
  , args !! 1 == "tune" = print (optimumPID start (args !! 2))
  | otherwise = print "Wrong arguments"
    where start = makeStartState ini

optimumPID :: Bike -> String -> [Double]
optimumPID b tunetype = 
  fst (Ngm.minimize Ngm.NMSimplex2 1E-3 20000 [5,5,5] minfunc [1,1,1])
  where
    minfunc :: [Double] -> Double
    minfunc pid = maximum (philist b pid tunetype)

philist :: Bike -> [Double] -> String -> [Double] 
philist b pid tunetype
  | tunetype == "steer" =
    take 2000 [abs (y s + w s * cos (psi s)) | s <- statelist]
  | tunetype == "lean" =
    take 2000 [abs (phi s) | s <- statelist]
  | otherwise = error "Invalid arguments." 
  where
    statelist = Dl.unfoldr (\a -> Just (a, state a)) withpid 
    state :: Bike -> Bike
    state d = stepper junk 0.03 d
    withpid = pidb pid
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
    junk = Gdv.ViewPort {Gdv.viewPortTranslate = (0,0)
                        ,Gdv.viewPortRotate = 0
                        ,Gdv.viewPortScale = 0 }
  
rungraphics :: Bike -> IO()
rungraphics startstate = 
  Gg.simulate window Gg.green 30 startstate toPic stepper 

makeStartState :: Either String Di.Ini -> Bike
makeStartState ini = calculateConstants basic
  where basic = ini2bike (
          De.either (\a -> error a) id ini) Dd.def

-- It sets the parameters for the graphics window.
window :: Gg.Display
window = Gg.InWindow "Robobike" (200,200) (10,10) 

-- It calculates the derivatives used in the numerical
-- integration.  See the main reference defined at the
-- top for the equations.
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
    randTorque = R.randomR ((-maxbiff b),maxbiff b) (randGen b)
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

-- It makes a round wheel centred on the origin, scales it
-- so that it looks more realistic when leaning and steering,
-- and moves it so that the origin is at the bottom of the
-- wheel.  The inputs are the initial radius of the wheel,
-- its angle, and the x and y scale factors.
scaledwheel :: Float -> Float -> Float -> Float -> Gg.Picture 
scaledwheel r theta xs ys =
  Gg.translate 0 (r*ys) (Gg.scale xs ys (wheel r theta))

-- It makes a wheel centred on the origin, with two spokes
-- at right angles to each other.  The inputs are the radius
-- of the wheel and its angle.
wheel :: Float -> Float -> Gg.Picture
wheel r theta = Gg.pictures [ Gg.circle r
                            , spoke r theta
                            , spoke r (theta+1.57) ]

-- It makes a wheel spoke centered on the origin.  The inputs
-- are the length of the spoke and its angle.
spoke :: Float -> Float -> Gg.Picture
spoke r theta = Gg.line [ (-r*cos theta,-r*sin theta)
                        , (r*cos theta, r*sin theta) ]
  
-- It makes the indicator for the bike lean angle.  The inputs
-- are the front wheel radius, the largest of the two wheel
-- radiuses, and the lean angle.
leanguide :: Float -> Float -> Float -> Gg.Picture
leanguide rr' maxr phi' =
  Gg.translate (-2*rr') 0 (
  Gg.line [(0,0), (-2*maxr*sin phi',2*maxr*cos phi')])

-- It makes the indicator for the bike steering angle.  The
-- inputs are: rear wheel radius, back wheel radius, steering
-- angle, and wheelbase length.
steerguide :: Float -> Float -> Float -> Float -> Gg.Picture
steerguide rr' rf' delta' w' =  
  Gg.translate (w'+2*rf') 0 (
  Gg.line [(0,0), (-maxr*sin delta', maxr*cos delta')])
  where maxr = maximum [rr',rf']

-- It makes the bicycle frame.  The inputs are: rear wheel radius,
-- front wheel radius, length of wheel base, y scaling factor, and
-- steering axis tilt angle.  The y scaling factor is to make the
-- bike look more realistic when leaning.
frame :: Float -> Float -> Float -> Float -> Float -> Gg.Picture
frame rr' rf' w' ys lamd' = 
  Gg.line [ (0,rr'*ys)
            , ( w' - rf'*1.2*sin lamd', ys*(rf' + 1.2*rf'*cos lamd'))
            , (w', ys*rf') ] 

-- It draws the straight line that marks the intended route.  The
-- inputs are the largest of the wheel radiuses and the length of
-- the wheelbase.
routeline :: Float -> Float -> Gg.Picture
routeline maxr w' =
  Gg.line [(-0.8*w',3*maxr), (1.8*w',3*maxr)]

-- It makes the black dot that marks the position of the contact
-- point between the back wheel and the ground.  The inputs are
-- the x and y coordinates, the length of the wheelbase, and the
-- larger of the two wheel radiuses.
backWheelMarker :: Float -> Float -> Float -> Float -> Gg.Picture
backWheelMarker x' y' w' maxr = 
  Gg.translate (x'-0.8*w') (y'+maxr*3) (Gg.circleSolid 3)

-- The inputs are the x and y coordinates of the marker, the larger
-- of the two wheel radiuses, the length of the wheelbase, and the
-- angle of the line connecting the wheel contact points with the
-- x-axis.
frontWheelMarker :: Float -> Float -> Float -> Float -> Float ->
                    Gg.Picture
frontWheelMarker x' y' maxr w' psi' = 
  Gg.translate xpos ypos (Gg.circleSolid 3)
  where
    xpos = x'-0.8*w'+20*cos psi'
    ypos = y'+maxr*3+20*sin psi'

-- The inputs are the radius of the back wheel and the scaling
-- factor for the text.
leanGuideLabel :: Float -> Float -> Gg.Picture
leanGuideLabel rr' textscale =
  Gg.translate (-2.8*rr') (-20) (
    Gg.scale textscale textscale (Gg.text "view from behind"))

sideViewLabel :: Float -> Float -> Gg.Picture
sideViewLabel w' textscale = 
  Gg.translate (w'/2) (-20) (
    Gg.scale textscale textscale (Gg.text "side view"))

steerAngleLabel :: Float -> Float -> Float -> Gg.Picture
steerAngleLabel w' rf' textscale = 
  Gg.translate (w'+1.5*rf') (-20) (
    Gg.scale textscale textscale (Gg.text "steering angle"))

routeLabel :: Float -> Float -> Float -> Gg.Picture
routeLabel w' maxradius textscale =
  Gg.translate (0.02*w') (3*maxradius-20) (
    Gg.scale textscale textscale (Gg.text label))
  where
    label = "position on ground of wheel contact points"
  
labels :: Float -> Float -> Float -> Float -> Gg.Picture
labels rr' rf' w' maxr =
  Gg.pictures
  [ leanGuideLabel rr' textscale
  , sideViewLabel w' textscale
  , steerAngleLabel w' rf' textscale
  , routeLabel w' maxr textscale ]
  -- This is the scaling factor for the text that makes it
  -- about the right size to read easily.
  where textscale = 0.1
  
-- It takes in the state of the bike and makes it into a
-- picture.
toPic :: Bike -> Gg.Picture
toPic b =
  -- It makes a list of pictures into a single picture and translates
  -- it so that it looks roughly in the right position on start-up.
  Gg.translate (-0.5*wmod) (-1*wmod/2) (Gg.pictures
  [ scaledwheel rrmod thetarfloat 1 yscale 
  , Gg.translate wmod 0 (scaledwheel rfmod thetaffloat xscale yscale)
  , frame rrmod rfmod wmod yscale lamdfloat
  , leanguide rfmod maxradius phisafe
  , steerguide rrmod rfmod deltamod wmod
  , routeline maxradius wmod
  , backWheelMarker xmod ymod wmod maxradius
  , frontWheelMarker xmod ymod maxradius wmod psimod
  , labels rrmod rfmod wmod maxradius ] )
  where
    xscale = cos (2*phimod)
    yscale = cos (2*deltamod)
    maxradius = maximum [rrmod, rfmod]
    -- This is the lean angle.  Since the equations of motion are
    -- linearized they are not valid for large lean angles, so this
    -- stops the program and prints an error if the bike leans more
    -- than 0.26 rad ~= 20 degrees.
    phisafe = if abs (phi b) < 0.26
              then d2f (2*phi b)
              else error "The bike has crashed."
    -- This is a scaling factor for lengths.  It is not magic,
    -- it just is about right for fitting the graphics on my
    -- laptop screen.
    f = 250
    d2f a = Gf.double2Float a
    thetarfloat = d2f (thetar b)
    thetaffloat = d2f (thetaf b)
    xmod = d2f (x b)
    ymod = d2f (y b)
    phimod = d2f (phi b)
    deltamod = d2f (delta b)
    rrmod = f * d2f (rr b)
    rfmod = f * d2f (rf b)
    wmod = f * d2f (w b)
    lamdfloat = d2f (lamd b)
    psimod = d2f (psi b)
