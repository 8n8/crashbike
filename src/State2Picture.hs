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

-- This module converts the state of the bicycle into
-- a picture.  The picture type is part of Graphics.Gloss.


module State2Picture ( toPic ) where


import BikeState
import qualified GHC.Float as Gf
import qualified Graphics.Gloss as Gg


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
