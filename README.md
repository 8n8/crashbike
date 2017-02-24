# Description

Crashbike is a Haskell program to simulate the motion of a bicycle and display the results.  It uses a bicycle model and linearized equations of motion taken from (Meijaard 07). 

The bicycle travels at a constant velocity over a perfectly smooth flat surface.  It is excited by random sideways forces.

The bicycle is controlled by two PID controllers, one to keep it upright and the other to keep it travelling along the desired route.  The controllers work by adjusting the torque applied to the steering.

# Installation

I have only tested the installation with Arch Linux.  Install the Haskell Stack Toolbox from the official repositories.  Also install blas and lapack for linear algebra, if necessary (they were already installed on my machine).  Clone this git repository and then run 'stack build' (without the quotes) in it.  The first build will take a while, because stack will have to download the compiler and all the libraries.

# Usage

Run the program with 'stack exec crashbike <arguments>'.  The arguments are:

1. the path of the file containing the parameters, such as the 'standardbike.ini' file provided here
2. (optional) the word "tune".  This will attempt to find the optimimum PID parameters for the controller that keeps the bike upright.

# References

J.P. Meijaard et al. "Linearized dynamics equations
for the balance and steer of a bicycle: a benchmark
and review". In: Proceedings of the Royal Society of
London A: Mathematical, Physical and Engineering
Sciences 463.2084 (2007), pp. 1955-1982. ISSN:
1362-5021. DOI: 10.1098/rspa . 2007 . 1857. eprint:
http://rspa.royalsocietypublishing.org/content/463/ 2084/1955.full.pdf. 
