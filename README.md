# olympic_swimming_simulator

Project Title: Olympic Swimming Race Simulator
Language:      Typed Racket

Motivation and Notes:
  - This project was completed in CMCS 15100 at the Unviersity of Chicago, Fall 2021.    
  - Running the simulation requires the cs151-core, cs151-image, and cs151-universe racket files. 
  - This directory includes sample swm_files which can be used to simulate races.

Description:
  This Olympic Swimming Race Simulator reads text files in a specific format and generates a visual simulation of the
  swimmers moving across a pool. Users can select the races to be displayed from a generated directory of race files,
  enter and exit races, control the speed of simulations, and pause, restart, and reset simulations.

Structure:
  The code file begins with structure definitions used throughout the project, shortcuts for image rendering, and
  a map of country symbols to strings used in the display of flags. The next sections define general polymorphic 
  functions used in the project, general key-and-value identification and split functions, and a clock format function.
  Then, there are functions to determine a swimmer's current position, compute their rank in the race, generate labels
  to be displayed beside each lane, and compute the end-time of the race. The following functions initialize a simulation,
  generate a simulation from a file, enable the user to select files from a directory to simulate and to use keyboard 
  input to control the simulation. In the next section, the functions define the layout of the pool and labels. The final 
  function runs the simulation.
