% Starlog Math Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% Simple math
double(X, Result) :- Result is X * 2.
triple(X, Result) :- Result is X * 3.

% Area calculations
area_rectangle(Width, Height, Area) :- 
    Area is Width * Height.
    
% Volume calculations
volume_box(Width, Height, Depth, Volume) :-
    Area is Width * Height,
    Volume is Area * Depth.
