% Test3: Math operations
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Math operations
double(X, Y) :- Y is X * 2.
triple(X, Y) :- Y is X * 3.
area_rectangle(Width, Height, Area) :- 
    Area is Width * Height.
volume_box(Width, Height, Depth, Volume) :-
    Area is Width * Height,
    Volume is Area * Depth.
