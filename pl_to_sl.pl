% Convert .pl files to .sl files
% Author: luciangreenPlease
% Date: 2025-07-08
main:-swipl_main([prolog_to_starlog,main_sl]),halt.
swipl_main(A):-consult(prolog_to_starlog.pl),main_sl.
