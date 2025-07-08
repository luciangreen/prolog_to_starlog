% Convert .sl files to .pl files
% Author: luciangreenPlease  
% Date: 2025-07-08
main:-swipl_main([starlog_to_prolog,main_sl]),halt.
swipl_main(A):-consult(starlog_to_prolog.pl),main_sl.
