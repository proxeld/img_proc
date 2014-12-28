%*************************************
% Simplified image record
%*************************************

-record(image, 
	{
		format = undefined, % gray8 | r8g8b8 | r8g8b8a8
		width = 0,	  		% width of a image (number of columns)
		height = 0,   		% height of a image (number of rows)
		matrix = [[]] 		% pixels in format [[integers]] | [[{R,G,B}]]
	}).