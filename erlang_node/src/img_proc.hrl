%*************************************
% Simplified image record
%*************************************

-record(image, 
	{
		format = undefined, % gray8 | gray8a8 | r8g8b8 | r8g8b8a8
		width = 0,	  		% width of a image (number of columns)
		height = 0,   		% height of a image (number of rows)
		alpha,				% alfa channel, if exists [[Num]]
		matrix = [[]] 		% pixels in format [[integers]] | [[{R,G,B}]] | array of arrays of integers		
	}).