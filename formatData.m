function [y] = formatData(x)
if ~ischar(x)
    error('Input must be a string')
end

% There are eight columns.
% Place copy and paste the data within single quotes

% Converts the char sequence into numeric values (each its own column)
data = str2num(x);

% If you have teh communications system toolbox use the line below
%table = vec2mat(data,8) 

% reshape function will take the single data string column into a specific
% dimension. However, will insert by going down the row, so we will want to
% transpose this matrix to get the final result
table = reshape(data, [8, length(data)/8]);
table = table'

gameID = table(:, 1);
date = table(:, 2);
team1_ID = table(:, 3);
team1_loc = table(:, 4);
team1_points = table(:, 5);
team2_ID = table(:, 6);
team2_loc = table(:, 7);
team2_points = table(:, 8);

end
