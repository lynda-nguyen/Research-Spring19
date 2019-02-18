function [y] = getSchoolList(x)
if ~ischar(x)
    error('Input must be a string')
end

schools = split(x);

% remove the last cell since it is unnecessary 
schools(length(schools), :) = [];

% Since half of our schools_string is an actual char value, we only to
% extract even even cell of the schools
schools_ID = schools(2:2:end,:);
College = cell2table(schools_ID);

num = 1:length(schools_ID);
num = num';
NumberID = array2table(num);

schools_list = horzcat(NumberID,College)
end
