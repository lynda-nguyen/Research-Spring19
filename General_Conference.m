%Code by Stephen Olsen

%X matrix from book example:
X = [-1, 1, 0, 0, 0;
    -1, 0, 1, 0, 0;
    -1, 0, 0, 1, 0;
    -1, 0, 0, 0, 1;
    0, 1, -1, 0, 0;
    0, 1, 0, -1, 0;
    0, 1, 0, 0, -1;
    0, 0, 1, -1, 0;
    0, 0, -1, 0, 1;
    0, 0, 0, -1, 1]

%Mtest matrix from book calculated and then final row replaced with 1's:
%Morig created for later use in T calculation
Morig = (X')*(X)
Mtest = (X')*(X)

rows = size(Mtest,1)

Mtest(rows,:) = [ones]

%Manually created M matrices for experimentation
%M = [4, -1, -1, -1, -1; -1, 4, -1, -1, -1; -1, -1, 4, -1, -1; -1, -1, -1, 4, -1; 1, 1, 1, 1, 1]
%M2 = [4, -1, -1, -1, -1; -1, 4, -1, -1, -1; -1, -1, 4, -1, -1; 1, 1, 1, 1, 1; -1, -1, -1, -1, 4]

%P vector from book calculated then final row replaced with 0:
p = [-124, 91, -40, -17, 90]'

p(rows,:) = [0]

%Experimental p vector
%p2 = [-124, 91, -40, 0, 90]'

r = Mtest\p

%Experimental ratings were the same when the second to last row was
%replaced by 1's in the M matrix and 0 in the p vector
%r2 = (M2)\p2

%T matrix from examples in book
T = diag(diag(Morig))

%P matrix from examples in book
P = -tril(Morig)-triu(Morig)+(2*(T))

%(T+P)d = Tr - f
%f is points for each team
%a is points against each team
f = [35, 138, 50, 74, 134]'
a = [159, 47, 90, 91, 44]'

%A = (T+P)
%b = ((T * r) - f)
%Ad = b
%d = A\b

d = (T+P)\((T*r)-f)

o = r - d


%AOffensive score-bdefensive +- /4


disp(ACCScoressheet);


