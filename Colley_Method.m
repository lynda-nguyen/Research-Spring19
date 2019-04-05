% code by Stephen Olsen

%Build X Matrix from Conference Teams Data
games = size(DATA,1); %number of games played

%number of teams in conference
teams = 0;
for i = 1:size(DATA,1)
    if DATA(i, 1) > teams
        teams = DATA(i, 1);
    end
end

%create matrices for later use
XCONF = zeros(games,teams);
p = zeros(teams, 1);
f = zeros(teams, 1);
a = zeros(teams, 1);
wins = zeros(teams, 1);
loss = zeros(teams, 1);

eigen = zeros(teams, teams);
%reig = zeros(teams, 1);


%Loop through the XCONF Matrix and place 1's and -1's
%Also create the P matrix of point differentials
%Create the f and a matrices for points for and points against
%Create the win and loss matrices for Colley
for i = 1:games
    
    %Team 1 data
    col = DATA(i, 1);
    diff1 = DATA(i, 3) - DATA(i, 6);
    for1 = DATA(i, 3);
    against1 = DATA(i, 6);
    
    %Team 2 data
    col2 = DATA(i, 4);
    diff2 = DATA(i, 6) - DATA(i, 3);
    for2 = DATA(i, 6);
    against2 = DATA(i, 3);
    
    %Compute 1 vs -1
    if for1 > for2
        wL = 1;
        wL2 = -1;
    else
        wL = -1;
        wL2 = 1;
    end
    
    %Compute win loss vectors
    if wL == 1
        wins(col) = wins(col) + 1;
    else
        loss(col) = loss(col) + 1;
    end
    
    if wL2 == 1
        wins(col2) = wins(col2) + 1;
    else 
        loss(col2) = loss(col2) + 1;
    end
    
    %Input to XCONF matrix
    XCONF(i, col) = wL;
    XCONF(i, col2) = wL2;
    
    %Input to P vector
    p(col) = p(col) + diff1;
    p(col2) = p(col2) + diff2;
    
    %Input to F vector
    f(col) = f(col) + for1;
    f(col2) = f(col2) + for2;
    
    %Input to A vector
    a(col) = a(col) + against1;
    a(col2) = a(col2) + against2;
    
    %Input to Eigen Matrix
    if wL == 1
        eigen(col, col2) = wL + eigen(col, col2);
    else
        eigen(col2, col) = wL2 + eigein(col2, col);
    end
    
end

%Creation of M matrix
M1 = (XCONF')*(XCONF);
M2 = (XCONF')*(XCONF);

rows = size(M2,1);

M2(rows,:) = ones;

p(rows,:) = 0;

%Massey Ratings for teams
r = M2\p;

%T and P matrices
T = diag(diag(M1));
P = -tril(M1)-triu(M1)+(2*(T));

%Offensive and Defensive Ratings from Massey
d = (T+P)\((T*r)-f);

o = r - d;

%Calculating b vector

b = zeros(teams, 1);

for i = 1:teams
    
    b(i) = 1 + ((1/2)*(wins(i) - loss(i)));
    
end

%Calculating C matrix
C = M1 + 2*eye(teams, teams);

%Colley Ratings
rcolley = C\b;

%Colleyize the Massey
rmasscol = C\p;

%Output for analysis
combo = [rcolley, rmasscol, r, o, d];

t = 1:14;

Output = [t', combo];

filename = 'SECMassColley03_13_18.csv';

csvwrite(filename, Output, 0, 0)


%Work from 3-13-18 with Eigen Method

[u v] = eig(eigen);

%diag(v);
%sum(u(:,1));
%u(:,1);
%(u(:,1))./norm(u(:,1));
%norm(u);

t = 1:14;
[t' u(:,1)];

[I X] = sort(u(:,1));
[X I]

