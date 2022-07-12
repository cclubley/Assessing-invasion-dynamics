% Import the data as a numeric matrix

% Extract the year as 'x' and create a new x that is the number of years
% since the first record rather than the actual year itself
x = AOOAll(:,1);
x2 = 0:1:56;
% Extract the Area as 'y'
y = AOOAll(:,2);

% Fit a curve based on the equation in Mineur et al. (2010)
function [fitresult, gof] = createFit(x2, y)
%CREATEFIT(X2,Y)
%  Create a fit.
%
%  Data for fit:
%      X Input : x2
%      Y Output: y
%  Output:
%      fitresult : a fit object representing the fit.
%      gof : structure with goodness-of fit info.
%
%  See also FIT, CFIT, SFIT.

%  Auto-generated by MATLAB on 12-Jul-2022 14:13:15


%% Fit
[xData, yData] = prepareCurveData( x2, y );

% Set up fittype and options.
ft = fittype( '(a*exp(c*x)/k+exp(c*x))*(1-exp(-b*x))', 'independent', 'x', 'dependent', 'y' );
opts = fitoptions( 'Method', 'NonlinearLeastSquares' );
opts.Display = 'Off';
opts.Lower = [0 0 0 0];
opts.MaxFunEvals = 2000;
opts.MaxIter = 2000;
opts.StartPoint = [203 1 0 1];
opts.Upper = [Inf 573 Inf Inf];

% Fit model to data.
[fitresult, gof] = fit( xData, yData, ft, opts );

% Plot fit with data.
figure( 'Name', 'untitled fit 1' );
h = plot( fitresult, xData, yData );
legend( h, 'y vs. x2', 'untitled fit 1', 'Location', 'NorthEast', 'Interpreter', 'none' );
% Label axes
xlabel( 'x2', 'Interpreter', 'none' );
ylabel( 'y', 'Interpreter', 'none' );
grid on


