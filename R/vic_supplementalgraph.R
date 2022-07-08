# Code for supplemental VIC graph
# Illustrates our earnings vs. expectation modeling approach
#
# PSH 7/7/2022

ntrials = 50;
trial_numbers = 1:ntrials

outcome_history = array(dim = c(ntrials,1));

outcome_history[1:10] = runif(10, min = 4, max = 8);
outcome_history[11:25] = runif(15, min = 30, max = 47);
outcome_history[26:35] = runif(10, min = 2, max = 7);
outcome_history[36:50] = runif(15, min = 19, max = 30);

zero_outcomes = sample.int(ntrials, size = 20);

outcome_history[zero_outcomes] = 0;



plot(outcome_history)

earnings = cumsum(outcome_history)
earnings = earnings/max(earnings)
plot(earnings, type = 'l')

trial_numbers_normalized = trial_numbers/ntrials;

line1 = earnings*.5 - trial_numbers_normalized*.3;
line2 = earnings*.5 - trial_numbers_normalized*.5;
line3 = earnings*.5 - trial_numbers_normalized*.7;

plot(trial_numbers, line1, type = 'l', col = 'red', lwd = 2,
     ylim = c(-.2, .2), ylab = 'p(gamble)', xlab = 'Trial Number')
lines(trial_numbers, line2, col = 'blue', lwd = 2)
lines(trial_numbers, line3, col = 'green', lwd = 2)
abline(h = 0, col = 'black', lty = 'dashed', lwd = 2)
abline(v = 25, col = 'black', lty = 'dotted')
points(x = 25, y = line1[25], col = 'red', lwd = 3)
points(x = 25, y = line2[25], col = 'blue', lwd = 3)
points(x = 25, y = line3[25], col = 'green', lwd = 3)

