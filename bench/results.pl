% Timings obtained on hilly (2015 Retina Macbook Pro 13inch, 2.7GHz Core i5, 8GB memory)
prism_settings(log_scale, on).
prism_settings(epsilon, 1.0e-6).

% system, K, N, T_expl/ms, T_learn/ms, learn_its
dice(prism, 10, 100,  1,  90,  2438).
dice(prism, 20, 100,  2, 171, 1410).
dice(prism, 20, 100,  2, 452, 3289).
dice(prism, 10, 200,  1, 141, 3167).
dice(prism, 10, 500,  1,  49, 1195).
dice(prism, 10, 500,  1,  69, 1785).
dice(prism, 10, 500,  1, 147, 3302).
dice(prism, 40, 100, 10, 5800, 10000).
dice(prism, 40, 100,  8, 7160, 10000).
dice(prism, 40, 100,  9, 5368, 10000).
dice(prism, 40, 100,  8, 4375, 8032).
dice(prism, 40, 200,  9, 5845, 10000).
dice(prism, 40, 200,  8, 9682, 10000).

% ccprism_settings(tolerance, 1.0e-6).
dice(ccp, 10, 100, 11, 19020, 3411).
dice(ccp, 10, 100, 15, 24000, 3988).
dice(ccp, 10, 100, 11, 9650, 1680).
dice(ccp, 20, 100, 32, 167000, 8000).
dice(ccp, 20, 100, 52, 86800, 3725).
% dice(ccp, 10, 200, 83, 19000, 1840). % outlier for expl time
dice(ccp, 10, 100, 28, 14020, 2299).
dice(ccp, 20, 100, 36, 52000, 2418).
% dice(ccp, 10, 500, 142, 30000, 4284).
dice(ccp, 10, 200, 40, 13500, 1840).
dice(ccp, 10, 200, 22, 14900, 1840).
dice(ccp, 20, 100, 43, 76000, 3455).

% ccprism_settings(tolerance, 1.0e-4).
dice(ccp, 10, 500, 78, 3558, 493).
dice(ccp, 10, 200, 34, 3824, 564).


summary(Sys, K, N, MeanExpl, MeanIt) :-
   aggregate(r(bag(T_exp), bag(T_learn), bag(Its)), dice(Sys, K, N, T_exp, T_learn, Its),
             r(ExplTimes, LearnTimes, Its)),
   mean(ExplTimes, MeanExpl),
   sumlist(LearnTimes, LearnTime),
   sumlist(Its, TotalIts),
   MeanIt is LearnTime / TotalIts.

mean(Xs, M) :- sumlist(Xs,Tot), length(Xs,N), M is Tot/N.
