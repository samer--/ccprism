% Timings obtained on hilly (2015 Retina Macbook Pro 13inch, 2.7GHz Core i5, 8GB memory)
prism_settings(log_scale, on).
prism_settings(epsilon, 1.0e-6).

% system, K, N, T_expl/ms, T_learn/ms, learn_its
dice(prism, 10, 100,  1,  90,  2438).
dice(prism, 10, 100,  1,  13,  332).
dice(prism, 10, 100,  1,  66,  1671).
dice(prism, 10, 100,  1, 116,  2708).
dice(prism, 10, 100,  1,  50,  1256).
dice(prism, 10, 100,  1,  46,  1248).

dice(prism, 20, 100,  2, 171, 1410).
dice(prism, 20, 100,  2, 452, 3289).
dice(prism, 20, 100,  3, 687, 5323).
dice(prism, 20, 100,  2, 400, 3070).
dice(prism, 20, 100,  2, 617, 4553).
dice(prism, 20, 100,  3, 1333, 10000).

dice(prism, 10, 200,  1, 141, 3167).
dice(prism, 10, 200,  1, 80, 1952).
dice(prism, 10, 200,  1, 39, 973).
dice(prism, 10, 200,  1, 7, 118).
dice(prism, 10, 200,  1, 143, 3615).
dice(prism, 10, 200,  1, 21, 515).

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

dice(Sys, A, B, C, 0, D, E) :- dice(Sys, A, B, C, D, E).

% ws = weighted sums?
dice(ccpws, 10, 100, 10, 214, 7377, 1507).
dice(ccpws, 10, 100, 15, 250, 7667, 1507).
dice(ccpws, 10, 100, 14, 255, 656, 125).
dice(ccpws, 10, 100, 17, 252, 632, 125).
% dice(ccpws, 10, 100, 15, 263, 3371, 670).
% dice(ccpws, 10, 100, 15, 254, 3416, 670).
dice(ccpws, 10, 100, 11, 259, 4771, 926).
dice(ccpws, 10, 100, 17, 253, 4673, 926).

dice(ccpws, 20, 100, 46, 1691, 19218, 945).
dice(ccpws, 20, 100, 45, 1711, 21751, 945).
dice(ccpws, 20, 100, 45, 1622, 64231, 3383).
dice(ccpws, 20, 100, 32, 1648, 63885, 3383).
% dice(ccpws, 20, 100, 45, 2797, 77503, 3825).
% dice(ccpws, 20, 100, 46, 1757, 58808, 2889).
dice(ccpws, 20, 100, 37, 1802, 4194, 202).
dice(ccpws, 20, 100, 35, 1789, 4302, 202).

dice(ccpws, 10, 200, 32, 264,  4301, 823).
dice(ccpws, 10, 200, 34, 270, 4341, 823).
dice(ccpws, 10, 200, 36, 259, 4908, 918).
dice(ccpws, 10, 200, 16, 267, 4873, 918).
dice(ccpws, 10, 200, 16, 248, 6224, 1189).
dice(ccpws, 10, 200, 34, 263, 6352, 1189).

% ccp_plflow: compile CHR graph to Prolog predicate, _wsum with weighted sums
dice(ccp_plflow_wsum, 20, 100, 54, 2314, 7968, 2993).
dice(ccp_plflow_wsum, 20, 100, 56, 2302, 7945, 2993).
dice(ccp_plflow_wsum, 20, 100, 57, 2174, 3087, 1260).
dice(ccp_plflow_wsum, 20, 100, 47, 2089, 3235, 1260).
dice(ccp_plflow, 20, 100, 53, 2126, 7306, 2993).
dice(ccp_plflow, 20, 100, 52, 2074, 7266, 2993).
dice(ccp_plflow, 20, 100, 57, 2422, 3468, 1260).
dice(ccp_plflow, 20, 100, 57, 2368, 3478, 1260).

dice(ccp_plflow, 10, 200, 47, 383, 77, 106).
dice(ccp_plflow, 10, 200, 42, 378, 88, 106).
dice(ccp_plflow, 10, 200, 44, 389, 357, 454).
dice(ccp_plflow, 10, 200, 43, 393, 356, 454).
dice(ccp_plflow_wsum, 10, 200, 44, 347, 85, 106).
dice(ccp_plflow_wsum, 10, 200, 43, 345, 86, 106).
dice(ccp_plflow_wsum, 10, 200, 42, 362, 332, 454).
dice(ccp_plflow_wsum, 10, 200, 31, 367, 342, 454).

dice(ccp_plflow, 10, 100, 26, 363, 77, 99).
dice(ccp_plflow, 10, 100, 14, 348, 273, 358).
dice(ccp_plflow_wsum, 10, 100, 24, 372, 75, 99).
dice(ccp_plflow_wsum, 10, 100, 19, 370, 257, 358).

% io = old explicitly coded IO algorim
dice(ccpio, 10, 100, 70, 17, 2016, 236).
dice(ccpio, 10, 100, 61, 16, 2026, 236).
dice(ccpio, 10, 100, 61, 19, 6801, 873).
dice(ccpio, 10, 100, 59, 22, 6895, 873).
dice(ccpio, 10, 100, 64, 21, 10100, 1176).
dice(ccpio, 10, 100, 62, 20, 10091, 1176).

dice(ccpio, 20, 100, 230, 87, 110100, 2960).
dice(ccpio, 20, 100, 159, 80, 113740, 2960).
dice(ccpio, 20, 100, 161, 76, 143200, 4070).
dice(ccpio, 20, 100, 162, 76, 100300, 2676).

dice(ccpio, 10, 200, 170, 17, 22550, 2776).
dice(ccpio, 10, 200, 279, 33, 26715, 2776).
dice(ccpio, 10, 200, 154, 18, 10980, 1298).
dice(ccpio, 10, 200, 146, 21, 19874, 1298).
dice(ccpio, 10, 200, 144, 20, 9614, 1138).
dice(ccpio, 10, 200, 147, 19, 9574, 1138).

summary(Sys, K, N, MeanExpl, MeanAD, MeanIt) :-
   aggregate(r(bag(T_exp), bag(T_AD), bag(T_learn), bag(Its)), dice(Sys, K, N, T_exp, T_AD, T_learn, Its),
             r(ExplTimes, ADTimes, LearnTimes, Its)),
   mean(ExplTimes, MeanExpl),
   mean(ADTimes, MeanAD),
   sumlist(LearnTimes, LearnTime),
   sumlist(Its, TotalIts),
   MeanIt is LearnTime / TotalIts.

summary(Sys, K, N, MeanExpl, MeanIt) :-
   aggregate(r(bag(T_exp), bag(T_learn), bag(Its)), dice(Sys, K, N, T_exp, T_learn, Its),
             r(ExplTimes, LearnTimes, Its)),
   mean(ExplTimes, MeanExpl),
   sumlist(LearnTimes, LearnTime),
   sumlist(Its, TotalIts),
   MeanIt is LearnTime / TotalIts.

mean(Xs, M) :- sumlist(Xs,Tot), length(Xs,N), M is Tot/N.

print_table :-
   format('~w ~16|~w ~20|~w ~27|~w ~36|~w ~46|~w\n\n', ['Model','K','N','MeanExpl','MeanAD', 'MeanIt']),
   forall((summary(Model, K, N, MeanExpl, MeanAD, MeanIt),
           \+member(Model,[ccp,ccp_plflow_wsum])),
          format('~w ~16|~w ~20|~w ~27|~2f ~36|~2f ~46|~2f\n', [Model,K,N,MeanExpl,MeanAD, MeanIt])).
